#' @export
theme_config <- list(
  # Base colors
  colors = list(
    primary = "#1f77b4",
    secondary = "#E74C3C",
    neutral = "#95A5A6",
    highlight = "#3498DB",
    background = "#FFFFFF",
    text = "#2C3E50"
  ),
  
  # Categorical palettes
  palettes = list(
    district = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                 "#8c564b", "#e377c2", "#bcbd22", "#17becf"),
    gender = c("#FF69B4", "#4169E1"),
    age_group = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854"),
    sequential = colorRampPalette(c("#FFFFFF", "#2C3E50"))(9),
    diverging = colorRampPalette(c("#E74C3C", "#FFFFFF", "#3498DB"))(11)
  ),
  
  # Typography
  typography = list(
    font_family = "Arial",
    sizes = list(
      title = 16,
      subtitle = 14,
      axis = 12,
      text = 10
    )
  ),
  
  # Layout
  layout = list(
    margin = list(l = 50, r = 20, t = 50, b = 50),
    padding = list(
      small = 5,
      medium = 10,
      large = 20
    )
  )
)

#' Create consistent plotly theme
#' @param p plotly object
#' @param title plot title
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @export
apply_plotly_theme <- function(p, title = "", xlab = "", ylab = "") {
  p %>%
    layout(
      title = list(
        text = title,
        font = list(
          family = theme_config$typography$font_family,
          size = theme_config$typography$sizes$title,
          color = theme_config$colors$text
        )
      ),
      xaxis = list(
        title = xlab,
        titlefont = list(
          family = theme_config$typography$font_family,
          size = theme_config$typography$sizes$axis,
          color = theme_config$colors$text
        ),
        tickfont = list(
          family = theme_config$typography$font_family,
          size = theme_config$typography$sizes$text
        ),
        gridcolor = theme_config$colors$neutral,
        showgrid = TRUE
      ),
      yaxis = list(
        title = ylab,
        titlefont = list(
          family = theme_config$typography$font_family,
          size = theme_config$typography$sizes$axis,
          color = theme_config$colors$text
        ),
        tickfont = list(
          family = theme_config$typography$font_family,
          size = theme_config$typography$sizes$text
        ),
        gridcolor = theme_config$colors$neutral,
        showgrid = TRUE
      ),
      paper_bgcolor = theme_config$colors$background,
      plot_bgcolor = theme_config$colors$background,
      margin = theme_config$layout$margin
    ) %>%
    config(displayModeBar = FALSE)
}



#' Create color palette function
#' @param palette_name name of the palette from theme_config$palettes
#' @param reverse boolean to reverse the palette
#' @export
get_color_palette <- function(palette_name, reverse = FALSE) {
  palette <- theme_config$palettes[[palette_name]]
  if(reverse) palette <- rev(palette)
  return(palette)
}

plot_functions <- list(
  # Bar plot with dynamic color handling
  bar = function(data, x, y, title = "", xlab = "", ylab = "", 
                orientation = "v", color_by = NULL, group_var = NULL) {
    # Determine colors based on grouping variable
    if (!is.null(color_by) && color_by %in% names(theme_config$palettes)) {
      colors <- theme_config$palettes[[color_by]]
    } else {
      colors <- theme_config$colors$primary
    }
    
    # Create the plot
    if(orientation == "v") {
      if (!is.null(group_var)) {
        p <- plot_ly(
          data = data,
          x = as.formula(paste0("~", x)),
          y = as.formula(paste0("~", y)),
          type = "bar",
          color = as.formula(paste0("~", group_var)),
          colors = colors
        )
      } else {
        p <- plot_ly(
          data = data,
          x = as.formula(paste0("~", x)),
          y = as.formula(paste0("~", y)),
          type = "bar",
          marker = list(color = colors)
        )
      }
    } else {
      if (!is.null(group_var)) {
        p <- plot_ly(
          data = data,
          y = as.formula(paste0("~", x)),
          x = as.formula(paste0("~", y)),
          type = "bar",
          orientation = 'h',
          color = as.formula(paste0("~", group_var)),
          colors = colors
        )
      } else {
        p <- plot_ly(
          data = data,
          y = as.formula(paste0("~", x)),
          x = as.formula(paste0("~", y)),
          type = "bar",
          orientation = 'h',
          marker = list(color = colors)
        )
      }
    }
    
    apply_plotly_theme(p, title = title, xlab = xlab, ylab = ylab)
  },
  
  # Line plot with dynamic color handling
  line = function(data, x, y, title = "", xlab = "", ylab = "", 
                 color_by = NULL, group_var = NULL) {
    # Determine colors based on grouping variable
    if (!is.null(color_by) && color_by %in% names(theme_config$palettes)) {
      colors <- theme_config$palettes[[color_by]]
    } else {
      colors <- theme_config$colors$primary
    }
    
    # Create the plot
    if (!is.null(group_var)) {
      p <- plot_ly(
        data = data,
        x = as.formula(paste0("~", x)),
        y = as.formula(paste0("~", y)),
        type = "scatter",
        mode = "lines",
        color = as.formula(paste0("~", group_var)),
        colors = colors
      )
    } else {
      p <- plot_ly(
        data = data,
        x = as.formula(paste0("~", x)),
        y = as.formula(paste0("~", y)),
        type = "scatter",
        mode = "lines",
        line = list(color = colors)
      )
    }
    
    apply_plotly_theme(p, title = title, xlab = xlab, ylab = ylab)
  },
  
  # Box plot with dynamic color handling
  box = function(data, x, y, title = "", xlab = "", ylab = "", 
                color_by = NULL, group_var = NULL) {
    # Determine colors based on grouping variable
    if (!is.null(color_by) && color_by %in% names(theme_config$palettes)) {
      colors <- theme_config$palettes[[color_by]]
    } else {
      colors <- theme_config$colors$primary
    }
    
    # Create the plot
    if (!is.null(group_var)) {
      p <- plot_ly(
        data = data,
        x = as.formula(paste0("~", x)),
        y = as.formula(paste0("~", y)),
        type = "box",
        color = as.formula(paste0("~", group_var)),
        colors = colors
      )
    } else {
      p <- plot_ly(
        data = data,
        x = as.formula(paste0("~", x)),
        y = as.formula(paste0("~", y)),
        type = "box",
        marker = list(color = colors)
      )
    }
    
    apply_plotly_theme(p, title = title, xlab = xlab, ylab = ylab)
  }
)