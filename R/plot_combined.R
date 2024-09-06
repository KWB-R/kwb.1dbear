#' Plot Combined
#'
#' @param bear1d_list Bear1D list as retrieved by \code{\link{calculate_bear1d}}
#'
#' @return combined plot
#' @export
#' @importFrom rlang .data
#' @importFrom  ggplot2 ggplot theme_bw geom_line scale_color_gradient2
#' scale_y_continuous scale_x_continuous theme element_text xlab ylab labs
plot_combined <- function(bear1d_list) {

  combined_plot <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::xlab("Elapsed Time (days)") +
    ggplot2::ylab(bquote(Concentration ~ (C/C[0])))  +

    # Enlarge axis + tick labels
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20),  # Adjust the size as needed
                   axis.text.y = ggplot2::element_text(size = 20),
                   axis.title.x = ggplot2::element_text(size = 20),
                   axis.title.y = ggplot2::element_text(size = 20))


  # Add lines for each combination with different color for hl
  for (data in bear1d_list) {
    combined_plot <- combined_plot +
      ggplot2::geom_line(
        data = data,
        ggplot2::aes_string(x = "time",
                            y = "Cx",
                            linetype = as.factor("log_koc"),
                            color = "hl"),
        size = 1.25, # Adjust line width if needed
      )
  }

  # Change legend labels for color and linetype
  # Use a gradient of light blue to red, with purple middle to avoid grayness
  combined_plot <- combined_plot +
    ggplot2::scale_color_gradient2(
      low = "red",
      mid = "purple",
      high = "blue",
      midpoint = mean(hl_values)
    ) +
    ggplot2::labs(linetype = "Log Koc", color = "Half-Life") +

    # Adjust the y-axis tick spacing
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1.4, by = 0.2)
    ) + # Adjust the tick spacing as needed
    ggplot2::scale_x_continuous(
      limits= c(0,max(t_values)),
      breaks = seq(0,max(t_values), by= 200)
    )

  # Print the combined plot
  combined_plot
}
