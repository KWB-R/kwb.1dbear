#' Plot Combined
#'
#' @param bear1d_list Bear1D list as retrieved by \code{\link{calculate_bear1d}}
#' @param text_size size of plot title and axis labels, default: 20
#' @param line_width line width, default: 1.25
#' @return combined plot
#' @export
#' @importFrom rlang .data
#' @importFrom  ggplot2 ggplot theme_bw geom_line scale_color_gradient2
#' scale_y_continuous scale_x_continuous theme element_text xlab ylab labs
plot_combined <- function(bear1d_list, text_size = 20, line_width = 1.25)
{
  inputs <- attr(bear1d_list, "inputs")

  # Enlarged axis + tick labels
  text_element <- ggplot2::element_text(size = text_size)

  combined_plot <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::xlab("Elapsed Time (days)") +
    ggplot2::ylab(bquote(Concentration ~ (C/C[0]))) +
    ggplot2::theme(
      axis.text.x = text_element,
      axis.text.y = text_element,
      axis.title.x = text_element,
      axis.title.y = text_element
    )

  # Add lines for each combination with different color for hl
  for (data in bear1d_list) {
    combined_plot <- combined_plot + ggplot2::geom_line(
      data = data,
      ggplot2::aes(
        x = .data$time,
        y = .data$Cx,
        linetype = as.factor(.data$log_koc),
        color = .data$hl
      ),
      size = line_width
    )
  }

  # Change legend labels for color and linetype
  # Use a gradient of light blue to red, with purple middle to avoid grayness
  tmax <- max(inputs$t_values)

  combined_plot +
    ggplot2::scale_color_gradient2(
      low = "red",
      mid = "purple",
      high = "blue",
      midpoint = mean(inputs$hl_values)
    ) +
    ggplot2::labs(linetype = "Log Koc", color = "Half-Life") +

    # Adjust the y-axis tick spacing
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1.4, by = 0.2)
    ) + # Adjust the tick spacing as needed
    ggplot2::scale_x_continuous(
      limits = c(0, tmax),
      breaks = seq(0, tmax, by = 200)
    )

   #min(inputs$log_koc_values)
   #max(inputs$log_koc_values)
}
