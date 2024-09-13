#' Plot Heatmap
#'
#' @param bear1d_list Bear1D list as retrieved by \code{\link{calculate_bear1d}}
#' @param rect_data data.frame with xmin/xmax/ymin/ymax parameters (default: NULL)
#' @param measured_C_C0 in percent (default: NULL)
#' @return heatmap plot
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows
#' @importFrom  ggplot2 geom_tile geom_rect scale_fill_gradientn theme_bw
#' theme element_text xlab ylab labs aes
plot_heatmap <- function(bear1d_list, rect_data = NULL, measured_C_C0 = NULL)
{
  inputs <- attr(bear1d_list, "inputs")


  text_element <- ggplot2::element_text(size = 18)

  data <- dplyr::bind_rows(bear1d_list)

  # Create a heatmap using ggplot2
  heatmap_plot <- data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$log_koc,
      y = .data$hl
    )) +
    ggplot2::geom_tile(ggplot2::aes(
      fill = .data$Cx
    ))

  if(!is.null(rect_data)) {
  heatmap_plot <- heatmap_plot +
    ggplot2::geom_rect(
      data = rect_data,
      ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax
      ),
      fill = "lightgrey",
      color = "black",
      alpha = 0.3,
      inherit.aes = FALSE
    )
  }


  if(!is.null(measured_C_C0)) {
  heatmap_plot <- heatmap_plot +
    # Generic contours
    # geom_contour(aes(z = Cx), breaks = c(0.1, 0.2, 0.3, 0.4, 0.5,
    #                                      0.6, 0.7, 0.8, 0.9), color = "white", size = 0.5) + # add contour lines

    # Vienna CS contours
    ggplot2::geom_contour(
      ggplot2::aes(z = .data$Cx),
      breaks = measured_C_C0,
      color = "white",
      linewidth = 1.5
    )
  }

  heatmap_plot <- heatmap_plot +


    ggplot2::scale_fill_gradientn(
      limits = c(0, 1),
      #colors=  viridis(5),
      #colors = c("red", "orange","green", "blue","black"),
      colors = c("darkblue","blue","green","orangered","darkred"),
      breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = text_element,
      axis.text.y = text_element,
      axis.title.x = text_element,
      axis.title.y = text_element
    ) +
    ggplot2::labs(
      title = sprintf(
        "Residence time[d] = %s, foc[-] = %s",
        round(inputs$x_values/inputs$v_values, 0),
        paste(inputs$foc_values, collapse = ",")
      ),
      x = "Log Koc",
      y = "Half-life (days)",
      fill = (bquote((C/C[0])))
    )

  # Return the heatmap plot
  heatmap_plot
}
