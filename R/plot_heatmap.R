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

  ## Create rectangle to plot parameter values on the heatmap plot
  ###############################################################

  ### Surany Parameters
  # rect_data <- data.frame(xmin = 1, xmax = 4, ymin = 120, ymax = 600)

  ### Surany Parameters
  # rect_data <- data.frame(xmin = 1, xmax = 1.9, ymin = 500, ymax = 2000)

  ### Vienna / Tahi (10 PFAS) Parameters
  #rect_data <- data.frame(xmin = 1, xmax = 2.9, ymin = 500, ymax = 2000)

  ################################################################

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
    # metR::geom_text_contour(aes(z = Cx), breaks = c(0.9,0.5,0.1), stroke = 0.1, skip = 0,
    # label.placer = label_placer_fraction(0.1)) +# Add contour lines with labels

    # Vienna CS contours
    ggplot2::geom_contour(
      ggplot2::aes(z = .data$Cx),
      breaks = measured_C_C0,
      color = "white",
      linewidth = 1.5
    )
  }

  heatmap_plot <- heatmap_plot +

    # C/C0 10 PFAS MW1
    # geom_contour(aes(z=Cx), breaks = c(0.96), color = "yellow", size = 1)+ # C/C0 Carbamazepine
    # geom_contour(aes(z=Cx), breaks = c(0.09), color = "hotpink", size = 1)+ # C/C0 Diclofenac
    # geom_contour(aes(z=Cx), breaks = c(0.98,0.8), color = "cornflowerblue", size = 0.75, linetype = "dashed")+ # C/C0 10 PFAS Min/Max

    # ## metR::geom_text_contour(aes(z = Cx), breaks = c(0.98,0.96,0.93,0.86,0.8, 0.09),
    # ##stroke = 0.1, skip = 0, label.placer = label_placer_fraction(0.01)) +# Add contour lines for 0.1 to 0.9 #Vienna CS values
    # #  rectangle

    # ## Tahi CS contours
    # geom_contour(aes(z=Cx), breaks = c(0.73), color = "white", size = 2)+ # Tahi (Budapest) CS
    # metR::geom_text_contour(aes(z = Cx), breaks = c(0.73),
    # stroke = 0.1, skip = 0, label.placer = label_placer_fraction(0.01)) +# Add contour lines for 0.1 to 0.9 #Tahi Budapest CS values
    #
    #   ## Surany CS contours
    #
    #     geom_contour(aes(z=Cx), breaks = c(0.8), color = "white", size = 2)+ # Surany (Budapest) CS
    ## # Surany Rectangle
    #   #metR::geom_text_contour(aes(z = Cx), breaks = c(0.8),
    #stroke = 0.1, skip = 0, label.placer = label_placer_fraction(0.31)) +# Add contour lines for 0.1 to 0.9 #Tahi Budapest CS values
    # # Surany Rectangle
    # geom_rect(data = rect_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    #             fill = "grey", color = "grey", alpha = 0.2) +

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
