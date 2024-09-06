#' Plot Heatmap
#'
#' @param bear1d_list Bear1D list as retrieved by \code{\link{calculate_bear1d}}
#'
#' @return heatmap plot
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows
#' @importFrom  ggplot2 geom_tile geom_rect scale_fill_gradientn theme_bw
#' theme element_text xlab ylab labs aes
plot_heatmap <- function(bear1d_list) {



  ## Create rectangle to plot parameter values on the heatmap plot
  ###############################################################
  # rect_data <- data.frame( ### Surany Parameters
  #   xmin = 1, xmax = 4,
  #   ymin = 120, ymax = 600
  # )
  # rect_data <- data.frame( ### Surany Parameters
  #  xmin = 1, xmax = 1.9,
  #  ymin = 500, ymax = 2000
  # )
  rect_data <- data.frame( ### Vienna / Tahi (10 PFAS) Parameters
    xmin = 1, xmax = 2.9,
    ymin = 500, ymax = 2000
  )
  ################################################################


  # Create a heatmap using ggplot2
  heatmap_plot <- bear1d_list %>%
    dplyr::bind_rows() %>%
    ggplot2::ggplot(ggplot2::aes(y = .data$hl,
                                 x = .data$log_koc,
                                 fill = .data$Cx)) +
    ggplot2::geom_tile() +
    ggplot2::geom_rect(data = rect_data,
                       ggplot2::aes(xmin = .data$xmin,
                                    xmax = .data$xmax,
                                    ymin = .data$ymin,
                                    ymax = .data$ymax),
                       fill = "lightgrey",
                       color = "black",
                       alpha = 0.3,
                       inherit.aes = FALSE) +
    # Generic contours
    # geom_contour(aes(z = Cx), breaks = c(0.1, 0.2, 0.3, 0.4, 0.5,
    #                                      0.6, 0.7, 0.8, 0.9), color = "white", size = 0.5) + # add contour lines
    # metR::geom_text_contour(aes(z = Cx), breaks = c(0.9,0.5,0.1), stroke = 0.1, skip = 0,
    # label.placer = label_placer_fraction(0.1)) +# Add contour lines with labels

    # Vienna CS contours
    ggplot2::geom_contour(aes(z=.data$Cx), breaks = c(0.93), color = "white", size = 1.5) + # C/C0 10 PFAS MW1
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
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 18),  # Adjust the size as needed
                   axis.text.y = ggplot2::element_text(size = 18),
                   axis.title.x = ggplot2::element_text(size = 18),
                   axis.title.y = ggplot2::element_text(size = 18))  +
    ggplot2::labs(
      title = paste0(
        "Residence time[d]=",round(x_values/v_values, 0),
        "  foc[-]= ", foc_values
      ),
      x = "Log Koc",
      y = "Half-life (days)",
      fill = (bquote((C/C[0]))))

  # Display the heatmap
  print(heatmap_plot)
}
