## new title: kwb.1dbear
# Install and load the profvis package (if not already installed)
# install.packages("profvis")
# Wrap your code with profvis (we have to use debug modes for models that take long to calculate..)
# load packages
library(ggplot2)
library(pracma)
library(patchwork)
library(profvis)
library(metR)



# Define the functions
###################
calculate_e <- function(k, D, retardation, v, t) {
  sqrt(1 + 4 * k * D * retardation / v^2)
}

calculate_f <- function(D, retardation, t) {
  2 * sqrt((D / retardation) * t)
}

calculate_g <- function(x, v, D) {
  x * v / (2 * D)
}

calculate_Cx <- function(x, t, v, D, retardation, C0, k) {
  e <- calculate_e(k, D, retardation, v, t)
  f <- calculate_f(D, retardation, t)
  g <- calculate_g(x, v, D)
  C0 / 2 * (exp(g * (1 - e)) * erfc((x - v * t / retardation * e) / f) +
              exp(g * (1 + e)) * erfc((x + v * t / retardation * e) / f))
}

##################




## # input parameter
###############################

# # # Generic
# n_values <- c(0.3) #Porosity [-]
# rs_values <- c(2.65) # density solids [g/g]
# foc_values <- c(0.0001) #fraction of organic matter
# log_koc_values <- c(1,4) # min, max Koc for relevant compounds Nguyen et al 2020
#
# # # input parameter degradation
# hl_values <- c(120,600) #half life
# C0 <- 1 #initial concentration of the solute [M/L3]
#
# # # input parameter geo-hydraulics Note: with higher velocities, low dispersion may result in model failing to produce result
# D_values <- c(2) #coefficient of longitudinal dispersion [L2/T]
# v_values <- c(1) #average linear ground water velocity [L/T]
# t_values <- seq(0, 1000, by=1) #time [T]
# x_values <- c(100) # 10m, 50m, 100m distance [L]

##################################
#
# Vienna
n_values <- c(0.15) #Porosity [-]
rs_values <- c(2.7) # density solids [g/g]
foc_values <- c(0.002) #fraction of organic matter
log_koc_values <- c(1,2.9) # min, max Koc for relevant compounds Nguyen et al 2020

# # input parameter degradation
hl_values <- c(500,2000) #half life
C0 <- 1 #initial concentration of the solute [M/L3]

# # input parameter geo-hydraulics Note: with higher velocities, low dispersion may result in model failing to produce result
D_values <- c(10) #coefficient of longitudinal dispersion [L2/T]
v_values <- c(21.3) #average linear ground water velocity [L/T]
t_values <- seq(0, 350, by=1) #time [T]
x_values <- c(141) #distance [L]
##################################
#
# # Budapest Tahi
# n_values <- c(0.3) #Porosity [-]
# rs_values <- c(2.7) # density solids [g/g]
# foc_values <- c(0.002) #fraction of organic matter
# log_koc_values <- c(1,2.9) # PFAS low, mid and higher Koc for relevant compounds
#
# # input parameter degradation
# hl_values <- c(500,2000) #half life
# C0 <- 1 #initial concentration of the solute [M/L3]
#
# # input parameter geo-hydraulics Note: with higher velocities, low dispersion may result in model failing to produce result
# D_values <- c(10) #coefficient of longitudinal dispersion [L2/T]
# v_values <- c(2.14) #average linear ground water velocity [L/T] 10-20 m/d
# t_values <- seq(0, 700, by=1) #time [T]
# x_values <- c(60) #distance [L]
# # ##################################
#
# # # # # # Budapest Surany
# n_values <- c(0.3) #Porosity [-]
# rs_values <- c(2.7) # density solids [g/g]
# foc_values <- c(0.002) #fraction of organic matter
# log_koc_values <- c(1,1.9) # PFAS low, mid and higher Koc for relevant compounds
#
# # input parameter degradation
# hl_values <- c(500,2000) #half life
# C0 <- 1 #initial concentration of the solute [M/L3]
#
# # input parameter geo-hydraulics Note: with higher velocities, low dispersion may result in model failing to produce result
# D_values <- c(10) #coefficient of longitudinal dispersion [L2/T]
# v_values <- c(1.38) #average linear ground water velocity [L/T]
# t_values <- seq(0, 700, by=1) #time [T]
# x_values <- c(228) #distance [L]

###################################


# Generate combinations for multi-value parameters
multi_value_params <- list(
  n = n_values,
  rs = rs_values,
  foc = foc_values,
  log_koc = log_koc_values,
  hl = hl_values
)

combinations <- expand.grid(multi_value_params)

# Create an empty list to store ggplot objects
plot_list <- list()

# Loop through combinations
for (i in 1:nrow(combinations)) {
  param_combination <- combinations[i, ]

  # Extract individual parameter values
  n <- param_combination$n
  rs <- param_combination$rs
  foc <- param_combination$foc
  log_koc <- param_combination$log_koc
  hl <- param_combination$hl

  # Calculate other parameters
  koc <- 10^(log_koc)
  rb <- rs * (1 - n) #bulk density
  kd <- koc * foc
  retardation <- 1 + (rb * kd / n)
  k <- 0.693 / hl #lambda aqueous phase decay constant [1/T]

  # Create a data frame for this parameter combination
  result <- data.frame(
    Cx = calculate_Cx(x_values, t_values, v_values, D_values, retardation, C0, k),
    time = rep(t_values, length(x_values)),
    retardation = rep(retardation, length(t_values) * length(x_values)),
    v = rep(v_values, length(t_values) * length(x_values)),
    hl = rep(hl, length(t_values) * length(x_values)),
    koc = rep(koc, length(t_values) * length(x_values)),
    foc = rep(foc, length(t_values) * length(x_values)),
    distance = rep(x_values, each = length(t_values)),
    dispersion = rep(D_values, each = length(t_values) * length(x_values)),
    Kd = rep(kd, length(t_values) * length(x_values)),
    log_koc = rep(log_koc, length(t_values) * length(x_values))
  )

  # Store the data frame in the list
  plot_list[[as.character(i)]] <- result
}

plot_list


bear1d_vienna_combined <- kwb.1dbear::calculate_bear1d(
  n_values = 0.15,
  rs_values = 2.7,
  foc_values = 0.002,
  log_koc_values = c(1,2.9),
  hl_values = c(500,2000),
  C0 = 1,
  D_values = 10,
  v_values = 21.3,
  t_values = seq(0, 350, by=1),
  x_values = 141
)


# Combine the plots
plot_combined(bear1d_list = plot_list)
plot_combined(bear1d_list = bear1d_vienna_combined)


# generate heat map

# heat map
####################

log_koc_values <- seq(0.1, 6, length.out = 100) #
hl_values <- seq(1, 2000, length.out = 100) #half life time
t_values <- c(max(hl_values * max(log_koc_values))) #time [T]


bear1d_vienna_heatmap <- kwb.1dbear::calculate_bear1d(
  n_values = 0.15,
  rs_values = 2.7,
  foc_values = 0.002,
  log_koc_values = seq(0.1, 6, length.out = 100),
  hl_values =  seq(1, 2000, length.out = 100),
  C0 = 1,
  D_values = 10,
  v_values = 21.3,
  t_values = max(hl_values * max(log_koc_values)),
  x_values = 141
)

kwb.1dbear::plot_heatmap(bear1d_vienna_heatmap)


# Generate combinations for multi-value parameters
multi_value_params <- list(
  n = n_values,
  rs = rs_values,
  foc = foc_values,
  log_koc = log_koc_values,
  hl = hl_values
)

combinations <- expand.grid(multi_value_params)

# Create an empty list to store ggplot objects
result_heatmap <- data.frame()
# Loop through combinations
for (i in 1:nrow(combinations)) {
  param_combination <- combinations[i, ]

  # Extract individual parameter values
  n <- param_combination$n
  rs <- param_combination$rs
  foc <- param_combination$foc
  log_koc <- param_combination$log_koc
  hl <- param_combination$hl

  # Calculate other parameters
  koc <- 10^(log_koc)
  rb <- rs * (1 - n) #bulk density
  kd <- koc * foc
  retardation <- 1 + (rb * kd / n)
  k <- 0.693 / hl #lambda aqueous phase decay constant [1/T]

  # Create a data frame for this parameter combination
  result <- data.frame(
    Cx = calculate_Cx(x_values, t_values, v_values, D_values, retardation, C0, k),
    time = rep(t_values, length(x_values)),
    retardation = rep(retardation, length(t_values) * length(x_values)),
    v = rep(v_values, length(t_values) * length(x_values)),
    hl = rep(hl, length(t_values) * length(x_values)),
    koc = rep(koc, length(t_values) * length(x_values)),
    foc = rep(foc, length(t_values) * length(x_values)),
    distance = rep(x_values, each = length(t_values)),
    dispersion = rep(D_values, each = length(t_values) * length(x_values)),
    Kd = rep(kd, length(t_values) * length(x_values)),
    log_koc = rep(log_koc, length(t_values) * length(x_values))
  )

  # Store the data frame in the list
  result_heatmap <- rbind(result_heatmap,result)

}
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

result_heatmap <- bear1d_vienna_heatmap

# Create a heatmap using ggplot2
heatmap_plot <- ggplot(result_heatmap, aes(y = hl, x = log_koc, fill = Cx)) +
  geom_tile() +
  geom_rect(data = rect_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "lightgrey", color = "black", alpha = 0.3) +
  # Generic contours
  # geom_contour(aes(z = Cx), breaks = c(0.1, 0.2, 0.3, 0.4, 0.5,
  #                                      0.6, 0.7, 0.8, 0.9), color = "white", size = 0.5) + # add contour lines
  # metR::geom_text_contour(aes(z = Cx), breaks = c(0.9,0.5,0.1), stroke = 0.1, skip = 0,
  # label.placer = label_placer_fraction(0.1)) +# Add contour lines with labels

  # Vienna CS contours
  geom_contour(aes(z=Cx), breaks = c(0.93), color = "white", size = 1.5)+ # C/C0 10 PFAS MW1
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

  scale_fill_gradientn(
    limits = c(0, 1),
    #colors=  viridis(5),
    #colors = c("red", "orange","green", "blue","black"),
    colors = c("darkblue","blue","green","orangered","darkred"),
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)
  ) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 18),  # Adjust the size as needed
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))  +
  labs(title = paste0("Residence time[d]=",round(x_values/v_values, 0),
                      "  foc[-]= ",foc_values),
       x = "Log Koc",
       y = "Half-life (days)",
       fill = (bquote((C/C[0]))))

# Display the heatmap
print(heatmap_plot)

