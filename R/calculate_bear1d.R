#' Calculate Bear 1D
#'
#' @param n_values Porosity (-)
#' @param rs_values density solids (g/g)
#' @param foc_values fraction of organic matter
#' @param log_koc_values min, max Koc for relevant compounds Nguyen et al 2020
#' @param hl_values half life (T)
#' @param C0 initial concentration of the solute (M/L3)
#' @param D_values coefficient of longitudinal dispersion (L2/T)
#' @param v_values average linear ground water velocity (L/T)
#' @param t_values time (T), default: max(hl_values * max(log_koc_values))
#' @param x_values flow path distance (L)
#'
#' @return Bear 1D results
#' @export
#'
#' @examples
#' bear1d_vienna <- calculate_bear1d(n_values = 0.15,
#' rs_values = 2.7,
#' foc_values = 0.002,
#' log_koc_values = c(1,2.9),
#' hl_values = c(500,2000),
#' C0 = 1,
#' D_values = 10,
#' v_values = 21.3,
#' t_values = seq(0, 350, by=1),
#' x_values = 141
#' )
calculate_bear1d <- function(
    n_values,
    rs_values,
    foc_values,
    log_koc_values,
    hl_values,
    C0 = 1,
    D_values,
    v_values,
    t_values = max(hl_values * max(log_koc_values)),
    x_values
)
{
  # Generate combinations for multi-value parameters
  multi_value_params <- list(
    n = n_values,
    rs = rs_values,
    foc = foc_values,
    log_koc = log_koc_values,
    hl = hl_values
  )

  combinations <- expand.grid(multi_value_params)

  # Loop through combinations
  result <- lapply(seq_len(nrow(combinations)), function(i) {

    combination <- combinations[i, ]

    # Extract individual parameter values
    n <- combination$n
    rs <- combination$rs
    foc <- combination$foc
    log_koc <- combination$log_koc
    hl <- combination$hl

    # Calculate other parameters
    koc <- 10^(log_koc)
    rb <- rs * (1 - n) #bulk density
    kd <- koc * foc
    retardation <- 1 + (rb * kd / n)
    k <- 0.693 / hl #lambda aqueous phase decay constant [1/T]

    # Create a data frame for this parameter combination
    size <- length(t_values) * length(x_values)

    data.frame(
      Cx = calculate_Cx(x_values, t_values, v_values, D_values, retardation, C0, k),
      time = rep(t_values, length(x_values)),
      retardation = rep(retardation, size),
      v = rep(v_values, size),
      hl = rep(hl, size),
      koc = rep(koc, size),
      foc = rep(foc, size),
      distance = rep(x_values, each = length(t_values)),
      dispersion = rep(D_values, each = size),
      Kd = rep(kd, size),
      log_koc = rep(log_koc, size)
    )

  })

  # Return all inputs as attributes
  structure(
    result,
    inputs = list(
      n_values = n_values,
      rs_values = rs_values,
      foc_values = foc_values,
      log_koc_values = log_koc_values,
      hl_values = hl_values,
      C0 = C0,
      D_values = D_values,
      v_values = v_values,
      t_values = t_values,
      x_values = x_values
    )
  )
}
