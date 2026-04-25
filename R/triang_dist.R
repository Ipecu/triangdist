#' Density function for the Triangular Distribution
#'
#' @description Computes the probability density function of the triangular distribution.
#'
#' @param x vector of quantiles.
#' @param min lower limit of the distribution (a).
#' @param max upper limit of the distribution (b).
#' @param mode the mode of the distribution (c).
#'
#' @return A numeric vector representing the density values.
#' @export
dtriang <- function(x, min, max, mode) {

  # 1. BARRERAS DE SEGURIDAD (Manejo de errores)
  if (any(min > max, na.rm = TRUE)) {
    stop("Error: The 'min' parameter cannot be greater than the 'max' parameter.")
  }
  if (any(mode < min | mode > max, na.rm = TRUE)) {
    stop("Error: The 'mode' parameter must be between 'min' and 'max'.")
  }

  # 2. CÁLCULO DE LA DENSIDAD (Matemáticas vectorizadas)
  # Usamos ifelse() para que funcione con vectores (R's recycling rules)
  densidad <- ifelse(x < min | x > max, 0,
                     ifelse(x < mode,
                            2 * (x - min) / ((max - min) * (mode - min)),
                            ifelse(x == mode,
                                   2 / (max - min),
                                   2 * (max - x) / ((max - min) * (max - mode))
                            )
                     )
  )

  return(densidad)
}
