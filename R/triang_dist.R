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

#' Quantile function for the Triangular Distribution
#'
#' @description Computes the quantile function (inverse CDF) of the triangular distribution.
#'
#' @param p vector of probabilities.
#' @param min lower limit of the distribution (a).
#' @param max upper limit of the distribution (b).
#' @param mode the mode of the distribution (c).
#'
#' @return A numeric vector representing the quantiles.
#' @export
qtriang <- function(p, min, max, mode) {

  # 1. BARRERAS DE SEGURIDAD
  if (any(min > max, na.rm = TRUE)) {
    stop("Error: The 'min' parameter cannot be greater than the 'max' parameter.")
  }
  if (any(mode < min | mode > max, na.rm = TRUE)) {
    stop("Error: The 'mode' parameter must be between 'min' and 'max'.")
  }
  # Nueva barrera de seguridad exigida para p
  if (any(p < 0 | p > 1, na.rm = TRUE)) {
    stop("Error: Probabilities 'p' must be between 0 and 1.")
  }

  # 2. CÁLCULO DE LOS CUANTILES
  # Primero calculamos la probabilidad que hay justo hasta la moda (el pico)
  pc <- (mode - min) / (max - min)

  # Usamos ifelse para aplicar la fórmula de subida o de bajada
  quantiles <- ifelse(p < pc,
                      min + sqrt(p * (max - min) * (mode - min)),
                      max - sqrt((1 - p) * (max - min) * (max - mode))
  )

  return(quantiles)
}


#' Random generation for the Triangular Distribution
#'
#' @description Generates random deviates from the triangular distribution.
#'
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param min lower limit of the distribution (a).
#' @param max upper limit of the distribution (b).
#' @param mode the mode of the distribution (c).
#'
#' @return A numeric vector of random values.
#' @importFrom stats runif
#' @export
rtriang <- function(n, min, max, mode) {

  # 1. BARRERAS DE SEGURIDAD
  if (any(min > max, na.rm = TRUE)) {
    stop("Error: The 'min' parameter cannot be greater than the 'max' parameter.")
  }
  if (any(mode < min | mode > max, na.rm = TRUE)) {
    stop("Error: The 'mode' parameter must be between 'min' and 'max'.")
  }

  # Adaptación estándar de R para el parámetro 'n'
  if (length(n) > 1) {
    n <- length(n)
  }
  if (n <= 0) {
    stop("Error: 'n' must be a positive number.")
  }

  # 2. GENERACIÓN ALEATORIA (Método de Inversión sugerido por el profesor)
  # Generamos 'n' probabilidades aleatorias uniformes entre 0 y 1
  u <- runif(n)

  # Usamos nuestra propia función de cuantiles
  random_values <- qtriang(p = u, min = min, max = max, mode = mode)

  return(random_values)
}

#' Distribution function for the Triangular Distribution
#'
#' @description Computes the cumulative distribution function (CDF) of the triangular distribution.
#'
#' @param q vector of quantiles.
#' @param min lower limit of the distribution (a).
#' @param max upper limit of the distribution (b).
#' @param mode the mode of the distribution (c).
#'
#' @return A numeric vector representing the cumulative probability values.
#' @export
ptriang <- function(q, min, max, mode) {

  if (any(min > max, na.rm = TRUE)) {
    stop("Error: The 'min' parameter cannot be greater than the 'max' parameter.")
  }
  if (any(mode < min | mode > max, na.rm = TRUE)) {
    stop("Error: The 'mode' parameter must be between 'min' and 'max'.")
  }

  prob <- ifelse(q < min, 0,
                 ifelse(q < mode,
                        ((q - min)^2) / ((max - min) * (mode - min)),
                        ifelse(q < max,
                               1 - ((max - q)^2) / ((max - min) * (max - mode)),
                               1
                        )
                 )
  )
  return(prob)
}
