#--- Numerical Summary Programs ---#
# Contains functions to calculate a number of unusual 
# location, spread, and skewness measures. 
# If you source this, you'll have access to the following:
# --- Location ---
# tri.mean: calculates the Triple Mean of a vector.
# --- Spread ---
# medianAbDev: calculates the mean absolute deviation of a vector
# --- Skewness ---
# skewness: calculates skewness of a vector.
# YKi: calculates the Yule-Kendall Index of a vector

tri.mean <- function(vec){
  val <- (quantile(vec, 0.25, na.rm = TRUE) + 
            2*quantile(vec, 0.5, na.rm = TRUE) + 
            quantile(vec, 0.75, na.rm = TRUE))/4.
  attr(val, "names") <- NULL
  return(val)
}

medianAbDev <- function(vec){
  val <- median(abs(vec - median(vec, na.rm = TRUE)), na.rm = TRUE)
  return(val)
}

IQR <- function(vec){
  val <- quantile(vec, 0.75, na.rm = TRUE)-
    quantile(vec, 0.25, na.rm = TRUE)
  attr(val, "names") <- NULL
  return(val)
}

skewness <- function(vec){
  val <- (1/(sum(is.finite(vec))-1))*
    sum((vec-mean(vec, na.rm = TRUE))^3)/
    sqrt(1/(sum(is.finite(vec))-1)*sum((vec-mean(vec, na.rm = TRUE))^2))^3
  return(val)
}

YKi <- function(vec){
  val <- quantile(vec, 0.25, na.rm = TRUE) - 
    2*quantile(vec, 0.5, na.rm =TRUE) +
    quantile(vec, 0.75, na.rm = TRUE)
  val <- val/IQR(vec)
  attr(val, "names") <- NULL
  return(val)
}