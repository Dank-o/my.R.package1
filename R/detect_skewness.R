detect_skewness <- function(sample) {
  mean_val <- mean(sample)
  median_val <- median(sample)
  #set threshold for closeness to zero
  epsilon <- 0.05 * sd(sample)
  if (abs(mean_val - median_val) < epsilon) {
    return("Symmetric")
  } else if (mean_val > median_val) {
    return("Right-skewed")
  } else {
    return("Left-skewed")
  }
}
