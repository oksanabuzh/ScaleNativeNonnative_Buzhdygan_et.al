#' Fit Power Law Model to Data
#'
#' This function fits a power law model of the form richness ~ c * scale ^z
#' to the given data using the nls function.
#' If the linear model fitting fails, NA values are returned for all coefficients.
#'
#' @param dat A data frame containing the variables 'richness' and 'scale'.
#' @return A tibble with the fitted coefficients 'c' and 'z', the p-value for the slope,
#'         and the R-squared value for the non-linear model.
#'         If the model fitting fails, NA values are returned for all coefficients.
#' @examples
#' data <- data.frame(richness = c(1, 2, 3), scale = c(0.1, 0.2, 0.3))
#' fit_power(data)
#' @export
fit_power <- function(dat) {
  # Try to fit the linear model, if it fails, return NA for all coefficients
  tryCatch(
    {
      model <- nls(
        formula = richness ~ c * scale^z,
        data = dat,
        start = list(c = 1, z = 0.1)
      )
      c <- summary(model)$coeff["c", "Estimate"]
      z <- summary(model)$coeff["z", "Estimate"]
      p_value_slope <- summary(model)$coeff["z", 4]
      # calculate r-squared for non-linear model
      # first, calculate residual sum of squares
      rss <- sum(resid(model)^2)

      # calculate the total sum of squares
      tss <- sum((dat$richness - mean(dat$richness))^2)
      # calculate r-squared
      r_squared <- 1 - rss / tss

      # combine the results in a data frame
      tibble(
        c = c,
        z = z,
        p_value_slope = p_value_slope,
        r_squared = r_squared
      )
    },
    error = function(e) {
      tibble(
        c = NA,
        z = NA,
        p_value_slope = NA,
        r_squared = NA
      )
    }
  )
}
