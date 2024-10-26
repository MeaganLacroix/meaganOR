#' @title OR and 95% confidence interval
#' @description a function that returns the odds ratios and 95% confidence intervals from a vector of coefficient estimates and a vector of standard errors
#' @param coef a vector of coefficients from a logistic regression model
#' @param se a vector of standard errors from a logistic regression model
#' @param siglevel the significance level e.g. .05
#' @param roundto number of digits to round to
#' @return ORresult
#' @author Meagan Lacroix
#' @examples OR_95CI(0.5, 0.1, 0.05, 2)
#' @export
#' @importFrom stats qnorm
OR_95CI <- function(coef, se, siglevel, roundto){
  if (!is.numeric(coef) || !is.numeric(se) || !is.numeric(siglevel) || !is.numeric(roundto)) {
    stop("All arguments must be numeric")
  }
  if (se <= 0) {
    stop("Standard error must be positive")
  }
  if (siglevel <= 0 || siglevel >= 1) {
    stop("Significance level must be between 0 and 1")
  }
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall = roundto),
                     " (",
                     format(round(ORlcl, roundto), nsmall = roundto),
                     ", ",
                     format(round(ORucl, roundto), nsmall = roundto),
                     ")"
  )
  return(ORresult)
}
