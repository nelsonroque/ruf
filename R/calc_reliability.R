#' ruf

#' @name calc_reliability
#' @param fit class: lme4 / nlme model object 
#' @param n_obs class: numeric
#' @keywords ecological momentary assessment data, intensive longitudinal data
#' @import tidyverse
#' @import nlme
#' @examples
#' calc_reliability(fit, n_obs=14)
#' @export
calc_reliability <- function(fit,n_obs=NA) {
  
  # extract random effects
  RandomEffects <- as.numeric(VarCorr(fit)[,1])
  
  # intercept and residual
  Intercept <- RandomEffects[1]
  Residual <- RandomEffects[2]
  
  # calculate ICC
  ICC_ <- Intercept/(Intercept+Residual) 
  ICC_pkg <- reghelper::ICC(fit)
  
  # calculate reliability
  between_reliability <- Intercept/(Intercept+(Residual/n_obs)) 
  within_reliability <- 1 - between_reliability
  
  # create dataframe to export
  export <- data.frame(intercept = Intercept,
                       residual = Residual,
                       ICC_pkg = ICC_pkg,
                       ICC = ICC_,
                       between_reliability = between_reliability,
                       within_reliability = within_reliability,
                       avg_obs = n_obs)
  names(export) <- c("intercept","residual",
                     "ICC_pkg",
                     "ICC",
                     "reliability_between","reliability_within",
                     "avg_observations")
  return(export)
}
