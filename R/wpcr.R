#' ruf

#' @name wpcr
#' @param data class: dataframe
#' @param id_var class: string
#' @param x class: string
#' @param y class: string
#' @param id_var class: string
#' @param cov_fixed_effects class: string, vector of strings
#' @param cov_random_effects class: string, vector of strings
#' @keywords ecological momentary assessment data, intensive longitudinal data
#' @import tidyverse
#' @import lme4
#' @import tibble
#' @examples
#' wpcr(data = data, x = "X", y = "Y", id_var = "ID", cov_fixed_effects = NULL, cov_random_effects = NULL)
#' @export
wpcr <- function(data = NULL, id_var = NULL, x = NULL, y = NULL, cov_fixed_effects = NULL, cov_random_effects = NULL) {
  
  # print status message
  print("-------------------------------------------------")
  print("WITHIN-PERSON COUPLING RELIABILITY (WPCR)")
  print("Citation (APA below)")
  print("Neubauer, A. B., Voelkle, M. C., Voss, A., & Mertens, U. K. (2019). Estimating reliability of within-person couplings in a multilevel framework. Journal of personality assessment, 1-12.")
  print("-------------------------------------------------")
  
  # STEP 1
  
  # prepare model formula
  mod1_formula <- paste0(y, "~", x, "+", paste0(cov_fixed_effects, collapse="+"), paste0("(", x, paste0(cov_random_effects, collapse="+"), "|", id_var,")"))
  
  # print status message
  print(paste0("STATUS | RUNNING MODEL: ", mod1_formula))
  
  # run multilevel model predicting Y from X and the covariates
  mod1 <- lmer(mod1_formula, data=data, na.action="na.omit", REML=T)   
  
  # get random slope variance
  # for info on VarCorr, see ?VarCorr
  v <- VarCorr(mod1)
  vc <- as.data.frame(v)
  
  # get the variance of the random slope of the focal predictor x | tau_1_1_squared
  tau_1_1_squared <- vc[vc$grp == id_var, ]$vcov[2] # *** SUSPECT LINE ***
  
  # get Level 1 residual variance | eps_squared
  eps_squared <- attr(v, "sc") ** 2
  
  # STEP 2
  
  # run multilevel model predicting 
  mod2_formula <- paste0(x, "~", 1, "+", paste0("(", 1, "|", id_var,")"))
  
  # print status message
  print(paste0("STATUS | RUNNING MODEL: ", mod2_formula))
  
  # run multilevel model
  mod2 <- lmer(mod2_formula, data=data, na.action="na.omit", REML=T)
  
  # get random slope variance
  # for info on VarCorr, see ?VarCorr
  v2 <- VarCorr(mod2)
  vc2 <- as.data.frame(v2)
  
  # get Level 1 variance of the focal predictor X | within-person (Level 1) variance of the focal predictor X
  VarX <- attr(v2, "sc") ** 2
  
  # STEP 3
  
  # Compute for each Level 2 unit, how many data points there are 
  print(paste0("STATUS | CALCULATING NUMBER OF RECORDS PER PERSON"))
  
  # The data frame n_records_person contains two variables: 
  # the ID Variable, and the person specific number of assessments (t_i)
  # Keep only observations without any missing variables ** IMPLEMENT **
  n_records_person <- n_obs_per_person(data, id_var = id_var)
  
  # Compute person specific WPCR estimates (Equation 9) 
  print(paste0("STATUS | CALCULATING PERSON-SPECIFIC WCPR (EQUATION 9)"))
  wpcr_person <- n_records_person %>%
    mutate(wpcr_i = tau_1_1_squared / (tau_1_1_squared + eps_squared / (VarX * (t_i - 1))))
  
  # Summarize person specific WPCR estimates into a sample mean (Equation 10)
  print(paste0("STATUS | CALCULATING SAMPLE MEAN WCPR (EQUATION 10)"))
  wpcr_group = wpcr_person %>%
    summarise(mean_wpcr = mean(wpcr_i, na.rm=T),
              median_wpcr = median(wpcr_i, na.rm=T),
              sd_wpcr = sd(wpcr_i, na.rm=T),
              min_wpcr = min(wpcr_i, na.rm=T),
              q10_wpcr = quantile(wpcr_i, probs=0.1, na.rm=T),
              q90_wpcr = quantile(wpcr_i, probs=0.9, na.rm=T),
              max_wpcr = max(wpcr_i, na.rm=T))
  
  # PREPARE OUTPUT
  
  # place all model parameters into a tibble
  param_table = tibble::tibble("epsilon_squared" = eps_squared,
                               "variance_x" = VarX,
                               "tau_1_1_squared" = tau_1_1_squared) %>%
    bind_cols(wpcr_group)
  
  # prepare list of all results
  out <- list("n_obs_person" = n_records_person,
              "tau_1_1_squared" = tau_1_1_squared,
              "epsilon_squared" = eps_squared,
              "wpcr_person" = wpcr_person,
              "params" = param_table)
  
  return(out)
  
}