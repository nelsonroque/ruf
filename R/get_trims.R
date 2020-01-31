
get_trims <- function(.data, trim_lower = NA, trim_upper = NA, p = NA) {

  # validate params ---------------------------------------------------------
  
  # validate function parameters
  if(!is.data.frame(.data)){
    idata = data.frame(.data)
  }

  if(is.na(trim_upper)){
    trim_upper = F
  } else {
    if(is.na(trim_lower)) {
      trim_lower = F
    }
  }
  
  if(is.na(p)){
   stop("[error: Missing percentage to trim.]")
  }
  

  # calculate params for trimming -------------------------------------------
  
  n_obs = nrow(idata) 
  n_obs_trim = n_obs * p
  
  if(trim_upper & trim_lower){
    n_obs_trim_lower = n_obs_trim / 2
    n_obs_trim_upper = n_obs_trim / 2
  } else if(trim_upper & !trim_lower) {
    n_obs_trim_lower = 0
    n_obs_trim_upper = n_obs_trim
  } else if(!trim_upper & trim_lower) {
    n_obs_trim_lower = n_obs_trim
    n_obs_trim_upper = 0
  } else {
    stop("[error: No specification of what part of the distribution to trim]")
  }

  # calculate number of observations remaining
  remain_obs = n_obs - (n_obs_trim_lower + n_obs_trim_upper)
  
  # trim upper and lower ----------------------------------------------------
  
  # get buckets for the top and bottom of the distributions
  obs_trim_lower = idata %>% 
    arrange(.data) %>%
    filter(between(row_number(), 1, n_obs_trim_lower))
  
  obs_trim_upper = idata %>% 
    arrange(desc(.data)) %>%
    filter(between(row_number(), 1, n_obs_trim_upper))
  
  # no way of keeping the middle with this method
  # alternative, mutate records based on filter?
  
  # debugging ---------------------------------------------------------------
  
  # create tibble to output this
  trim_stats = tibble(param_trim_lower = trim_lower,
                      param_trim_upper = trim_upper,
                      param_trim_percentage = p,
                      n_obs = n_obs,
                      n_obs_trim_lower = n_obs_trim_lower,
                      n_obs_trim_lower_iseven = ruf::is_even(n_obs_trim_lower),
                      n_obs_trim_upper = n_obs_trim_upper,
                      n_obs_trim_upper_iseven = ruf::is_even(n_obs_trim_upper),
                      n_obs_trim_total = n_obs_trim,
                      n_obs_remain = remain_obs)
  
  # output ------------------------------------------------------------------
  
  return(list(trim_stats = trim_stats,
              trim_lower = obs_trim_lower, 
              trim_upper = obs_trim_upper))
}
