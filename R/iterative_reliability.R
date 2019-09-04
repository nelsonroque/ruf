#' ruf

#' @name iterative_reliability
#' @param data class: data frame
#' @param varname class: string
#' @param cluster_var class: string
#' @param id_var class: string
#' @param min_day class: numeric
#' @param max_day class: numeric
#' @param autocorrelated class: boolean
#' @keywords ecological momentary assessment data, intensive longitudinal data
#' @import tidyverse
#' @import nlme
#' @examples
#' iterative_reliability(data, "response_time", "studyday", min_day=0, max_day=14, autocorrelated=T)
#' @export
iterative_reliability <- function(data, varname, cluster_var='studyday', id_var="id", min_day=0, max_day=14, autocorrelated=T) {
  cur.varname = varname
  
  reliability.data <- data.frame()
  for(DAYS in 1:max_day){
    # filter data
    day.data <- data %>% 
      filter(!!sym(cluster_var) >= min_day & !!sym(cluster_var) <= DAYS)
    print(nrow(day.data))
    cat(DAYS)
    
    if(autocorrelated){
      # specify models
      day.fit <- lme(fixed= formula(paste0(cur.varname," ~ 1")), 
                     random= ~ 1|id, 
                     data=day.data,
                     cor=corAR1(),
                     na.action=na.exclude)
    } else {
      # specify models
      day.fit <- lme(fixed= formula(paste0(cur.varname," ~ 1")), 
                     random= ~ 1|id, 
                     data=day.data,
                     na.action=na.exclude)
    }
    
    #print(summary(day.fit))
    
    cur.export <- calc_reliability(day.fit,avg_obs(day.data, id_var=id_var)) %>% 
      mutate(days = DAYS,
             variable = varname)
    
    reliability.data <- rbind(reliability.data,cur.export)
    
    # clear to not have looping overwriting issues
    rm(day.data)
    rm(day.fit)
    
  }
  return(reliability.data)
}
