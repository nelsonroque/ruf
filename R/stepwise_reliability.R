#' ruf

#' @name stepwise_reliability
#' @param data class: data frame
#' @param varname class: string
#' @param cluster_var class: string
#' @param listen class: vector (which points of cluster_var do you want reliability for)
#' @param autocorrelated class: boolean
#' @keywords ecological momentary assessment data, intensive longitudinal data
#' @import tidyverse
#' @examples
#' stepwise_reliability(data, "response_time", "studyday", min_day=0, max_day=14, autocorrelated=T)
#' @export
stepwise_reliability <- function(data, varname, cluster_var='studyday', listen=c(1,2), autocorrelated=T) {
  cur.varname = varname
  
  uq = pull(data %>% ungroup() %>% select(cluster_var) %>% distinct())
  uq.f = uq[uq %in% listen]

  reliability.data <- data.frame()
  for(point in uq.f){
    # filter data
    day.data <- data %>% 
      filter(!!sym(cluster_var) == point)
    
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
    
    cur.export <- calc_reliability(day.fit,avg.obs(day.data)) %>% 
      mutate(step = point,
             variable = varname)
    
    reliability.data <- rbind(reliability.data,cur.export)
    
    # clear to not have looping overwriting issues
    rm(day.data)
    rm(day.fit)
    
  }
  return(reliability.data)
}