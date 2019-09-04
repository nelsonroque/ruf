# -------------------------------------------------------
# custom function #### 
# -------------------------------------------------------

LMM2Pie <- function(LMM.obj,plot=T) {
  require(lme4)
  require(dplyr)
  require(ggplot2)
  
  # get df with variance components
  var.df <- as.data.frame(VarCorr(LMM.obj))
  
  # create label for pie chart for each component of variance
  var.df <- var.df %>% 
    # replace parenthesis in '(Intercept)'
    mutate(var1 = ifelse(var1 == "(Intercept)","Intercept",var1)) %>%
    # turn all to uppercase
    mutate(grp = toupper(grp),
           var1 = toupper(var1),
           var2 = toupper(var2)) %>%
    # create simple label for pie chart
    mutate(label = ifelse(is.na(var2), paste0(grp," : ",var1),paste0(grp," : ",var1," : ",var2)))
  
  # calculate total variance
  var.df$total.var <- sum(var.df$vcov[var.df$vcov > 0])
  
  # percent variance accounted for by each grouping
  var.df$perc.var <- (var.df$vcov/var.df$total.var)*100
  
  # remove component with negative variance
  var.df.graph <- var.df %>% filter(vcov > 0) 
  
  if(plot == T) {
    ggplot(var.df.graph,aes("",perc.var,fill=label)) +
      geom_bar(width=1,stat="identity") +
      coord_polar("y")
  }
  
  return(var.df)
}
