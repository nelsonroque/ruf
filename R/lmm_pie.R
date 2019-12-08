#' ruf

#' @name lmm_pie
#' @param fit class: model fit output object
#' @param pkg class: string
#' @keywords ecological momentary assessment data, intensive longitudinal data
#' @import tidyverse
#' @import nlme
#' @examples
#' lmm_pie(fit, pkg="lme4")
#' @export
lmm_pie <- function(fit, pkg="lme4") {
  
  pie_df <- tibble::as.tibble(lme4::VarCorr(fit)) %>%
    dplyr::mutate(prop_var = (vcov/sum(vcov)),
                  perc_var = (vcov/sum(vcov))*100,
                  label = paste0(grp,"*",var1,"*",var2))
  
  pie_sum <- pie_df %>% 
    dplyr::group_by(grp) %>% 
    dplyr::summarise(prop_var = sum(prop_var))
  
  pie_plot <- ggplot2::ggplot(pie_sum, aes(x="", y=prop_var, fill=grp)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y",start=0) +
    theme_minimal() +
    xlab("") +
    ylab("Proportion of Variance") +
    scale_fill_viridis_d()

  return(list(variance_table = pie_df, 
              summary = pie_sum,
              plot = pie_plot))
}