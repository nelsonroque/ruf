lmm2pie <- function(fit, pkg="lme4") {
  
  pie.df <- tibble::as.tibble(lme4::VarCorr(fit)) %>%
    dplyr::mutate(prop_var = (vcov/sum(vcov)),
                  perc_var = (vcov/sum(vcov))*100,
                  label = paste0(grp,"*",var1,"*",var2))
  
  pie.sum <- pie.df %>% 
    dplyr::group_by(grp) %>% 
    dplyr::summarise(prop_var = sum(prop_var))
  
  pie.plot <- ggplot2::ggplot(pie.sum, aes(x="", y=prop_var, fill=grp)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y",start=0) +
    theme_minimal() +
    xlab("") +
    ylab("Proportion of Variance") +
    scale_fill_viridis_d()

  return(list(variance_table = pie.df, 
              summary = pie.sum,
              plot = pie.plot))
}

set.seed(516)
df <- expand.grid(people=seq(1,100,1),
                  days=seq(1,7,1),
                  moments=seq(1,6,1))
df$y <- rnorm(500,50,n=nrow(df))
df$cov <- rnorm(30,6,n=nrow(df))

fit <- lme4::lmer(y ~ (1 + cov | people/days), 
                  data=df)

o <- lmm2pie(fit)
o$summary
o$plot