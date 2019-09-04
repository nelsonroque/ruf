#' ruf

#' @name add_metadate_cols
#' @param df class: dataframe
#' @param time_var class: string
#' @param time_format  class: string
#' @keywords to add date features to timestamp
#' @import tidyverse
#' @examples
#' add_metadate_cols(df)
#' @export
add_metadate_cols <- function(df, time_var = "timestamp", time_format="%m/%d/%Y") {
  df.w.dates <- df %>%
    # pass in time_stamp as string
    mutate(current_datetime = as.POSIXct(UQ(sym(time_var)), origin = "1960-01-01", format=time_format)) %>%
    mutate(week = lubridate::week(current_datetime),
           month = lubridate::month(current_datetime),
           day = lubridate::day(current_datetime),
           year = lubridate::year(current_datetime),
           time_hms = lubridate::hms(current_datetime),
           time_hour = lubridate::hour(current_datetime),
           weekday_value = lubridate::wday(current_datetime),
           weekday_label = lubridate::wday(current_datetime,label=T),
           
           day_hour_round_15 = (lubridate::round_date(current_datetime, "15 minutes")),
           day_hour_round_30 = (lubridate::round_date(current_datetime, "30 minutes")),
           
           hour_round_15 = hms::as.hms(lubridate::round_date(current_datetime, "15 minutes")),
           hour_round_30 = hms::as.hms(lubridate::round_date(current_datetime, "30 minutes"))) %>%
    mutate(weekend = ifelse(weekday_label == "Sat" | weekday_label == "Sun",1,0))
  return(df.w.dates)
}