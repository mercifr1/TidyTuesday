#'####################################################
#'
#'   Francois Mercier 
#'   TidyTuesday
#'   2020-Week 18
#'   Broadway show les marrons chauds!
#'   
#'####################################################



#' 0. Load relevant libraries
#' --------------------------------------------------
library(tidyverse)
library(lubridate)
library(scales)
library(extrafont)


#' 1. Import and cleanup a bit the raw data
#' --------------------------------------------------
grosses_raw<-readr::read_csv("https://raw.githubusercontent.com/tacookson/data/master/broadway-grosses/grosses.csv", guess_max=10000)
cpi_raw<-readr::read_csv("https://raw.githubusercontent.com/tacookson/data/master/broadway-grosses/cpi.csv")
pre_1985_starts<-readr::read_csv("https://raw.githubusercontent.com/tacookson/data/master/broadway-grosses/pre-1985-starts.csv")
cpi<-cpi_raw %>%  mutate(jan_2020_dollars=cpi[year_month=="2020-01-01"] / cpi)


#' Dealing with missing data as suggested by Alex Cookson
#' https://www.alexcookson.com/post/most-successful-broadway-show-of-all-time/
#' Turn metrics into missing values if there were no shows OR if metrics have a value of zero
grosses_fixed_missing<-grosses_raw %>%
  mutate_at(vars(weekly_gross:pct_capacity),
            ~ifelse(performances+previews==0 | . ==0, NA, .))

grosses_clean_temp<-grosses_fixed_missing %>%
  group_by(show) %>%
  arrange(week_ending) %>%
  mutate(run_number=cumsum(row_number()==1 | week_ending-lag(week_ending)>90)) %>%
  group_by(show, run_number) %>%
  mutate(week_of_run=row_number()) %>%
  ungroup()


calculate_weeks_since_start<-function(x) {
  as.integer(pmax(1, difftime("1985-06-09", x, units="weeks")))
}

pre_1985_starts_calculated<-grosses_clean_temp %>%
  group_by(show, run_number) %>%
  filter(min(week_ending)=="1985-06-09") %>%
  ungroup() %>%
  select(week_ending, show) %>%
  left_join(pre_1985_starts, by="show") %>%
  group_by(show) %>%
  mutate(week_of_run_originals=calculate_weeks_since_start(start_date)+row_number()) %>%
  ungroup() %>%
  select(week_ending, show, week_of_run_originals)

grosses_clean<-grosses_clean_temp %>%
  left_join(pre_1985_starts_calculated, by=c("show", "week_ending")) %>%
  mutate(week_of_run=coalesce(week_of_run_originals, week_of_run)) %>%
  select(-week_of_run_originals)

real_grosses<-grosses_clean %>%
  mutate(year_month=floor_date(week_ending, unit = "month")) %>%
  left_join(cpi, by="year_month") %>%
  mutate_at(
    vars(
      weekly_gross_overall,
      weekly_gross,
      potential_gross,
      avg_ticket_price,
      top_ticket_price
    ),
    ~ . * jan_2020_dollars) %>%
  select(-year_month:-jan_2020_dollars)


#' 2. Import and cleanup a bit the raw data
#' --------------------------------------------------




