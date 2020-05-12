#'####################################################
#'
#'   Francois Mercier 
#'   TidyTuesday
#'   2020-Week 20
#'   Eruptions volcaniques
#'   
#'####################################################



#' 0. Load relevant libraries
#' --------------------------------------------------
library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)
library(extrafont)
sessionInfo()


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



#' 2. Derive cumulative gross by week
#' --------------------------------------------------

cumgross<-real_grosses %>%
  filter(run_number==1) %>%
  group_by(show, run_number) %>%
  mutate(
    # Use coalesce() for shows that have some NAs
    weekly_gross=coalesce(weekly_gross, 0),
    cumulative_gross=cumsum(weekly_gross),
    show_label=ifelse(week_ending==max(week_ending), paste0(" ", show), NA_character_)
  ) %>%
  ungroup() %>%
  mutate(show_and_run=paste(show, run_number))

cumgross %>% filter(show=="The Phantom of the Opera") %>%
  filter(row_number() %in% c(1, n())) %>%
  select(show, week_ending, week_of_run, cumulative_gross) 

#' 4. Explore a little
#' --------------------------------------------------
#' Initial graph
wrapper<-function(x, ...) paste(strwrap(x, ...), collapse = "\n")
mytext<-c("The Phantom of the Opera has been running for more than 30 years and
          has cumulated 1677 weeks of run.")
dat_text<-data.frame(label=wrapper(mytext, width=40),
                     x=ymd("2000-01-01"), y=1250*1e6)
arrow_dat<-data.frame(x=ymd("2000-01-01"), xend=ymd("2002-06-01"), 
                      y=1200*1e6, yend=900*1e6)


p1<-cumgross %>%
  drop_na(week_ending, cumulative_gross) %>%
  ggplot(., aes(week_ending, cumulative_gross))+
  #' geoms
  geom_line(aes(group=show, colour=cumulative_gross), lwd=1)+
  geom_text(data=dat_text, size=3.5, hjust=0, colour="white", 
             mapping=aes(x=x, y=y, label=label))+
  geom_curve(data=arrow_dat, aes(x=x, y=y, xend=xend, yend=yend),
             inherit.aes=FALSE, colour="white",
             arrow=arrow(length=unit(0.03, "npc"), type="closed",
                         angle=c(20, 80)), curvature=-0.3)+
  #' scales and coords
  scale_y_continuous("", labels=label_dollar(scale=1/1e6, suffix="M"))+
  scale_colour_viridis_c(option="inferno", direction=-1)+
  coord_flip()+
  #' labs
  labs(x="", y="",
       title="Eternal flames of Broadway Shows",
       subtitle="Long lasting Broadway shows")+
  #' themes
  theme_dark()+
  theme(legend.position="none", 
        panel.grid.major=element_line(colour="grey62"), 
        panel.grid.minor=element_line(colour="grey55"), 
        panel.grid.major.y=element_blank(),   
        panel.grid.minor.y=element_blank(), 
        axis.text=element_text(colour="white"),
        axis.ticks=element_line(colour="grey50"),
        plot.background=element_rect(fill="grey50", color="grey50"),
        plot.title=element_text(colour="white", hjust=0, size=16),
        plot.subtitle=element_text(colour="white", hjust=0, size=10)) 
p1

p2<-cumgross %>%
  group_by(show) %>%
  arrange(cumulative_gross) %>%
    slice(n()) %>%
  ungroup() %>%
  filter(cumulative_gross>500*1e6) %>%
  arrange(cumulative_gross) %>%
  mutate(topshow=factor(show)) %>%
  mutate(toposhow=fct_inorder(topshow)) %>%
  
  ggplot(., aes(toposhow, cumulative_gross))+
  #' geoms
  geom_col(aes(fill=cumulative_gross))+
  geom_text(aes(label=round(cumulative_gross/1e6,0),
                y=cumulative_gross-50e6), 
            color="white", size=3.5, hjust=1)+
  #' scales and coords
  scale_y_continuous("", labels=label_dollar(scale=1/1e6, suffix="M"))+
  scale_fill_viridis_c(option="inferno", end=0.7, direction=-1)+
  coord_flip()+
  #' labs
  labs(x="", y="",
       subtitle="Shows with cumulative revenue >500 million USD",
       caption=paste0("Data: Playbill | Data prep: @alexcookson"))+
  theme_dark()+
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_text(colour="white"),
        axis.ticks=element_line(colour="grey50"),
        panel.grid.major=element_blank(),   
        panel.grid.minor=element_blank(), 
        plot.background=element_rect(fill="grey50", color="grey50"),
        plot.subtitle=element_text(colour="white", hjust=0, size=10),
        plot.caption=element_text(colour="white"))

pall<-p1/p2+
  plot_layout(heights=c(3, 1))

ggsave("./2020Wk18- Broadway/Broadway.png", pall, width=9, height=10, dpi=300)
