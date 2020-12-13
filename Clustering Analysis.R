library(dplyr)
library(readr)
library(stringr)
library(cluster)
library(ggplot2)
library(ggthemes)
library(plotly)
library(viridis)
library(zoo)
library(forcats)
setwd("C:/Users/frbrit/Documents")

#Pull in State Data
x <-"https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
original_COVID19 <- read.csv(x)
COVID19 <- original_COVID19
#Remove cases and format the date values

National <- COVID19 %>%
  group_by(date) %>%
  summarise(deaths=sum(deaths)) %>%
  mutate(state = "-Nationwide-") %>%
  mutate(fips = 0)
COVID19 <- bind_rows(National,COVID19)
colnames(COVID19)[4] <- "FIPS"
CensusData <- read_csv("~/Academy PreWork/COVID Demo/CensusData.csv")
COVID19 <- merge(COVID19,CensusData[,c(4,6,8,9)], 
                 by = "FIPS")

constant <- 1
COVID19Final <- COVID19 %>%
  filter(!is.na(FIPS)) %>%
  group_by(FIPS) %>%
  arrange(FIPS,date) %>%
  mutate(date = as.Date(date)) %>%
  mutate(deaths = deaths - lag(deaths,1)) %>%
  mutate(deaths = ifelse(is.na(deaths),0,deaths)) %>%
  mutate(cumulative_deaths = cumsum(na.omit(deaths))) %>%
  #mutate(daily_change = (deaths - lag(deaths,1))/lag(deaths,1)) %>%
  mutate(daily_change = deaths - lag(deaths,1)) %>%
  mutate(daily_change = ifelse(!is.finite(daily_change),NA,daily_change)) %>%
  mutate(weekly_average_change = rollapply(daily_change,7,mean, na.rm=T, align='right', fill=NA)) %>%
  mutate(weekly_average_change = ifelse(!is.finite(weekly_average_change),NA,weekly_average_change)) %>%
  #mutate(theta = 90*(weekly_average_change)) %>%
  #mutate(theta = ifelse(theta>90,90,theta)) %>%
  #mutate(x1 = date-cos(theta*pi/180)*constant) %>%
  #mutate(x2 = date+cos(theta*pi/180)*constant) %>%
  #mutate(y1 = FIPS-sin(theta*pi/180)*constant) %>%
  #mutate(y2 = FIPS+sin(theta*pi/180)*constant) %>%
  mutate(y1 = FIPS*1000-daily_change) %>%
  mutate(FIPS_String = str_pad(FIPS,5,pad="0")) %>%
  mutate(Total_Deaths = Deaths_2019/365) %>%
  mutate(Total_Deaths_per_100k = 100000* Deaths_2019/Pop_Estimate_2019) %>%
  mutate(Deaths_per_100k = 100000*deaths/Pop_Estimate_2019) %>%
  mutate(Density_Estimate.2019 = as.numeric(Density_Estimate.2019)) %>%
  mutate(state_ordered = reorder(FIPS, Pop_Estimate_2019)) %>%
  mutate(pending = ifelse(date > Sys.Date()-7, .5,1)) %>%
  filter(deaths >0) 

################################################

p1 <- ggplotly(
          p = ggplot(data=COVID19Final) +
          geom_line(aes(color = state,
                        x=date,
                        y=cumulative_deaths,
                        label1 = date, 
                        label2 = cumulative_deaths, 
                        label3 = deaths)) + 
          scale_color_viridis(discrete = T) +
          theme_solarized() +
          theme(axis.title.y = element_text(size=8)) +
          guides(color = F, linetype = F) +  
          scale_y_continuous(labels = scales::comma) +
          labs(x = "Date",
               y = "Cumulative <br> Deaths"),
      dynamicTicks = T, tooltip = c("state", "label1", "label2")) 

p2 <- ggplotly(
          p = ggplot(data=COVID19Final) +
          geom_line(aes(color = state,
                        x=date,y=deaths, 
                        label1 = date, 
                        label2 = deaths, 
                        label3 = Total_Deaths)) + 
          scale_color_viridis(discrete = T) +
          theme_solarized() +
          theme(axis.title.y = element_text(size=8)) +
          guides(color = F) +
          scale_y_continuous(labels = scales::comma) +
          labs(x = "Date",
               y = "Daily <br> Deaths"),
      dynamicTicks = T, tooltip = c("state", "label1", "label2", "label3"))

p3 <- ggplotly(
  p = ggplot(data=COVID19Final) +
    geom_line(aes(color = state,
                  x=date,
                  y=Deaths_per_100k, 
                  label1 = date, 
                  label2 = deaths, 
                  label3 = Total_Deaths), ) + 
    scale_color_viridis(discrete = T) +
    theme_solarized() +
    theme(axis.title.y = element_text(size=8)) +
    guides(color = F) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = "Date",
         y = "Daily Deaths <br> per 100k People"),
  dynamicTicks = T, tooltip = c("state", "label1", "label2", "label3")) %>%
  layout(title = list(text = paste0('Deaths by State')))

subplot(p1,p2,p3,nrows = 3, shareX = T ,titleY = T)

##########################################################
test <- subset(COVID19Final, state == "Washington")

p4 <- ggplotly(
          p = ggplot(data=COVID19Final) +
          geom_point(aes(color = state,
                         y=Pop_Estimate_2019,
                         x=cumulative_deaths, 
                         label1 = date, 
                         label2 = cumulative_deaths)) +
          geom_segment(aes(color = state, 
                           x=cumulative_deaths,
                           xend=cumulative_deaths,
                           y=Pop_Estimate_2019,
                           yend=Pop_Estimate_2019+weekly_average_change,
                           label3 = date,
                           label4 = deaths)) +
          geom_segment(aes(color = state, 
                             x=cumulative_deaths,
                             xend=cumulative_deaths,
                             y=Pop_Estimate_2019,
                             yend=Pop_Estimate_2019+deaths,
                             label3 = date,
                             label4 = deaths), alpha=.2) +
          #scale_color_viridis(discrete = T) +
          theme_solarized() +
          labs(y = "Population Size",
               x = "Cumulative Deaths"), 
      dynamicTicks = T, tooltip = c("state", "label1", "label2", "label3", "label4")) %>%
      layout(title = list(text = paste0('Cumulative Deaths by Population',
                                        '<br>',
                                        '<sup>',
                                        'Each Dot Represents Cumulative Total by Day',
                                        '</sup>')))
p4

#######################################################

ggplotly(
  p = ggplot(data=COVID19Final) +
    geom_line(aes(color = state,
                  x=date,
                  y=prop_of_maxdeaths*daily_change_average,
                  label1 = state, 
                  label2 = weekly_average_deaths, 
                  label3 = daily_change_average)) + 
    geom_abline(intercept = 0, slope=0, color = "gray", alpha = .5) +
    scale_color_viridis(discrete = T) +
    theme_tufte(ticks = F) +
    theme(plot.title = element_text(size = 10),
          axis.title.y = element_text(size=8)) +
    guides(color = F, linetype = F) +  
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Deaths by State",
         x = "Date",
         y = "Cumulative <br> Deaths"),
  dynamicTicks = T, tooltip = c("label1", "label2","label3"), alpha=.8) 



