#' =============================================================================
#' Project: ECHO LUR
#' Date created: November 26, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script processes the DTT assay data and estimates ROS concentrations
#' =============================================================================

library(ggplot2)
library(ggthemes)
library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)
library(broom)

#' For ggplots
simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=10),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

#' -----------------------------------------------------------------------------
#' Read in the calibration curve data
#' Make sure you've run 01_ROS_Calibration_Curve.R first
#' -----------------------------------------------------------------------------

curve_name <- "DTT Calibration Curve_5 October 2018.xlsx"
curve_name2 <- paste0("Fitted ", gsub(".xlsx", ".csv", curve_name))

cal_curve <- read_csv(here::here("Data/Calibration", curve_name2))

cal_intercept <- cal_curve$estimate[1]
cal_slope <- cal_curve$estimate[nrow(cal_curve)]
 
#' Additional values for the calculations
first_time <- 0 # when was the first time point?
second_time <- (36+(11/60)) # when was the second time point?

#' -----------------------------------------------------------------------------
#' Read in the raw data from the instrument
#' -----------------------------------------------------------------------------

#' Change this depending on which sheet you're processing
raw_name <-  "7 November 2018.xlsx"

raw_sheets <- excel_sheets(here::here("Data/Raw_Data", raw_name))
raw_sheets <- raw_sheets[which(str_detect(raw_sheets, "Row "))]

raw_data <- data.frame()
for(i in 1:length(raw_sheets)) {
  temp <- read_excel(here::here("Data/Raw_Data", raw_name),
                     sheet = raw_sheets[i]) %>% 
    slice(-c(1:3)) %>% 
    rename(injection_name = "Injection Name",
           injection = Inj.) 
  
  raw_data <- bind_rows(raw_data, temp)
  rm(temp)
}

#' drop water samples
raw_data <- filter(raw_data, !injection_name == "WATER")

#' -----------------------------------------------------------------------------
#' Calculate DTT Concentrations
#' -----------------------------------------------------------------------------

raw_data <- raw_data %>% 
  mutate(Area = as.numeric(Area)) %>%  
  mutate(dtt_conc = (Area - cal_intercept) / cal_slope)

#' blanks in a separate data frame
blank_data <- filter(raw_data, str_detect(injection_name, "blank"))

sample_data <- filter(raw_data, !str_detect(injection_name, "blank"))

#' -----------------------------------------------------------------------------
#' fit linear model to each of the samples to estimate loss rate
#' generate a plot for each sample
#' -----------------------------------------------------------------------------

sample_ids <- unique(sample_data$injection_name)

loss_rates <- data.frame()

for (i in 1:length(sample_ids)) {
  temp_df <- filter(sample_data, injection_name == sample_ids[i]) %>% 
    arrange(Position) %>% 
    mutate(injection_time = c(rep(first_time, 3), rep(second_time, 3)))
  
  t1 <- filter(temp_df, injection_time == first_time) %>% 
    summarize(mean_dtt = mean(dtt_conc))
  
  t2 <- filter(temp_df, injection_time == second_time) %>% 
    summarize(mean_dtt = mean(dtt_conc))
  
  temp_lm <- lm(dtt_conc ~ injection_time, data = temp_df)
  summary(temp_lm)
  
  temp_rate <- tidy(temp_lm) %>% 
    mutate(sample_id = sample_ids[i]) %>% 
    filter(term == "injection_time") %>% 
    select(-term) %>% 
    mutate(mean_dtt_conc_t1 = t1$mean_dtt,
           mean_dtt_conc_t2 = t2$mean_dtt)
  
  loss_rates <- bind_rows(loss_rates, temp_rate)
  
  #' Generate the plot
  ggplot(temp_df, aes(x = injection_time, y = dtt_conc)) +
    ggtitle(paste("Loss Rate (\u03bcm/min): Sample no.", sample_ids[i])) + 
    geom_point(color = "blue") +
    geom_smooth(method = lm, se = T, color = "red") +
    xlab("Sample Time (min)") + ylab("DTT Concentraton (\u03bcm)") +
    simple_theme
  plot_name <- paste0("Loss_Rate_", sample_ids[i], ".jpeg")
  ggsave(here::here("Figs", plot_name), device = "jpeg",
         height = 5, width = 5, units = "in")
  
  rm(temp_df, temp_lm, temp_rate)
}

loss_rates_summary <- loss_rates %>% 
  select(sample_id, estimate, std.error, mean_dtt_conc_t1, mean_dtt_conc_t2) %>% 
  rename(loss_rate = estimate,
         loss_rate_sd = std.error) %>% 
  mutate(cal_curve = gsub(".xlsx", "", curve_name),
         raw_data = gsub(".xlsx", "", raw_name))

#' -----------------------------------------------------------------------------
#' Calculate loss rates for the blanks, too
#' -----------------------------------------------------------------------------

summary(filter(blank_data, str_detect("filter blank", injection_name)))
summary(filter(blank_data, str_detect("solution blank", injection_name)))

blank_ids <- unique(blank_data$injection_name)

blank_loss_rates <- data.frame()

for (i in 1:length(blank_ids)) {
  temp_df <- filter(blank_data, injection_name == blank_ids[i]) %>% 
    arrange(Position)
  
  positions <- unique(temp_df$Position)
  n_blanks <- length(positions) / 2 
  
  for (j in 1:n_blanks) {
    temp_df2 <- filter(temp_df, Position %in% c(positions[j], positions[j+1])) %>% 
      mutate(injection_time = c(rep(first_time, 3), rep(second_time, 3)))
    
    t1 <- filter(temp_df2, injection_time == first_time) %>% 
      summarize(mean_dtt = mean(dtt_conc))
    
    t2 <- filter(temp_df2, injection_time == second_time) %>% 
      summarize(mean_dtt = mean(dtt_conc))
    
    temp_lm <- lm(dtt_conc ~ injection_time, data = temp_df2)
    summary(temp_lm)
    
    temp_rate <- tidy(temp_lm) %>% 
      mutate(sample_id = paste0(blank_ids[i], "_", i, "_", j)) %>% 
      filter(term == "injection_time") %>% 
      select(-term) %>% 
      mutate(mean_dtt_conc_t1 = t1$mean_dtt,
             mean_dtt_conc_t2 = t2$mean_dtt)
    
    blank_loss_rates <- bind_rows(blank_loss_rates, temp_rate)
    
    #' skip the even j's (since they're in the previous iteration)
    if(j < n_blanks) j <- j + 2 
    rm(temp_df2, temp_lm, temp_rate)
  }
  rm(temp_df)
}

blank_loss_rates_summary <- blank_loss_rates %>% 
  select(sample_id, estimate, std.error) %>% 
  rename(loss_rate = estimate,
         loss_rate_sd = std.error) %>% 
  mutate(cal_curve = gsub(".xlsx", "", curve_name),
         raw_data = gsub(".xlsx", "", raw_name))


#' -----------------------------------------------------------------------------
#' Save the data as a .csv
#' -----------------------------------------------------------------------------

loss_rates_summary <- bind_rows(loss_rates_summary, blank_loss_rates_summary)

rates_name <- paste0(gsub(".xlsx", "", raw_name), " Loss Rates.csv")
write_csv(loss_rates_summary, here::here("Data", rates_name))
