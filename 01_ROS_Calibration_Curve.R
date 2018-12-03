#' =============================================================================
#' Project: ECHO LUR
#' Date created: November 26, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script calculates the TWA for each filter
#' =============================================================================

library(ggplot2)
library(ggthemes)
library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)

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
#' -----------------------------------------------------------------------------

curve_name <- "DTT Calibration Curve_5 October 2018.xlsx"

curve_sheets <- excel_sheets(here::here("Data/Calibration", curve_name))
curve_sheets <- curve_sheets[which(str_detect(curve_sheets, "Row "))]

curve_data <- data.frame()
for(i in 1:length(curve_sheets)) {
  temp <- read_excel(here::here("Data/Calibration", curve_name),
                     sheet = curve_sheets[i]) %>% 
    select(Inj., "Injection Name", Area) %>% 
    rename(injection_name = "Injection Name",
           injection = Inj.) %>% 
    filter(injection %in% c(as.character(1:50)))
  
  curve_data <- bind_rows(curve_data, temp)
  rm(temp)
}

#' drop water and blanks
curve_data <- filter(curve_data, 
                   injection_name %in% c(as.character(seq(50, 150, 25)))) %>%
  mutate_if(is.character, as.numeric)

#' -----------------------------------------------------------------------------
#' Fit the linear regression model and write out the results
#' -----------------------------------------------------------------------------

curve_lm <- lm(Area ~ injection_name, data = curve_data)
summary(curve_lm)

library(broom)
write_csv(tidy(curve_lm), here::here("Data/Calibration", "cal_curve.csv"))
write_csv(glance(curve_lm), here::here("Data/Calibration", 
                                       "cal_curve_diagnostics.csv"))

tidy(curve_lm)
glance(curve_lm)
