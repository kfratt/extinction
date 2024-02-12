rm(list=ls())
graphics.off()

setwd("/Users/kaylafratt/Desktop/DOI Paper Data")
library(tidyverse)

# manually cleaned data to move notes into separate column, fixed typos,
# remove x's for n/as, and removed sessions for June 7 onwards.
# June 8-16 training was focused on generalizing the DOI, not actual extinction.

doi_clean <- read.csv("doi_data_clean.csv")
class(doi_clean$Date)

#coercing Date column to date
doi_clean$Date <- as.Date(doi_clean$Date, format = "%m/%d/%Y")
class(doi_clean$Date)
class(doi_clean$Dog)

# split into two dataframes based on dog learner
library(dplyr)
madi <- filter(doi_clean, Dog == "Madi")
persi <- filter(doi_clean, Dog == "Persi")

# add row for training repetition number overall
madi$rep_total <- seq(1, length.out = nrow(madi))
persi$rep_total <- seq(1, length.out = nrow(persi))

# remove rows (training reps) where there was no opportunity
# for false alert
no_ds_available_persi <- which(persi$Number.False.Alerts == "n/a")
no_ds_available_persi
persi <- persi[-no_ds_available_persi, ]

no_ds_available_madi <- which(madi$Number.False.Alerts == "n/a")
no_ds_available_madi
madi <- madi[-no_ds_available_madi, ]
library(ggplot2)

# make a plot
ggplot(persi, aes(x = seq_along(rep_total), y = Number.False.Alerts)) +
  geom_point() +
  labs(title = "Persi False Alerts Over Time",
       x = "Repetition Number",
       y = "Number of False Alerts")

# will want to remove the lines with n/a - not just the n/a, but
# the whole repetition. Talk about it in text.



p