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

library(ggplot2)




# Assuming your dataframe is named df and you want to plot columns
# "rep.total" and another column (e.g., "some_other_column")
ggplot(persi, aes(x = seq_along(rep.total), y = Number.False.Alerts)) +
  geom_point() +
  labs(title = "Line Graph Example", x = "Index", y = "rep.total")

# will want to remove the lines with n/a - not just the n/a, but
# the whole repetition. Talk about it in text.