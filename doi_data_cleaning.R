rm(list=ls())
graphics.off()

setwd("/Users/kaylafratt/Desktop/DOI Paper Data")
library(tidyverse)

# manually cleaned data to move notes into separate column, fixed typos,
# remove x's for n/as, and removed sessions for June 7 onwards.
# June 8-16 training was focused on generalizing the DOI, not actual extinction.

setwd("C:/Documents/K9C/DOI Paper")
doi_clean <- read.csv("doi_data_clean.csv")
library(tidyverse)
library(dplyr)

#coercing Date column to date
doi_clean$Date <- as.Date(doi_clean$Date, format = "%m/%d/%Y")
class(doi_clean$Date)
class(doi_clean$Dog)

#coerce false alerts, correct dismissals, misses to numeric
doi_clean$Number.False.Alerts <- as.numeric(doi_clean$Number.False.Alerts)
doi_clean$X..of.correct.dismissals <- as.numeric(doi_clean$X..of.correct.dismissals)
doi_clean$X..of.misses <- as.numeric(doi_clean$X..of.misses)


# split into two dataframes based on dog learner
library(dplyr)
madi <- filter(doi_clean, Dog == "Madi")
persi <- filter(doi_clean, Dog == "Persi")

# add row for training repetition number overall
madi$rep_total <- seq(1, length.out = nrow(madi))
persi$rep_total <- seq(1, length.out = nrow(persi))

# remove row with nulls
madi <- filter(madi, Number.True.Alerts != "NA")
persi <- filter(persi, Number.True.Alerts != "NA")

# remove rows (training reps) where there was no opportunity
# for false alert (no negatives available)
madi <- filter(madi, Negative.1 != "none")
persi <- filter(persi, Negative.1 != "none")

# remove rows (training reps) where there was no opportunity
# for true alert (no cheetah available)
madi <- filter(madi, Cheetah != "none")
persi <- filter(persi, Cheetah != "none")


######################################################################
############Data exploration##########################################


# make a plot
library(ggplot2)
?ggplot
ggplot(persi, aes(x = rep_total, y = Number.False.Alerts)) +
  geom_col (fill = "skyblue") +
labs(title = "Persi False Alerts Over Time (Repitions)", x = "Repetition #", y = "# False Alerts")

ggplot(persi, aes(x = rep_total, y = FA1..s.)) +
  geom_col (fill = "skyblue") +
  labs(title = "Persi False Alerts Over Time (Repitions)", x = "Repetition #", y = "False Alert 1 Duration")



# Add sum of all false alerts for each repetition
#Sum across rows to create new column
#Work in progress
?rowSums()
madi$totalduration = rowSums(madi[, c("FA1..s.", "FA2..s.", "FA3..s.", "FA4..s.", "FA5..s.", "FA6..s.", "FA7..s.", "FA8..s.", na.rm = TRUE)
