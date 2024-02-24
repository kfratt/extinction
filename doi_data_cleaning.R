rm(list=ls())
graphics.off()

#setwd("/Users/kaylafratt/Desktop/DOI Paper Data")
#library(tidyverse)

# manually cleaned data to move notes into separate column, fixed typos,
# remove x's for n/as, and removed sessions for June 7 onwards.
# June 8-16 training was focused on generalizing the DOI, not actual extinction.

setwd("C:/Documents/K9C/DOI Paper")
doi_clean <- read.csv("doi_data_clean.csv")
library(tidyverse)
library(dplyr)
library(tidyr)

#coercing Date column to date
doi_clean$Date <- as.Date(doi_clean$Date, format = "%m/%d/%Y")
class(doi_clean$Date)
class(doi_clean$Dog)

#coerce false alerts, correct dismissals, misses to numeric
doi_clean$Number.False.Alerts <- as.numeric(doi_clean$Number.False.Alerts)
doi_clean$X..of.correct.dismissals <- as.numeric(doi_clean$X..of.correct.dismissals)
doi_clean$X..of.misses <- as.numeric(doi_clean$X..of.misses)


# split into two dataframes based on dog learner
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

#Add column FAduration as sum of false alerts
madi <- madi %>%
  mutate(FAduration = rowSums(select(., FA1..s.:FA8..s.), na.rm = TRUE))
persi <- persi %>%
  mutate(FAduration = rowSums(select(., FA1..s.:FA8..s.), na.rm = TRUE))



######################################################################
############Data exploration##########################################

#correlation tests
cor.test(madi$rep_total, madi$Number.False.Alerts)
cor.test(madi$rep_total, madi$FAduration)
cor.test(persi$rep_total, persi$Number.False.Alerts)
cor.test(persi$rep_total, persi$FAduration)

#Plots Persi number of false alerts and sum duration of false alerts
library(ggplot2)
ggplot(persi, aes(x = rep_total, y = Number.False.Alerts)) +
  geom_col (fill = "skyblue") +
labs(title = "Persi False Alerts Over Time (Repetitions)", x = "Repetition #", y = "# False Alerts")

ggplot(persi, aes(x = rep_total, y = FAduration)) +
  geom_col (fill = "skyblue") +
  labs(title = "Persi False Alerts Over Time (Repetitions)", x = "Repetition #", y = "False Alert Duration")

ggplot(persi, aes(x = Number.False.Alerts, y = FAduration)) +
  geom_col (fill = "skyblue") +
  labs(title = "Persi False Alerts Over Time (Repetitions)", x = "Repetition #", y = "False Alert Duration")

#Plots Madi number of false alerts and sum duration of false alerts
ggplot(madi, aes(x = rep_total, y = Number.False.Alerts)) +
  geom_col (fill = "skyblue") +
  labs(title = "Madi False Alerts Over Time (Repetitions)", x = "Repetition #", y = "# False Alerts")

ggplot(madi, aes(x = rep_total, y = FAduration)) +
  geom_col (fill = "skyblue") +
  labs(title = "Madi False Alerts Over Time (Repetitions)", x = "Repetition #", y = "False Alert Duration")

#Number False Alerts per Negative Sample
ggplot(doi_clean, aes(x = Negative.1, y = Number.False.Alerts)) +
  geom_col (fill = "skyblue") +
  labs(title = "Number of False Alerts Per Negative Sample", x = "Sample", y = "Number of False Alerts")



##### Limitation: duration was measured for false alerts but not true alerts
###Break false alerts down so that each false alert is its own record with its own single duration
###How many true alerts happened without a false alert before it
###On any day, how does first, second, etc. false alerts impact others. ex on a day with 5 false alerts does duration decrease more than on a day with 2 false alerts
#Bar chart - create ID of date and FA1, date and FA2, atc
#Each day is a time scale - looking at memory consolidation (latent learning)
#How does one days events affect the next day
#Within subjects test
#Should check in
#Collect data on people in course. Be able to observe issues common within each breed


#Create new datasets that break each FA into their own record. Unique ID = date + FA#
#Madi
# Exclude specified columns before reshaping
madi_filtered <- madi[, !grepl("^FAduration$|^Number.True.Alerts$|^Number.False.Alerts$|^X..of.correct.dismissals$|^X..of.misses$", colnames(madi))]


# Reshape the dataframe using pivot_longer
madi_falses <- pivot_longer(madi_filtered, 
                              cols = starts_with("FA"), 
                              names_to = "FA", 
                              values_to = "FA_time")

# Replace NA values in FA_time with 0 where FA is FA1..s.
madi_falses$FA_time[madi_falses$FA == "FA1..s." & is.na(madi_falses$FA_time)] <- 0

# Remove rows where FA_time is NA
madi_falses <- madi_falses[complete.cases(madi_falses), ]

madi_falses$FA <- gsub("FA", "", madi_falses$FA)
madi_falses$FA <- gsub("\\.\\.s\\.$", "", madi_falses$FA)
madi_falses$FA <- as.numeric(madi_falses$FA)


#Persi
# Exclude specified columns before reshaping
persi_filtered <- persi[, !grepl("^FAduration$|^Number.True.Alerts$|^Number.False.Alerts$|^X..of.correct.dismissals$|^X..of.misses$", colnames(persi))]

# Reshape the dataframe using pivot_longer
persi_falses <- pivot_longer(persi_filtered, 
                             cols = starts_with("FA"), 
                             names_to = "FA", 
                             values_to = "FA_time")

# Replace NA values in FA_time with 0 where FA is FA1..s.
persi_falses$FA_time[persi_falses$FA == "FA1..s." & is.na(persi_falses$FA_time)] <- 0

# Remove rows where FA_time is NA
persi_falses <- persi_falses[complete.cases(persi_falses), ]

# Change values in the FA column
persi_falses$FA <- gsub("FA", "", persi_falses$FA)
persi_falses$FA <- gsub("\\.\\.s\\.$", "", persi_falses$FA)
persi_falses$FA <- as.numeric(persi_falses$FA)

#Remove filtered tables to keep data environment cleaner
rm(madi_filtered)
rm(persi_filtered)





#####################################################################################
#########Explore falses data#########################################################






# Kayla finding average number of repetitions per session
# first create a new column for session number
doi_clean$session <- NA

# create new variable
counter <- 0

# assign values based on repetition resets
for (i in 1:length(doi_clean$session)) {
  if (doi_clean$Rep.Number[i] == 1) {
    counter <- counter + 1
  }
  doi_clean$session[i] <- counter
}

 # count number of repetitions within a session
reps_by_session <- count(doi_clean, session, name = "reps")

# get summary stats
summary(reps_by_session)

#Here's another way -RH
#This gets a slightly different answer because we had a few days where the dogs had more than 1 session per date - KF
result <- madi %>%
  group_by(Date) %>%
  summarize(AverageValue = mean(Rep.Number, na.rm = TRUE))
mean(result$AverageValue)