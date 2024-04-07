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
library(ggplot2)

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

# add row for training repetition number overall
madi$rep_total <- seq(1, length.out = nrow(madi))
persi$rep_total <- seq(1, length.out = nrow(persi))

#Create new datasets that break each FA into their own record
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

#Add new column to where value is 1 if that repetition was completed without any false alerts, 0 when false alerts were recorded.
madi_falses <- madi_falses %>%
  mutate(no_false = ifelse(FA_time == 0, 1, 0))
persi_falses <- persi_falses %>%
  mutate(no_false = ifelse(FA_time == 0, 1, 0))

#Add numbers for unique false alert identifiers
madi_falses$false_id <- seq(from = 1, to = nrow(madi_falses))
persi_falses$false_id <- seq(from = 1, to = nrow(persi_falses))


#Remove filtered tables to keep data environment cleaner
rm(madi_filtered)
rm(persi_filtered)


#Add no false to madi and persi
madi <- madi %>%
  mutate(no_false = ifelse(FAduration == 0, 1, 0))
persi <- persi %>%
  mutate(no_false = ifelse(FAduration == 0, 1, 0))

madi_date <- madi %>%
  group_by(Date) %>%
  summarise(
    repetitions = n(),
    FA_duration = sum(FAduration),
    num_false_alerts = sum(ifelse(no_false == 1, 0, 1))
  )
madi_date$falseperrep <- madi_date$num_false_alerts / madi_date$repetitions
madi_date$day <- seq_len(nrow(madi_date))


persi_date <- persi %>%
  group_by(Date) %>%
  summarise(
    repetitions = n(),
    FA_duration = sum(FAduration),
    num_false_alerts = sum(ifelse(no_false == 1, 0, 1))
  )
persi_date$falseperrep <- persi_date$num_false_alerts / persi_date$repetitions
persi_date$day <- seq_len(nrow(persi_date))

#######Add misses to _date dataframes
madi_misses <- madi %>%
  group_by(Date) %>%
  summarise(misses = sum(X..of.misses, na.rm = TRUE))

madi_date <- merge(madi_date, madi_misses, by = "Date", all.x = TRUE)

persi_misses <- persi %>%
  group_by(Date) %>%
  summarise(misses = sum(X..of.misses, na.rm = TRUE))

persi_date <- merge(persi_date, persi_misses, by = "Date", all.x = TRUE)

rm(madi_misses)
rm(persi_misses)


###################################################################################
#########################Visualizations############################################
#correlation tests
cor.test(madi$rep_total, madi$Number.False.Alerts)
cor.test(madi$rep_total, madi$FAduration)
cor.test(persi$rep_total, persi$Number.False.Alerts)
cor.test(persi$rep_total, persi$FAduration)

#Plots Persi number of false alerts by repetition and sum duration of false alerts
library(ggplot2)
plot1 <- ggplot(persi, aes(x = rep_total, y = Number.False.Alerts)) +
  geom_col (fill = "skyblue") +
  labs(title = "Persi False Alerts Over Time (Repetitions)", x = "Repetition #", y = "# False Alerts")+
  xlim(0, 150) +
  ylim(0, 10) + theme_classic()

plot2 <- ggplot(persi, aes(x = rep_total, y = FAduration)) +
  geom_col (fill = "skyblue") +
  labs(title = "Persi False Alert Duration Over Time (Repetitions)", x = "Repetition #", y = "False Alert Duration, Sum for Repetition")+
  xlim(0, 150)+ 
  ylim(0, 115) + theme_classic()


#Plots Madi number of false alerts by repetition and sum duration of false alerts
plot3 <- ggplot(madi, aes(x = rep_total, y = Number.False.Alerts)) +
  geom_col (fill = "skyblue") +
  labs(title = "Madi False Alerts Over Time (Repetitions)", x = "Repetition #", y = "# False Alerts") + theme_classic()

plot4 <- ggplot(madi, aes(x = rep_total, y = FAduration)) +
  geom_col (fill = "skyblue") +
  labs(title = "Madi False Alert Duration Over Time (Repetitions)", x = "Repetition #", y = "False Alert Duration, Sum for Repetition")+
  ylim(0, 600) + theme_classic()

#Number False Alerts per Negative Sample
ggplot(doi_clean, aes(x = Negative.1, y = Number.False.Alerts)) +
  geom_col (fill = "skyblue") +
  labs(title = "Number of False Alerts Per Negative Sample", x = "Sample", y = "Number of False Alerts") + theme_classic()


#########Explore falses data (repetition)################################################
plot5 <- ggplot(persi_falses, aes(x = false_id, y = FA_time)) +
  geom_col (fill = "skyblue") +
  labs(title = "Persi False Alert Duration Over Time (Repetitions)", x = "False alert #", y = "False Alert Duration, per Individual False Alert")+
  ylim(0, 80) + scale_x_continuous(breaks = seq(0, 160, by = 10)) +theme_classic()

plot6 <- ggplot(madi_falses, aes(x = false_id, y = FA_time)) +
  geom_col (fill = "skyblue") +
  labs(title = "Madi False Alerts Over Time (Repetitions)", x = "False alert #", y = "False Alert Duration, per Individual False Alert")+
  ylim(0, 600) +  scale_x_continuous(breaks = seq(0, 110, by = 10)) + theme_classic()


###Data by date (session)
#Madi
plot7 <- ggplot(madi_date, aes(x = Date, y = falseperrep)) +
  geom_col (fill = "skyblue") +
  labs(title = "Average Number of False Alerts per Repetition", x = "Date", y = "Sum False Alerts/Number of Reps") + theme_classic()

plot8 <- ggplot(madi_date, aes(x = Date, y = FA_duration)) +
  geom_col (fill = "skyblue") +
  labs(title = "Sum of False Alert Duration per Day", x = "Date", y = "False Alert Duration (seconds)") + theme_classic()

plot9 <- ggplot(madi_date, aes(x = day, y = falseperrep)) +
  geom_col (fill = "skyblue") +
  labs(title = "False Alerts per Session", x = "Session", y = "Sum False Alerts/Number of Reps") + theme_classic()

plot10 <- ggplot(madi_date, aes(x = day, y = FA_duration)) +
  geom_col (fill = "skyblue") +
  labs(title = "Sum of False Alert Duration per Session", x = "Session", y = "Duration (seconds)") + theme_classic()

#Persi
plot11 <- ggplot(persi_date, aes(x = Date, y = falseperrep)) +
  geom_col (fill = "skyblue") +
  labs(title = "Average Number of False Alerts per Repetition", x = "Date", y = "Sum False Alerts/Number of Reps") + theme_classic()

plot12 <- ggplot(persi_date, aes(x = Date, y = FA_duration)) +
  geom_col (fill = "skyblue") +
  labs(title = "Sum of False Alert Duration per Day", x = "Date", y = "False Alert Duration (seconds)") + theme_classic()

plot13 <- ggplot(persi_date, aes(x = day, y = falseperrep)) +
  geom_col (fill = "skyblue") +
  labs(title = "False Alerts per Session", x = "Session", y = "Sum False Alerts/Number of Reps") + theme_classic() + ylim(0, 3)

plot14 <- ggplot(persi_date, aes(x = day, y = FA_duration)) +
  geom_col (fill = "skyblue") +
  labs(title = "Sum of False Alert Duration per Session", x = "Session", y = "False Alert Duration (seconds)") + theme_classic()


#Put multiple charts into one grid
install.packages("gridExtra")
library(gridExtra)
grid.arrange(plot13, plot14, nrow = 1, ncol = 2)


##############Misses####################
ggplot(persi_date, aes(x = day, y = misses)) +
  geom_col (fill = "skyblue") +
  labs(title = "Misses per Session", x = "Session", y = "Misses") + theme_classic() +ylim(0, 10)

ggplot(madi_date, aes(x = day, y = misses)) +
  geom_col (fill = "skyblue") +
  labs(title = "Misses per Session", x = "Session", y = "Misses") + theme_classic() +ylim(0, 10)






###Average number of repetitions per session
#For reference within paper
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
