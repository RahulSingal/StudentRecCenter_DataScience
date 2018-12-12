completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}


DemoGraphics = as.data.frame(read.csv("DS_ClassProject_Fall2018_data/Demographics.csv"))
numNA_Trandate <- sum(is.na(DemoGraphics$TRANDATE))
NA_Percent_Trandate <- (numNA_Trandate / length(DemoGraphics$TRANDATE)) * 100
DemoGraphics <- completeFun(DemoGraphics, "TRANDATE")

# We do not care about the TRANSID, student_flag, adj_admit_type, admit_term, and HOUSING_TYPE, 
#                                                             so we will remove these 5 columns
DemoGraphics <- subset(DemoGraphics, select = -c(TRANSID, student_flag, adj_admit_type, admit_term, HOUSING_TYPE))

new = by(DemoGraphics, DemoGraphics[,"record_term"], function(x) x)

Fall2012 = new[[1]] ## Fall 2012 [2012-08-17 to 2013-01-06] 
Spring2013 = new[[2]] ## Spring 2013 [2013-01-07 to 2013-05-05] 
Fall2013 = new[[4]] ## Fall 2013 [2013-08-09 to 2014-01-12] 
Spring2014 = new[[5]] ## Spring 2014 [2014-01-13 to 2014-05-11] 
Fall2014 = new[[7]] ## Fall 2014 [2014-08-15 to 2015-01-11] 
Spring2015 = new[[8]] ## Spring 2015 [2015-01-12 to 2015-05-10] 

### Academic Year 2012-2013
Fall2012_Unique_Swipes <- as.data.frame(table(Fall2012$PATRONID.x))
Fall2012_Freq_Count <- as.data.frame(Fall2012_Unique_Swipes[order(-Fall2012_Unique_Swipes$Freq), ])
names(Fall2012_Freq_Count)[1] <- "PATRONID" #Change column name
Spring2013_Unique_Swipes <- as.data.frame(table(Spring2013$PATRONID.x))
Spring2013_Freq_Count <- as.data.frame(Spring2013_Unique_Swipes[order(-Spring2013_Unique_Swipes$Freq), ])
names(Spring2013_Freq_Count)[1] <- "PATRONID" #Change column name

##### Fall 2012 and Spring 2013 Frequency Profiles
Fall2012_Frequent <- subset(Fall2012_Freq_Count, Freq<=160 & Freq>=100)
Fall2012_Moderate <- subset(Fall2012_Freq_Count, Freq<=99 & Freq>=50)
Fall2012_Ocassional <- subset(Fall2012_Freq_Count, Freq<=49 & Freq>=25)
Fall2012_Infrequent <- subset(Fall2012_Freq_Count, Freq<=24 & Freq>=10)
Spring2013_Frequent <- subset(Spring2013_Freq_Count, Freq<=175 & Freq>=100)
Spring2013_Moderate <- subset(Spring2013_Freq_Count, Freq<=99 & Freq>=50)
Spring2013_Ocassional <- subset(Spring2013_Freq_Count, Freq<=49 & Freq>=25)
Spring2013_Infrequent <- subset(Spring2013_Freq_Count, Freq<=24 & Freq>=10)

# Step 1 Getting all the times for Frequent goers in Fall 2012 and Spring 2013
Fall2012_Frequent_Times = subset(Fall2012, PATRONID.x %in% Fall2012_Frequent$PATRONID)
Spring2013_Frequent_Times = subset(Spring2013, PATRONID.x %in% Spring2013_Frequent$PATRONID)
# Step 2 Calculating the Number of Times user went at peak times and the percentages
Fall2012_Frequent_Times = Fall2012_Frequent_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Fall2012_Frequent_Peak_Count <- subset(Fall2012_Frequent_Times, Time>="14:00:00" & Time<="19:30:00")
Fall2012_Frequent_Peak_Percentage <- length(Fall2012_Frequent_Peak_Count$Date) / (length(Fall2012_Frequent_Times$Date)) *100
Spring2013_Frequent_Times = Spring2013_Frequent_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Spring2013_Frequent_Peak_Count <- subset(Spring2013_Frequent_Times, Time>="14:00:00" & Time<="19:30:00")
Spring2013_Frequent_Peak_Percentage <- length(Spring2013_Frequent_Peak_Count$Date) / (length(Spring2013_Frequent_Times$Date)) *100


# Step 1 Getting all the times for Moderate goers in Fall 2012 and Spring 2013
Fall2012_Moderate_Times = subset(Fall2012, PATRONID.x %in% Fall2012_Moderate$PATRONID)
Spring2013_Moderate_Times = subset(Spring2013, PATRONID.x %in% Spring2013_Moderate$PATRONID)
# Step 2 Calculating the Number of Times user went at peak times and the percentages
Fall2012_Moderate_Times = Fall2012_Moderate_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Fall2012_Moderate_Peak_Count <- subset(Fall2012_Moderate_Times, Time>="14:00:00" & Time<="19:30:00")
Fall2012_Moderate_Peak_Percentage <- length(Fall2012_Moderate_Peak_Count$Date) / (length(Fall2012_Moderate_Times$Date)) *100
Spring2013_Moderate_Times = Spring2013_Moderate_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Spring2013_Moderate_Peak_Count <- subset(Spring2013_Moderate_Times, Time>="14:00:00" & Time<="19:30:00")
Spring2013_Moderate_Peak_Percentage <- length(Spring2013_Moderate_Peak_Count$Date) / (length(Spring2013_Moderate_Times$Date)) *100

# Step 1 Getting all the times for Ocassional goers in Fall 2012 and Spring 2013
Fall2012_Ocassional_Times = subset(Fall2012, PATRONID.x %in% Fall2012_Ocassional$PATRONID)
Spring2013_Ocassional_Times = subset(Spring2013, PATRONID.x %in% Spring2013_Ocassional$PATRONID)
# Step 2 Calculating the Number of Times user went at peak times and the percentages
Fall2012_Ocassional_Times = Fall2012_Ocassional_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Fall2012_Ocassional_Peak_Count <- subset(Fall2012_Ocassional_Times, Time>="14:00:00" & Time<="19:30:00")
Fall2012_Ocassional_Peak_Percentage <- length(Fall2012_Ocassional_Peak_Count$Date) / (length(Fall2012_Ocassional_Times$Date)) *100
Spring2013_Ocassional_Times = Spring2013_Ocassional_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Spring2013_Ocassional_Peak_Count <- subset(Spring2013_Ocassional_Times, Time>="14:00:00" & Time<="19:30:00")
Spring2013_Ocassional_Peak_Percentage <- length(Spring2013_Ocassional_Peak_Count$Date) / (length(Spring2013_Ocassional_Times$Date)) *100

# Step 1 Getting all the times for Infrequent goers in Fall 2012
Fall2012_Infrequent_Times = subset(Fall2012, PATRONID.x %in% Fall2012_Infrequent$PATRONID)
Spring2013_Infrequent_Times = subset(Spring2013, PATRONID.x %in% Spring2013_Infrequent$PATRONID)
# Step 2 Calculating the Number of Times user went at peak times and the percentages
Fall2012_Infrequent_Times = Fall2012_Infrequent_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Fall2012_Infrequent_Peak_Count <- subset(Fall2012_Infrequent_Times, Time>="14:00:00" & Time<="19:30:00")
Fall2012_Infrequent_Peak_Percentage <- length(Fall2012_Infrequent_Peak_Count$Date) / (length(Fall2012_Infrequent_Times$Date)) *100
Spring2013_Infrequent_Times = Spring2013_Infrequent_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Spring2013_Infrequent_Peak_Count <- subset(Spring2013_Infrequent_Times, Time>="14:00:00" & Time<="19:30:00")
Spring2013_Infrequent_Peak_Percentage <- length(Spring2013_Infrequent_Peak_Count$Date) / (length(Spring2013_Infrequent_Times$Date)) *100

### Academic Year 2013-2014
Fall2013_Unique_Swipes <- as.data.frame(table(Fall2013$PATRONID.x))
Fall2013_Freq_Count <- as.data.frame(Fall2013_Unique_Swipes[order(-Fall2013_Unique_Swipes$Freq), ])
names(Fall2013_Freq_Count)[1] <- "PATRONID" #Change column name
Spring2014_Unique_Swipes <- as.data.frame(table(Spring2014$PATRONID.x))
Spring2014_Freq_Count <- as.data.frame(Spring2014_Unique_Swipes[order(-Spring2014_Unique_Swipes$Freq), ])
names(Spring2014_Freq_Count)[1] <- "PATRONID" #Change column name

##### Fall 2012 and Spring 2013 Frequency Profiles
Fall2013_Frequent <- subset(Fall2013_Freq_Count, Freq<=175 & Freq>=100)
Fall2013_Moderate <- subset(Fall2013_Freq_Count, Freq<=99 & Freq>=50)
Fall2013_Ocassional <- subset(Fall2013_Freq_Count, Freq<=49 & Freq>=25)
Fall2013_Infrequent <- subset(Fall2013_Freq_Count, Freq<=24 & Freq>=10)
Spring2014_Frequent <- subset(Spring2014_Freq_Count, Freq<=175 & Freq>=100)
Spring2014_Moderate <- subset(Spring2014_Freq_Count, Freq<=99 & Freq>=50)
Spring2014_Ocassional <- subset(Spring2014_Freq_Count, Freq<=49 & Freq>=25)
Spring2014_Infrequent <- subset(Spring2014_Freq_Count, Freq<=24 & Freq>=10)

# Step 1 Getting all the times for Frequent goers in Fall 2013 and Spring 2014
Fall2013_Frequent_Times = subset(Fall2013, PATRONID.x %in% Fall2013_Frequent$PATRONID)
Spring2014_Frequent_Times = subset(Spring2014, PATRONID.x %in% Spring2014_Frequent$PATRONID)
# Step 2 Calculating the Number of Times user went at peak times and the percentages
Fall2013_Frequent_Times = Fall2013_Frequent_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Fall2013_Frequent_Peak_Count <- subset(Fall2013_Frequent_Times, Time>="14:00:00" & Time<="19:30:00")
Fall2013_Frequent_Peak_Percentage <- length(Fall2013_Frequent_Peak_Count$Date) / (length(Fall2013_Frequent_Times$Date)) *100
Spring2014_Frequent_Times = Spring2014_Frequent_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Spring2014_Frequent_Peak_Count <- subset(Spring2014_Frequent_Times, Time>="14:00:00" & Time<="19:30:00")
Spring2014_Frequent_Peak_Percentage <- length(Spring2014_Frequent_Peak_Count$Date) / (length(Spring2014_Frequent_Times$Date)) *100


# Step 1 Getting all the times for Moderate goers in Fall 2013 and Spring 2014
Fall2013_Moderate_Times = subset(Fall2013, PATRONID.x %in% Fall2013_Moderate$PATRONID)
Spring2014_Moderate_Times = subset(Spring2014, PATRONID.x %in% Spring2014_Moderate$PATRONID)
# Step 2 Calculating the Number of Times user went at peak times and the percentages
Fall2013_Moderate_Times = Fall2013_Moderate_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Fall2013_Moderate_Peak_Count <- subset(Fall2013_Moderate_Times, Time>="14:00:00" & Time<="19:30:00")
Fall2013_Moderate_Peak_Percentage <- length(Fall2013_Moderate_Peak_Count$Date) / (length(Fall2013_Moderate_Times$Date)) *100
Spring2014_Moderate_Times = Spring2014_Moderate_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Spring2014_Moderate_Peak_Count <- subset(Spring2014_Moderate_Times, Time>="14:00:00" & Time<="19:30:00")
Spring2014_Moderate_Peak_Percentage <- length(Spring2014_Moderate_Peak_Count$Date) / (length(Spring2014_Moderate_Times$Date)) *100

# Step 1 Getting all the times for Ocassional goers in Fall 2014 and Spring 2014
Fall2013_Ocassional_Times = subset(Fall2013, PATRONID.x %in% Fall2013_Ocassional$PATRONID)
Spring2014_Ocassional_Times = subset(Spring2014, PATRONID.x %in% Spring2014_Ocassional$PATRONID)
# Step 2 Calculating the Number of Times user went at peak times and the percentages
Fall2013_Ocassional_Times = Fall2013_Ocassional_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Fall2013_Ocassional_Peak_Count <- subset(Fall2013_Ocassional_Times, Time>="14:00:00" & Time<="19:30:00")
Fall2013_Ocassional_Peak_Percentage <- length(Fall2013_Ocassional_Peak_Count$Date) / (length(Fall2013_Ocassional_Times$Date)) *100
Spring2014_Ocassional_Times = Spring2014_Ocassional_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Spring2014_Ocassional_Peak_Count <- subset(Spring2014_Ocassional_Times, Time>="14:00:00" & Time<="19:30:00")
Spring2014_Ocassional_Peak_Percentage <- length(Spring2014_Ocassional_Peak_Count$Date) / (length(Spring2014_Ocassional_Times$Date)) *100

# Step 1 Getting all the times for Infrequent goers in Fall 2013 and Spring 2014
Fall2013_Infrequent_Times = subset(Fall2013, PATRONID.x %in% Fall2013_Infrequent$PATRONID)
Spring2014_Infrequent_Times = subset(Spring2014, PATRONID.x %in% Spring2014_Infrequent$PATRONID)
# Step 2 Calculating the Number of Times user went at peak times and the percentages
Fall2013_Infrequent_Times = Fall2013_Infrequent_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Fall2013_Infrequent_Peak_Count <- subset(Fall2013_Infrequent_Times, Time>="14:00:00" & Time<="19:30:00")
Fall2013_Infrequent_Peak_Percentage <- length(Fall2013_Infrequent_Peak_Count$Date) / (length(Fall2013_Infrequent_Times$Date)) *100
Spring2014_Infrequent_Times = Spring2014_Infrequent_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Spring2014_Infrequent_Peak_Count <- subset(Spring2014_Infrequent_Times, Time>="14:00:00" & Time<="19:30:00")
Spring2014_Infrequent_Peak_Percentage <- length(Spring2014_Infrequent_Peak_Count$Date) / (length(Spring2014_Infrequent_Times$Date)) *100


### Academic Year 2014-2015
Fall2014_Unique_Swipes <- as.data.frame(table(Fall2014$PATRONID.x))
Fall2014_Freq_Count <- as.data.frame(Fall2014_Unique_Swipes[order(-Fall2014_Unique_Swipes$Freq), ])
names(Fall2014_Freq_Count)[1] <- "PATRONID" #Change column name
Spring2015_Unique_Swipes <- as.data.frame(table(Spring2015$PATRONID.x))
Spring2015_Freq_Count <- as.data.frame(Spring2015_Unique_Swipes[order(-Spring2015_Unique_Swipes$Freq), ])
names(Spring2015_Freq_Count)[1] <- "PATRONID" #Change column name

##### Fall 2012 and Spring 2013 Frequency Profiles
Fall2014_Frequent <- subset(Fall2014_Freq_Count, Freq<=140 & Freq>=75)
Fall2014_Moderate <- subset(Fall2014_Freq_Count, Freq<=74 & Freq>=40)
Fall2014_Ocassional <- subset(Fall2014_Freq_Count, Freq<=39 & Freq>=20)
Fall2014_Infrequent <- subset(Fall2014_Freq_Count, Freq<=19 & Freq>=10)
Spring2015_Frequent <- subset(Spring2015_Freq_Count, Freq<=140 & Freq>=75)
Spring2015_Moderate <- subset(Spring2015_Freq_Count, Freq<=74 & Freq>=40)
Spring2015_Ocassional <- subset(Spring2015_Freq_Count, Freq<=39 & Freq>=20)
Spring2015_Infrequent <- subset(Spring2015_Freq_Count, Freq<=19 & Freq>=10)

# Step 1 Getting all the times for Frequent goers in Fall 2012 and Spring 2013
Fall2014_Frequent_Times = subset(Fall2014, PATRONID.x %in% Fall2014_Frequent$PATRONID)
Spring2015_Frequent_Times = subset(Spring2015, PATRONID.x %in% Spring2015_Frequent$PATRONID)
# Step 2 Calculating the Number of Times user went at peak times and the percentages
Fall2014_Frequent_Times = Fall2014_Frequent_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Fall2014_Frequent_Peak_Count <- subset(Fall2014_Frequent_Times, Time>="14:00:00" & Time<="19:30:00")
Fall2014_Frequent_Peak_Percentage <- length(Fall2014_Frequent_Peak_Count$Date) / (length(Fall2014_Frequent_Times$Date)) *100
Spring2015_Frequent_Times = Spring2015_Frequent_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Spring2015_Frequent_Peak_Count <- subset(Spring2015_Frequent_Times, Time>="14:00:00" & Time<="19:30:00")
Spring2015_Frequent_Peak_Percentage <- length(Spring2015_Frequent_Peak_Count$Date) / (length(Spring2015_Frequent_Times$Date)) *100


# Step 1 Getting all the times for Moderate goers in Fall 2012 and Spring 2013
Fall2014_Moderate_Times = subset(Fall2014, PATRONID.x %in% Fall2014_Moderate$PATRONID)
Spring2015_Moderate_Times = subset(Spring2015, PATRONID.x %in% Spring2015_Moderate$PATRONID)
# Step 2 Calculating the Number of Times user went at peak times and the percentages
Fall2014_Moderate_Times = Fall2014_Moderate_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Fall2014_Moderate_Peak_Count <- subset(Fall2014_Moderate_Times, Time>="14:00:00" & Time<="19:30:00")
Fall2014_Moderate_Peak_Percentage <- length(Fall2014_Moderate_Peak_Count$Date) / (length(Fall2014_Moderate_Times$Date)) *100
Spring2015_Moderate_Times = Spring2015_Moderate_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Spring2015_Moderate_Peak_Count <- subset(Spring2015_Moderate_Times, Time>="14:00:00" & Time<="19:30:00")
Spring2015_Moderate_Peak_Percentage <- length(Spring2015_Moderate_Peak_Count$Date) / (length(Spring2015_Moderate_Times$Date)) *100

# Step 1 Getting all the times for Ocassional goers in Fall 2012 and Spring 2013
Fall2014_Ocassional_Times = subset(Fall2014, PATRONID.x %in% Fall2014_Ocassional$PATRONID)
Spring2015_Ocassional_Times = subset(Spring2015, PATRONID.x %in% Spring2015_Ocassional$PATRONID)
# Step 2 Calculating the Number of Times user went at peak times and the percentages
Fall2014_Ocassional_Times = Fall2014_Ocassional_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Fall2014_Ocassional_Peak_Count <- subset(Fall2014_Ocassional_Times, Time>="14:00:00" & Time<="19:30:00")
Fall2014_Ocassional_Peak_Percentage <- length(Fall2014_Ocassional_Peak_Count$Date) / (length(Fall2014_Ocassional_Times$Date)) *100
Spring2015_Ocassional_Times = Spring2015_Ocassional_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Spring2015_Ocassional_Peak_Count <- subset(Spring2015_Ocassional_Times, Time>="14:00:00" & Time<="19:30:00")
Spring2015_Ocassional_Peak_Percentage <- length(Spring2015_Ocassional_Peak_Count$Date) / (length(Spring2015_Ocassional_Times$Date)) *100

# Step 1 Getting all the times for Infrequent goers in Fall 2012
Fall2014_Infrequent_Times = subset(Fall2014, PATRONID.x %in% Fall2014_Infrequent$PATRONID)
Spring2015_Infrequent_Times = subset(Spring2015, PATRONID.x %in% Spring2015_Infrequent$PATRONID)
# Step 2 Calculating the Number of Times user went at peak times and the percentages
Fall2014_Infrequent_Times = Fall2014_Infrequent_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Fall2014_Infrequent_Peak_Count <- subset(Fall2014_Infrequent_Times, Time>="14:00:00" & Time<="19:30:00")
Fall2014_Infrequent_Peak_Percentage <- length(Fall2014_Infrequent_Peak_Count$Date) / (length(Fall2014_Infrequent_Times$Date)) *100
Spring2015_Infrequent_Times = Spring2015_Infrequent_Times %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Spring2015_Infrequent_Peak_Count <- subset(Spring2015_Infrequent_Times, Time>="14:00:00" & Time<="19:30:00")
Spring2015_Infrequent_Peak_Percentage <- length(Spring2015_Infrequent_Peak_Count$Date) / (length(Spring2015_Infrequent_Times$Date)) *100

ColorPalette <- c("#c2e699", "#78c679", "#31a354","#006837")
Plot <- data.frame("Frequency" = "Frequent", "Semester" = "Fall2012", "Percentage" = Fall2012_Frequent_Peak_Percentage, stringsAsFactors = FALSE)
Plot <- rbind(Plot, list("Frequent", "Spring2013", Spring2013_Frequent_Peak_Percentage))
Plot <- rbind(Plot, list("Frequent", "Fall2013", Fall2013_Frequent_Peak_Percentage))
Plot <- rbind(Plot, list("Frequent", "Spring2014", Spring2014_Frequent_Peak_Percentage))
Plot <- rbind(Plot, list("Frequent", "Fall2014", Fall2014_Frequent_Peak_Percentage))
Plot <- rbind(Plot, list("Frequent", "Spring2015", Spring2015_Frequent_Peak_Percentage))
Plot <- rbind(Plot, list("Moderate", "Fall2012", Fall2012_Moderate_Peak_Percentage))
Plot <- rbind(Plot, list("Moderate", "Spring2013", Spring2013_Moderate_Peak_Percentage))
Plot <- rbind(Plot, list("Moderate", "Fall2013", Fall2013_Moderate_Peak_Percentage))
Plot <- rbind(Plot, list("Moderate", "Spring2014", Spring2014_Moderate_Peak_Percentage))
Plot <- rbind(Plot, list("Moderate", "Fall2014", Fall2014_Moderate_Peak_Percentage))
Plot <- rbind(Plot, list("Moderate", "Spring2015", Spring2015_Moderate_Peak_Percentage))
Plot <- rbind(Plot, list("Ocassional", "Fall2012", Fall2012_Ocassional_Peak_Percentage))
Plot <- rbind(Plot, list("Ocassional", "Spring2013", Spring2013_Ocassional_Peak_Percentage))
Plot <- rbind(Plot, list("Ocassional", "Fall2013", Fall2013_Ocassional_Peak_Percentage))
Plot <- rbind(Plot, list("Ocassional", "Spring2014", Spring2014_Ocassional_Peak_Percentage))
Plot <- rbind(Plot, list("Ocassional", "Fall2014", Fall2014_Ocassional_Peak_Percentage))
Plot <- rbind(Plot, list("Ocassional", "Spring2015", Spring2015_Ocassional_Peak_Percentage))
Plot <- rbind(Plot, list("Infrequent", "Fall2012", Fall2012_Infrequent_Peak_Percentage))
Plot <- rbind(Plot, list("Infrequent", "Spring2013", Spring2013_Infrequent_Peak_Percentage))
Plot <- rbind(Plot, list("Infrequent", "Fall2013", Fall2013_Infrequent_Peak_Percentage))
Plot <- rbind(Plot, list("Infrequent", "Spring2014", Spring2014_Infrequent_Peak_Percentage))
Plot <- rbind(Plot, list("Infrequent", "Fall2014", Fall2014_Infrequent_Peak_Percentage))
Plot <- rbind(Plot, list("Infrequent", "Spring2015", Spring2015_Infrequent_Peak_Percentage))
Actual_Plot <- ggplot(Plot, aes(Semester, Percentage)) + geom_bar(aes(fill=Frequency),position="dodge",stat="identity",width=.55)+
  xlab("Semester")+ylab("Percentages")+ggtitle("Frequent Peak Percentages for All Semesters")+
  scale_fill_manual(values=ColorPalette)






