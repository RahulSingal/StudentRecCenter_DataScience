# Code for 475 Project - Rahul Singal

library(expss)

DemoGraphics = as.data.frame(read.csv("DS_ClassProject_Fall2018_data/Demographics.csv"))


# We do not care about the TRANSID, student_flag, adj_admit_type, admit_term, and HOUSING_TYPE, 
#                                                             so we will remove these 5 columns
DemoGraphics <- subset(DemoGraphics, select = -c(TRANSID, student_flag, adj_admit_type, admit_term, HOUSING_TYPE))

# Seperates by semester
new = by(DemoGraphics, DemoGraphics[,"record_term"], function(x) x)

Fall2012 = new[[1]] ## Fall 2012 [2012-08-17 to 2013-01-06] 
Spring2013 = new[[2]] ## Spring 2013 [2013-01-07 to 2013-05-05] 
Summer2013 = new[[3]] ## Summer 2013 [2013-05-06 to 2013-08-08]
Fall2013 = new[[4]] ## Fall 2013 [2013-08-09 to 2014-01-12] 
Spring2014 = new[[5]] ## Spring 2014 [2014-01-13 to 2014-05-11] 
Summer2014 = new[[6]] ## Summer 2014 [2014-05-12 to 2014-08-14] 
Fall2014 = new[[7]] ## Fall 2014 [2014-08-15 to 2015-01-11] 
Spring2015 = new[[8]] ## Spring 2015 [2015-01-12 to 2015-05-10] 
Summer2015 = new[[9]] ## Summer 2015 [2015-05-11 to 2015-08-13] 
Fall2015 = new[[10]] ## Fall 2015 [INCOMPLETE. ONLY 3 DAYS]

sumation <- length(Fall2012$TRANDATE) + length(Spring2013$TRANDATE) + length(Fall2013$TRANDATE) + length(Spring2014$TRANDATE) +
            length(Fall2014$TRANDATE) + length(Spring2015$TRANDATE) + length(Fall2015$TRANDATE) + length(Summer2013$TRANDATE) +
            length(Summer2014$TRANDATE) + length(Summer2015$TRANDATE)

### Academic Year 2012-2013
Fall2012_Unique_Swipes <- as.data.frame(table(Fall2012$PATRONID.x))
Fall2012_Freq_Count <- as.data.frame(Fall2012_Unique_Swipes[order(-Fall2012_Unique_Swipes$Freq), ])
names(Fall2012_Freq_Count)[1] <- "PATRONID" #Change column name
Spring2013_Unique_Swipes <- as.data.frame(table(Spring2013$PATRONID.x))
Spring2013_Freq_Count <- as.data.frame(Spring2013_Unique_Swipes[order(-Spring2013_Unique_Swipes$Freq), ])
names(Spring2013_Freq_Count)[1] <- "PATRONID" #Change column name
Summer2013_Unique_Swipes <- as.data.frame((table(Summer2013$PATRONID.x)))
Summer2013_Freq_Count <- as.data.frame(Summer2013_Unique_Swipes[order(-Summer2013_Unique_Swipes$Freq), ])
names(Summer2013_Freq_Count)[1] <- "PATRONID" #Change column name

### Academic Year 2013-2014
Fall2013_Unique_Swipes <- as.data.frame(table(Fall2013$PATRONID.x))
Fall2013_Freq_Count <- as.data.frame(Fall2013_Unique_Swipes[order(-Fall2013_Unique_Swipes$Freq), ])
names(Fall2013_Freq_Count)[1] <- "PATRONID" #Change column name
Spring2014_Unique_Swipes <- as.data.frame(table(Spring2014$PATRONID.x))
Spring2014_Freq_Count <- as.data.frame(Spring2014_Unique_Swipes[order(-Spring2014_Unique_Swipes$Freq), ])
names(Spring2014_Freq_Count)[1] <- "PATRONID" #Change column name
Summer2014_Unique_Swipes <- as.data.frame((table(Summer2014$PATRONID.x)))
Summer2014_Freq_Count <- as.data.frame(Summer2014_Unique_Swipes[order(-Summer2014_Unique_Swipes$Freq), ])
names(Summer2014_Freq_Count)[1] <- "PATRONID" #Change column name

### Academic Year 2014-2015
Fall2014_Unique_Swipes <- as.data.frame(table(Fall2014$PATRONID.x))
Fall2014_Freq_Count <- as.data.frame(Fall2014_Unique_Swipes[order(-Fall2014_Unique_Swipes$Freq), ])
names(Fall2014_Freq_Count)[1] <- "PATRONID" #Change column name
Spring2015_Unique_Swipes <- as.data.frame(table(Spring2015$PATRONID.x))
Spring2015_Freq_Count <- as.data.frame(Spring2015_Unique_Swipes[order(-Spring2015_Unique_Swipes$Freq), ])
names(Spring2015_Freq_Count)[1] <- "PATRONID" #Change column name
Summer2015_Unique_Swipes <- as.data.frame((table(Summer2015$PATRONID.x)))
Summer2015_Freq_Count <- as.data.frame(Summer2015_Unique_Swipes[order(-Summer2015_Unique_Swipes$Freq), ])
names(Summer2015_Freq_Count)[1] <- "PATRONID" #Change column name

#############################################
# Make tables for Fall 2012, Spring 2013, Summer 2013, Fall 2013, Spring 2014, Summer 2014, Fall 2014, Spring 2015, and Summer 2015
# Include following columns: Swipes, Unique Swipes, Top 5 Frequencies, Frequent Range, Moderately Frequent Range, Occassional Range, Infrequent Range

# Total_Swipes <- length(Fall2012$TRANDATE)
# Unique_Swipes <- length(Fall2012_Unique_Swipes$Freq)
# Top_5 <- Fall2012_Freq_Count[0:5,]$Freq
# Frequent_Range = ?
# Moderately_Frequent_Range = ?
# Ocassional_Range = ?
# Infrequent_Range = ?
#############################################

##### Fall 2012 Frequency Profiles
Fall2012_Frequent <- subset(Fall2012_Freq_Count, Freq<=160 & Freq>=100)
Fall2012_Mod_Frequent <- subset(Fall2012_Freq_Count, Freq<=99 & Freq>=50)
Fall2012_Ocassional <- subset(Fall2012_Freq_Count, Freq<=49 & Freq>=25)
Fall2012_Infrequent <- subset(Fall2012_Freq_Count, Freq<=24 & Freq>=10)
#Get 5 randomly generated users based on the number of total users 
Frequent_Random <- sample(1:length(Fall2012_Frequent$PATRONID), 5, replace=F)
Mod_Frequent_Random <- sample(1:length(Fall2012_Mod_Frequent$PATRONID), 5, replace=F)
Ocassional_Random <- sample(1:length(Fall2012_Ocassional$PATRONID), 5, replace=F)
Infrequent_Random <- sample(1:length(Fall2012_Infrequent$PATRONID), 5, replace=F)
# Makes the Data Structures that holds the information on each Random Frequent User
Fall2012_All_Frequent <- Fall2012_Frequent[Frequent_Random[1],]
Fall2012_All_Frequent <- rbind(Fall2012_All_Frequent, Fall2012_Frequent[Frequent_Random[2],])
Fall2012_All_Frequent <- rbind(Fall2012_All_Frequent, Fall2012_Frequent[Frequent_Random[3],])
Fall2012_All_Frequent <- rbind(Fall2012_All_Frequent, Fall2012_Frequent[Frequent_Random[4],])
Fall2012_All_Frequent <- rbind(Fall2012_All_Frequent, Fall2012_Frequent[Frequent_Random[5],])
## PatronID's are - 324966, 334766, 227770, 239720, 334556
Fall2012_Times_Frequent <- Fall2012[Fall2012$PATRONID.x == 324966, ]
Fall2012_Times_Frequent <- rbind(Fall2012_Times_Frequent, Fall2012[Fall2012$PATRONID.x == 334766, ])
Fall2012_Times_Frequent <- rbind(Fall2012_Times_Frequent, Fall2012[Fall2012$PATRONID.x == 227770, ])
Fall2012_Times_Frequent <- rbind(Fall2012_Times_Frequent, Fall2012[Fall2012$PATRONID.x == 239720, ])
Fall2012_Times_Frequent <- rbind(Fall2012_Times_Frequent, Fall2012[Fall2012$PATRONID.x == 334556, ])
Fall2012_Times_Frequent = Fall2012_Times_Frequent %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Fall2012_Frequent_Peak_Count <- subset(Fall2012_Times_Frequent, Time>="14:00:00" & Time<="19:30:00")
Fall2012_Frequent_Peak_Percentage <- (length(Fall2012_Frequent_Peak_Count$Date) / length(Fall2012_Times_Frequent$Date)) 
Fall2012_Frequent_Non_Peak_Percentage <- (1 - Fall2012_Frequent_Peak_Percentage) 
Fall2012_Frequent_Peak_Percentage <- Fall2012_Frequent_Peak_Percentage * 100
Fall2012_Frequent_Non_Peak_Percentage <- Fall2012_Frequent_Non_Peak_Percentage * 100

##### Makes the Data Structures that holds the information on each Random Mod Frequent User
Fall2012_All_Mod_Frequent <- Fall2012_Mod_Frequent[Mod_Frequent_Random[1],]
Fall2012_All_Mod_Frequent <- rbind(Fall2012_All_Mod_Frequent, Fall2012_Mod_Frequent[Mod_Frequent_Random[2],])
Fall2012_All_Mod_Frequent <- rbind(Fall2012_All_Mod_Frequent, Fall2012_Mod_Frequent[Mod_Frequent_Random[3],])
Fall2012_All_Mod_Frequent <- rbind(Fall2012_All_Mod_Frequent, Fall2012_Mod_Frequent[Mod_Frequent_Random[4],])
Fall2012_All_Mod_Frequent <- rbind(Fall2012_All_Mod_Frequent, Fall2012_Mod_Frequent[Mod_Frequent_Random[5],])
#PatronID's are - 217761, 336354, 69626, 234304, 299160
Fall2012_Times_Mod_Frequent <- Fall2012[Fall2012$PATRONID.x == 217761, ]
Fall2012_Times_Mod_Frequent <- rbind(Fall2012_Times_Mod_Frequent, Fall2012[Fall2012$PATRONID.x == 336354, ])
Fall2012_Times_Mod_Frequent <- rbind(Fall2012_Times_Mod_Frequent, Fall2012[Fall2012$PATRONID.x == 69626, ])
Fall2012_Times_Mod_Frequent <- rbind(Fall2012_Times_Mod_Frequent, Fall2012[Fall2012$PATRONID.x == 234304, ])
Fall2012_Times_Mod_Frequent <- rbind(Fall2012_Times_Mod_Frequent, Fall2012[Fall2012$PATRONID.x == 299160, ])
Fall2012_Times_Mod_Frequent = Fall2012_Times_Mod_Frequent %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Fall2012_Mod_Frequent_Peak_Count <- subset(Fall2012_Times_Mod_Frequent, Time>="14:00:00" & Time<="19:30:00")
Fall2012_Mod_Frequent_Peak_Percentage <- (length(Fall2012_Mod_Frequent_Peak_Count$Date) / length(Fall2012_Times_Mod_Frequent$Date)) 
Fall2012_Mod_Frequent_Non_Peak_Percentage <- (1 - Fall2012_Mod_Frequent_Peak_Percentage) 
Fall2012_Mod_Frequent_Peak_Percentage <- Fall2012_Mod_Frequent_Peak_Percentage * 100
Fall2012_Mod_Frequent_Non_Peak_Percentage <- Fall2012_Mod_Frequent_Non_Peak_Percentage * 100

##### Makes the Data Structures that holds the information on each Ocassional User
Fall2012_All_Ocassional <- Fall2012_Ocassional[Ocassional_Random[1],]
Fall2012_All_Ocassional <- rbind(Fall2012_All_Ocassional, Fall2012_Ocassional[Ocassional_Random[2],])
Fall2012_All_Ocassional <- rbind(Fall2012_All_Ocassional, Fall2012_Ocassional[Ocassional_Random[3],])
Fall2012_All_Ocassional <- rbind(Fall2012_All_Ocassional, Fall2012_Ocassional[Ocassional_Random[4],])
Fall2012_All_Ocassional <- rbind(Fall2012_All_Ocassional, Fall2012_Ocassional[Ocassional_Random[5],])
#PatronID's are - 271266, 332232, 244398, 70295, 242952
Fall2012_Times_Ocassional <- Fall2012[Fall2012$PATRONID.x == 271266, ]
Fall2012_Times_Ocassional <- rbind(Fall2012_Times_Ocassional, Fall2012[Fall2012$PATRONID.x == 332232, ])
Fall2012_Times_Ocassional <- rbind(Fall2012_Times_Ocassional, Fall2012[Fall2012$PATRONID.x == 244398, ])
Fall2012_Times_Ocassional <- rbind(Fall2012_Times_Ocassional, Fall2012[Fall2012$PATRONID.x == 70295, ])
Fall2012_Times_Ocassional <- rbind(Fall2012_Times_Ocassional, Fall2012[Fall2012$PATRONID.x == 242952, ])
Fall2012_Times_Ocassional = Fall2012_Times_Ocassional %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Fall2012_Ocassional_Peak_Count <- subset(Fall2012_Times_Ocassional, Time>="14:00:00" & Time<="19:30:00")
Fall2012_Ocassional_Peak_Percentage <- (length(Fall2012_Ocassional_Peak_Count$Date) / length(Fall2012_Times_Ocassional$Date)) 
Fall2012_Ocassional_Non_Peak_Percentage <- (1 - Fall2012_Ocassional_Peak_Percentage) 
Fall2012_Ocassional_Peak_Percentage <- Fall2012_Ocassional_Peak_Percentage * 100
Fall2012_Ocassional_Non_Peak_Percentage <- Fall2012_Ocassional_Non_Peak_Percentage * 100

##### Makes the Data Structures that holds the information on each Infrequent User
Fall2012_All_Infrequent <- Fall2012_Infrequent[Infrequent_Random[1],]
Fall2012_All_Infrequent <- rbind(Fall2012_All_Infrequent, Fall2012_Infrequent[Infrequent_Random[2],])
Fall2012_All_Infrequent <- rbind(Fall2012_All_Infrequent, Fall2012_Infrequent[Infrequent_Random[3],])
Fall2012_All_Infrequent <- rbind(Fall2012_All_Infrequent, Fall2012_Infrequent[Infrequent_Random[4],])
Fall2012_All_Infrequent <- rbind(Fall2012_All_Infrequent, Fall2012_Infrequent[Infrequent_Random[5],])
#PatronID's are - 317892, 80261, 278184, 269094, 298347
Fall2012_Times_Infrequent <- Fall2012[Fall2012$PATRONID.x == 317892, ]
Fall2012_Times_Infrequent <- rbind(Fall2012_Times_Infrequent, Fall2012[Fall2012$PATRONID.x == 80261, ])
Fall2012_Times_Infrequent <- rbind(Fall2012_Times_Infrequent, Fall2012[Fall2012$PATRONID.x == 278184, ])
Fall2012_Times_Infrequent <- rbind(Fall2012_Times_Infrequent, Fall2012[Fall2012$PATRONID.x == 269094, ])
Fall2012_Times_Infrequent <- rbind(Fall2012_Times_Infrequent, Fall2012[Fall2012$PATRONID.x == 298347, ])
Fall2012_Times_Infrequent = Fall2012_Times_Infrequent %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Fall2012_Infrequent_Peak_Count <- subset(Fall2012_Times_Infrequent, Time>="14:00:00" & Time<="19:30:00")
Fall2012_Infrequent_Peak_Percentage <- (length(Fall2012_Infrequent_Peak_Count$Date) / length(Fall2012_Times_Infrequent$Date)) 
Fall2012_Infrequent_Non_Peak_Percentage <- (1 - Fall2012_Infrequent_Peak_Percentage) 
Fall2012_Infrequent_Peak_Percentage <- Fall2012_Infrequent_Peak_Percentage * 100
Fall2012_Infrequent_Non_Peak_Percentage <- Fall2012_Infrequent_Non_Peak_Percentage * 100





### Spring 2013 Frequency Profiles
Spring2013_Frequent <- subset(Spring2013_Freq_Count, Freq<=175 & Freq>=100)
Spring2013_Mod_Frequent <- subset(Spring2013_Freq_Count, Freq<=99 & Freq>=50)
Spring2013_Ocassional <- subset(Spring2013_Freq_Count, Freq<=49 & Freq>=25)
Spring2013_Infrequent <- subset(Spring2013_Freq_Count, Freq<=24 & Freq>=10)

#Get the 5 randomly generated users based on the number of total users 
Frequent_Random <- sample(1:length(Spring2013_Frequent$PATRONID), 5, replace=F)
Mod_Frequent_Random <- sample(1:length(Spring2013_Mod_Frequent$PATRONID), 5, replace=F)
Ocassional_Random <- sample(1:length(Spring2013_Ocassional$PATRONID), 5, replace=F)
Infrequent_Random <- sample(1:length(Spring2013_Infrequent$PATRONID), 5, replace=F)

##### Makes the Data Structures that holds the information on each Random Frequent User
Spring2013_All_Frequent <- Spring2013_Frequent[Frequent_Random[1],]
Spring2013_All_Frequent <- rbind(Spring2013_All_Frequent, Spring2013_Frequent[Frequent_Random[2],])
Spring2013_All_Frequent <- rbind(Spring2013_All_Frequent, Spring2013_Frequent[Frequent_Random[3],])
Spring2013_All_Frequent <- rbind(Spring2013_All_Frequent, Spring2013_Frequent[Frequent_Random[4],])
Spring2013_All_Frequent <- rbind(Spring2013_All_Frequent, Spring2013_Frequent[Frequent_Random[5],])
#PatronID's are - 333880, 335680, 295869, 296416, 205604
Spring2013_Times_Frequent <- Spring2013[Spring2013$PATRONID.x == 333880, ]
Spring2013_Times_Frequent <- rbind(Spring2013_Times_Frequent, Spring2013[Spring2013$PATRONID.x == 335680, ])
Spring2013_Times_Frequent <- rbind(Spring2013_Times_Frequent, Spring2013[Spring2013$PATRONID.x == 295869, ])
Spring2013_Times_Frequent <- rbind(Spring2013_Times_Frequent, Spring2013[Spring2013$PATRONID.x == 296416, ])
Spring2013_Times_Frequent <- rbind(Spring2013_Times_Frequent, Spring2013[Spring2013$PATRONID.x == 205604, ])
Spring2013_Times_Frequent = Spring2013_Times_Frequent %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Spring2013_Frequent_Peak_Count <- subset(Spring2013_Times_Frequent, Time>="14:00:00" & Time<="19:30:00")
Spring2013_Frequent_Peak_Percentage <- (length(Spring2013_Frequent_Peak_Count$Date) / length(Spring2013_Times_Frequent$Date)) 
Spring2013_Frequent_Non_Peak_Percentage <- (1 - Spring2013_Frequent_Peak_Percentage) 
Spring2013_Frequent_Peak_Percentage <- Spring2013_Frequent_Peak_Percentage * 100
Spring2013_Frequent_Non_Peak_Percentage <- Spring2013_Frequent_Non_Peak_Percentage * 100

##### Makes the Data Structures that holds the information on each Random Mod Frequent User
Spring2013_All_Mod_Frequent <- Spring2013_Mod_Frequent[Mod_Frequent_Random[1],]
Spring2013_All_Mod_Frequent <- rbind(Spring2013_All_Mod_Frequent, Spring2013_Mod_Frequent[Mod_Frequent_Random[2],])
Spring2013_All_Mod_Frequent <- rbind(Spring2013_All_Mod_Frequent, Spring2013_Mod_Frequent[Mod_Frequent_Random[3],])
Spring2013_All_Mod_Frequent <- rbind(Spring2013_All_Mod_Frequent, Spring2013_Mod_Frequent[Mod_Frequent_Random[4],])
Spring2013_All_Mod_Frequent <- rbind(Spring2013_All_Mod_Frequent, Spring2013_Mod_Frequent[Mod_Frequent_Random[5],])
#PatronID's are - 238039, 205391, 295465, 197175, 334995
Spring2013_Times_Mod_Frequent <- Spring2013[Spring2013$PATRONID.x == 205391, ]
Spring2013_Times_Mod_Frequent <- rbind(Spring2013_Times_Mod_Frequent, Spring2013[Spring2013$PATRONID.x == 238039, ])
Spring2013_Times_Mod_Frequent <- rbind(Spring2013_Times_Mod_Frequent, Spring2013[Spring2013$PATRONID.x == 295465, ])
Spring2013_Times_Mod_Frequent <- rbind(Spring2013_Times_Mod_Frequent, Spring2013[Spring2013$PATRONID.x == 197175, ])
Spring2013_Times_Mod_Frequent <- rbind(Spring2013_Times_Mod_Frequent, Spring2013[Spring2013$PATRONID.x == 334995, ])
Spring2013_Times_Mod_Frequent = Spring2013_Times_Mod_Frequent %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Spring2013_Mod_Frequent_Peak_Count <- subset(Spring2013_Times_Mod_Frequent, Time>="14:00:00" & Time<="19:30:00")
Spring2013_Mod_Frequent_Peak_Percentage <- (length(Spring2013_Mod_Frequent_Peak_Count$Date) / length(Spring2013_Times_Mod_Frequent$Date)) 
Spring2013_Mod_Frequent_Non_Peak_Percentage <- (1 - Spring2013_Mod_Frequent_Peak_Percentage) 
Spring2013_Mod_Frequent_Peak_Percentage <- Spring2013_Mod_Frequent_Peak_Percentage * 100
Spring2013_Mod_Frequent_Non_Peak_Percentage <- Spring2013_Mod_Frequent_Non_Peak_Percentage * 100

##### Makes the Data Structures that holds the information on each Ocassional User
Spring2013_All_Ocassional <- Spring2013_Ocassional[Ocassional_Random[1],]
Spring2013_All_Ocassional <- rbind(Spring2013_All_Ocassional, Spring2013_Ocassional[Ocassional_Random[2],])
Spring2013_All_Ocassional <- rbind(Spring2013_All_Ocassional, Spring2013_Ocassional[Ocassional_Random[3],])
Spring2013_All_Ocassional <- rbind(Spring2013_All_Ocassional, Spring2013_Ocassional[Ocassional_Random[4],])
Spring2013_All_Ocassional <- rbind(Spring2013_All_Ocassional, Spring2013_Ocassional[Ocassional_Random[5],])
#PatronID's are - 296839, 340553, 280702, 247148, 244004
Spring2013_Times_Ocassional <- Spring2013[Spring2013$PATRONID.x == 296839, ]
Spring2013_Times_Ocassional <- rbind(Spring2013_Times_Ocassional, Spring2013[Spring2013$PATRONID.x == 340553, ])
Spring2013_Times_Ocassional <- rbind(Spring2013_Times_Ocassional, Spring2013[Spring2013$PATRONID.x == 280702, ])
Spring2013_Times_Ocassional <- rbind(Spring2013_Times_Ocassional, Spring2013[Spring2013$PATRONID.x == 247148, ])
Spring2013_Times_Ocassional <- rbind(Spring2013_Times_Ocassional, Spring2013[Spring2013$PATRONID.x == 244004, ])
Spring2013_Times_Ocassional = Spring2013_Times_Ocassional %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Spring2013_Ocassional_Peak_Count <- subset(Spring2013_Times_Ocassional, Time>="14:00:00" & Time<="19:30:00")
Spring2013_Ocassional_Peak_Percentage <- (length(Spring2013_Ocassional_Peak_Count$Date) / length(Spring2013_Times_Ocassional$Date)) 
Spring2013_Ocassional_Non_Peak_Percentage <- (1 - Spring2013_Ocassional_Peak_Percentage) 
Spring2013_Ocassional_Peak_Percentage <- Spring2013_Ocassional_Peak_Percentage * 100
Spring2013_Ocassional_Non_Peak_Percentage <- Spring2013_Ocassional_Non_Peak_Percentage * 100

##### Makes the Data Structures that holds the information on each Infrequent User
Spring2013_All_Infrequent <- Spring2013_Infrequent[Infrequent_Random[1],]
Spring2013_All_Infrequent <- rbind(Spring2013_All_Infrequent, Spring2013_Infrequent[Infrequent_Random[2],])
Spring2013_All_Infrequent <- rbind(Spring2013_All_Infrequent, Spring2013_Infrequent[Infrequent_Random[3],])
Spring2013_All_Infrequent <- rbind(Spring2013_All_Infrequent, Spring2013_Infrequent[Infrequent_Random[4],])
Spring2013_All_Infrequent <- rbind(Spring2013_All_Infrequent, Spring2013_Infrequent[Infrequent_Random[5],])
#PatronID's are - 326242, 300048, 334727, 333109, 225840
Spring2013_Times_Infrequent <- Spring2013[Spring2013$PATRONID.x == 326242, ]
Spring2013_Times_Infrequent <- rbind(Spring2013_Times_Infrequent, Spring2013[Spring2013$PATRONID.x == 300048, ])
Spring2013_Times_Infrequent <- rbind(Spring2013_Times_Infrequent, Spring2013[Spring2013$PATRONID.x == 334727, ])
Spring2013_Times_Infrequent <- rbind(Spring2013_Times_Infrequent, Spring2013[Spring2013$PATRONID.x == 333109, ])
Spring2013_Times_Infrequent <- rbind(Spring2013_Times_Infrequent, Spring2013[Spring2013$PATRONID.x == 225840, ])
Spring2013_Times_Infrequent = Spring2013_Times_Infrequent %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Spring2013_Infrequent_Peak_Count <- subset(Spring2013_Times_Infrequent, Time>="14:00:00" & Time<="19:30:00")
Spring2013_Infrequent_Peak_Percentage <- (length(Spring2013_Infrequent_Peak_Count$Date) / length(Spring2013_Times_Infrequent$Date)) 
Spring2013_Infrequent_Non_Peak_Percentage <- (1 - Spring2013_Infrequent_Peak_Percentage) 
Spring2013_Infrequent_Peak_Percentage <- Spring2013_Infrequent_Peak_Percentage * 100
Spring2013_Infrequent_Non_Peak_Percentage <- Spring2013_Infrequent_Non_Peak_Percentage * 100




#Peak times are between 3-730pm, seperate TRANDATE into DATE and TIME. Compare time with >=14:00:00 & <=19:30:00

x <- data.frame("GymType" = "Frequent", "Semester" = "Fall2012", "Percentage" = Fall2012_Frequent_Peak_Percentage, stringsAsFactors = FALSE)
x <- rbind(x, list("ModFrequent", "Fall2012", Fall2012_Mod_Frequent_Peak_Percentage))
x <- rbind(x, list("Ocassional", "Fall2012", Fall2012_Ocassional_Peak_Percentage))
x <- rbind(x, list("Infrequent", "Fall2012", Fall2012_Infrequent_Peak_Percentage))
y <- data.frame("GymType" = "Frequent", "Packedness" = "NonPeak", "Percentage" = Fall2012_Frequent_Non_Peak_Percentage, stringsAsFactors = FALSE)
y <- rbind(y, list("ModFrequent", "NonPeak", Fall2012_Mod_Frequent_Non_Peak_Percentage))
y <- rbind(y, list("Ocassional", "NonPeak", Fall2012_Ocassional_Non_Peak_Percentage))
y <- rbind(y, list("Infrequent", "NonPeak", Fall2012_Infrequent_Non_Peak_Percentage))


x1 <- data.frame("GymType" = "Frequent", "Semester" = "Spring2013", "Percentage" = Spring2013_Frequent_Peak_Percentage, stringsAsFactors = FALSE)
x1 <- rbind(x1, list("ModFrequent", "Spring2013", Spring2013_Mod_Frequent_Peak_Percentage))
x1 <- rbind(x1, list("Ocassional", "Spring2013", Spring2013_Ocassional_Peak_Percentage))
x1 <- rbind(x1, list("Infrequent", "Spring2013", Spring2013_Infrequent_Peak_Percentage))
y1 <- data.frame("GymType" = "Frequent", "Packedness" = "NonPeak", "Percentage" = Spring2013_Frequent_Non_Peak_Percentage, stringsAsFactors = FALSE)
y1 <- rbind(y1, list("ModFrequent", "NonPeak", Spring2013_Mod_Frequent_Non_Peak_Percentage))
y1 <- rbind(y1, list("Ocassional", "NonPeak", Spring2013_Ocassional_Non_Peak_Percentage))
y1 <- rbind(y1, list("Infrequent", "NonPeak", Spring2013_Infrequent_Non_Peak_Percentage))


x2 <- data.frame("GymType" = "Frequent", "Semester" = "Summer2013", "Percentage" = Spring2013_Frequent_Peak_Percentage, stringsAsFactors = FALSE)
x2 <- rbind(x2, list("ModFrequent", "Summer2013", Spring2013_Mod_Frequent_Peak_Percentage))
x2 <- rbind(x2, list("Ocassional", "Summer2013", Spring2013_Ocassional_Peak_Percentage))
x2 <- rbind(x2, list("Infrequent", "Summer2013", Spring2013_Infrequent_Peak_Percentage))
x2$GymType <- factor(x2$GymType, levels=x2$GymType)


x1$GymType <- factor(x1$GymType, levels=x1$GymType)

y1$GymType <- factor(y1$GymType, levels=x1$GymType)

#This will make sure that the order of gymtype is maintained by ggplot
x$GymType <- factor(x$GymType, levels=x$GymType)
y$GymType <- factor(y$GymType, levels=x$GymType)

ColorPalette <- c("#c2e699", "#78c679", "#31a354","#006837")

g <- ggplot(x, aes(Packedness, Percentage)) + geom_bar(aes(fill=GymType),position="dodge",stat="identity",width=.55)+
    xlab("Packedness")+ylab("Percentages")+ggtitle("Peak Percentages in Fall 2012")+
    scale_fill_manual(values=ColorPalette)

z <- rbind(x, x1)
z <- rbind(z, x2)

z_plot <- ggplot(z, aes(Semester, Percentage)) + geom_bar(aes(fill=GymType),position="dodge",stat="identity",width=.55)+
  xlab("Semester")+ylab("Percentages")+ggtitle("Peak Percentages in 2012-2013")+
  scale_fill_manual(values=ColorPalette)

h <- ggplot(x1, aes(Packedness, Percentage)) + geom_bar(aes(fill=GymType),position="dodge",stat="identity",width=.55)+
  xlab("Packedness")+ylab("Percentages")+ggtitle("Peak Percentages in Spring 2013")+
  scale_fill_manual(values=ColorPalette)

g1 <- ggplot(y, aes(Packedness, Percentage)) + geom_bar(aes(fill=GymType),position="dodge",stat="identity",width=.55)+
  xlab("Packedness")+ylab("Percentages")+ggtitle("Non Peak Percentages in Fall 2012")+
  scale_fill_manual(values=ColorPalette)

Fall2012_Times_Frequent = Fall2012_Times_Frequent %>% separate(TRANDATE, c('Date', 'Time'), sep = " ")
Fall2012_Peak_Count <- subset(Fall2012_Times_Frequent, Time>="14:00:00" & Time<="19:30:00")

patron <- 332738 #Went 158 times
student <- T20123[T20123$PATRONID.x == patron, ]
student_date <- as.data.frame(student$TRANDATE)

patron1 <- 52310 #Went 26 times
student1 <- T20123[T20123$PATRONID.x == patron1, ]
student1_date <- as.data.frame(student1$TRANDATE)

patron2 <- 375932 #Went 1 time
student2 <- T20153[T20153$PATRONID.x == patron2,]
student2_date <- as.data.frame(student2$TRANDATE)


# Plot the times on a graph. X axis has the date visited (8/16 to 1/6). Y axis has the time of day (515 to 2330)
# Highlight on the graph the peak times as specified by Yunshu Du
# Count the number of occurences at every time (x at empty, x at moderetly empty, x at crowded)
###### Do this for a range of people and see if there is any correlation









#Use as.character(student_date[150,])


