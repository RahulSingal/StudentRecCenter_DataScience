library(tidyr)
library(dplyr)

#Card Swipes contains the data for all the card swipes in the data set from 2012-08-17 to 2014-01-12
#Columns: TRANDATE, REASONTYPE, REASONMSG, PATRONID
#There are a total 1,048,575 observations
Card_Swipes = read.csv("DS_ClassProject_Fall2018_data/Card_Swipes.csv")

#We do not care about REASONTYPE and REASONMSG, so we will remove those two columns
Card_Swipes <- subset(Card_Swipes, select = -c(REASONTYPE, REASONMSG))

#DemoGraphics contains the data for all the demographics of card swipes from 2012-08-17 to 2015-08-17
#Columns: TRANSID, TRANDATE, PATRONID.x, PATRONID.y, sex, student_flag, record_term, academic_level, acad_plan_descr, adj_admit_type, admit_term, HOUSING_TYPE
#There are a total 1,048,575 observations
DemoGraphics = as.data.frame(read.csv("DS_ClassProject_Fall2018_data/Demographics.csv"))

#We do not care about the TRANSID, student_flag, adj_admit_type, admit_term, and HOUSING_TYPE, so we will remove these 5 columns
DemoGraphics <- subset(DemoGraphics, select = -c(TRANSID, student_flag, adj_admit_type, admit_term, HOUSING_TYPE))

#Card Swipes now contains the following columns: 
# 1. TRANDATE
# 2. PATRONID

#DemoGraphics now contains the following columns:
# 1. TRANDATE 
# 2. PATRONID.x - Combine x and y into one dataset and see when there is missing values in either. 
# 3. PATRONID.y - Make a determination of what to do when NA's are present and how common NA's are
# 4. sex
# 5. record_term - Change to "Fall/Spring/Summer" + "Year" ie (20123 -> Fall2012)
# 6. academic_level - 
# 7. acad_plan_descr - Change column name to major

#saves the total unique PATRONID in the card swipe data
number_of_unique_ID_card_swipe <- length(unique(DemoGraphics$PATRONID.x))

#Makes a dataframe that has the Unique PATRONID and the frequency in the dataset
UniqueID_Frequency_Card_Swipes <- as.data.frame(table(DemoGraphics$PATRONID.x))

#Sort UniqueID_Frequency_Card_Swips by frequency (Ascending order)
Frequency_Card_Swipes <- as.data.frame(UniqueID_Frequency_Card_Swipes[order(-UniqueID_Frequency_Card_Swipes$Freq), ])
names(Frequency_Card_Swipes)[1] <- "PATRONID" #Change column name

size_ID <- length(Frequency_Card_Swipes$PATRONID)

#Create the Top and Bottom 10% to 50%, in 5% increments

#Top/Bottom 10%
top_10 <- as.integer(size_ID * 0.10)
top_10 <- Frequency_Card_Swipes[0:top_10,]
bottom_10 <- as.integer(size_ID *.9)
bottom_10 <- Frequency_Card_Swipes[bottom_10:size_ID,]
#Top/Bottom 15%
top_15 <- as.integer(size_ID * 0.15)
top_15 <- Frequency_Card_Swipes[0:top_15,]
bottom_15 <- as.integer(size_ID *.85)
bottom_15 <- Frequency_Card_Swipes[bottom_15:size_ID,]
#Top/Bottom 20%
top_20 <- as.integer(size_ID * 0.20)
top_20 <- Frequency_Card_Swipes[0:top_20,]
bottom_20 <- as.integer(size_ID *.8)
bottom_20 <- Frequency_Card_Swipes[bottom_20:size_ID,]
#Top/Bottom 25%
top_25 <- as.integer(size_ID *0.25)
top_25 <- Frequency_Card_Swipes[0:top_25,]
bottom_25 <- as.integer(size_ID *0.75)
bottom_25 <- Frequency_Card_Swipes[bottom_25:size_ID,]
#Top/Bottom 30%
top_30 <- as.integer(size_ID * 0.30)
top_30 <- Frequency_Card_Swipes[0:top_30,]
bottom_30 <- as.integer(size_ID *.7)
bottom_30 <- Frequency_Card_Swipes[bottom_30:size_ID,]
#Top/Bottom 35%
top_35 <- as.integer(size_ID * 0.35)
top_35 <- Frequency_Card_Swipes[0:top_35,]
bottom_35 <- as.integer(size_ID *.65)
bottom_35 <- Frequency_Card_Swipes[bottom_35:size_ID,]
#Top/Bottom 40%
top_40 <- as.integer(size_ID * 0.40)
top_40 <- Frequency_Card_Swipes[0:top_40,]
bottom_40 <- as.integer(size_ID *.6)
bottom_40 <- Frequency_Card_Swipes[bottom_40:size_ID,]
#Top/Bottom 45%
top_45 <- as.integer(size_ID * 0.45)
top_45 <- Frequency_Card_Swipes[0:top_45,]
bottom_45 <- as.integer(size_ID *.55)
bottom_45 <- Frequency_Card_Swipes[bottom_45:size_ID,]
#Top/Bottom 50%
top_50 <- as.integer(size_ID *0.50)
top_50 <- Frequency_Card_Swipes[0:top_50,]
bottom_50 <- as.integer(size_ID*0.50)
bottom_50 <- Frequency_Card_Swipes[bottom_50:size_ID,]

Range <- c(1:750)
means <- mean(top_10$Freq) 
means <- c(means, mean(top_20$Freq))
means <- c(means, mean(top_40$Freq))

plot(Range, means, main="Plz")

#Make a plot of the means of the top and bottoms

#This guy frequents ? times -> DemoGraphics[1000007,]
patron <- 367416

#This guys frequents ? times
patron1 <- 299255


student1 <- DemoGraphics[DemoGraphics$PATRONID.x == patron1, ]

#This gives me all the instances with patrons transactions at the gym (102)
student <- DemoGraphics[DemoGraphics$PATRONID.x == patron, ]
student <- student[0:93,]
