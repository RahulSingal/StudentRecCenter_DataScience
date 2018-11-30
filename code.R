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
DemoGraphics = read.csv("DS_ClassProject_Fall2018_data/Demographics.csv")

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
number_of_unique_ID_card_swipe <- length(unique(Card_Swipes$PATRONID))

#Makes a dataframe that has the Unique PATRONID and the frequency in the dataset
UniqueID_Frequency_Card_Swipes <- as.data.frame(table(Card_Swipes$PATRONID))

#Sort UniqueID_Frequency_Card_Swips by frequency (Ascending order)
Frequency_Card_Swipes <- UniqueID_Frequency_Card_Swipes[order(-UniqueID_Frequency_Card_Swipes$Freq), ]
names(Frequency_Card_Swipes)[1] <- "PATRONID" #Change column name

size_ID <- length(Frequency_Card_Swipes$PATRONID)

#Top 25%
top_25 <- as.integer(size_ID *0.25)
top_25 <- Frequency_Card_Swipes[0:top_25,]

#Bottom 25%
bottom_25 <- as.integer(size_ID *0.75)
bottom_25 <- Frequency_Card_Swipes[bottom_25:size_ID,]

#Top 10%
top_10 <- as.integer(size_ID * 0.10)
top_10 <- Frequency_Card_Swipes[0:top_10,]

#Bottom 10%
bottom_10 <- as.integer(size_ID *.9)
bottom_10 <- Frequency_Card_Swipes[bottom_10:size_ID,]

