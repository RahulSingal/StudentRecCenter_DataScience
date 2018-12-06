DemoGraphics = as.data.frame(read.csv("DS_ClassProject_Fall2018_data/Demographics.csv"))

DemoGraphics$TRANDATE <- as.POSIXct(as.Date(DemoGraphics$TRANDATE), "%Y-%m-%d %H:%M:%S") ## FROM YUNSHU's Code

new = by(DemoGraphics, DemoGraphics[,"record_term"], function(x) x)

T20123 = new[[1]] ## Fall 2012 [2012-08-17 to 2013-01-06]
T20131 = new[[2]] ## Spring 2013 [2013-01-07 to 2013-05-05]
T20132 = new[[3]] ## Summer 2013 [2013-05-06 to 2013-08-08]
T20133 = new[[4]] ## Fall 2013 [2013-08-09 to 2014-01-12]
T20141 = new[[5]] ## Spring 2014 [2014-01-13 to 2014-05-11]
T20142 = new[[6]] ## Summer 2014 [2014-05-12 to 2014-08-14]
T20143 = new[[7]] ## Fall 2014 [2014-08-15 to 2015-01-11]
T20151 = new[[8]] ## Spring 2015 [2015-01-12 to 2015-05-10]
T20152 = new[[9]] ## Summer 2015 [2015-05-11 to 2015-08-13]
T20153 = new[[10]] ## Fall 2015 [INCOMPLETE. ONLY 3 DAYS]

number_of_unique_ID_card_swipe <- length(unique(T20123$PATRONID.x))
UniqueID_Frequency_Card_Swipes <- as.data.frame(table(T20123$PATRONID.x))
Frequency_Card_Swipes <- as.data.frame(UniqueID_Frequency_Card_Swipes[order(-UniqueID_Frequency_Card_Swipes$Freq), ])

names(Frequency_Card_Swipes)[1] <- "PATRONID" #Change column name


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


