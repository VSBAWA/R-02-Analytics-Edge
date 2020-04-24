#AN ANALYTICAL DETECTIVE

#In the United States, violent crimes and property crimes are recorded by the Federal Bureau of Investigation (FBI).  Additionally, each city documents crime, and some cities release data regarding crime rates. The city of Chicago, Illinois releases crime data from 2001 onward online (https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2)

#There are two main types of crimes: violent crimes, and property crimes. In this problem, we'll focus on one specific type of property crime, called "motor vehicle theft" (sometimes referred to as grand theft auto). This is the act of stealing, or attempting to steal, a car. In this problem, we'll use some basic data analysis in R to understand the motor vehicle thefts in Chicago. 

#read csv file
mvt = read.csv("mvtWeek1.csv")
#see how many observations and variables in the dataset
str(mvt) #191641
#see what is the maximum value of the variable "ID"
max(mvt$ID) #ID: unique identifier for each observation
#the minimum value of the variable "Beat"
min(mvt$Beat) #Beat: the smallest regional division defined by the Chicago police department
#how many observations have value TRUE in the Arrest variable
table(mvt$Arrest) #Arrest: whether or not an arrest was made for the crime
#how many observations have a LocationDescription value of ALLEY
table(mvt$LocationDescription) #LocationDescription: the location where the crime occurred

#see the format of date
mvt$Date[1] #Date: the date the crime occurred
#R does not automatically recognize entries that look like dates
#extract date and time and converting the characters into date object in R
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert) #the characters are converted into a Date object
#extract the month and the day of the week and add these variables to the data frame
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
#in which month did the fewest motor vehicle thefs occur?
table(mvt$Month)
#in which weekday did the most motor vehicle thefs occur?
table(mvt$Weekday)
#which month has the largest number of arrest?
tapply(mvt$Arrest, mvt$Month, summary)

#create a histogram for the variable Date to see crime trends
hist(mvt$Date, breaks = 100)
#crime increases or decreases from 2005 - 2008?
Year05to08 = subset(mvt, mvt$Date>"2005-01-01" & mvt$Date < "2008-12-31")
hist(Year05to08$Date, breaks = 100)
#create a box plot to see how crime trends change over the time
boxplot(mvt$Date, mvt$Arrest)
#For what proportion of motor vehicle thefts in 2001 was an arrest made? (UNSOLVED)
Arrest2001 = subset(mvt, mvt$Date>"2000-12-31" & mvt$Date < "2002-01-01")
table(Arrest2001$Arrest)
summary(Arrest2001$Arrest)
2152/(2152+18517) #first create the subset, extract infos, then compute
#For what proportion of motor vehicle thefts in 2007 was an arrest made? 
Arrest2007 = subset(mvt, mvt$Date > "2006-12-31" & mvt$Date < "2008-01-01")
table(Arrest2007$Arrest)
1212/(1212+13068)
#For what proportion of motor vehicle thefts in 2012 was an arrest made?
Arrest2012 = subset(mvt,mvt$Date > "2011-12-31" & mvt$Date < "2013-01-01")
table(Arrest2012$Arrest)
550/(550+13542)

#find the top five locations where motor vehicle thefts occur
sort(table(mvt$LocationDescription)) #R sorts out the value from smallest to largest
#create a subset for top 5 locations each
Top5 = subset(mvt,mvt$LocationDescription == "STREET"|mvt$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)"|mvt$LocationDescription == "ALLEY"|mvt$LocationDescription == "GAS STATION"| mvt$LocationDescription == "DRIVEWAY - RESIDENTIAL")
#alternatively, create 5 subsets then use rbind
Street = subset(mvt, mvt$LocationDescription == "STREET")
Parking = subset(mvt, mvt$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)")
Alley = subset(mvt, mvt$LocationDescription == "ALLEY")
GasStation = subset(mvt, mvt$LocationDescription == "GAS STATION")
Driveway = subset(mvt, mvt$LocationDescription == "DRIVEWAY - RESIDENTIAL")
Top5 = rbind(Street, Parking, Alley, Driveway, GasStation)
str(Top5)
#make the table only shows the expected observations
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription, Top5$Arrest)
#see which day of the week the most motor vehicle thefts at gas stations happen
table(GasStation$Weekday)
#see which day of the week do the fewest motor vehicle thefts in residential driveways happen
table(Driveway$Weekday)
