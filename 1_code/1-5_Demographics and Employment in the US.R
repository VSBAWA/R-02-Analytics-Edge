#In the US, the government measures unemployment using the Current Population Survey (CPS), which collects demographic and employment information from a wide range of Americans each month.
#The observations in the dataset represent people surveyed in the September 2013 CPS who actually completed a survey.
# While the full dataset has 385 variables, in this exercise we will use a more compact version of the dataset, CPSData.csv

#Loading and Summarizing the Dataset ####

#Read csv file
CPS <- read.csv("CPSData.csv")
#How many interviewees are in the dataset?
str(CPS) #interviewee = observation (131302)
#Among the interviewees with a value reported for the Industry variable, what is the most common industry of employment?
summary(CPS) #educational and health services
#call a sort function on a table() function to obtain a sorted breakdown of a variable
sort(table(CPS$State)) 
#Which state has the fewest interviewees? #New Mexico 
#Which state has the largest number of interviewees? #California
#What proportion of interviewees are citizens of the United States? 
(7073+116639)/131302 #compute the numbers showing in summary result of "Citizenship", we got ca. 94.2%
#For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity?
Hispanic <- subset(CPS$Race, CPS$Hispanic == "1")
summary(Hispanic) #American Indian, Black, Multiracial, White
#Or break down the race and Hispanic ethnicity with table
table(CPS$Race, CPS$Hispanic)

#Evaluating Missing Values ####

#Which variables have at least one interviewee with a missing (NA) value?
summary(CPS) #MetroAreaCode, Married, Education, EmploymentStatus, Industry
#Often when evaluating a new dataset, we try to identify if there is a pattern in the missing values in the dataset. 
#Determine if there is a pattern in the missing values of the Married variable
is.na(CPS$Married) #returns a vector of TRUE/FALSE values for whether the Married variable is missing.
#Break down whether Married is missing based on the reported value of the Region variable
table(CPS$Region, is.na(CPS$Married))
#or based on the Sex value
table(CPS$Sex, is.na(CPS$Married))
#or based on the Age value
table(CPS$Age, is.na(CPS$Married)) #Married is missing for all interviewees Aged 0-14 and is present for all interviewees aged 15 and older. This is because the CPS does not ask about marriage status for interviewees 14 and younger. 
#How many states had all interviewees living in a non-metropolitan area (aka they have a missing MetroAreaCode value)? 
table(CPS$State, is.na(CPS$MetroAreaCode)) #Alaska, Wyoming 
#How many states had all interviewees living in a metropolitan area? #District of Columbia, New Jersey, Rhode Island
#Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
table(CPS$Region, is.na(CPS$MetroAreaCode)) 
10674/(20010 + 10674) #Midwest with ca. 34.8%
5609/(20330 + 5609) #Northeast, 21.6%
9871/(31631 + 9871) #South, 23.8%
8084/(25093 + 8084) #West, 24.4%
#Use tapply instead of tedious computing
tapply(is.na(CPS$MetroAreaCode), CPS$Region, mean)
#Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
tapply(is.na(CPS$MetroAreaCode), CPS$State, mean) #Wisconsin
#Which state has the largest proportion of non-metropolitan interviewees, ignoring states where all interviewees were non-metropolitan?
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)) #Montana

#Integrating Metropolitan Area Data ####

#Codes like MetroAreaCode and CountryOfBirthCode are a compact way to encode factor variables with text as their possible values, and they are therefore quite common in survey datasets. In fact, all but one of the variables in this dataset were actually stored by a numeric code in the original CPS datafile.
#When analyzing a variable stored by a numeric code, we will often want to convert it into the values the codes represent. To do this, we will use a dictionary, which maps the the code to the actual value of the variable.
#Read the two available dictionaries into data frames
MetroAreaMap <- read.csv("MetroAreaCodes.csv")
CountryMap <- read.csv("CountryCodes.csv")
#How many observations (codes for metropolitan areas) are there in MetroAreaMap?
str(MetroAreaMap) #271
#How many observations (codes for countries) are there in CountryMap?
str(CountryMap) #149
#To merge in the metropolitan areas, connect MetroAreaCode from the CPS data frame with the field Code in MetroAreaMap.
CPS <- merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE) #merges the two data frames on these columns, overwriting the CPS data frame with the result
#The first two arguments determine the data frames to be merged ("x" and "y", respectively). 
#by.x="MetroAreaCode" means we're matching on the MetroAreaCode variable from the "x" data frame (CPS), while by.y="Code" means we're matching on the Code variable from the "y" data frame (MetroAreaMap). 
#all.x=TRUE means we want to keep all rows from the "x" data frame (CPS), even if some of the rows MetroAreaCode doesn't match any codes in MetroAreaMap
#Review the new version of the CPS
str(CPS) #new variable MetroArea is added
#How many interviewees have a missing value for the new metropolitan area variable?
summary(CPS) #34238. all of these interviewees would have been removed from the merged data frame if we did not include the all.x=TRUE parameter.
#Which of the following metropolitan areas has the largest number of interviewees?
sort(summary(CPS$MetroArea)) #New York-Northern New Jersey
#Compare the number of interviews among areas
sort(summary(CPS$MetroArea))["Atlanta-Sandy Springs-Marietta, GA"]
sort(summary(CPS$MetroArea))["Baltimore-Towson, MD"]
sort(summary(CPS$MetroArea))["Boston-Cambridge-Quincy, MA-NH"]
sort(summary(CPS$MetroArea))["San Francisco-Oakland-Fremont, CA"]
#Or check in the table
table(CPS$MetroArea)
#Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity? 
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean)) #Laredo, TX
#How many metropolitan areas in the US from which at least 20% of interviewees are Asian?
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean)) #4
#Which metropolitan area has the smallest proportion of interviewees who have received no high school diploma?
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm = TRUE))[1] #Iow City. IA
#none of the interviewees aged 14 and younger have an education value reported, na.rm = TRUE removes the NA value.

#Integrating Country of Birth Data ####

#Merge in the country of birth information from the CountryMap data frame, replacing the CPS data frame with the result
CPS <- merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
#Check the data frame structure again
str(CPS) #new variable "Country" is added.
#How many interviewees have a missing value for the new country of birth variable?
summary(CPS) #176
#Among all interviewees born outside of North America, which country was the most common place of birth?
sort(table(CPS$Country)) #Philippines
#What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area have a country of birth that is not the United States? 
table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States") #1668
1668/(1668+3736) #30.9%
#Which metropolitan area has the largest number of interviewees with a country of birth in India? 
sort(tapply(CPS$Country == "India", CPS$MetroArea,sum, na.rm = TRUE)) #New York-Northern New Jersey-Long Island, NY-NJ-PA 
#In Brazil?
sort(tapply(CPS$Country == "Brazil", CPS$MetroArea,sum, na.rm = TRUE)) #Boston-Cambridge-Quincy, MA-NH
#In Somalia?
sort(tapply(CPS$Country == "Somalia", CPS$MetroArea,sum, na.rm = TRUE)) #Minneapolis-St Paul-Bloomington, MN-WI
     