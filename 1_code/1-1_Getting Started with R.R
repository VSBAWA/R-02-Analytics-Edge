#BASIC CALCULATION ####

#* for multiplying
6*8 #Ctrl + Enter to run a single command
#^ for raising the power
2^16
#Square root function
sqrt(2)
#Absolute value function
abs(-96)
#Get help in R: ?FunctionName
?sqrt
#Saving the output of a function by saving it to a variable: = OR <-
SquareRoot2 <- sqrt(2) 
#Print the variable
SquareRoot2 
#Naming variable: not using space but capital letters, dots or dashes instead
Year_Hours <- 365*24
Year_Hours
#See the list of all variables in the dataset: ls( )
ls()

#VECTORS AND VARIABLES ####

#Create a vector by the combine function: 
c(2, 3, 4, 5)
#Entering some data about the life expectancy in different countries
Country <- c("Brazil", "China", "India", "Switzerland", "USA")
LifeExpectancy <- c(74, 76, 65, 83, 79) #except numbers, characters are stored in "..." format 
#Display an element of a vector, use the square bracket: [ ]
Country[1]
LifeExpectancy[3]
#Display a sequence of numbers, use the sequence function: 
seq(1, 100, 2) #first number, end number, and the increment 

#DATA FRAME ####

#Create a dataframe
CountryData <- data.frame(Country, LifeExpectancy)
CountryData #print the data frame
#Add another variable into the data frame: $
CountryData$Population <- c(199000, 1390000, 1240000, 7999, 318000)
CountryData #see a new column appears in the data frame
#add new observations
Country <- c("Australia", "Greece")
LifeExpectancy <- c(82, 81)
Population <- c(23050, 11125)
NewCountryData <- data.frame(Country, LifeExpectancy, Population)
#Combine data frames by stacking the rows
AllCountryData <- rbind(CountryData, NewCountryData)
AllCountryData #but most of the time, data is imported from an external file

#LOADING DATA FILES ####

#See the pathway in which the dataset is stored
getwd() #navigate to the directory: Ctrl+Shift+H
#Read an csv file
WHO <- read.csv("WHO.csv") #csv: comma separate value
#See the structure of the data
str(WHO)
#Or get a nummerical summary of each variable
summary(WHO)
#Create a subset
WHO_Europe <- subset(WHO, Region == "Europe")
#Save the new data frame to a csv file
write.csv(WHO_Europe, "WHO_Europe")
#Remove a variable or data frame: rm( )

#STATISTICAL FUNCTIONS ####

#See the average value
mean(WHO$Under15) #mean" is the average, where "median" is the "middle" value in the list of numbers.
#Standard deviation
sd(WHO$Under15) #measure of how spread out numbers are
#Statiscal summary of just one variable
summary(WHO$Under15)
#Which country has the minimal population under 15?
which.min(WHO$Under15)
#86 is the row number of the observation (Country)
WHO$Country[86] #Japan has the minimal population under 15
#Which country has the maximal population under 15?
which.max(WHO$Under15)
WHO$Country[124] #Niger is the country with largest population under 15

#PLOT AND SUMMARY ####

#Create a scatter plot: plot(x,y)
#A scater plot is a mathematical diagram using Cartesian coordinates to display values for typically two variables for a set of data
plot(WHO$GNI, WHO$FertilityRate) #countries with higher GNI (Gross National Income) have lower birth rate
#Use subset to identify the observations with specific conditions
Outliers <- subset(WHO,GNI>10000 & FertilityRate>2.5)
#How many rows of data in this subset
nrow(Outliers) #7 countries meet this criteria
#Extract a few variables from a dataset
Outliers[c("Country","GNI","FertilityRate")]

#Create a histogram
#A histogram is an approximate representation of the distribution of numerical or categorical data
hist(WHO$CellularSubscribers) #most frequent value of CellularSubscribers is around 100

#Create a box plot of LifeExpectancy sorted by Region
boxplot(WHO$LifeExpectancy ~ WHO$Region)
#Boxplot is a method for graphically depicting groups of numerical data through their quartiles
#Give a new labels to any of the plots by adding a few argument
boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab = "", ylab = "Life Expectancy", main = "Life Expectancy of Countries by Region")
#Write code faster by copying the previous one and modify

#Have a look at the summary table
table(WHO$Region) #tables work well for variables with only a few possible values
#Use tapply function to see nummerical variables
tapply(WHO$Over60, WHO$Region, mean) 
#this splits  the observations by Region and then computes the mean of the variable Over60
tapply(WHO$LiteracyRate, WHO$Region, min) #NA represents the missing value in the data. 
#Remove NA by setting into TRUE value
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm = TRUE)

#run the entire code: Ctrl+Shift+Enter 
#run the entire code without showing the results on the Console: Ctrl+Shift+s