#BASIC CALCULATION

#* for multiplying
6*8 #Ctrl + Enter to run a single command
#^ for raising the power
2^16
#square root function
sqrt(2)
#absolute value function
abs(-96)
#get help in R: ?FunctionName
?sqrt
#saving the output of a function by saving it to a variable: = OR <-
SquareRoot2 <- sqrt(2) 
#print the variable
SquareRoot2 
#naming variable: not using space but capital letters, dots or dashes instead
Year_Hours <- 365*24
Year_Hours
#see the list of all variables in the dataset: ls( )
ls()

#VECTORS AND VARIABLES

#create a vector by the combine function: 
c(2, 3, 4, 5)
#entering some data about the life expectancy in different countries
Country <- c("Brazil", "China", "India", "Switzerland", "USA")
LifeExpectancy <- c(74, 76, 65, 83, 79) #except numbers, characters are stored in "..." format 
#display an element of a vector, use the square bracket: [ ]
Country[1]
LifeExpectancy[3]
#display a sequence of numbers, use the sequence function: 
seq(1, 100, 2) #first number, end number, and the increment 

#DATA FRAME

#create a dataframe
CountryData <- data.frame(Country, LifeExpectancy)
CountryData #print the data frame
#add another variable into the data frame: $
CountryData$Population <- c(199000, 1390000, 1240000, 7999, 318000)
CountryData #see a new column appears in the data frame
#add new observations
Country <- c("Australia", "Greece")
LifeExpectancy <- c(82, 81)
Population <- c(23050, 11125)
NewCountryData <- data.frame(Country, LifeExpectancy, Population)
#combine data frames by stacking the rows
AllCountryData <- rbind(CountryData, NewCountryData)
AllCountryData #but most of the time, data is imported from an external file

#LOADING DATA FILES

#see the pathway in which the dataset is stored
getwd() #navigate to the directory: Ctrl+Shift+H
#read an csv file
WHO <- read.csv("WHO.csv") #csv: comma separate value
#see the structure of the data
str(WHO)
#or get a nummerical summary of each variable
summary(WHO)
#create a subset
WHO_Europe <- subset(WHO, Region == "Europe")
#save the new data frame to a csv file
write.csv(WHO_Europe, "WHO_Europe")
#remove a variable or data frame: rm( )

#run the entire code: Ctrl+Shift+Enter 
#run the entire code without showing the results on the Console: Ctrl+Shift+s