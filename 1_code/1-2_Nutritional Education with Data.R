#NUTRITIONAL EDUCATION

#Import csv file
USDA <- read.csv("USDA.csv")
#See structure of the data
str(USDA)
#See high-level information of the data (minimum, maximum, mean value, NA entries...)
summary(USDA)
#See all the numbers associated with the amount of sodium
USDA$Sodium
#See the index of the food that has the highest level of sodum
which.max(USDA$Sodium)
#See the vector correspondend with the variable
names(USDA)
#The vector correspond with the variable is called "Description"
USDA$Description[265] #of course it's salt!
#Find out which food has more than 10000 gr sodium
HighSodium <- subset(USDA, Sodium>10000)
#See how many observations in this data frame
nrow(HighSodium) #there're 10 food that meet this criteria
#Output the names of these food with Description vector
HighSodium$Description
#Find the index of a variable
match("CAVIAR", USDA$Description)
#Find out the sodium level of CAVIAR
USDA$Sodium[4154]
#Find out the sodium level of CAVIAR in just one step
USDA$Sodium[match("CAVIAR", USDA$Description)]
#Compare to the standard deviation and the mean of the sodium across the dataset
summary(USDA$Sodium)
#Find out the standard deviation
sd(USDA$Sodium)
#Remove NA 
sd(USDA$Sodium, na.rm = TRUE)
#Plot the scatter plot: plot(X,Y)
plot(USDA$Protein, USDA$TotalFat) #food that has high protein has low fat
#Modify the labels of axes and title, color...
plot(USDA$Protein,USDA$TotalFat, xlab = "Protein", ylab = "Fat", main = "Protein vs. Fat", col = "red")
#Plot a histogram with a title: hist(x, main = "...")
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C Levels")
#Zoom into this section to get a finer understanding of the data by limiting the X-axis
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C Levels", xlim = c(0,100))
#Break up into smaller divisions
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C Levels", xlim = c(0,100), breaks = 100)
#The length of each cell = max value divides the number of breaks
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C Levels", xlim = c(0,100), breaks = 2000)
#Plot a boxplot
boxplot(USDA$Sugar, ylab="Sugar (g)", main="Boxplot of Sugar Levels")
#To check if the food is higher or lower than the avarage sodium
USDA$Sodium[1]>mean(USDA$Sodium, na.rm = TRUE)
USDA$Sodium[50]>mean(USDA$Sodium, na.rm = TRUE) #the numbers are chosen randomly
#Save the output to a vector
HighSodium <- USDA$Sodium >mean(USDA$Sodium, na.rm = TRUE)
#all the values of HighSodium are logical, i.e TRUE or FALSE
str(HighSodium)
#Change TRUE or FALSE into 1 and 0
HighSodium <- as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
#Check out the structure of HighSodium again
str(HighSodium)
#Attach the column to the data frame
USDA$HighSodium <- as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))
#Check out the structure of the date frame again
str(USDA)
#Do similarly with different measurements for food
USDA$HighProtein <- as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=TRUE))
USDA$HighFat <- as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=TRUE))
USDA$HighCarb <- as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=TRUE))
#Check out the structure of the date frame again
str(USDA) #new vectors are added
#Find out how many foods which have higher sodium level by counting the foods that have the value 1
table(USDA$HighSodium)
#See how many foods that have both high sodium and high fat
table(USDA$HighSodium, USDA$HighFat)
#Compute the average amount of iron sorted by protein levels
tapply(USDA$Iron, USDA$HighProtein, mean, na.rm = TRUE)
#Similarly, find out the maximum level of Vitamin C in foods with high and low carbs
tapply(USDA$VitaminC, USDA$HighCarb, max, na.rm=TRUE)
#To see if it's true that foods that are high in carbs have generally high Vitamin C content? 
tapply(USDA$VitaminC, USDA$HighCarb, summary, na.rm=TRUE)
