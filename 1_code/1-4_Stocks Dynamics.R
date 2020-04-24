#STOCKS DYNAMICS

#The largest stock market in the world is the New York Stock Exchange (NYSE), located in New York City. About 2,800 companies are listed on the NYSE. In this problem, we'll look at the monthly stock prices of five of these companies: IBM, General Electric (GE), Procter and Gamble, Coca Cola, and Boeing. 

#Import csv files
IBM <- read.csv("IBMStock.csv")
GE <- read.csv("GEStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
Boeing <- read.csv("BoeingStock.csv")

#Summary Statistics ####

#How many observations are there in each data set?
str(IBM) #480
#See the date structure
IBM[1,] #1/1/70
#Convert to date object in R
IBM$Date <- as.Date(IBM$Date, "%m/%d/%y")
GE$Date <- as.Date(GE$Date, "%m/%d/%y")
ProcterGamble$Date <- as.Date(ProcterGamble$Date, "%m/%d/%y")
CocaCola$Date <- as.Date(CocaCola$Date, "%m/%d/%y")
Boeing$Date <- as.Date(Boeing$Date, "%m/%d/%y")
#What is the earliest year in the dataset?
min(c(IBM$Date, GE$Date, Boeing$Date, CocaCola$Date, ProcterGamble$Date)) #1970
#What is the latest year in the datasets?
max(c(IBM$Date, GE$Date, Boeing$Date, CocaCola$Date, ProcterGamble$Date)) #2009
#What is the mean stock price of IBM over this time period?
mean(IBM$StockPrice) #144.375
#What is the minimum stock price of General Electric (GE) over this time period?
min(GE$StockPrice) #9.294
#What is the maximum stock price of Coca-Cola over this time period?
max(CocaCola$StockPrice) #146.584
#What is the median stock price of Boeing over this time period?
median(Boeing$StockPrice) #44.883
#What is the standard deviation of the stock price of Procter & Gamble over this time period?
sd(ProcterGamble$StockPrice) #18.19414

#Visualizing Stock Dynamics ####

#Plot the Date on the x-axis and the StockPrice on the y-axis, for Coca-Cola
plot(CocaCola$Date, CocaCola$StockPrice)
#See the plot as line
plot(CocaCola$Date, CocaCola$StockPrice, "l")
#Around what year did Coca-Cola has its highest stock price in this time period? #1973
#Around what year did Coca-Cola has its lowest stock price in this time period? #1980
#Add the line for Procter & Gamble, too
lines(ProcterGamble$Date, ProcterGamble$StockPrice)
#Distinguish different lines by colors
plot(CocaCola$Date, CocaCola$StockPrice, "l", col = "red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue")
#Alternatively, make the line dasheshed 
plot(CocaCola$Date, CocaCola$StockPrice, "l", lty = 2, col = "red")
#To draw a vertical line at a certain date
abline(v = as.Date(c("2000-03-01")),lwd=2)
abline(v = as.Date(c("1983-03-01")),lwd=2)
#In March of 2000, the technology bubble burst, and a stock market crash occurred. According to this plot, which company's stock dropped more?  #Procter and Gamble
#Around 1983, the stock for one of these companies (Coca-Cola or Procter and Gamble) was going up, while the other was going down. Which one was going up? #Coca-cola
#In the time period shown in the plot, which stock generally has lower values? #Coca-cola

#Visualizing Stock Dynamics 1995-2005 ####

#Take a look at how the stock prices changed from 1995-2005 for all five companies
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type = "l", col = "red", ylim = c(0,210))
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue")
lines(Boeing$Date, Boeing$StockPrice, col = "orange")
lines(IBM$Date, IBM$StockPrice, col = "purple")
lines(GE$Date, GE$StockPrice, lty = 2)
#Which stock fell the most right after the technology bubble burst in March 2000? 
abline(v = as.Date(c("2000-03-01")),lwd=2)
abline(v = as.Date(c("1997-09-01")),lwd=2)
abline(v = as.Date(c("1997-11-01")),lwd=2) #GE
#Which stock reaches the highest value in the time period 1995-2005? #IBM
#In October of 1997, there was a global stock market crash that was caused by an economic crisis in Asia. Comparing September 1997 to November 1997, which companies saw a decreasing trend in their stock price? #Procter and Gamble, Boeing
#In the last two years of this time period (2004 and 2005) which stock seems to be performing the best, in terms of increasing stock price? #Boeing

#Monthly Trends ####

#Extract the month and add this variable to the data frame IBM
IBM$Month <- months(IBM$Date)
tapply(IBM$StockPrice, IBM$Month, mean)
mean(IBM$StockPrice)
#Compare the monthly averages to the overall average stock price. In which months has IBM historically had a higher stock price (on average)? #Jan - May
#Similar to GE and CocaCola
GE$Month <- months(GE$Date)
CocaCola$Month <- months(CocaCola$Date)
tapply(CocaCola$StockPrice, CocaCola$Month, mean)
tapply(GE$StockPrice, GE$Month, mean)
#General Electric and Coca-Cola both have their highest average stock price in the same month. Which month is this? #April
#For the months of December and January, every company's average stock is higher in one month and lower in the other. In which month are the stock prices lower? #December
