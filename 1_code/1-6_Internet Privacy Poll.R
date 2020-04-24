#INTERNET PRIVACY POLL

#To measure the degree to which people are concerned about hot-button issues like Internet privacy, social scientists conduct polls in which they interview a large number of people about the topic. We will analyze data from a July 2013 Pew Internet and American Life Project poll on Internet anonymity and privacy, which involved interviews across the US.
#Full polling can be found at: https://www.pewresearch.org/internet/2013/09/05/anonymity-privacy-and-security-online/

#Loading and Summarizing the Dataset ####
poll <- read.csv("AnonymityPoll.csv")
#How many people participated in the poll?
str(poll) #1002
#How many interviewees responded that they use a smartphone?
table(poll$Smartphone == 1) #487
#How many interviewees did not respond to the question, resulting in a missing value, or NA?
summary(poll$Smartphone) #43
#Using the table() function on two variables to see how they are related
table(poll$Sex, poll$Region)
#Which states in the Midwest census region?
table(poll$State, poll$Region) #Kansas, Missouri, Ohio...
#Which was the state in the South census region with the largest number of interviewees?
sort(tapply(poll$Region == "South", poll$State, sum)) #Texas
#Another way to approach these problems is to subset the data frame and then use table on the limited data frame
MidWestInterviewees <- subset(poll, Region == "Midwest")
sort(table(MidWestInterviewees$State))
SouthInterviewees <- subset(poll, Region == "South")
sort(table(SouthInterviewees$State))

#Internet and Smartphone Users ####

#Many of the response variables (Info.On.Internet, Worry.About.Info, Privacy.Importance, Anonymity.Possible, and Tried.Masking.Identity) were not collected if an interviewee does not use the Internet or a smartphone, meaning the variables will have missing values for these interviewees.
#How many interviewees reported not having used the Internet and not having used a smartphone?
table(poll$Internet.Use, poll$Smartphone) #186
#How many interviewees have a missing value for their Internet use?
summary(poll$Internet.Use) #1
#Use the subset function to obtain a data frame which is limited to interviewees who reported Internet use or who reported smartphone use
limited <- subset(poll, Internet.Use == 1 | Smartphone == 1)
#How many interviewees are in the new data frame?
str(limited) #792

#Summarizing Opinions about Internet Privacy ####

#Which variables have missing values in the limited data frame? 
summary(limited) #Smartphone, Age, Conservativeness...
#What is the average number of pieces of personal information on the Internet, according to the Info.On.Internet variable?
summary(limited$Info.On.Internet) #3.795
#How many interviewees reported a value of 0 for Info.On.Internet?
table(limited$Info.On.Internet) #105
#What proportion of interviewees who answered the Worry.About.Info question worry about how much information is available about them on the Internet? 
table(limited$Worry.About.Info)
386/(404 + 386) #0.4886
#Note that we did not divide by 792 (the total number of people in the data frame) to compute this proportion.
#What proportion of interviewees who answered the Anonymity.Possible question think it is possible to be completely anonymous on the Internet
table(limited$Anonymity.Possible)
278/(475 + 278) #0.3692
#This can also be read from summary
summary(limited$Anonymity.Possible) #Mean
#What proportion of interviewees who answered the Tried.Masking.Identity question have tried masking their identity on the Internet?
summary(limited$Tried.Masking.Identity) #0.1633
#What proportion of interviewees who answered the Privacy.Laws.Effective question find United States privacy laws effective?
summary(limited$Privacy.Laws.Effective) #0.2559

#Relating Demographics to Polling Results ####

#Often, we are interested in whether certain characteristics of interviewees affect their opinions on the topic of the poll. Investigate the relationship between the characteristics Age and Smartphone and outcome variables Info.On.Internet and Tried.Masking.Identity:
hist(limited$Age) #peaks at around 60 years old.
#Both Age and Info.On.Internet are variables that take on many values, so a good way to observe their relationship is through a graph:
plot(limited$Age, limited$Info.On.Internet) #because Info.On.Internet takes on a small number of values, multiple points will be plotted in exactly the same location on this graph, making the distribution hard to see.
#To avoid points covering each other up, use jitter()
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))
#For younger people aged 18-30, the average value of Info.On.Internet appears to be roughly 5, while most peopled aged 60 and older have a value less than 5. Therefore, older age appears to be associated with a smaller value of Info.On.Internet, but from the spread of dots on the image, it's clear the association is not particularly strong. 
#What is the largest number of interviewees that have exactly the same value in their Age variable AND the same value in their Info.On.Internet variable?
max(table(limited$Age, limited$Info.On.Internet)) 
#What is the average Info.On.Internet value for smartphone users?
tapply(limited$Info.On.Internet, limited$Smartphone, summary)
#4.368
#What proportion of smartphone users who answered the Tried.Masking.Identity question have tried masking their identity when using the Internet?
tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary) #0.1925
#We can also get the breakdown for smartphone and non-smartphone users with
tapply(limited$Tried.Masking.Identity, limited$Smartphone, table)
