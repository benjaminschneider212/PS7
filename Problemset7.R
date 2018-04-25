###########################
###### Problem Set 7 ######
### Benjamin  Schneider ###
###########################

#putting in all of the packages
library(dplyr)
library(ggplot2)
library(readr)

#1
march2018 <- read_csv("~/Documents/GitHub/PS7/March2018.CSV.html") #reading in the data set


#2
numcrimeperday <- march2018 %>% #subsetting by the different descriptions
  group_by(Description) %>% 
  summarise(count=n()) 
numcrimeperday #checking the output once the descriptions are extracted

arrange(numcrimeperday, desc(count)) #organizing the descriptions by frequency

#so looking at this we can see that leaving scene of accident was the most common crime of the month with 464 incidents

#3
numcrimeperneighborhood <- march2018 %>%  #subsetting the data by neighborhoof
  group_by(Neighborhood) %>% 
  summarise(count=n())
numcrimeperneighborhood #checking the data on neighborhood and number of crimes

arrange(numcrimeperneighborhood , desc(count)) #organizing the data on the counts from high to low to see which neighborhoods have the highest crime

#here we can see that neighborhood 35 had the most crime

#4
crimetorobberyprop <- march2018 %>% #this specifically does the proportion of robery to other crime
  group_by(District) %>% #this groups everything by district
  dplyr::filter(grepl('ROBBERY', Description)) %>% #finds which cases are described as robbery
  summarise (count = n()) %>% #gets the overall counts
  mutate(robbery.frequency = count / sum(count)) #takes the count of roberry divded by the sum count for all crimes to get frequency of robbery

arrange(crimetorobberyprop , desc(robbery.frequency)) #organizes by the highesr frequency of robery to crime

#district 5 in this case has a higher proportion of robbery for crime

#5
march2018$DateOccur<-as.Date(march2018$DateOccur,"%m/%d/%Y")#this is to subset the data on a dat basis
data_date<-arrange(march2018, march2018$DateOccur) #arranges the data chronologically
date <- march2018 %>% #creates and element to be plotted in ggplot
  filter(DateOccur > as.Date("2018-3-1")) %>% #everything in March the dataset, and the bounds
  group_by(DateOccur) %>% #grouping by the data
  summarise(count=n()) #lastly getting the counts in order

ggplot(date, aes(x=DateOccur, y=count)) + #this is just the shell of the plot to plot what is above
  geom_line() + 
  labs( #obviously labels
    y="Number of crimes", 
    x="Date", 
    title="Crime by date in March")

#6
march2018$DateOccur<-as.Date(march2018$DateOccur,"%m/%d/%Y") #basically all of this is the same as above, but the filter is by district
data_date<-arrange(march2018, march2018$DateOccur)
date <- march2018 %>%
  filter(DateOccur > as.Date("2018-3-1")) %>%
  group_by(DateOccur,District) %>% #here is the part that is different by district
  summarise(count=n()) 

ggplot(date, aes(x=DateOccur, y=count,group=(District))) + #setting up the plot based on the groupings by district
  geom_line(aes(color=factor(District)))+
  scale_color_manual(name="District",values=c('red', 'orange','yellow','green','blue','purple','violet'))+ #making sure it is colorful!
  labs(
    y="Number of crimes", 
    x="Date", 
    title="Crimes by Date and District in March")

