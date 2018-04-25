###########################
###### Problem Set 7 ######
### Benjamin  Schneider ###
###########################

library(dplyr)
library(ggplot2)

#1
library(readr)
march2018 <- read_csv("~/Documents/GitHub/PS7/March2018.CSV.html")


#2
numcrimeperday <- march2018 %>% 
  group_by(Description) %>% 
  summarise(count=n()) 
numcrimeperday

arrange(numcrimeperday, desc(count))

#so looking at this we can see that leaving scene of accident was the most common crime of the month with 464 incidents

#3
numcrimeperneighborhood <- march2018 %>% 
  group_by(Neighborhood) %>% 
  summarise(count=n())
numcrimeperneighborhood 

arrange(numcrimeperneighborhood , desc(count))

#here we can see that neighborhood 35 had the most crime

#4
crimetorobberyprop <- march2018 %>% 
  group_by(District) %>% 
  dplyr::filter(grepl('ROBBERY', Description)) %>% 
  summarise (count = n()) %>%
  mutate(freq = count / sum(count))

arrange(crimetorobberyprop , desc(freq))

#district 5 in this case has a higher proportion of robbery for crime

#5
library(ggplot2)
march2018$DateOccur<-as.Date(march2018$DateOccur,"%m/%d/%Y")
data_date<-arrange(march2018, march2018$DateOccur)
date <- march2018 %>%
  filter(DateOccur > as.Date("2018-1-1")) %>%
  group_by(DateOccur) %>%
  summarise(count=n()) 

ggplot(date, aes(x=DateOccur, y=count)) + 
  geom_line() + 
  labs(
    y="Number of crimes", 
    x="Date", 
    title="Crimes by dates")

#6
march2018$DateOccur<-as.Date(march2018$DateOccur,"%m/%d/%Y")
data_date<-arrange(march2018, march2018$DateOccur)
date <- march2018 %>%
  filter(DateOccur > as.Date("2018-1-1")) %>%
  group_by(DateOccur,District) %>%
  summarise(count=n()) 

ggplot(date, aes(x=DateOccur, y=count,group=(District))) + 
  geom_line(aes(color=factor(District)))+
  scale_color_manual(name="District",values=c('red', 'orange','yellow','green','blue','purple','violet'))+
  labs(
    y="Number of crimes", 
    x="Date", 
    title="Crimes by dates and District")

