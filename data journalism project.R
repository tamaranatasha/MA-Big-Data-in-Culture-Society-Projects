#Load libraries and dataset
library(foreign)
library(tidyverse)
library(tidyr)
library(knitr)
library(dplyr)
library(ggplot2)
library(lubridate)

GWPF<- Tufton_Dataset_Countries_Added

#engagement metrics by dates
table(GWPF$Date)
str(GWPF)
ggplot(data=GWPF, aes(x = Date, y = `engagement-metrics-actual`)) +
  geom_point(color = "darkorchid4" ) +
  labs(title = "2020 Engagement Metrics",
       subtitle = "via Facebook link",
       y = "Engagement count",
       x = "Date") + theme_bw(base_size = 10)
ggsave('../Social and Cultural Analytics/barchart5.pdf')

#engagement metrics by date and country
ggplot(data = GWPF, mapping = aes(x = Date, y =`engagement-metrics-actual`)) +
  geom_point(mapping = aes(color = account_pageAdminTopCountry)) +
  labs(title = "2020 Engagement Metrics",
       subtitle = "via country and date",
       y = "Engagement count",
       x = "Date",
       color = "Country") + theme_bw(base_size = 10)
ggsave('../Social and Cultural Analytics/barchart4.pdf')

#top 10 most engaged posts 
GWPF%>%
  group_by(Date, account_name, `engagement-metrics-actual`, account_pageAdminTopCountry) %>%
  summarise(`engagement-metrics-actual`)%>%
  arrange(desc(`engagement-metrics-actual`))


  write_csv('../Social and Cultural Analytics/engagement.csv')

#top accounts sharing most content

#table showing top 10
GWPF%>%
  group_by(account_name, account_pageAdminTopCountry)%>%
  summarise(n=n())%>%
  arrange(desc(n))

#new dataframe for bar chart
topaccounts<-filter(GWPF, account_name %in% c("The Global Warming Policy Forum", "Craig Kelly", "Friends of Science",
                                              "Climate change is natural", "Australian Climate Sceptics Group","Klimatsans",
                                              "Klimatbalans", "NO WIND TURBINES", "Climate Crisis? There is NO Climate Crisis!",
                                              "Climate & Energy UK"))
#barchart showing distribution
ggplot(data = topaccounts) +
  geom_bar(mapping = aes(x = account_name, fill = account_pageAdminTopCountry))+ coord_flip()+
  labs(title = "Top Facebook Accounts",
      subtitle = "sharing GWPF content",
      y = "Number of Posts",
      x = "Account Name",
      fill = "Country") + theme_bw(base_size = 6) 
ggsave('../Social and Cultural Analytics/barchart3.pdf')

#Account verification
tabaccountverification<-table(GWPF$account_verified)
tabaccountverification
addmargins(tabaccountverification)
#as a proportion
prop.table(tabaccountverification)
#as a percentage
prop.table(tabaccountverification)*100


GWPF%>%
  group_by(account_name, account_verified)%>%
  summarise(subscriberCount)%>%
  arrange(desc(subscriberCount))

ggplot(data = GWPF)+
  geom_bar(mapping = aes(x = account_verified))

ggplot(data = accounts) +
  geom_smooth(mapping = aes(x =`Facebook Account Name` , y = `Number of Posts`))

accounts%>%
  select(`Facebook Account Name`)

