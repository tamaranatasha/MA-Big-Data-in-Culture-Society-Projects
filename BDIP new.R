library(tidyverse)
library(DataExplorer)
library(kableExtra)

ADdata <- read_csv("../MA Big Data Year 1/KAG_conversion_data.csv")
#Rename variables
ADdata<- ADdata %>% rename(campaignID=xyz_campaign_id, conv=Total_Conversion,
                           appconv=Approved_Conversion)
#Create KPI's
ADdata<- ADdata%>%
  mutate(CTR = round(Clicks/Impressions*100,2), TotConv = conv+appconv,
         CPM = round(Spent/Impressions*1000, 2),
         CR = round(TotConv/Clicks*100)) 
view(ADdata)

#Exploration of performance
ggplot(ADdata, aes(as.factor(campaignID), Clicks))+geom_boxplot()+scale_y_log10()
ggsave('../MA Big Data Year 1/boxplot.pdf')


ggplot(ADdata, aes(as.factor(campaignID), TotConv))+geom_boxplot()+scale_y_log10()
ggsave('../MA Big Data Year 1/boxplot1.pdf')

#Total Conversion average per campaign
ADdata %>%
  select(campaignID, TotConv) %>%
  group_by(campaignID)%>%
  summarise(medianTC = median(TotConv), meanTC = mean(TotConv))

#Dataframe for Ad1178

AD1178<-ADdata%>%
  filter(campaignID== 1178)
view(AD1178)

#Create variable ROAS
AD1178 <-
  AD1178 %>%
  mutate(ConVal = conv * 5, AppConVal = appconv *100) %>%
  mutate(TotConVal = ConVal + AppConVal) %>%
  mutate(ROAS = round(TotConVal / Spent, 2)) %>%
  filter(ROAS != 'Inf') %>%
  filter(ROAS != 'NaN') 

#Gender and age analysis
table(AD1178$gender)

ggplot(data = AD1178) +
  geom_boxplot(mapping = aes(x = gender, y = ROAS))+scale_y_log10()
ggsave('../MA Big Data Year 1/boxplot2.pdf')

AD1178 %>%
  select(gender, ROAS) %>%
  group_by(gender)%>%
  summarise(medianROAS = median(ROAS), meanROAS = mean(ROAS))             

ggplot(data = AD1178) +
  geom_boxplot(mapping = aes(x = age, y = CR))+scale_y_log10()

AD1178 %>%
  select(age, ROAS, gender) %>%
  group_by(age, gender)%>%
  summarise(medianROAS = median(ROAS), meanROAS = mean(ROAS))%>%
  arrange(desc(medianROAS))%>%
write_csv('../MA Big Data Year 1/ROAS.csv')

ggplot(data = AD1178) +
  geom_boxplot(mapping = aes(x = age, y = ROAS, fill = gender))+scale_y_log10()
ggsave('../MA Big Data Year 1/boxplot2.pdf')

wilcox.test(ROAS ~ gender, data=AD1178)

ggplot(data = AD1178) +
geom_boxplot(mapping = aes(x = age, y = CR, fill = gender))+scale_y_log10()
ggsave('../MA Big Data Year 1/boxplot3.pdf')

AD1178 %>%
  select(age, CR, gender) %>%
  group_by(age, gender)%>%
  summarise(medianCR = median(CR), meanCR = mean(CR))%>%
  arrange(desc(medianCR))


Age30_40<- AD1178%>%
  filter(age %in% c('30-34', '35-39'))
view(Age30_40)

ggplot(data = Age30_40) +
  geom_boxplot(mapping = aes(x = as.factor(interest), y = CR))+ scale_y_log10()
ggsave('../MA Big Data Year 1/boxplot4.pdf')

ggplot(data = Age30_40) +
  geom_boxplot(mapping = aes(x = as.factor(interest), y = ROAS))+ scale_y_log10()
ggsave('../MA Big Data Year 1/boxplot5.pdf')

Age30_40 %>%
  group_by(interest)%>%
  summarise(medianCR = median(CR), meanCR= mean(CR))%>%
  arrange(desc(meanCR))%>%
write_csv('../MA Big Data Year 1/CRinterestID.csv')
Age30_40 %>%
  group_by(interest)%>%
  summarise(medianROAS = median(ROAS), meanROAS= mean(ROAS))%>%
  arrange(desc(meanROAS))%>%
  write_csv('../MA Big Data Year 1/ROASinterestID.csv')

Age30_40 %>%
  select(age, interest, gender, ROAS, Clicks) %>%
  group_by(age, interest) %>%
  filter(ROAS != 'Inf', interest == 21 | interest == 15, gender == 'M') %>%
  summarise(medianROAS = round(median(ROAS), 2),
            meanROAS = round(mean(ROAS) ,2), clicks = sum(Clicks)) %>%
  arrange(desc(meanROAS))
