#load libraries 
library(tidyverse)
library(readr)
library(stringr)
library(splitstackshape)
library(knitr)

#Run before ordering variable
skintonelevels<- c("F", 
                   "M", 
                   "D",
                   "NA")
#Load Data
ThatGirlData <- read_csv('../Data Analysis/that girl data.csv',
                         col_types = cols(
                          col_factor(),
                          col_factor(),
                          col_factor(),
                          col_factor(),
                          col_factor(),
                          col_factor(),
                          col_factor(),
                          col_number(),
                          col_factor(),
                          col_factor(ordered = TRUE, levels = skintonelevels)))
                         
spec(ThatGirlData)

#specify level names for factors
levels(ThatGirlData$`Early morning routine`)<- c("Yes", "No")
levels(ThatGirlData$`Homemade smoothie`)<- c("Yes", "No")
levels(ThatGirlData$`Read/Journal`)<- c("Yes", "No")
levels(ThatGirlData$Exercise)<- c("Yes", "No")
levels(ThatGirlData$Skincare)<- c("Yes", "No")
levels(ThatGirlData$`Fresh fruit/veg`)<- c("Yes", "No")
levels(ThatGirlData$Water)<- c("Yes", "No")
levels(ThatGirlData$Video_type)<- c("Healthy Lifestyle", "Promoting Products",
                                   "Balanced Lifestyle")
levels(ThatGirlData$Skin_tone)<- c("Fair", "Medium","Dark", "NA")
view(ThatGirlData)

#Create table for proportions (smoothie)
tabHMS<-table(ThatGirlData$`Homemade smoothie`)
tabHMS
#adds sum
addmargins(tabHMS)
#as a proportion
prop.table(tabHMS)
#as a percentage
prop.table(tabHMS)*100

#create a crosstab which represents the disribution of skintone and vieo type
SkinVid<-table(ThatGirlData$Skin_tone, ThatGirlData$Video_type)
addmargins(SkinVid)
#as a percentage
prop.table(SkinVid, 2)*100
SkinVid

spearman.test(ThatGirlData$Likes, ThatGirlData$Skin_tone)
wilcox.test(SkinVid, exact = FALSE)

#clustered bar chart
ggplot(data = ThatGirlData[!is.na(ThatGirlData$Skin_tone)& !is.na(ThatGirlData$Video_type),],aes(x=Video_type))+
  geom_bar(stat = "Count", aes(fill=Skin_tone), position = "fill")+ scale_fill_manual(values = c("dark turquoise", "hotpink", "coral", "black"))
+xlab("Video Type")+ylab("Percentage of Respondents")

fisher.test(SkinVid)
#Create table for proportions (early)
tabEMR<-table(ThatGirlData$`Early morning routine`)
tabEMR
#adds sum
addmargins(tabEMR)
#as a proportion
prop.table(tabEMR)
#as a percentage
prop.table(tabEMR)*100

#Create table for proportions (Read/journal)
tabRJ<-table(ThatGirlData$`Read/Journal`)
tabRJ
#adds sum
addmargins(tabRJ)
#as a proportion
prop.table(tabRJ)
#as a percentage
prop.table(tabRJ)*100

#Create table for proportions (exercise)
tabEx<-table(ThatGirlData$Exercise)
tabEx
#adds sum
addmargins(tabEx)
#as a proportion
prop.table(tabEx)
#as a percentage
prop.table(tabEx)*100

#Create table for proportions (skin)
tabSC<-table(ThatGirlData$Skincare)
tabSC
#adds sum
addmargins(tabSC)
#as a proportion
prop.table(tabSC)
#as a percentage
prop.table(tabSC)*100

#Create table for proportions (exercise)
tabEx<-table(ThatGirlData$Exercise)
tabEx
#adds sum
addmargins(tabEx)
#as a proportion
prop.table(tabEx)
#as a percentage
prop.table(tabEx)*100

#Create table for proportions (fresh fruit/veg)
tabFFV<-table(ThatGirlData$`Fresh fruit/veg`)
tabFFV
#adds sum
addmargins(tabFFV)
#as a proportion
prop.table(tabFFV)
#as a percentage
prop.table(tabFFV)*100

#Create table for proportions (water)
tabW<-table(ThatGirlData$Water)
tabW
#adds sum
addmargins(tabW)
#as a proportion
prop.table(tabW)
#as a percentage
prop.table(tabW)*100

#Create table for proportions (skintone)
tabST<-table(ThatGirlData$Skin_tone)
tabST
#adds sum
addmargins(tabST)
#as a proportion
prop.table(tabST)
#as a percentage
prop.table(tabST)*100

#Create table for proportions (skintone)
tabVT<-table(ThatGirlData$Video_type)
tabVT
#adds sum
addmargins(tabVT)
#as a proportion
prop.table(tabVT)
#as a percentage
prop.table(tabVT)*100


# skintone as a bar chart which excludes missing values
ggplot(ThatGirlData[!is.na(ThatGirlData$Skin_tone),],aes(x=Skin_tone, y= ..prop.., group = 1))+
  geom_bar(stat = "Count", fill = 'coral', color = 'coral3') + xlab("Skin Tone")+ ylab("Proportion of respondents")


#clustered bar chart
ggplot(data = ThatGirlData[!is.na(ThatGirlData$Skin_tone)& !is.na(ThatGirlData$Video_type),],aes(x=Video_type))+
  geom_bar(stat = "Count", aes(fill=Skin_tone), position = "fill")+xlab("Video Type")+ylab("Percentage of Respondents")



#Bivariate analysis create a crosstab which represents the distribution of exercise and fruit/veg
ExVeg <- table(ThatGirlData$Exercise, ThatGirlData$`Fresh fruit/veg`)
addmargins(ExVeg)
#as a percentage
prop.table(ExVeg,2)*100
#run chi-squared test
chi_ExVeg<-chisq.test(ExVeg, correct = F)

chisq.test(ExVeg, correct = F)

#margin of error - for sample between 50-100 moe is higher between 10-14% 
# Opinion pollsters say ‘the proportion is 50% and the 95% margin of error is ± 3.1%’
moe.proportion <- function(p, n){1.96*sqrt(p*(1-p)/n)}
moe.proportion(0.63, 60)
#margin of error
n <- 60
p <- 0.63
margin<-1.96*sqrt(p*(1-p)/n)
margin

#calculate lower and upper bounds of confidence interval
low <- p - margin
low

high <- p + margin
high

#average likes per video types
ThatGirlData%>%
  group_by(Video_type)%>%
  summarise(Average_Likes = mean(Likes), N = n())

sd(ThatGirlData$Likes)
histlikes<-ThatGirlData$Likes

#Distribution of likes
hist(histlikes,
     main = "Distribution of Likes", cex = 2,
     xlab = "Likes",
     ylab = "Frequency",
     col = "turquoise",
     cex = 1) +
abline(v = medianlikes, col = 'hotpink2', lwd = 2)

text(600000, 22, round(medianlikes),col = 'hotpink2', cex = 1)
text(600000, 25, "Median = ", cex = 1, col = 'hotpink2')
abline(v = meanlikes, col = 'coral', lwd = 2)
text(600000, 10, round(meanlikes),col = 'coral', cex = 1)
text(600000, 13, "Mean = ", cex = 1, col = 'coral')

ThatGirlData%>%
  select(Likes)%>%
  arrange(desc(Likes))


meanlikes<-mean(ThatGirlData$Likes)
medianlikes<-median(ThatGirlData$Likes)

#Occurrences in videos
occuranceDF <- ThatGirlData %>% select(`Homemade smoothie`:Water)
occuranceDF
occurancesbarplot<-barplot(sapply(occuranceDF, function(x) prop.table(table(x))),
                main = "Occurrances in Videos", legend = TRUE, args.legend = list(x = "topright", inset = c(-0, -0.3), cex = 0.5),
                col = c('coral', 'hotpink2'), names.arg = c("Homemade Smoothie", 
               "Early Morning Routine","Read/Journal","Exercise", 
               "Skincare", "Fruit/Veg", 
               "Water"), space = c(0.5), las = 2,
                  cex.names = 0.5, cex.axis = 0.5)

barplot(SkinVid)

barplot(ThatGirlData$Skin_tone, ThatGirlData$Video_type,
        main = "Stacked barchart",
        xlab = "Transmission type", ylab = "Frequency",
        col = c("darkgrey", "darkblue", "red"),
        legend.text = rownames(other_table),
        beside = FALSE) # Stacked bars (default)



SkinVid<- ThatGirlData %>% select(Video_type, Skin_tone)
SkinVid
#top outliers highest likes
outliersturned<-t(outliers)                          

 outliers<-ThatGirlData%>%
  filter(Likes >=1500000)
write.csv(outliersturned, "outliersturned.csv", row.names = TRUE)
view(outliers)

#mean likes per video type
ThatGirlData %>%
  group_by(Video_type) %>%
  summarise( mean_likes = mean(Likes),
             n = n())

#Occurrances in videos
occuranceDF <- ThatGirlData %>% select(`Homemade smoothie`:Water)
occuranceDF
occurancesbarplot<-barplot(sapply(occuranceDF, function(x) prop.table(table(x))), 
                  title("Occurances in Video's"),
                  legend = TRUE, args.legend = list(x = "topright", inset = c(-0, -0.3), cex = 0.5),
                   col = rainbow(7), names.arg = c("Homemade Smoothie", 
                   "Early Morning Routine","Read/Journal","Exercise", "Skincare", "Fruit/Veg", 
                   "Water"), space = c(0.5), las = 2,
                   cex.names = 0.5, cex.axis = 0.5)
ThatGirlData %>%
  mutate(Video_type = Video_type == '')

#create dummy variables
Promoting_products <- ifelse(ThatGirlData$Video_type == 'Promoting Products', 1, 0)
Advocating_Balance <- ifelse(ThatGirlData$Video_type == 'Balanced Lifestyle', 1, 0)
Lifestyle_change <- ifelse(ThatGirlData$Video_type == 'Healthy Lifestyle', 1, 0)
Fair <- ifelse(ThatGirlData$Skin_tone == 'Fair', 1, 0)
Medium <- ifelse(ThatGirlData$Skin_tone == 'Medium', 1, 0)
Dark <- ifelse(ThatGirlData$Skin_tone == 'Dark', 1, 0)
TGreg <- data.frame(Likes = ThatGirlData$Likes,
                     Promoting_Products = Promoting_products,
                     Balanced_Lifestyle = Advocating_Balance,
                    Lifestyle_Change = Lifestyle_change,
                    Fair = Fair,
                    Medium = Medium,
                    Dark = Dark,
                    Smoothie = ThatGirlData$`Homemade smoothie`,
                    Early_Routine = ThatGirlData$`Early morning routine`,
                    ReadJournal = ThatGirlData$`Read/Journal`,
                    Exercise = ThatGirlData$Exercise,
                    Skincare = ThatGirlData$Skincare,
                    Fresh_fruit_Veg = ThatGirlData$`Fresh fruit/veg`,
                    Water = ThatGirlData$Water)

#restrict the data to only include complete cases
TGreg<-TGreg[complete.cases(TGreg$Promoting_Products, TGreg$Balanced_Lifestyle, TGreg$Promoting_Products, 
                            TGreg$Fair, TGreg$Medium, TGreg$Dark, TGreg$Smoothie,
                            TGreg$Early_Routine, TGreg$ReadJournal, TGreg$Exercise,
                            TGreg$Skincare, TGreg$Fresh_fruit_Veg, TGreg$Water,
                            TGreg$Likes),]
TGreg




TGreg
df_reg <- data.frame(income = df$income,
                     age = df$age,
                     married = married,
                     divorced = divorced)
model <- lm(Lifestyle_Change ~ Fair + Medium + Dark, data=TGreg)
mode2 <- glm(Promoting_Products ~ Fair + Medium + Dark, data=TGreg)
mode3 <- glm(Advocating_Balance ~ Fair + Medium + Dark, data=TGreg)
summary(mode3)
install.packages("devtools")
library(devtools)
library(broom)

skinvidplot<-barplot(ThatGirlData$Skin_tone)



tidymodel3<- tidy(mode3)
tidymodel3

mode4<- lm(Promoting_products ~ Smoothie + Early_Routine + ReadJournal +
              Exercise + Skincare + Fresh_fruit_Veg + Water, data = TGreg)
summary(mode4)
write.csv(tidymodel2, "tidy_model3.csv")

man
mode5 <- lm(Likes ~ Lifestyle_Change + Promoting_Products + Advocating_Balance,
             data = TGreg)

fisher.test(TGreg$Medium, TGreg$Promoting_Products)

model2 <-glm(likes~Lifestyle_change + Likes, data = TGreg, family = "binomial")
summary(model2)
exp(cbind(OR=coef(model), confint(model)))

