###############################
#Data Cleaning and Preparation#
###############################

library(openxlsx)
library(rio)
library(tidyr)
library(dplyr)
library(caret)

#Reading the Profit Loss data
branch_pl <- import("F:/Vivek/Branch Pofitability wo allocationsFY18-19 (003).xls", skip=2)
branch_pl <- branch_pl[-c(56:58),]
branch_pl <- branch_pl %>% separate("Branch",c("BranchCode","branch"), remove = TRUE) #To split the data on hyphen, space or other character
str(branch_pl)
branch_pl <- branch_pl[,-c(3:14)]
names(branch_pl)[5] <- "finalvalue"
colnames(branch_pl) <-  tolower(make.names(colnames(branch_pl)))
branch_pl$result <- ifelse(branch_pl$finalvalue > 0,"Profit","Loss")

#Reading demographic data
demographic <- read.xlsx("Masterdata.xlsx",sheet = "Demographic")
str(demographic)
demographic$increase <- as.numeric(gsub("\\%","",demographic$increase)) #removing % sign
demographic$literacy <- as.numeric(gsub("\\%","",demographic$literacy)) #removing % sign
demographic$population <- as.numeric(gsub("\\,","",demographic$population)) #removing comma in population data
demographic <- transform(demographic, sex.ratio = as.numeric(sex.ratio), density = as.numeric(density))
demographic[demographic=="westBengal"]<- "West Bengal"
demographic[demographic=="bihar"]<- "Bihar"
demographic[demographic=="Odissa"]<- "Odisha"
demographic[demographic=="jharkhand"]<- "Jharkhand"

#Reading Unemployment Data
unemp <-read.xlsx("Masterdata.xlsx", sheet = "UnempData")
str(unemp)
names(unemp)[1]<- "state"
names(unemp)[2]<- "rate"
unemp <- unemp[-c(1,29),]
unemp$rate <- as.numeric(unemp$rate)

#Reading MFI Growth Data
mfigrowth <-read.xlsx("Masterdata.xlsx", sheet = "MFIGrowth")
str(mfigrowth)
names(mfigrowth)[1]<- "state"
names(mfigrowth)[2]<- "count.shg"
names(mfigrowth)[3]<- "saving.amount.lakh.rs"

#Reading GSDP Data
gsdp <- read.xlsx("Masterdata.xlsx",sheet = "GSDPPCData")
str(gsdp)
gsdp <- gsdp[-c(1,36),-c(2:3)]
names(gsdp)[1] <- "state"
names(gsdp)[2] <- "percapita16-17"
names(gsdp)[3] <- "billion.percapita"
gsdp$`percapita16-17` <- as.numeric(gsub("\\,","",gsdp$`percapita16-17`))
gsdp$billion.percapita <- as.numeric(gsub("\\,","",gsdp$billion.percapita))


#Reading State wise branchList
branch <- import("State and District wise branch list.xlsx")
str(branch)
colnames(branch)=tolower(make.names(colnames(branch)))
names(branch)[4] <- "district"
unique(demographic$district)
unique(branch$district)
demographic$district <- toupper(x = demographic$district)
demographic$district <- ifelse(demographic$district == "NORTH TWENTY FOUR PARGANAS","NORTH 24 PARGANAS",demographic$district)
demographic$district <- ifelse(demographic$district == "SOUTH TWENTY FOUR PARGANAS","SOUTH 24 PARGANAS",demographic$district)

#Merging to data frames
d1 <- left_join(branch,demographic, by = "district")

#Imputing missing values

summary(d1)

for(i in 1:ncol(d1)){
    if (is.numeric(d1[,i]) == TRUE){
      d1[is.na(d1[,i]),i] <- median(d1[,i], na.rm = TRUE)
    }
  }

datafinal <- d1[-c(1),-c(1,11)]
str(datafinal)
names(datafinal)[2] <- "branch" 
names(datafinal)[4] <- "state"
head(datafinal)

#Combining demographic data and Branch level profit data

finaldata <- datafinal %>%
  left_join(branch_pl, by = "branch")%>%
  left_join(unemp,by = "state")%>%
  left_join(mfigrowth,by = "state")%>%
  left_join(gsdp,by = "state")

head(finaldata)
finaldata <- finaldata[,-c(1,10)]
str(finaldata)


#Original copy of final data
original <- finaldata

#Imputing missing data for branches not mentioned in previous list
finaldata[sapply(finaldata, is.numeric)] <- lapply(finaldata[sapply(finaldata, is.numeric)], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
finaldata$result <- as.factor(ifelse(finaldata$finalvalue > 0,"Profit","Loss"))
finaldata <- finaldata%>% select(-result,result) #Moving Result column to the end !
#write.csv(finaldata, file = "Finaldataforanalysis1.csv") 


#Correlation Plot to check multicollinearity
finaldata <- finaldata
corrplot::corrplot(cor(finaldata[,unlist(lapply(finaldata, is.numeric))]),method = "number",
                   diag = TRUE,title = "Corelation Plot",order = "AOE",tl.cex = 0.8,number.cex = 0.6)

#Building the models

m1 = glm(result~ density+sex.ratio+literacy,family=binomial(link = "logit"), data = finaldata)
m2 = glm(result~ density+sex.ratio+literacy+rate,family=binomial(link = "logit"), data = finaldata)
m3 = glm(result~ (density*rate)+sex.ratio+literacy,family=binomial(link = "logit"), data = finaldata)
m4 = glm(result~ density+sex.ratio+literacy+count.shg,family=binomial(link = "logit"), data = finaldata)
m5 = glm(result~ density+sex.ratio+literacy+rate+count.shg,family=binomial(link = "logit"), data = finaldata)
m6 = glm(result~ density+sex.ratio+rate,family=binomial(link = "logit"), data = finaldata)

summary(m6)

library(stargazer)
stargazer(m1,m2,m3,m4,m5,m6,type = "text")


#Creating Test Data Set

test_data <- subset(demographic, district %in% c("BHAGALPUR","SAHARSA","KHAGARIA","BADARPUR","KARIMGANJ","CACHAR")) #to be extracted from demographic file as the branch is not present in Finaldata
test_data <- test_data %>% 
  left_join(unemp, by ="state")%>%
  left_join(gsdp, by = "state")%>%
  left_join(mfigrowth, by="state")


#m1 Predict

m1.predictedvalues <- as.factor(if_else(fitted.values(m1) >0.5,"Profit","Loss"))
confusionMatrix(m1.predictedvalues,finaldata$result)

m1.predict <- predict(m1,test_data,type = "response");m1.predict
m1.predict <- ifelse(m1.predict >0.5,"Profit","Loss");m1.predict


#m2 Predict

m2.predictedvalues <- as.factor(if_else(fitted.values(m2) >0.5,"Profit","Loss"))
confusionMatrix(m2.predictedvalues,finaldata$result)

m2.predict <- predict(m2,test_data,type = "response");m2.predict
m2.predict <- ifelse(m2.predict >0.5,"Profit","Loss");m2.predict

#m4 Predict

m4.predictedvalues <- as.factor(if_else(fitted.values(m4) >0.5,"Profit","Loss"))
confusionMatrix(m4.predictedvalues,finaldata$result)

m4.predict <- predict(m4,test_data,type = "response");m4.predict
m4.predict <- ifelse(m4.predict >0.5,"Profit","Loss");m4.predict

#m6 Predict

m6.predictedvalues <- as.factor(if_else(fitted.values(m6) >0.5,"Profit","Loss"))
confusionMatrix(m6.predictedvalues,finaldata$result)

m6.predict <- predict(m6,test_data,type = "response");m6.predict
m6.predict <- ifelse(m6.predict >0.5,"Profit","Loss");m6.predict

#Result Plots

#Average Profit per State
profit.state = finaldata %>%
  group_by(state)%>%
  summary(finalvalue)
 

#aggregate(finaldata$finalvalue, list(finaldata$state), mean)
plot(finaldata$finalvalue,finaldata$total.income,col=c("Blue","Red")[finaldata$result],pch = 20, main = "Branch Wise Profitability Quadrant", xlab = "Net Income", ylab = "Total Income")
abline(a=0,b=4000000,h = 4000000)

library(ggplot2)

ggplot(finaldata, aes(factor(state), finalvalue, fill = result),) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")



plot(finaldata$finalvalue, col=c("Blue","Red")[finaldata$result], pch = 20)
plot(finaldata$finalvalue, by)