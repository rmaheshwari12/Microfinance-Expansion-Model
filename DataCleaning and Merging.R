###############################
#Data Cleaning and Preparation#
###############################

library(openxlsx)
library(rio)
library(tidyr)
library(dplyr)

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

unemp$rate <- as.numeric(unemp$rate)

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

finaldata <- left_join(datafinal,branch_pl, by = "branch")
finaldata <- left_join(finaldata,unemp, by = "state")
finaldata <- left_join(finaldata,mfigrowth,by ="state")
head(finaldata)
finaldata <- finaldata[,-c(1,11)]
str(finaldata)

#Original copy of final data
original <- finaldata

#Imputing missing data for branches not mentioned in previous list
finaldata[sapply(finaldata, is.numeric)] <- lapply(finaldata[sapply(finaldata, is.numeric)], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
finaldata$result <- as.factor(ifelse(finaldata$finalvalue > 0,"Profit","Loss"))
write.csv(finaldata, file = "Finaldataforanalysis.csv")

m1 = glm(result~ density+sex.ratio+literacy+increase,family=binomial(link = "logit"), data = finaldata)
m2 = glm(result~ density+sex.ratio+literacy+increase+rate,family=binomial(link = "logit"), data = finaldata)
m3 = glm(result~ density+sex.ratio+literacy+increase+rate+finaldata$count.shg,family=binomial(link = "logit"), data = finaldata)
m4 = glm(result~ (density*rate)+sex.ratio+literacy+increase+rate,family=binomial(link = "logit"), data = finaldata)
m5 = glm(result~ density+sex.ratio+literacy*rate+increase,family=binomial(link = "logit"), data = finaldata)
m6 = glm(result~ density+sex.ratio+literacy+increase*rate,family=binomial(link = "logit"), data = finaldata)m7 = glm(result~ density+sex.ratio+literacy*increase*rate,family=binomial(link = "logit"), data = finaldata)
m7 = glm(result~ density+sex.ratio+literacy,family=binomial(link = "logit"), data = finaldata)


library(stargazer)


stargazer(m1,m2,m3,type = "text")

summary(m7)

fitted.values(m1)
m1$fitted.values
confu
summary(m1)
plot(m1)
rep

#Creating Test Data Set

test_data <- subset(demographic, district %in% c("BHAGALPUR","SAHARSA","KHAGARIA","BADARPUR","KARIMGANJ","CACHAR")) #to be extracted from demographic file as the branch is not present in Finaldata
#predict.glm(test_data,m1)
head(test_data)

exvalue <- function(x)
  {
  y <- exp(-x)/(1+(exp(-x)))
  return(y)
}
  
exvalue(m1$coefficients)

test.prob = predict(m1, test_data, type="response")


test_pred_num <- ifelse(test.prob > 0.5, "Profit", "Loss")
y_pred <- factor(test_pred_num, levels=c("Loss", "Profit"))
y_act <- test_data$
mean(y_pred == y_act)

test.predict = rep("Loss", dim(finaldata)[12])
test.predict[test.prob > .5] = "Profit"
table(test.pred, as.factor(finaldata$result))

