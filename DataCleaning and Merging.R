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
head(finaldata)
finaldata <- finaldata[,-c(1,11)]
str(finaldata)

#Original copy of final data
original <- finaldata

#Imputing missing data for branches not mentioned in previous list
finaldata[sapply(finaldata, is.numeric)] <- lapply(finaldata[sapply(finaldata, is.numeric)], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
finaldata$result <- ifelse(finaldata$finalvalue > 0,"Profit","Loss")
write.csv(finaldata, file = "Finaldataforanalysis.csv")

m1 = glm(as.factor(result)~ density+sex.ratio+literacy+increase,family=binomial(link = "logit"), data = finaldata)
fitted.values(m1)
m1$fitted.values
confu
summary(m1)
plot(m1)

#Creating Test Data Set

test_data <- subset(finaldata, district == "Bhagalpur") #to be extracted from demographic file as the branch is not present in Finaldata


