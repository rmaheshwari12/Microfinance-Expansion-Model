###############################
#Data Cleaning and Preparation#
###############################

library(openxlsx)
library(rio)
library(tidyr)
library(dplyr)
library(caret)
library(car)

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
m2 = glm(result~ density+sex.ratio+literacy+count.shg,family=binomial(link = "logit"), data = finaldata)
m3 = glm(result~ density+sex.ratio+rate,family=binomial(link = "logit"), data = finaldata)


summary(m3)

library(stargazer)
stargazer(m1,m2,m3,type = "text")


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


#m3 Predict

m3.predictedvalues <- as.factor(if_else(fitted.values(m3) >0.5,"Profit","Loss"));m3.predictedvalues
finaldata$modelresult <- as.factor(if_else(fitted.values(m3) >0.5,"Profit","Loss"))
confusionMatrix(m3.predictedvalues,finaldata$result)
summary(finaldata$result)


m3.predict <- predict(m3,test_data,type = "response");m3.predict
m3.predict <- ifelse(m3.predict >0.5,"Profit","Loss");m3.predict

finaldata[,c(1,17:18)]
finaldata$truevalue <- 0
finaldata$truevalue <- if_else((finaldata$result == finaldata$modelresult),'True','False')


#Result Plots

#Average Profit per State
profit.state = finaldata %>%
  group_by(state)%>%
  summary(finalvalue)


#aggregate(finaldata$finalvalue, list(finaldata$state), mean)
plot(finaldata$finalvalue,finaldata$total.income,col = c("Blue", "red")[finaldata$result],pch = 20, main = "Branch Wise Profitability Quadrant", xlab = "Net Income", ylab = "Total Income")
abline(a=0,b=4000000,h = 4000000)

#Main Graph with 2 variations for profit loss and State
#plot(finaldata$finalvalue,finaldata$total.income,col = as.numeric(factor(finaldata$state)),pch = c(1,20)[finaldata$result], main = "Branch Wise Profitability Quadrant", xlab = "Net Income", ylab = "Total Income")

plot(finaldata$finalvalue,finaldata$total.income,col = as.numeric(factor(finaldata$state)),pch = c(1,19)[finaldata$result], main = "Branch Wise Profitability Quadrant", xlab = "Net Income", ylab = "Total Income")
legend(x="bottomright", legend=unique(finaldata$state), col=as.numeric(finaldata$state), pch=1)

#Predicted Profitability Graph
plot(finaldata$finalvalue,finaldata$total.income,col = as.numeric(factor(finaldata$state)),pch = c(1,20)[finaldata$modelresult], main = "Branch Wise Predicted Profitability Quadrant", xlab = "Net Income", ylab = "Total Income")
legend(x="bottomright", legend=unique(finaldata$state), col=c('blue','black','green','red'), pch=19, cex = 0.8, bg = )


plot(finaldata$finalvalue,finaldata$total.income,col = as.numeric(factor(finaldata$state)),pch = 19, main = "Branch Wise Profitability Quadrant", xlab = "Net Income", ylab = "Total Income")
legend(x="bottomright", legend=unique(finaldata$state), col=c('blue','black','green','red'), pch=19, cex = 0.8, bg = "transparent")

plot(finaldata$finalvalue,finaldata$total.income,col =  as.numeric(factor(finaldata$state))[finaldata$state)[finaldata$result],pch = c(1,3,15,20)[finaldata$state], main = "Branch Wise Profitability Quadrant", xlab = "Net Income", ylab = "Total Income")


  scatterplot(data = finaldata, finalvalue ~ total.income,pch = 20, main = "Branch Wise Profitability Quadrant", xlab = "Net Income", ylab = "Total Income")


#Computing State Wise Profit-Loss
aggprofit <- (finaldata %>% group_by(result,state) %>% summarise(aggPL = mean(finalvalue)))
aggprofit1 <- (finaldata %>% group_by(state) %>% summarise(aggPL = mean(finalvalue)))

library(ggplot2)


#Average State wise Profitability
ggplot(aggprofit1,aes(factor(state),aggPL)) +
  geom_bar(stat = "identity", position = "dodge", fill = 'steelblue') +
  scale_fill_brewer(palette = "Paired")+ ggtitle("State-wise Average Profitability") + ylab("Average Net Income (INR)") + xlab("States") + 
  geom_text(aes(label = round(aggPL,digits = 0)),cex = 3.5, vjust = -0.2)+
  theme_minimal()

#State Wise Average Profit and Loss
ggplot(aggprofit,aes(factor(state),aggPL, fill = result)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Paired")+ ggtitle("State-wise Average Profit - Loss") + ylab("Average Net Income (INR)") + xlab("States") + 
  geom_text(aes(label = round(aggPL,digits = 0)),cex = 3.5, vjust = -0.3, position =  'nudge')+
  theme_minimal()


install.packages("plotly")
library(plotly)

#original Plot
#plot_ly(finaldata,x = ~total.income, y = ~expenditure , hovertext = ~branch, type = "scatter", mode = 'markers', size = ~finalvalue, color = ~result, colors = 'Paired', marker = list(opacity = 0.8, sizemode = 'area'))%>%
# layout(title = "Branch Profitability - Predicted", xaxis = list(showgrid = TRUE), yaxis = list(showgrid = TRUE), showlegend = TRUE)

plot_ly(finaldata,x = ~total.income, y = ~expenditure , hoverinfo = 'text', 
        text = ~paste('</br>Branch: ',branch,'</br>NetProfit: ', finalvalue), 
        type = "scatter", mode = 'markers', size = ~finalvalue, color = ~result, colors = 'Paired', 
        marker = list(opacity = 0.8, sizemode = 'area'))%>%
  layout(title = "Branch Profitability - Actual", xaxis = list(showgrid = TRUE, title = 'Total Income'),
         yaxis = list(showgrid = TRUE, title = 'Expenditure'), showlegend = TRUE)


plot_ly(finaldata,x = ~total.income, y = ~expenditure , hoverinfo = 'text',
        text = ~paste('</br>Branch: ',branch,'</br>NetProfit: ', finalvalue),
        type = "scatter", mode = 'markers', size = ~finalvalue, color = ~modelresult, colors = 'Paired', 
        marker = list(opacity = 0.8, sizemode = 'area'))%>%
  layout(title = "Branch Profitability - Predicted", xaxis = list(showgrid = TRUE, title = 'Total Income'),
         yaxis = list(showgrid = TRUE, title = 'Expenditure'), showlegend = TRUE)

plot_ly(finaldata,x = ~total.income, y = ~expenditure , hoverinfo = 'text',
        text = ~paste('</br>Branch: ',branch,'</br>Net Profit: ',finalvalue),
        type = "scatter", mode = 'markers', size = ~finalvalue, color = ~factor(truevalue), colors = 'RdYlGn', 
        marker = list(opacity = 0.8, sizemode = 'area'))%>%
  layout(title = "Branch Profitability - Comparison", xaxis = list(showgrid = TRUE, title = 'Total Income'),
         yaxis = list(showgrid = TRUE, title = 'Expenditure'), showlegend = TRUE)


#plot_ly (x =c(1,2, 3 ),y =c(5,6, 7 ),type = 'scatter',mode = 'markers',size =c(1,5,10),
 # marker = list(color =c('red','blue','green')))

