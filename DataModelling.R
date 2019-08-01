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


library(stargazer)
stargazer(m1,m2,m3,m4,m5,type = "text")


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

