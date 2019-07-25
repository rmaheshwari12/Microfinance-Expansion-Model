install.packages("rvest")
install.packages("xml2")

library(rvest)
library(xml2)
library(openxlsx)
library(rio)
library(tidyr)

setwd("F:/Vivek")

###############################################################
# Collecting Data for population, density, sex ratio, litracy #
###############################################################

links = c("https://www.census2011.co.in/census/state/districtlist/west+bengal.html",
          "https://www.census2011.co.in/census/state/districtlist/bihar.html",
          "https://www.census2011.co.in/census/state/districtlist/jharkhand.html",
          "https://www.census2011.co.in/census/state/districtlist/orissa.html",
          "https://www.census2011.co.in/census/state/districtlist/assam.html")

for (i in 1:5)
{
  url <- links[i]
  webpage <- read_html(url)
  
  #converting html to dataframe
  state_html <- html_nodes(webpage,xpath='/html/body/div[1]/div/div[1]/table')
  state_data <-html_table(state_html, fill = TRUE)
  typeof(state_data)
  state_data <- as.data.frame(state_data);state_data
  
  #Cleaning the dataframe
  statename = c("westBengal","bihar","jharkhand","orissa","Assam")
  state_data <- state_data[,-c(1,3,9:99)]
  colnames(state_data) <- tolower(make.names(colnames(state_data))) 
  state_data <- state_data[-c(8,16),]
  state_data$state <- statename[i]
  assign(paste("statedata_",statename[i], sep = ""), state_data)
  rm(state_data)
}  

collateddata <-  rbind(statedata_westBengal,statedata_jharkhand,statedata_orissa,statedata_bihar,statedata_Assam)
#write.xlsx(masterdata, "sampel_scarped_data.xlsx")

#############################################

#State wise Unemployment rate

url <-  "https://unemploymentinindia.cmie.com/"
webpage <- read_html(url)

# Converting html to datafram
tbls <- html_nodes(webpage, "table")
unemp_html <- html_nodes(tbls, xpath = '/html/body/table/tbody/tr/td/table/tbody/tr[2]/td/table/tbody/tr/td[2]/table[2]')
unempdata <- html_table(tbls,fill = TRUE)
unempdata <- as.data.frame(unempdata)

unempdata <- unempdata[[16]]

#######################################

#NDP data

url <-  "https://data.gov.in/major-indicator/state-wise-net-domestic-product-ndp-current-price"
webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")

NDP_html <- html_nodes(tbls, xpath ='//*[@id="data_table"]')
NDPdat <- html_table(tbls,fill = TRUE)
NDPdat <- as.data.frame(NDPdat)

######################################

#Growth under MFI and SHGs (2015)

mfgrowth <-  read.csv("rs_session_239_AU1837_1.1_1.csv")
#mfgrowth <- subset(mfgrowth, mfgrowth$REGION == "EASTERN REGION" )
mfgrowth <- mfgrowth[-c(1,6),-c(1,2)]

######################################

#GSDP per capita Data

url <-  "https://statisticstimes.com/economy/gdp-capita-of-indian-states.php"
webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")

gsdp_html <- html_nodes(tbls, xpath ='//*[@id="table_id"]')
gsdp_data <- html_table(gsdp_html,fill = TRUE)
gsdp_data <- as.data.frame(gsdp_data)

#Cleaning

gsdp_data <- gsdp_data[,-c(1,6,8:10)]

######################################

#GSDP data

url <-  "https://en.wikipedia.org/wiki/List_of_Indian_states_and_union_territories_by_GDP"
webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")

gsdp_html1 <- html_nodes(tbls, xpath ='//*[@id="mw-content-text"]/div/table[2]')
gsdp_data1 <- html_table(gsdp_html1,fill = TRUE)
gsdp_data1 <- as.data.frame(gsdp_data1)

#####################################

#Writing the excel file

# Create a blank workbook
masterdata <- createWorkbook()

# Add some sheets to the workbook
addWorksheet(masterdata, "Demographic")
addWorksheet(masterdata, "UnempData")
addWorksheet(masterdata, "MFIGrowth")
addWorksheet(masterdata, "GSDPPCData")

# Write the data to the sheets
writeData(masterdata, sheet = "Demographic", x = collateddata)
writeData(masterdata, sheet = "UnempData", x = unempdata)
writeData(masterdata, sheet = "MFIGrowth", x = mfgrowth)
writeData(masterdata, sheet = "GSDPPCData", x = gsdp_data)

# Reorder worksheets
worksheetOrder(masterdata) <- c(1,2,3,4)

# Export the file
saveWorkbook(masterdata, "Masterdata.xlsx")

####################################################
#Data Cleaning and Preparation


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
demographic <- read_csv("Book1.csv")
demographic <- demographic[,-c(8)]
str(demographic)

#Reading State wise branchList
branch <- import("State and District wise branch list.xlsx")
str(branch)
aa <- left_join(branch,demographic, by = "district")

colnames(branch)=tolower(make.names(colnames(branch)))
names(branch)[4] <- "district"
demodist <- unique(demographic$district)
branchdist <- unique(branch$`District Name`)
demographic$district <- toupper(x = demographic$district)
demographic$district <- ifelse(demographic$district == "NORTH TWENTY FOUR PARGANAS","NORTH 24 PARGANAS",demographic$district)
demographic$district <- ifelse(demographic$district == "SOUTH TWENTY FOUR PARGANAS","SOUTH 24 PARGANAS",demographic$district)

#Imputing missing values
install.packages("mice")
library(mice)
summary(aa)
aa.mice <-  
  
  for(i in 1:ncol(aa)){
    if (is.numeric(aa[,i]) == TRUE){
      aa[is.na(aa[,i]),i] <- median(aa[,i], na.rm = TRUE)
    }
  }

datafinal <- aa[,-c(11)]
str(datafinal)
names(datafinal)[3] <- "branch" 
datafinal <- datafinal[-c(1),]
#Combining demographic data and Branch level profit data
colnames(branch) <- tolower(make.names(colnames(branch)))
str(branch)

finaldata <- left_join(datafinal,branch_pl, by = "branch")
head(finaldata)
finaldata <- finaldata[,-c(1,11)]
head(finaldata)

#Original copy of final data
original <- finaldata

#Imputing data for missing branches
finaldata[sapply(finaldata, is.numeric)] <- lapply(finaldata[sapply(finaldata, is.numeric)], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
finaldata$result <- ifelse(finaldata$finalvalue > 0,"Profit","Loss")
write.csv(finaldata, file = "Finaldataforanalysis.csv")

m1 = glm(as.factor(result)~ density+sex.ratio+literacy+increase,family=binomial(link = "logit"), data = finaldata)
fitted.values(m1)
m1$fitted.values
confu
summary(m1)
plot(m1)

