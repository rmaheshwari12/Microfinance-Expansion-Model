
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
#unempdata <- as.data.frame(unempdata)

unempdata <- unempdata[[16]]

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
#saveWorkbook(masterdata, "Masterdata.xlsx")

