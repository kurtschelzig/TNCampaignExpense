library(RSelenium)
library(rvest)
library(readr)
#Years of Data You Want
StartYear <- 2020 
EndYear <-2025
## Initialize the Selenium Instance
ss <- rsDriver(browser = "firefox", check = F, verbose = F, phantomver = NULL)
remDr <- ss$client
remDr$open()

# Search Year with Settings
SearchYear <- function(Year){
  remDr$navigate("https://apps.tn.gov/tncamp/public/cesearch.htm")
  
  #Contribution/Expenditure, Set label:nth-child to 1 for Contribution, and 2 for expenditure
  remDr$findElement("css", "#frmContributions > div:nth-child(4) > div.seven.columns.control.inline-block > label:nth-child(1)")$clickElement()
  #Source
  remDr$findElement("css", "#frmContributions > div:nth-child(5) > div.seven.columns.control.inline-block > label:nth-child(3)")$clickElement()
  
  #From
  remDr$findElement("css", "#frmContributions > div:nth-child(6) > div > div.seven.columns.control.inline-block > label:nth-child(1)")$clickElement()  # Canidate
  remDr$findElement("css", "#frmContributions > div:nth-child(6) > div > div.seven.columns.control.inline-block > label:nth-child(2)")$clickElement()  # PACs
  remDr$findElement("css", "#frmContributions > div:nth-child(6) > div > div.seven.columns.control.inline-block > label:nth-child(3)")$clickElement()  # Private Individuals
  remDr$findElement("css", "#frmContributions > div:nth-child(6) > div > div.seven.columns.control.inline-block > label:nth-child(4)")$clickElement()  # Biusness/Organizations
  #Year
  remDr$findElement("css", "#yearSelection_chosen")$clickElement()
  remDr$findElement("css", "#yearSelection_chosen > div > div > input[type=text]")$sendKeysToElement(list(as.character(Year),key = "enter"))
  #Feilds Of Data
  remDr$findElement("css", "div.six:nth-child(1) > label:nth-child(5)")$clickElement() #Election Year
  remDr$findElement("css", "div.six:nth-child(1) > label:nth-child(6)")$clickElement() #Report Name
  remDr$findElement("css", "#contAdrCheck")$clickElement() # Contributor Adress
  remDr$findElement("css", "#contOccCheck")$clickElement() # Contributor Employer
  remDr$findElement("css", "#contEmpCheck")$clickElement() # Desciption
  
  #Search
  remDr$findElement("css", "#descCheck")$clickElement()
  
  remDr$findElement("css", "#_continue")$clickElement()
}

SavePage <- function(){
  remDr$findElement("css", "#content > form > div.exportlinks > a:nth-child(1)")$clickElement()
}
NextPage <- function(){
  remDr$findElement("css", "p.center:nth-child(2) > a:nth-child(1)")$clickElement()
}
MorePages <- function(){
  Test <- tryCatch({remDr$findElement("css", "p.center:nth-child(2) > a:nth-child(1)"); TRUE}, error = function(e){return(FALSE)})
  return(Test)
} # Checks if it is the final Page or if there is More

ALL <- NULL
for(j in 2020:2025){
  SearchYear(j)
  More <- TRUE
  count <- -1
  while(More == TRUE){
    SavePage()
    NextPage()
    Sys.sleep(2)
    More <- MorePages()
    count <- count+1
  }
  report <- read_csv("report.csv")
  file.remove("report.csv")
  for(i in 1:count){
    hold <- read_csv(paste("report(",i,").csv", sep = ""))
    file.remove(paste("report(",i,").csv", sep = ""))
    report <- rbind(report,hold)
  }
  if(j == StartYear){
    ALL <- report
  }
  else{
    ALL <- rbind(ALL,report)
  }
}
