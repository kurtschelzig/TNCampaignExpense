library(RSelenium)
library(rvest)
library(readr)
ss <- rsDriver(browser = "firefox", check = F, verbose = F)

remDr <- ss$client
remDr$open()


SearchYear <- function(Year){
remDr$navigate("https://apps.tn.gov/tncamp/public/cesearch.htm")

#Contribution/Expenditure
remDr$findElement("css", "#frmContributions > div:nth-child(4) > div.seven.columns.control.inline-block > label:nth-child(1)")$clickElement()
#Source
remDr$findElement("css", "#frmContributions > div:nth-child(5) > div.seven.columns.control.inline-block > label:nth-child(3)")$clickElement()

#To
remDr$findElement("css", "#frmContributions > div:nth-child(6) > div > div.seven.columns.control.inline-block > label:nth-child(1)")$clickElement()
remDr$findElement("css", "#frmContributions > div:nth-child(6) > div > div.seven.columns.control.inline-block > label:nth-child(2)")$clickElement()
remDr$findElement("css", "#frmContributions > div:nth-child(6) > div > div.seven.columns.control.inline-block > label:nth-child(3)")$clickElement()
remDr$findElement("css", "#frmContributions > div:nth-child(6) > div > div.seven.columns.control.inline-block > label:nth-child(4)")$clickElement()
#Year
remDr$findElement("css", "#yearSelection_chosen")$clickElement()
remDr$findElement("css", "#yearSelection_chosen > div > div > input[type=text]")$sendKeysToElement(list(as.character(Year),key = "enter"))

remDr$findElement("css", "div.six:nth-child(1) > label:nth-child(5)")$clickElement()
remDr$findElement("css", "div.six:nth-child(1) > label:nth-child(6)")$clickElement()
remDr$findElement("css", "#contAdrCheck")$clickElement()
remDr$findElement("css", "#contOccCheck")$clickElement()
remDr$findElement("css", "#contEmpCheck")$clickElement()
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
}
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
report <- read_csv("Test/report.csv")
file.remove("Test/report.csv")
for(i in 1:count){
  hold <- read_csv(paste("Test/report(",i,").csv", sep = ""))
  file.remove(paste("Test/report(",i,").csv", sep = ""))
  report <- rbind(report,hold)
}
if(j == 2020){
  ALL <- report
}
else{
  ALL <- rbind(ALL,report)
}
}