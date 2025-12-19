Hello. This repo has files related to the export and processing of TN PAC and canidate campaign contributions data from the TN Goverment. The files are as follows:

1. SeleniumExportContributions.R
    This is an R Script which pulls down the Xontriubtion and expenditure data from "https://apps.tn.gov/tncamp/public/cesearch.htm". It then exports that data into the working directory by year, and then again as a single CSV cointaing all data pulled down
2. ProcessToTable.R
    This file converts the raw data collected from SeleniumExportContributions and formats it into a set of tables. Namely an ID Table called UserLookup which contains Individual level Data. A transaction table called links which tracks transaction level data, and a few other tables which are just reformatings of links and UserLookup to be compatible with Network3d Library
3. LearnGroups.R
   Learn groups is a script which groups the data acording to a closest negihbor searching alogrithm. Basicly, a set number of "Searchers" are assigned to random people, PAcs, or corperations in the data. The Searchers then take turns adding the Pac, coperation or person that exchanges the most money with them to the group. If there are no possible options remaning then the group is "finished" and a new random node is selected. The idea is that this will parition the transaction into groups where canidate and donors who are closely assosiated can be easily identified.
4. Learn Groups Expanded -- BETA.R
   Thjis script follows the same concept as LearnGroups does, but it repeates the process 100 or more time, and only groups together nodes which pair together over some threshold. The idea here is that if this process is run enough times it should get rid of some of the varriation which is observed in multiple runs of 3.
   
