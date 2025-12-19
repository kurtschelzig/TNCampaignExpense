library(readr)
library(networkD3)
library(dplyr)
library(tidyr)
library(tidyverse)
#Select Source File

#Process Contribution Data into DataFrame
DonationRecord <- read_csv("2025.csv")
DonationRecord$Amount <- parse_number(DonationRecord$Amount)
DonationRecord$`Contributor Name` <- iconv(DonationRecord$`Contributor Name`,"latin1","UTF-8",sub = "")
DonationRecord$`Recipient Name` <- iconv(DonationRecord$`Recipient Name`,"latin1","UTF-8",sub = "")
DonationRecord <- DonationRecord[which(DonationRecord$`Recipient Name` != "CWA-COPE PCC"),]
DonationRecord$ContributorID <- NA
DonationRecord$RecipientID <- NA


#Limit for Donation Size
DonationRecord <-DonationRecord[which(DonationRecord$Amount >=5000),]


#Creates a Table of USer ID's and Donation Amounnts by ID
User_C <- unique(DonationRecord$`Contributor Name`)
User_R <- unique(DonationRecord$`Recipient Name`)
User <- unique(c(User_C,User_R))
UserLookup <- data.frame(UserID = c(1:(length(User))), UserName = User, size = 0)


#Assigns User ID's to transaction Table
for(i in 1:length(DonationRecord$Amount)){
  DonationRecord$ContributorID[i] <- UserLookup$UserID[which(UserLookup$UserName == DonationRecord$`Contributor Name`[i])]
  DonationRecord$RecipientID[i] <- UserLookup$UserID[which(UserLookup$UserName == DonationRecord$`Recipient Name`[i])]
}

#Transaction Between Users simplifed into Matrix to Avoid dupliate transactions
IncidenceMatrix <- matrix(0, nrow = length(UserLookup$UserID), ncol = length(UserLookup$UserID))
for(i in 1:length(DonationRecord$ContributorID)){
  IncidenceMatrix[DonationRecord$ContributorID[i],DonationRecord$RecipientID[i]]  <- IncidenceMatrix[DonationRecord$ContributorID[i],DonationRecord$RecipientID[i]] + DonationRecord$Amount[i]
}

for( i in 1:length(UserLookup$UserID)){
  UserLookup$size[i] <- sum( DonationRecord$Amount[ which(DonationRecord$ContributorID == UserLookup$UserID[i] | DonationRecord$RecipientID == UserLookup$UserID[i])])
}
rownames(IncidenceMatrix) <- UserLookup$UserName
colnames(IncidenceMatrix) <- UserLookup$UserName



#Converts Incidence Maatrix for networkD3
links <- IncidenceMatrix %>% 
  as.data.frame() %>% 
  rownames_to_column(var="source") %>% 
  gather(key="target", value="value", -1) %>%
  filter(value != 0)

nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)

links$IDsource <- match(links$source, nodes$name)-1
links$IDtarget <- match(links$target, nodes$name)-1




simpleNetwork(links[,c(1:2)], zoom = TRUE)

MisNodes <- data.frame(name = UserLookup$UserName, group = 1,size = sqrt(UserLookup$size))
MisLinks  <- data.frame(source = links$IDsource, target = links$IDtarget ,value = log(links$value))



########################
  # Number of Searching Nodes, If Searches is Higher the Searching Algo will included more groups, If it is smaller it will include less Groups
Searches <- 5
Starts <- sample(0:(length(MisNodes$name)-1), Searches, replace = TRUE)
Eligable <- 0:(length(MisNodes$name)-1)
Eligable <- setdiff(Eligable, Starts)
Process <- data.frame(
  V1     = 1:Searches,
  Starts = Starts,
  Holds  = I(vector("list", Searches)),
  Group  = I(vector("list", Searches))
)

# initialize Holds/Group to empty vectors (optional but nice)
for (i in 1:Searches) {
  Process$Holds[[i]] <- integer(0)
  Process$Group[[i]] <- Starts[i]
}

for(i in 1:length(Starts)){
  
  targets <- MisLinks$target[ which((MisLinks$source == Process$Starts[i]) &  (MisLinks$target %in% Eligable) )]
  sources <- MisLinks$source[ which((MisLinks$target == Process$Starts[i]) &  (MisLinks$source %in% Eligable) )]
  sources <- unique(sources)
  targets <- unique(targets)
  Process$Holds[[i]] <- c( Process$Holds[[i]] ,targets,sources)
}

########Inctiment
while(length(Eligable) > 0 ){
  for(i in 1:length(Starts)){
    if(length(Eligable) == 0){
      break();
    }
    LinkAdress <- which(((links$IDsource %in% Process$Holds[[i]][Process$Holds[[i]] %in% Eligable]) |(links$IDtarget %in% Process$Holds[[i]][Process$Holds[[i]] %in% Eligable])  ))
    MinDistance <- LinkAdress[which.min(1/links$value[LinkAdress])]
    option <- c(links$IDsource[MinDistance],links$IDtarget[MinDistance])
    
    selection <- option[option %in% Eligable]
    
    if(length(selection) == 0 ) {
      
      Process[length(Process$V1)+1,] <- Process[i,]
      Process[i,1]  <- length(Process$V1)
      Process[i,2] <- sample(Eligable,1)
      Eligable <- setdiff(Eligable,  Process[i,2])
      targets <- MisLinks$target[ which((MisLinks$source == Process$Starts[i]) )]
      sources <- MisLinks$source[ which((MisLinks$target == Process$Starts[i]) )]
      sources <- unique(sources)
      targets <- unique(targets)
      Process$Holds[[i]] <- c(targets, sources,Process[i,2])
      Process$Group[[i]] <- c(Process[i,2] )
     
    }
    else{
      
      
      Process$Group[[i]] <- c(Process$Group[[i]], selection)
      
      Eligable <- setdiff(Eligable, selection)
      targets <- MisLinks$target[ which((MisLinks$source %in% Process$Group[[i]]) &  (MisLinks$target %in% Eligable) )]
      sources <- MisLinks$source[ which((MisLinks$target %in% Process$Group[[i]]) &  (MisLinks$source %in% Eligable) )]
      sources <- unique(sources)
      targets <- unique(targets)
      
      Process$Holds[[i]] <- c( Process$Holds[[i]] ,targets,sources)
    }
  }
}


#####Back Filter
Drops <- c()
Count <- 1
for(i in 1:length(Process$V1)){
  if(length(Process$Group[[i]]) == 1){
    Drops[Count] <-Process$V1[i]
    Count <- Count+1
    
  }
}
for( i in Drops){
  if(length(Process$Holds[[which(Process$V1 == i)]]) == 0){
    print(paste("ERR",i))
  targets <- MisLinks$target[ which((MisLinks$source == Process$Starts[[which(Process$V1 == i)]]))]
  sources <- MisLinks$source[ which((MisLinks$target == Process$Starts[[which(Process$V1 == i)]]) )]
  Process$Holds[[which(Process$V1 == i)]]<- c( Process$Holds[[which(Process$V1 == i)]] ,targets,sources)
  }
  LinkAdress <- which(((links$IDsource %in% Process$Holds[[which(Process$V1 == i)]]) |(links$IDtarget %in% Process$Holds[[which(Process$V1 == i)]]  )))
  MinDistance <- LinkAdress[which.min(1/links$value[LinkAdress])]
  option <- c(links$IDsource[MinDistance],links$IDtarget[MinDistance])
  selection <- option[which(option %in% Process$Holds[[which(Process$V1 == i)]])]
  
  for(j in 1:length(Process$V1)){
    
    
    if((selection %in% Process$Group[[j]])[1]){
      Process$Group[[j]] <- c(Process$Group[[j]],Process$Group[[which(Process$V1 == i)]])
      
    }
  }
  Process$V1[which(Process$V1 == i)] <- -1
  Process <- Process[-which(Process$V1 == -1),]
}



####Reduce
ResultsMatrix <- matrix(data = 0, nrow = length(MisNodes$name), ncol = length(MisNodes$name))
for(i in 1:length(Process$Group)){
  Process$Group[[i]] <- unique(Process$Group[[i]])
  for(j in unique(Process$Group[[i]])){
    for(k in unique(Process$Group[[i]])){
      ResultsMatrix[k+1,j+1] <- ResultsMatrix[k+1,j+1] +1
    if(ResultsMatrix[k+1,j+1] > 1){
      ResultsMatrix[k+1,j+1] <- 1
    }
    }
  }
}
print(max(ResultsMatrix))
Hold <- Hold + ResultsMatrix
print(max(Hold))



ResultsMatrix <- Hold
HOLD <- ResultsMatrix
Groupings <- data.frame(
  Group     = 1:1000,
  Members  = I(vector("list", 1000))
)
REM <- TRUE
Eligable <- 0:(length(MisNodes$name)-1)
Count <- 1
Row <- 1
Mean <- mean(HOLD[which(HOLD !=0)]) 
while(REM == TRUE){
 GROUPING <- which(ResultsMatrix[,Count] >Mean & ResultsMatrix[,Count]-1 %in% Eligable )-1
 if(length(GROUPING > 0)){
   Eligable <- setdiff(Eligable,GROUPING-1)
   Groupings$Group[Row] <- Row
   Groupings$Members[[Row]] <- GROUPING+1
   Row <- Row+1
   Count <- Count +1
   ResultsMatrix[(GROUPING+1),] <- 0
 }
  else{Count <- Count +1}
  if(sum(ResultsMatrix)==0){
    REM <- FALSE
  }
}
print(Eligable)

Groupings <- Groupings[1:(Row-1),]
for(i in 1:length(Groupings$Group)){

  MisNodes$group[Groupings$Members[[i]]] <- Groupings$Group[i]
}


forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", Nodesize = "size",  charge = -200, fontSize = 15,opacity = 0.6,arrows = TRUE, zoom = TRUE, opacityNoHover = 0.6)

