library(readr)
library(networkD3)
DonationRecord <- read_csv("Test/2025.csv")
DonationRecord$Amount <- parse_number(DonationRecord$Amount)
DonationRecord$ContributorID <- NA
DonationRecord$RecipientID <- NA

DonationRecord <-DonationRecord[which(DonationRecord$Amount >5000),]

User_C <- unique(DonationRecord$`Contributor Name`)
User_R <- unique(DonationRecord$`Recipient Name`)

User <- unique(c(User_C,User_R))

UserLookup <- data.frame(UserID = c(1:length(User)), UserName = User, size = 0)

for(i in 1:length(DonationRecord$Amount)){
  DonationRecord$ContributorID[i] <- UserLookup$UserID[which(UserLookup$UserName == DonationRecord$`Contributor Name`[i])]
  DonationRecord$RecipientID[i] <- UserLookup$UserID[which(UserLookup$UserName == DonationRecord$`Recipient Name`[i])]
}

IncidenceMatrix <- matrix(0, nrow = length(UserLookup$UserID), ncol = length(UserLookup$UserID))

for(i in 1:length(DonationRecord$ContributorID)){
  print(i)
  IncidenceMatrix[DonationRecord$ContributorID[i],DonationRecord$RecipientID[i]]  <- IncidenceMatrix[DonationRecord$ContributorID[i],DonationRecord$RecipientID[i]] + DonationRecord$Amount[i]
}
for( i in 1:length(UserLookup$UserID)){
  UserLookup$size[i] <- sum( DonationRecord$Amount[ which(DonationRecord$ContributorID == UserLookup$UserID[i] | DonationRecord$RecipientID == UserLookup$UserID[i])])
}


rownames(IncidenceMatrix) <- UserLookup$UserName
colnames(IncidenceMatrix) <- UserLookup$UserName

links <- IncidenceMatrix %>% 
  as.data.frame() %>% 
  rownames_to_column(var="source") %>% 
  gather(key="target", value="value", -1) %>%
  filter(value != 0)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)


# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1



#simpleNetwork(links[,c(1:2)], zoom = TRUE)

MisNodes <- data.frame(name = UserLookup$UserName, group = 1,size = log10(UserLookup$size))
MisLinks  <- data.frame(source = links$IDsource, target = links$IDtarget,value = log10(links$value))
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", Nodesize = "size", opacity = 0.8,arrows = TRUE, zoom = TRUE)
