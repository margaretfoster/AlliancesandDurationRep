
## Note to self: data work on the Phillips late 80s data.

## Preliminaries:

rm(list=ls())

loadPkg=function(toLoad){
  for(lib in toLoad){
  if(! lib %in% installed.packages()[,1])
    { install.packages(lib, repos='http://cran.rstudio.com/') }
  suppressMessages( library(lib, character.only=TRUE) ) }}

packs=c("ggplot2", 'lmtest', 'car', 'sandwich', 'foreign', 'Hmisc', 'stargazer', 'xtable')
loadPkg(packs)

##Save function-- sick of copy-pasting.
## Takes a dataframe, name for the file, writes the .Rda and .CSV

savescript <- function(d, filename){
    save(d, file=paste0(filename,'.Rda'))
    print(".rda saved")
    write.csv(d, file=paste0(filename,'.csv'))
    return(print("Files saved"))
}

Path='~/Documents/Classes/Networks/'
setwd=Path

###Some SNA code I pulled from the internet and am using as a template:
### URL: http://econometricsense.blogspot.com/2012/04/introduction-to-social-network-analysis.html
 
dataPath2=('~/Documents/Classes/Networks/')
docPath2=paste0(dataPath2, 'phillips8789.csv')

fresh <- read.csv(docPath2, header=TRUE)
row.names(fresh) <- fresh$row.names
fresh<- fresh[,-1]
dim(fresh)

#Now to clean up:
which(is.na(fresh)==TRUE)

fresht <- as.matrix(fresh) #this is the data that i'm going to impute the missing data onto
dim(fresht)

inds <-  which(is.na(fresht)==TRUE, arr.ind=TRUE) #returns the row & column
inds #super helpful, but no names when I try for the rownames

indsdf <-  which(is.na(fresh)==TRUE, arr.ind=TRUE) #returns the row & column
indsdf


##Find the group names, to check up on the data. 
## Two ways: 1. sapply

nalist <- c(1, 165, 208, 247)
testfun <-sapply(nalist, function(list){
              for(i in list){l= c(colnames(fresh)[i])}
              return(l)
          })
testfun

##2. brute force
naGroupNames <- c( colnames(fresh)[1],
                  colnames(fresh)[165],
                  colnames(fresh)[208],
                  colnames(fresh)[247])
names(naGroupNames) <- c('column/row 1 is',
                         'column/row 165 is',
                         'column/row 208 is',
                         'column/row 247 is')
naGroupNames

##I'm imputing 0s into these links. See email correspondance with
## DK on 2/26 for the rationale.

fresht[is.na(fresht)] <- 0

inds <-  which(is.na(fresht)==TRUE, arr.ind=TRUE) #returns the row & column
inds #super helpful, but no names when I try for the rownames

#########
## March 17: Want to build an edgelist so that I can match group names and group IDs
######

class(fresht)
graphobj <- igraph::graph.adjacency(fresht,mode="undirected",weighted=NULL)
elist <- get.edgelist(graphobj, names=TRUE)

head(elist)
elist2 <- as.data.frame(elist)
elist2 <- cbind(elist2, 1)
colnames(elist2) <- c('group1', 'group2', 'alliancetie')
head(elist2)

load("/Users/Huginn/Documents/Classes/MLE/Lab/bpnA6.Rda")
head(bpn)
stuff <- bpn
head(stuff)

justnames <- stuff[1:2]
head(justnames)
justnames <- justnames[!duplicated(justnames),]
attributes <-c("groupid", "name", "size", "religious", "ethnic", "drugs", "statesponsored", "namerica", "latamerica", "ssafrica", "meast", "asia", "europe", "ccode")
attributes <- stuff[attributes]
attributes <- attributes[!duplicated(attributes),]
## There are too many results in the attributes (should have same dimensions
## as justnames, but dim(attributes)-dim(justnames) tells me that attributes has

list <- which(duplicated(attributes$name))

nodeID2<- function(data, listofInterest){
    l <- NULL
     for(i in listofInterest){
         l <- c(l, (data$group2)[i])
         print(i)
     }
       return(l)
           }

list <- which(is.na(mydataFull$g1groupid))
namesToCheck <- nodeID2(mydataFull, list)
name <- as.data.frame(unique(namesToCheck))


list2 <- which(is.na(mydataFull$g2groupid))

names2 <- nodeID2(mydataFull, list2)

## Exported the list of names
write.csv(name, file='accountIDstoCheck.csv')
## importing the names hand-matched with IDs

namePath <- paste0(dataPath2, 'accountIDstoCheck.csv')
names <- read.csv(namePath)
names <- names[2:3]
colnames(names) <- c('group1', 'g1groupid')


##Names to check provides the list 184 entries in which the group ID didn't get picked up by matching
## group names. looking for the unique lists returns 78 entries.
         
## the node list wasn't that useful, actually, but here is a bigger attribute
## list with dimensions that match the justnames
attr2 <-attributes[!duplicated(attributes$name),]

elist2$group1 <- gsub("\\."," ",elist2$group1)
elist2$group2 <- gsub("\\."," ",elist2$group2)


## This is what I want to add the attributes for the group1. NA is when group1 doesn't have the same
##exact name as "name" in phillips' full data.
mydata <- merge(elist2, attr2, by.x=c("group1"), by.y=c("name"), all.x=TRUE)
colnames(mydata) <- c('group1', 'group2', 'alliancetie', 'g1groupid' , 'g1size', 'g1religious', 'g1ethnic', 'g1drugs', 'g1statesponsored' ,'g1namerica', 'g1latamerica', 'g1ssafrica', 'g1meast','g1asia', 'g1europe', 'g1ccode')

mydataFull <- merge(mydata, attr2, by.x=c('group2'), by.y=c("name"), all.x=TRUE)
colnames(mydataFull) <- c('group1', 'group2', 'alliancetie', 'g1groupid' , 'g1size', 'g1religious', 'g1ethnic', 'g1drugs', 'g1statesponsored' ,'g1namerica', 'g1latamerica', 'g1ssafrica', 'g1meast','g1asia', 'g1europe', 'g1ccode', 'g2groupid' , 'g2size', 'g2religious', 'g2ethnic', 'g2drugs', 'g2statesponsored' ,'g2namerica', 'g2latamerica', 'g2ssafrica', 'g2meast','g2asia', 'g2europe', 'g2ccode')

#for (i in nrow(mydataFull)) {
#for (j in nrow(names)) {
#    if (mydataFull$group1[j] == names$group1[i]){
#        mydataFull$g1groupid[i] <- names$g1groupid[j]
 #   break
#}}

### still need to update, but:
write.csv(mydataFull, file='80stiesattributes.csv')

##Save:
save(fresht, file='Phil80sNAsImputed.Rda')
write.csv(fresht, file='Phil80sNAsImputed.csv')



library(igraph)

T <- igraph::graph.adjacency(fresht, mode=c('undirected'))
class(T)

bet=betweenness(T) #returns a vector of numbers.
eig=evcent(T) #this is a list
print(eig[[1]])

cent<-data.frame(bet=betweenness(T), eig=evcent(T)) # Calculate betweeness & eigenvector centrality 
res<-as.vector(lm(eig~bet,data=cent)$residuals)           # calculate residuals
cent<-transform(cent,res=res)                             # add to centrality data set
write.csv(cent,"phil80s_keyactorcentrality.csv")                # save in project folder

plot(T, layout = layout.fruchterman.reingold)  



T2 <- igraph::graph.adjacency(fresht, mode=c("undirected"), weighted=NULL, diag=FALSE,
                              add.colnames=NULL, add.rownames=NA)
TA<-V(T2)[degree(T2)<1] #identify those vertices with less than 1 edge (ie, isolates)
TA<-delete.vertices(T2, TA) #exclude them from the graph
#Note could use the above to only graph more highly connected groups, ofc

plot(TA,
     main='Late 1980s Terror Group Connections',
     vertex.size=4, 
     vertex.color="blue",
     vertex.label.font=2,
     vertex.label.dist=0.25,
     vertex.label.cex=.35)

png(filename="Rough80sNetwork.png", height=800, width=600)

dev.off()

###

##########################
##########################
## Late 1990s-early 2000s Data
##########################
##########################


docPath3=paste0(dataPath2, 'phillips9805.csv')

datatest <- read.csv(docPath3, header=TRUE)
datatest[,1] <- as.character(datatest[,1])
datatest <- datatest[,-1]

##This is the function version of the process of importing data from a csv.
##I'm sick of copy-pasting.
process <- function(dp){
    dt <- NULL
    dt <- read.csv(dp, header=TRUE)
    row.names(dt) <- dt$row.names
    dt <- dt[,-1]
    return(dt)
}

## Note for the future:
## Datatest is the frame that has groupnames along the rows too
row.names(datatest) <- datatest[,1]
dt <- dt[,-1]

data90s <- process(docPath3)
dim(data90s) #should be the same as for the hand-imported.
head(data90s)

## Some additional dataprocessing:

naid <-  which(is.na(data90s)==TRUE, arr.ind=TRUE)
naid #looks like no NAs in this data, which is pretty sweet.

## The following script will identify nodes from a list
##(such as a list of missing)

nodeIDs <- function(data, listofInterest){
    for(i in length(listofInterest)){
        l <- c(colnames(data)[i])
        print(i)
    }
      return(l)
          }

savescript(data90s, "90sdata")
savescript(datatest, "90sDataWithNames")

####Network work with the data files
## Key for this data: 1= allies, 2= violent rivals, 3=both
####

## Trying to make a list of names with their numbers
## so I can match the groups together:
names <-as.data.frame(cbind(rownames(datatest), num=1:429))
which(rownames(names)!=names$num)
## I hope that the above indicates that all of the row names are the same
## as the numbers.

##Lovers and fighters:
## (This is kind of a cool group, but maybe not ultimately necessary since we're
## looking to compare from the 80s)

threes <-  which(data90s==3, arr.ind=TRUE)
threes

## Dataframes for rivals+alies data frame
rivalAndAlliesdf <- as.data.frame(cbind(threes, 1))
colnames(rivalAndAlliesdf) <- c('ego', 'alter', 'rivalAllytie')

## Also for the version with names:

threenames <-  which(datatest==3, arr.ind=TRUE)
threenames

##We should prob figure out which of these groups have no name.
##That's kind of sketchy. Update: figured out that groups whose names
## are numbers were not reporting. Changed by converting from factor
## to character

##Now for alliances:

allies <- which(data90s==1, arr.ind=TRUE)
alliesdf <- as.data.frame(cbind(allies, 1))
colname(alliesdf) <- c('group1', 'group2', 'alliance')
print(alliesdf)

## Rivalries

rivals <- which(data90s==2, arr.ind=TRUE)
rivalsdf <- as.data.frame(cbind(rivals, 1))
colnames(rivalsdf) <- c('ego', 'alter', 'rivalry')
print(rivalsdf)

## Create a dataframe with both rownames (shortID) and group names

#Allies
namesdf <- data.frame(v1=alliesdf$row, v2=alliesdf$col,
                     v3=names[match(alliesdf$row, names$shortID),1],
                     v4=names[match(alliesdf$col, names$shortID),1], v5=1)
colnames(namesdf) <- c('group1', 'group2', 'g1name','g2name', 'tie')

#Rivals

namespt2 <- data.frame(data.frame(v1=rivalsdf$ego, v2=rivalsdf$alter,
                     v3=names[match(rivalsdf$ego, names$shortID),1],
                     v4=names[match(rivalsdf$alter, names$shortID),1], v5=2))
colnames(namespt2) <- c('group1', 'group2', 'g1name','g2name', 'tie')

                                        #both
namespt3 <- data.frame(v1=rivalAndAlliesdf$ego, v2=rivalAndAlliesdf$alter,
                     v3=names[match(rivalAndAlliesdf$ego, names$shortID),1],
                     v4=names[match(rivalAndAlliesdf$alter, names$shortID),1], v5=3)
colnames(namespt3) <- c('group1', 'group2', 'g1name','g2name', 'tie')

## Put together-- creates an edgelist in probably the least efficient way possible.
edgelist <- rbind(namesdf, namespt2, namespt3)

savescript(edgelist, "90sedges")

## the payoff for having done that messy edgelist is a hope to merge more data:
load("/Users/Huginn/Documents/Classes/MLE/Lab/bpnA6.Rda")
stuff <- bpn
smallstuff <- stuff[unique(stuff$groupid),] #this is failing to only return a list of groups
# and their attributes- I'm out of energy for it. Prob a dif. dataset would be better anyway

###Graphs:

R1 <- igraph::graph.data.frame(rivalsdf)
class(R1)

## going to have it print out a list of network attributes- igd= igraph data function
    
bet=betweenness(idg) #returns a vector of numbers.
eig=evcent(idg) #this is a list

#cent<-data.frame(bet=betweenness(idg), eig=evcent(idg)) # Calculate betweeness & eigenvector centrality 
#res<-as.vector(lm(eig~bet,data=cent)$residuals)           # calculate residuals
#cent<-transform(cent,res=res)                             # add to centrality data set

library(igraph)

plot(R1, layout = fruchterman.reingold, main = "Rivalries",
     V(R1)$label=names$group.name)

png(filename="90sNetworkRivals.png", height=800, width=600)
dev.off()


A1 <- igraph::graph.data.frame(alliesdf)
A1 <- as.undirected(A1)


plot(A1)

plot(A1,
     main="Late 1990s-early2000s Terror Alliances",
     vertex.size=3
     vertex.color="gray",
     vertex.label.font=3)
png(filename="test.png", height=800, width=600)
dev.off()



plot(R1,main="Late 1990s-early2000s Terror Rivalries", vertex.size=3, vertex.color="gray", vertex.label.font=3)

plot(AR1)


### Descriptive stats for the networks:
##
## Transitivity in 1990s/ 2000s (entire) graph: alliances, rivals

trans90sAllies <- transitivity(A1, type=c("undirected"), vids=NULL, weights=NULL, isolates=c("NaN"))

trans90sRivals <- transitivity(R1, type=c("undirected"), vids=NULL, weights=NULL, isolates=c("NaN"))

## Centrality

## Betweenness

## Modularity: How separated are the different vertex types from each other. 
## running the walktrap community with the 4 default as well as seeing how robust to other walks
wtc90sAllies <- walktrap.community(A1, steps=5)
wtc90sAllies
modAllies <- modularity(wtc90sAllies)
modAllies2 <- modularity(A1, membership(wtc90sAllies))
plot(wtc90sAllies, A1,  main="Late 1990s-early2000s Terror Alliances (communities 5 walks)", vertex.size=10, vertex.label.cex=.75)

plot(A1, vertex.color=membership(wtc90sAllies),
     main="Late 1990s-early2000s Terror Alliances (communities)",
     vertex.size=6,  vertex.label.dist=0.35)

## Communities:

## Test significance of communities

## Walktrap seems to most promising:

community.significance.test <- function(graph, vs, ...) {
    if (is.directed(graph)) stop("This method requires an undirected graph")
    subgraph <- induced.subgraph(graph, vs)
    in.degrees <- degree(subgraph)
    out.degrees <- degree(graph, vs) - in.degrees
    wilcox.test(in.degrees, out.degrees, ...)
}


edgelist
