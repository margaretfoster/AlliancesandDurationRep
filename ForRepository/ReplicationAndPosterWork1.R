## notes for visualization:
## cross-val
## plot with and without robust standard errors: see if that has an effect on the signifiance
###########
##Preliminaries: setting up the workspace
##########

rm(list=ls()) 

loadPkg=function(toLoad){
  for(lib in toLoad){
  if(! lib %in% installed.packages()[,1])
    { install.packages(lib, repos='http://cran.rstudio.com/') }
  suppressMessages( library(lib, character.only=TRUE) ) }}

packs=c("ggplot2", 'lmtest', 'car', 'sandwich',
    'foreign', 'Hmisc', 'stargazer', 'xtable',
    'rms', 'igraph', 'foreign', 'stringdist')
loadPkg(packs)

## Savescript takes a dataframe and a filename and saves the .rda and .csv
## throughout the script, savescript calls are commented out, to avoid accidentally overwriting saved data

savescript <- function(d, filename){
    save(d, file=paste0(filename,'.Rda'))
    print(".rda saved")
    write.csv(d, file=paste0(filename,'.csv'))
    return(print("Files saved"))
}

## Process takes a link to a .csv file in which rownames are identified as the first "column"
## it imports the .csv into R, corrects the rownames, and returns a dataframe.

process <- function(dp){
    dt <- NULL
    dt <- read.csv(dp, header=TRUE)
    row.names(dt) <- dt[,1]
    dt <- dt[,-1]
    return(dt)
}

## Starting path:
Path='~/Documents/Classes/MLE/Replication/'

## Data for regression models

bpn= read.dta('/Users/Huginn/Documents/Classes/MLE/Replication/PhillipsForStata10.dta', 
              convert.dates = TRUE, 
              convert.factors = TRUE, 
              missing.type = FALSE,
              convert.underscore = FALSE,
    warn.missing.labels = TRUE)

#savescript(bpn, 'PhlRepPlay')

#################
## Part 1: Identifying Missing Data
## At the moment, this is primarily for informational purposes: the replication uses a
## subset dataframe only the estimated idvs.
##The "only" missing data that prompts listwise deletion are omitted  polity2 values.
################

## Step 1: what is missing
missingvals <- which(is.na(bpn)==TRUE, arr.ind=TRUE)
missingvals # row and column. For group name, will need either the groupID or group name for the info to be useful


missingrows <- as.numeric(missingvals[,1])
missingcolumns <- as.numeric(missingvals[,2])

## namesofMissingGroups takes a vector identifying the rows that have missing data
##  and returns a list of the group names that correspond to the row with missing data.

groupsWMissData<-sapply(missingrows, function(list){
              for(i in list){l= c(bpn$name[i])}
              return(l)
          })

print(unique(groupsWMissData))

##takes a list of the columns that have missing data and identifies which idv the column corresponds to
missingObsIvs <-sapply(missingcolumns, function(list){
              for(i in list){l= c(colnames(bpn)[i])}
              return(l)
          })

missingObsIvs
table(missingObsIvs)

###Only variables that I care about for the first two models ares: wbgi, polity2, cinc.
                                      
## NamesOfMissingCountries takes a list of the missing rows and:
##identifies the country that corresponds to the missing row.
## This is useful to do because missing data in the idvs relate to state variables.
## note that Phillips does not say what ccode comprises,
##but Asal and Rethmeyer have a ccode variable that is from the polity and COW datasets

namesOfMissingCountries <-sapply(missingrows, function(list){
              for(i in list){l= c(bpn$ccode[i])}
              return(l)
          })

table(namesOfMissingCountries) ## 12 countries are driving the missing data.

missCountries <- unique(namesOfMissingCountries)
length(missCountries)

names <- c("115=Suriname","265=German Democratic Republic",
           "255=Germany", "260=German Federal Republic",
           "343=Macedonia", "645=Iraq",
           "660=Lebanon", "679=Yemen",
           "700=Afghanistan", "771=Bangladesh",
           "775=Myanmar", "811=Cambodia") #this is for my notes

## also need years that the data is missing for:

##script below takes the list of the missing rows and returns an entry with
## countrycode and year. Should have been a 2x700ish matrix, but was actually
## a 700ish x2 matrix. thus, when moving into data-frame to match up, transposed it

missDates <-sapply(missingrows, function(list){
    for(i in list){l= rbind(cbind(bpn$ccode[i],bpn$year[i]))}
              return(l)
})

## missingData$factor ==115 <- 'Suriname',
## missingData$factor==265 <- "German Democratic Republic",
## missingData$factor== 255 <- "Germany",
## missingData$factor==260 <- "German Federal Republic",
## missingData$factor==343 <- "Macedonia",
## missingData$factor==645 <- "Iraq",
## missingData$factor== 660 <- "Lebanon",
## missingData$factor==679 <- "Yemen",
## missingData$factor==700 <- "Afghanistan",
## missingData$factor==771 <- "Bangladesh",
## missingData$factor==775 <- "Myanmar",
## missingData$factor==811 <- "Cambodia"
           
missingData <- as.data.frame(t(missDates))
colnames(missingData) <- c("CountryCode", "YearMissing")
summary(missingData$YearMissing)
summary(missingData$CountryCode)
table(missingData$CountryCode)
missingData$factor <- as.factor(missingData$CountryCode)
missingData$CountryCode <- as.numeric(missingData$CountryCode)


##What are the patterns of the missing years:
hist(missingData$YearMissing, xlab="Year of Missing Data", xlim=c(1987, 2005))

p <-ggplot(missingData,aes(x=YearMissing, fill=factor))
p <- p+ geom_histogram(colour= )+ facet_wrap(~factor)+
    theme_bw()
p

## Check whether the data exists in the Polity 4 DB:
polity4 <- read.csv(paste0(Path,"Polity4Data.csv"))

## Function to merge when there is data:
## extracts countrycode and year from each row of missingData
##looks for an entry in polity4 that matchs the two fields, inputs the results

correction <- function(data1, data2){
    results <- NULL
    intermediate <- NULL
    for(i in 1:nrow(missingData)){
       code <- missingData$CountryCode[i]
       date <- missingData$YearMissing[i]
       k <- which(polity4$ccode==code & polity4$year==date)
       intermediate <- cbind(polity4$ccode[k], polity4$year[k], polity4$polity2[k])
       results <- rbind(results, intermediate)
    }
    colnames(results) <- c("ccode","year", "polity2")
    return(results)
}

rest <- as.data.frame(correction(missingData, polity4))
dim(rest)
head(rest)
stillmissing <- which(is.na(rest$polity2))
length(stillmissing)

##now continue to build the correction by: 1) add in the interactions

library(countrycode)
codes.of.origin <- countrycode::missCountries # Vector of values to be converted
countrycode(missCountries, "cowc", "country.name")

## Checking which groups tend to have missing values for polity (just out of curiosity)
## but, of course, it is really the countries that I need

polityMissing <- as.vector(which(is.na(bpn[,13])))
polityMissing <- names(polityMissing)
capacityMissing <- which(missingvals[,2]==12)
## Note for the above: I'm 99% certain that the regression that
##Phillips ran has data omitted for rows in which one or more IDV was missing
##not for a full na.rm...
##

## dataframe of just the missing polity data:
new_DF <- subset(bpn, is.na(bpn$polity2))
plot(density(new_DF$allies))
plot(density(bpn$allies))

capacMiss <- subset(bpn, is.na(bpn$wbgi_gee_cs))

namesMissingPolity <-sapply(polityMissing, function(list){
    l <- NULL
    for(i in list){
        l= c(bpn$name[i])}
              return(l)
})

countriesMissingCapac <- function(list){
    for(i in 1:length(list)){
        output <- rbind(bpn[i,])
    }
    return(output)
    }

Polity2 missing countries:
table(new_DF$ccode)
lebanon<- subset(polity4, ccode==660) ## Lebanon. Dates looking for:1990-2005 (115 obs)
suriname <- subset(polity4, ccode==115) ##suriname (7 obs)
iraq<- subset(polity4, ccode==645) ##Iraq (53 obs)
afghanistan<- subset(polity4, ccode==700) ## Afghanistan (44 obs)
cambodia<- subset(polity4, ccode==811) ## Cambodia. (2 obs)
                                        #
##Table of the polity2 missing countries:
##115 645 660 700 811 
## 7  53 115  44   2

######################################
######################################
#### Models
######################################
######################################

## Replicating model: naive first step replicating just the models printed in the paper
##Naive because I'm not imputing missing data, extending the model, or
# including robust standard errors 

#Naive Model 2, allies

##Given the conservative listwise delete, might be best to create an object that is only the ivs
## needed for each run. Then, delete:

##The following processes the full bpn dataframe into only the variables used for the study as
## it was published in ISQ. What I think happened here is that Phillips added the variables named in
## the drops list after he ran the model printed- they were then used in the appendix and alternative
## specifications.  Removing these and then listwise deleting the rows with missing data
## returns the same number of observations as reported in the paper.

drops <- c("rgdpch","fhouse17", "alliesXrgdpch", "alliesXfhouse17", "alliesXcinc", "cinc")

bpnMod1 <- bpn[,!(names(bpn) %in% drops)]
bpnMod1 <- na.omit(bpnMod1)
dim(bpnMod1)
#Answer:  [1] 3922   29. Jackpot!

## Comparing density of number of allies in the listwise deletion of missing polity data
## graph plots density curves for the entries without a polity score and remaining elements

listwiseDel <- data.frame(NumAllies=bpnMod1$allies)
theRemoved <- data.frame(NumAllies=new_DF$allies)

##m graphs the densities for the group-years with missing and non-missing polity data
## Motivation here is to show that removing the groups in countries with missing polity2 data is consequential

compMissingDat <- ggplot() + geom_density(aes(x=NumAllies),
                                          colour="#0072B2", data=listwiseDel) +
geom_density(aes(x=NumAllies),
             colour="#D55E00", data=theRemoved) +
theme_bw()+
    ggtitle("Density of Variable Allies
With Missing (orange) and Non-Missing (blue) Polity2 Values")

compMissingDat

## More direct comparison of how the listwise deletion changes the data-
##note that the removed section has a longer tail
table(listwiseDel)
table(theRemoved)

###########
### Now that the data is mostly squared away: replication
## Step 1: Specify the elements for the paper model
###########

dv='failure'
ivs=c('allies', 'size', 'religious',
    'ethnic', 'drugs', 'statesponsored',
    'wbgi_gee_cs', 'polity2', 'logpop',
    'namerica', 'latamerica', 'ssafrica',
    'meast', 'asia','spline1','spline2','spline3')
modElements=formula(paste0( dv, '~ ', paste(ivs, collapse=' + ') ))

#this will be used in presentation:
ivsPretty <- c('Number of Allies', 'Size Class', 'Religious Motivation',
               'Ethnic Motivation', 'Drug Funding', 'State Sponsor',
               'State Capacity', 'State Polity2', 'Log State Population',
               'N.America', 'L.America', 'SS.Africa', 'M.East',
               'Asia','spline1','spline2','spline3')


#######################
##CLUSTERED STANDARD ERRORS:
## Code from Dania Chiba
######################
### Function to calculate clustered s.e. for logit model
## function takes a fitted glm object (ie, model), name of the variable for clustering
## returns: vector with named elements
## requires no NA values, if finds them will throw an incompatibility error

logit.cluster <- function(fit){        
  ## Note: this is hardwired for the clustering to be groupid
  F <- function(x){1/(1+exp(-x))}       # logit CDF
  f <- function(x){exp(x)/(1+exp(x))^2} # logit PDF
  fml <- fit$formula
  data <- fit$data
  beta <- fit$coef
  vcov <- vcov(fit)
  k <- length(beta)
  #print(paste0('k is ', k))      
  y <- fit$y
  m <- dim(table(data$groupid))    # N of clusters
  xvars <- names(beta)            # Name of covariates ordered accordingly
  xvars <- xvars[2:length(xvars)] # Delete (Intercept)
  xmat <- as.matrix(data[,xvars]) # Design matrix
  xmat <- cbind(1, xmat)          # Add intercept
  xb <- xmat %*% beta             # linear predictor (xb)
  ## Now, obtain clustered s.e.
  u <- ((y==1) * f(xb)/F(xb) + (y==0) * -f(xb)/(1-F(xb)))[,1] * xmat
  u.clust <- matrix(NA, nrow=m, ncol=k)
  fc <- factor(data$groupid)
  for (i in 1:k){ ## loop over covariates
    u.clust[,i] <- tapply(u[,i], fc, sum) ## sum over dyad
  }
  cl.vcov <- vcov %*% ((m/(m-1)) * t(u.clust)%*%(u.clust)) %*% vcov ## sandwich
  cl.se <- sqrt(diag(cl.vcov)) ## clustered s.e.
  return(list(cl.se, cl.vcov))
}

## Presentation:
## short function that formats the table of result
## takes a fitted model value and a named vector of errors

presentation <- function(model, errors){
betas <- model$coefficients
rnd <- 3
z <- betas/errors
pval <- 2*(1-pnorm(abs(betas/errors)))
cl.tbl <- cbind(betas = round(betas, rnd),
                se = round(errors,rnd),
                z = round(z,2),
             pval = round(pval,rnd))
colnames(cl.tbl) <- c("coef", "s.e.", "z", "P>|z|")
print("Results w/ Robust s.e. clustered by groupid")
print(cl.tbl)
}

##############
###Coefficient Plots
###############

coefFormat <- function(fitted, stderr){
stdErr <- stderr
upper95 <-fitted$coefficients + qt(.975, df=fitted$df.residual)*stdErr
lower95 <- fitted$coefficients - qt(.975, df=fitted$df.residual)*stdErr

ggData <- as.data.frame(cbind(fitted$coefficients, stdErr, upper95, lower95))
colnames(ggData) <- c("coef", "stdErr", "Upper95", "Lower95")
#rownames(ggData) <- c(ivs)
ggData$var <- rownames(ggData)
ggData$prettyvar <- c("Intercept", ivsPretty)
return(ggData)
}

#CoefTables for the baseline model:

coefFormat(paperTables, cl.se.pt)


####################
##### k-fold Cross-validation:
####################

## repXValidate runs kfold xvalidation on a logit model with group-year data.
##Takes s data, number of folds, model specification
## Returns a list
## Modifications beyond standard kfold:

## 1. repXValidate creates a dataframe with a list of group IDs that are used to select in and out-sample
## the motivation for this is to keep the groups-years for a group together for the kfold
## 2. repXValidate calls the logit.cluster() function to generate clustered robust standard errors.
## Note that logit.cluster() bakes groupid in as the clustering attribute

repXValidate<- function(data, folds, modElements, threshold){
    set.seed(6886)
    randdf <- as.data.frame(unique(data$groupid))
    colnames(randdf) <- c('groupid')
    randdf$sample <- sample(folds, nrow(randdf), replace=T)
    testData <- na.omit(merge(data, randdf, by=c("groupid")))
    coefsCrossVal=NULL
    performance=NULL
    predictions <- NULL
    outList <- NULL
    logLike <- NULL
    ABIC <- NULL
    predProbsout <- NULL
    truesall <- NULL

    ##subset into train and test
    for(ii in 1:folds){
        train=testData[testData$sample != ii,]
        test=testData[testData$sample==ii,]
         ## Run model     
        modsumxval <- glm(modElements, family=binomial(link=logit), data=train)
        print(modsumxval)

        stdErr <- logit.cluster(modsumxval)[[1]]
        upper95 <-modsumxval$coefficients + qt(.975, df=modsumxval$df.residual)*stdErr
        lower95 <- modsumxval$coefficients - qt(.975, df=modsumxval$df.residual)*stdErr
        tstats <- modsumxval$coefficients/stdErr
        xvcov <- logit.cluster(modsumxval)[[2]] 

        ##bind together:
        ##trainResults is a matrix that takes:
        ##a numeric for the coefficients, standard error, upper95, lower95, tstats, and cut
        trainResults=cbind(modsumxval$'coefficients', stdErr, upper95, lower95,tstats, ii)
        names(trainResults) <-  c('coefficients', 'StdError',
                                  'upper95CI', 'lower95CI', 'Tstatistics', 'cut')
       coefsCrossVal=rbind(coefsCrossVal, trainResults)
    
        testIDVs <-data.matrix(cbind(1, test$failure))
        trainBetas <- modsumxval$'coefficients'
        print(c(dim(trainBetas), class(trainBetas)))
        
        ## Performance: AIC and BIC 
        ## first step: data matrix for performance.
        ##Takes a vector of data, with one standing in for the intercept. 
        ## Data input should be test data.
        X = data.matrix( cbind(1, test[, names(trainBetas)[2:length(trainBetas)] ] ) )
       predProbs = 1/( 1 + exp(-X %*% trainBetas)) 
        ##redefine dv here, because need to switch to the test dataset
        dvtest <- test$failure
        predProbscut <- cbind(dvtest, predProbs, ii)
        names(predProbscut) <- c('dvtest', 'predprob', 'cut')
       predProbsout <- rbind(predProbsout, predProbscut) #observe that this is on the test data
       # Convert to log-Liklihood. Takes a 
        logLike <- sum(dvtest * log(predProbs) + (1 - dvtest)*log(1-predProbs))

       # Using this we can calculate the AIC and BIC
       # AIC: -2 * log-likelihood + 2 * npar
        AIC <- -2 * logLike + 2 * length(modsumxval$coefficients) # AIC(m2)
       
        ## BIC Equation: -2 * log-likelihood + log(nrow(data)) * nparameters
        BIC <-  -2 * logLike + log(nrow(test)) * length(modsumxval$coefficients)
                                        # AIC(m2, k=log(nrow(data)))
        
        perfresults <- cbind(AIC, BIC, ii)
        ## to return:
        ABIC=rbind(ABIC, perfresults) # The smaller the AIC and BIC the better the fit

        ## Classify predictions as zero or one based on threshold defined at outset.
        ## takes the predicted probability and the threshold value.
        predRoc = as.numeric(predProbs > threshold)

      ## Calculate False positive rate and True positive rate
      ## FPR: probability to be predicted positive, given that someone is negative
      ## TPR: probability to be predicted positive, given that someone is positive
       FPR = sum( (predRoc==1)*(dvtest==0) ) / sum(dvtest==0)
       TPR = sum( (predRoc==1)*(dvtest==1) ) / sum(dvtest==1)

        ## the aggregation of false positive and true positive rates as a matrix
        ## for use later
        trues <-  cbind(FPR, TPR, ii)
        names(trues) <- c('FPR', 'TPR', 'cut')
        truesall <- rbind(truesall, trues)
    }

    #Send salient components back out:
    outList <- list(coefsCrossVal, predProbsout , ABIC, truesall)
    names(outList) <- c('coefsCrossVal', 'predProbsout',
                        'performanceMeasures', 'FTPosRate' )
    return(outList)
    }

   
##############
### Separation Plots:
###############

### This block of code has four components:
### 1. sepPlotFun: builds separation plots for the cross-validation
##takes a dataframe and filename and saves a png file
## 2. graphs(): a simple loop to iterate through the number of cuts in the Xval
## 3.  A line of code that calls graphs with output from the xval
## 4. A couple of lines that converts a predProbsOut object into a df used for the separation plot
## 5. The predProbsOut is for the out-sample data.

sepPlotFun <- function(data, filename){  
    sepPlot <- ggplot(data=data, aes(ymin=0,
                          ymax=1, xmin=0, xmax=1:length(dv))) +theme_bw() 
    sepPlot = sepPlot + geom_rect(fill='white')+theme_bw() 
                                        #    ggtitle("Out of Sample Separation Plot") + 
    sepPlot = sepPlot + geom_linerange(aes(color=factor(dv), x=1:length(dv)), alpha=.75)
                                        # Color event lines
    sepPlot = sepPlot + scale_color_manual(values = c('skyblue', 'navyblue'))
    sepPlot = sepPlot + scale_y_continuous("", breaks = c(0, 0.25, 0.5, 0.75, 1.0), expand=c(.125, 0))
    sepPlot = sepPlot + scale_x_continuous("", breaks = NULL)
    sepPlot = sepPlot + theme(
	axis.ticks = element_blank(), text=element_text(family='serif'),
	legend.position = "none", 
	panel.background = element_blank(), 
        panel.grid = element_blank() )
    sepPlot=sepPlot +  theme(legend.position="none")
    sepPlot= sepPlot+ geom_line(aes(y=prob, x=1:length(dv)), lwd=.4, colour="white")
    print(sepPlot)
    ggsave(file=paste0('separationplot',filename,'.pdf'), width=8, height=3)
    dev.off()
}


graphs <- function(data){
for (i in 1:10){
    name <- paste0("sepDat",i)
    currData <- sepData[which(sepData$cut==i),]
    currData <-  currData[order(currData$prob),]
    sepPlotFun(currData, name)
    }}

graphsUniModel <- function(data){
    name <- paste0("FullModel",1)
    currData <- sepData
    currData <-  currData[order(currData$prob),]
    sepPlotFun(currData, name)
}


###############
##Models and Presentation Thereof
###############

## Replicate "Terrorist Group Cooperation and Longevity" Model 1:
paperTables <- glm(modElements, family=binomial(link=logit), data=bpnMod1)
round(summary(paperTables)$'coefficients',3)

##Corrected Standard Error and vcov
cl.se.pt <- logit.cluster(paperTables)[[1]]
vcov.pt <- logit.cluster(paperTables)[[2]]

##Table of results
paperMod <- presentation(paperTables, cl.se.pt)

##10-fold cross-validation of the model: 
uruz <- repXValidate(bpnMod1, 10, modElements, .8)
##note that threshold isn't something that I'm going to use

#############
###Presentation of Replicated ('paperTables') model
## Functions for creating the objects below.
#############

##1. coefficient plots
ggpT <- coefFormat(paperTables, cl.se.pt )

#removing splines
ggdata <- ggpT[2:15,]

##graphing the coefficient plots for the paperTables model.
## This has become obsolete, now that I match the coefplots of the full and limited models

tmp=ggplot(data=ggdata , aes(x=prettyvar, y=coef))
tmp <- tmp + geom_point(cex=2, color="darkgray")
tmp <- tmp + geom_linerange(aes(ymin=Lower95,
                                ymax=Upper95), lwd=1, size=.3, alpha=.55, color='springgreen4') 
tmp=tmp + geom_hline(xintercept=0, linetype=2, color = "red")
tmp <- tmp+ theme(strip.text.x=element_text(angle=-90))
tmp=tmp + xlab('Variables') + ylab('Estimated Coefficient')
tmp=tmp + ggtitle("Coefficient and 95% CI Range \n Allies Model")
tmp=tmp+theme_bw() + coord_flip()
tmp
dev.copy(pdf, "paperTableCoefPlot.pdf")
dev.off()

### 3. Predicted Probs:
## takes a fitted model and data

PredProbs <- function(fitted, data){
    mod <- fitted
    data <- data
    Betas <- mod$'coefficients'
    X = data.matrix( cbind(1, data[, names(Betas)[2:length(Betas)] ] ) )  
    dvtest <- data$failure
    predProbs = 1/( 1 + exp(-X %*% Betas) )
    logLike <- sum(dvtest * log(predProbs) + (1 - dvtest)*log(1-predProbs))
    predProbs <- cbind(dvtest, predProbs, logLike)
    colnames(predProbs) <- c('dvtest', 'predprob', 'logLike')
    return(predProbs)
}

## Probability function for paper Tables model
ptProbts <- PredProbs(paperTables, bpnMod1)

ptProbts <- cbind(ptProbts, 1)
colnames(ptProbts) <-  c('dv', 'prob', 'logLike', 'cut')

ptProbts <- as.data.frame(ptProbts)

## ordered for the separation plots:
ptProbts <- ptProbts[order(ptProbts$prob),]
#note that I need to sort the probability ggData=ggData[order(ggData$probs),]

sepPlotFunFull(ptProbts, 'fullmodelSepPlot')
##note that predicted probs of failure is too low
# in this model, prob it needs to be log-liklihood

sepPlotFunFull <- function(data, filename){  
    sepPlot <- ggplot(data=data, aes(ymin=0,
                          ymax=1, xmin=0, xmax=1:length(dv)))
    sepPlot = sepPlot + geom_rect()
                                        #    ggtitle("Out of Sample Separation Plot") + 
    sepPlot = sepPlot + geom_linerange(aes(color=factor(dv), x=1:length(dv)), alpha=.25)
                                        # Color event lines
    sepPlot = sepPlot + scale_color_manual(values = c('skyblue', 'navyblue'))
    sepPlot = sepPlot + scale_y_continuous("", breaks = c(0, 0.25, 0.5, 0.75, 1.0), expand=c(.125, 0))
    sepPlot = sepPlot + scale_x_continuous("", breaks = NULL)
    sepPlot = sepPlot + theme(
	axis.ticks = element_blank(), text=element_text(family='serif'),
	legend.position = "none", 
	panel.background = element_blank(), 
        panel.grid = element_blank() )
    sepPlot=sepPlot+theme_bw() +  theme(legend.position="none")
    sepPlot= sepPlot+ geom_line(aes(y=prob, x=1:length(dv)), lwd=.4, colour="white")
    print(sepPlot)
    ggsave(file=paste0('separationplot',filename,'.pdf'), width=8, height=3)
    dev.off()
}

#############
### Coefficient Plots For the Cross-Validation
#############

coefDt <-data.frame(uruz$coefsCrossVal)

colnames(coefDt) <- c('coef', 'stdErr', 'upper95' , 'lower95', 'tstats', 'cut')
n <- rep(c("Intercept",ivsPretty), 10) #ivsPretty declared above
coefDt$var<- n

##Dropping the splines:
restricted <- which(coefDt$var!='spline1' & coefDt$var!='spline2' & coefDt$var!='spline3')
displayDt <- coefDt[restricted,]

## remember: don't want to plot the splines:

## Cross-Val Data Coef Plots
tmp=ggplot(data=displayDt , aes(x=factor(cut), y=coef))
tmp <- tmp + geom_point(cex=1.5, color="black")+ facet_wrap(~ var, scales='free_y', ncol=3,)
tmp=tmp + geom_linerange(aes(ymin=lower95,
    ymax=upper95), lwd=2, size=.3, alpha=.75, color='navyblue') 
tmp=tmp + geom_hline(yintercept=0, linetype=2, color = "darkgray")
tmp=tmp + xlab('Folds') + ylab('Estimate')
tmp=tmp +
    ggtitle("Coefficient and 95% CI Range \nAllies Model Replication \n  ")
tmp=tmp+theme_bw()
tmp
dev.copy(pdf, 'XVal_Poster_coefPlots.pdf')
dev.off()
    
################
## 2.separation plots
## grapj takes the predicted probabilities
###############

## First of these is for the x-validation (out of sample) separation plots
sepData <- as.data.frame(uruz$predProbsout)
colnames(sepData) <- c('dv', 'prob', 'cut')
graphs(sepData) #this is the call that produces the plot

## For replicated model
sepData2 <-  ptProbts
graphsUniModel(sepData2)

#############################
##### Simulations
#############################
    
#######
    ## Marginal Effects
    ##Simulates across the range of values for allies, for a specific year, all other variables at
    ## central tendency
#######

mod = paperTables
library(MASS)
sims = 10000
draws = mvrnorm(sims, coef(mod), vcov.pt)

splineMat = unique( bpnMod1[,paste0('spline',1:3)] )
splineMat = splineMat[order(splineMat$spline1),]
print(data.frame(splineMat,row.names=NULL))

alliesRange = with(bpnMod1, seq(min(allies), max(allies), 1))

## times selected as start of first wave of data, end of first wave of data
## start of second wave of data, end of second wave of data.

scenME87 = with(bpnMod1,
    cbind(1, alliesRange, median(size), median(religious),
          median(ethnic),median(drugs), median(statesponsored),
          median(wbgi_gee_cs), median(polity2),
          median(logpop), 0, 0, 0, 1, 0,
          splineMat[1,1], splineMat[1,2], splineMat[1,3] ) )

scenME89 = with(bpnMod1,
    cbind(1, alliesRange, median(size), median(religious),
          median(ethnic),median(drugs), median(statesponsored),
          median(wbgi_gee_cs), median(polity2),
          median(logpop), 0, 0, 0, 1, 0,
          splineMat[3,1], splineMat[3,2], splineMat[3,3] ) )

scenME98 = with(bpnMod1,
    cbind(1, alliesRange, median(size),
          median(religious), median(ethnic),
          median(drugs), median(statesponsored),
          median(wbgi_gee_cs), median(polity2),
          median(logpop), 0, 0, 0, 1, 0, splineMat[12,1],
          splineMat[12,2], splineMat[12,3] )) 

scenME05 = with(bpnMod1,
    cbind(1, alliesRange, median(size), median(religious),
          median(ethnic),median(drugs), median(statesponsored),
          median(wbgi_gee_cs), median(polity2),
          median(logpop), 0, 0, 0, 1, 0,
          splineMat[19,1], splineMat[19,2], splineMat[19,3] ) )

scen = rbind(scenME87, scenME89,scenME98, scenME05)

## Get predicted values
predVals = draws %*% t(scen)
predProbsSim = 1/(1+exp(-predVals))

## Summary stats
info = function(x){c( mean(x), quantile(x, probs=c(0.025, 0.975)) ) }
predSumm = t( apply(predProbsSim, 2, info) )

## Prep data for plotting
simsPlot = data.frame(scen[,c(2,16)], predSumm)
names(simsPlot) = c('Allies', 'Year','Mu', 'Lo', 'Hi')

## plot
tmp = ggplot(simsPlot, aes(x=Allies, y=Mu, ymin=Lo, ymax=Hi))
tmp = tmp  + geom_ribbon(alpha=.75, fill="navyblue")
tmp=tmp+ geom_line(color="lightsteelblue4") + facet_wrap(~Year, ncol=4) + theme_bw() +
        labs(x=" \n Number of Allies", y="Mean Probability of Failure")
tmp
ggsave("AlliesSimulations.pdf", plot = tmp, width = 6, height = 4)   


############
###ROC Curves
############

rocGraph <-  function(data, cuts, threshold){
    colClasses = c("numeric", "numeric", "numeric")
    col.names = c("threshold", "FPR", "TPR")
    
    dataAll <- read.table(text = "",
                          colClasses = colClasses,
                          col.names = col.names)

    ##dta is a subset of the dataframe passed to rocGraph,
    ##threshold : vector of numerics
    ## returns a matrix    
    roca <- function(dta, threshold){
 	# Set up output matrix
	pr <- matrix(NA, ncol=3, nrow=length(threshold), 
		dimnames=list(NULL, c('Threshold', 'FPR', 'TPR') ) )

	# Loop through thresholds
	for(j in 1:length(threshold)){
            predRoc = as.numeric(dta$predProbs > threshold[j] )
             #print(paste("sum(dta$dv==0)", sum(dta$dv==0)))
            FPR = sum( (predRoc==1)*(dta$dv==0) ) / sum(dta$dv==0)
            #print(paste("sum(dta$dv==1)", sum(dta$dv==1)))
            TPR = sum( (predRoc==1)*(dta$dv==1) ) / sum(dta$dv==1)
            pr[j,1] = threshold[j]
            pr[j,2] = FPR
            pr[j,3] = TPR
	}

	# Return output
	return( data.frame( pr ) )
    }
    
    for (i in 1:cuts){ 
        dta <- data[data$ii == i,]
        names(dta) <- c('dv', 'predProbs', 'cut')
        #print(head(dta))
        
        round <-cbind(roca(dta, threshold), i)
        #print(head(round))
        dataAll <- rbind(dataAll, round)
    }
    
    return (dataAll)
}

## ROC Curve for the cross-val:

rocdat <- as.data.frame(uruz$predProbsout)
rocCurve = rocGraph(rocdat, 10, seq(0, 1, .0001) )

        ##XVAL ROC Plots
rocCurve$i <- as.factor(rocCurve$i) #make factor for the lines
colnames(rocCurve) <- c('Threshold', 'FPR', 'TPR', 'Fold')

    
rocplot <- ggplot(rocCurve, aes(x=FPR, y=TPR))+
    geom_line(aes(colour=Fold)) +
            geom_abline(intercept=0, slope=1, color="red", size=.5, alpha=0.75) +
                ggtitle("ROC Plots for 10-fold Cross-Validation of Allies Model") + theme_bw() +
                    scale_colour_brewer() + theme(legend.position="none") 
rocplot

dev.copy(pdf, "XfoldRocplot.pdf")
dev.off()


### Area under curve:

AUCList <- function(data){
    aucList <- as.numeric()
    for(i in 1:10){
        test <- rocCurve[rocCurve$Fold==i,]
        n = 2:nrow(test)
        auc[i] = abs((test$FPR[j] - test$FPR[j - 1]) %*% (test$TPR[j] + test$TPR[j - 1])/2)
        aucList <- c(aucList, auc[i])
}
    return(aucList)
}

XValAUC <- AUCList(rocCurve)

#######################
### Roc Plot for the baseline model:
## Reminder 4-9-15: this doesn't work
#######################



ptProbts$logLike <- NULL
rocdatpt <- ptProbts
colnames(rocdatpt) <- c('dv', 'predProbs', 'cut')
head(rocdatpt)
rocCurvept <- rocGraph(rocdatpt, 10, seq(0, 1, .0001) ) #throws lots of NaN.
head(rocCurvept)

rocplotpt <- ggplot(rocCurvept, aes(x=FPR, y=TPR))+
    geom_line() +
        geom_abline(intercept=0, slope=1, color="red", size=.5, alpha=0.75) +
            ggtitle("ROC Plots for Full Allies Model") + theme_bw()
rocplotpt
dev.copy(pdf, "rocplotptFull.pdf")
dev.off()

####################
##Model Extensions
## Extension with communities
####################

Path2 <- "~/Documents/Classes/Networks/"

comeights <- read.csv(paste0(Path2,'80scommunities.csv'), header=TRUE)
comeights <- comeights[,2:4]
colnames(comeights) <- c('ComP1','GroupName','groupid')

                                        #size of the communities: 
table(comeights$ComP1) ## when do x-val, chose the biggest groups by hand from here

##now make a variable with only the big groups:
comeights$Com5 <-  comeights$ComP1
comeights$Com5[comeights$ComP1!=5] <- 0
comeights$Com2 <- comeights$ComP1
comeights$Com2[comeights$ComP1!=2] <- 0

######
## nineties communities:
#######
    
comNineties <- read.csv(paste0(Path,'communities90s.csv'), header=TRUE)
comNineties <- comNineties[, 2:4]
colnames(comNineties) <- c('ComP2','nodeID','groupid')

## 29 communities, which is really too many to x-validate:
ncom <- unique(comNineties$ComP2)

##which have more than 2:
table(comNineties$ComP2) ## when do x-val, chose the biggest groups by hand from here
which(comNineties$ComP2 > 2)

##variables for the big groups:
comNineties$Com290 <- comNineties$ComP2
comNineties$Com290[comNineties$ComP2!=2] <- 0

comNineties$Com490 <- comNineties$ComP2
comNineties$Com490[comNineties$ComP2!=4] <- 0

comNineties$Com690 <- comNineties$ComP2
comNineties$Com690[comNineties$ComP2!=6] <- 0

## Df for extension, 80s data:
extenDf <-  merge(bpnMod1, comeights, by=c('groupid'), all.x=TRUE)

##don't need the groupname variable from the eighties:
extenDf$GroupName <- NULL
head(extenDf)

## The following for-loops code "NA"s for isolates (introduced by the merge) into 0s
zeros <- which(is.na(extenDf$ComP1))
for(i in zeros){
    extenDf[i,30] <- 0
}

## bigGroups
zeros1 <- which(is.na(extenDf$Com5))
for(i in zeros1){
    extenDf[i,31] <- 0
}

zeros2 <- which(is.na(extenDf$Com2))
for(i in zeros1){
    extenDf[i,32] <- 0
}
which(is.na(extenDf))


## plus 90s:
extenDf2 <-  merge(extenDf, comNineties, by=c('groupid'), all.x=TRUE)

extenDf2$nodeID <- NULL  #need all entries from the bpnMod1 df,
                            
zerosa <- which(is.na(extenDf2$ComP2))
for(i in zerosa){
    extenDf2[i,33] <- 0
}

zerosb <- which(is.na(extenDf2$Com290))
for(i in zerosb){
    extenDf2[i,34] <- 0
}
    
zerosc <- which(is.na(extenDf2$Com490))
for(i in zerosc){
    extenDf2[i,35] <- 0
}

zerosd <- which(is.na(extenDf2$Com690))
for(i in zerosd){
    extenDf2[i,36] <- 0
}

which(is.na(extenDf2))
 
##Now make the groups variables a factor, with 0 meaning no community membership.
extenDf2$ComP1 <- as.factor(extenDf2$ComP1)
extenDf2$ComP2 <- as.factor(extenDf2$ComP2)
extenDf2$Com5 <- as.factor(extenDf2$Com5)
extenDf2$Com2 <- as.factor(extenDf2$Com2)
extenDf2$Com290 <- as.factor(extenDf2$Com290)
extenDf2$Com490 <- as.factor(extenDf2$Com490)
extenDf2$Com690 <- as.factor(extenDf2$Com690)

#################
#### Applying model with communities as a control
#################

########
### Requirement: Estimate a new model that I can test on both the baseline
### and on the communities.
## Extension 2: model using communities as a control;
## Works by treating big communities as factor, value of 0 means isolate or small community

## New model1, removes the regional controls.

### Function to fit clustered robust std errors for the "gebo" model 
    
logit.clusterGebo <- function(fit){        
  ## Note: this is hardwired for the clustering to be groupid
  F <- function(x){1/(1+exp(-x))}       # logit CDF
  f <- function(x){exp(x)/(1+exp(x))^2} # logit PDF
  fml <- fit$formula
  data <- fit$data
  ivsse=c('allies', 'size', 'drugs', 'statesponsored', 'wbgi_gee_cs',
      'polity2', 'logpop', 'Com2', 'Com5', 'Com290',
      'Com490', 'Com690','spline1','spline2','spline3')
  beta <- fit$coef
  print(class(beta))
  vcov <- vcov(fit)
  rgr <- colnames(vcov(fit))
  k <- length(beta)
  #print(paste0('k is ', k))      
  y <- fit$y
  m <- dim(table(data$groupid))   # N of clusters
  xvars <- names(beta)            # Name of covariates ordered accordingly
  xvars <- xvars[2:length(beta)] # Delete (Intercept)
  xmat <- data.matrix(data[,ivsse]) # columns selected are the variables used in the regression
  xmat <- cbind(1, xmat)          # Add intercept
  xb <- xmat %*% beta             # linear predictor (xb)
  ## Now, obtain clustered s.e.
  u <- ((y==1) * f(xb)/F(xb) + (y==0) * -f(xb)/(1-F(xb)))[,1] * xmat
  u.clust <- matrix(NA, nrow=m, ncol=k)
  fc <- factor(data$groupid)
  for (i in 1:k){ ## loop over covariates
    u.clust[,i] <- tapply(u[,i], fc, sum) ## sum over dyad
  }
  cl.vcov <- vcov %*% ((m/(m-1)) * t(u.clust)%*%(u.clust)) %*% vcov ## sandwich
  cl.se <- sqrt(diag(cl.vcov)) ## clustered s.e.
  return(list(cl.se, cl.vcov))
}

#################
## the following is the very parsimonious ("limited") model
### Arrived at by removing most elements that are linearly dependent
## so that I can estimate effect of allies, controlling for community structure

dvrr='failure'
ivsrr=c('allies', 'size', 'drugs', 'statesponsored','wbgi_gee_cs',
    'polity2','logpop','spline1','spline2','spline3')
modElementsRR=formula(paste0( dvrr, '~ ', paste(ivsrr, collapse=' + ') ))

ivsPresentationR <- c('Number of Allies', 'Size Class', 'Religious Motivation',
                      'Ethnic Motivation', 'Drug Funding', 'State Sponsor',
                      'State Capacity', 'State Polity2', 'Log State Population')

ivsRR <- c('Number of Allies', 'Size Class', 'Drug Funding', 'State Sponsor',
           'State Capacity', 'State Polity2', 'Log State Population', 'spline1', 'spline2', 'spline3')

### Model for the comparisons:
small <- glm(formula=modElementsRR, family=binomial(link=logit), data=extenDf2)
small.cl.se <- logit.cluster(small)[[1]]
smallTable <- presentation(small, small.cl.se)

## Now, small model controlling for the largest communities (code is "se" for "small extension")

dvse='failure'
ivsse=c('allies', 'size', 'drugs', 'statesponsored',
    'wbgi_gee_cs', 'polity2', 'logpop', 'Com2', 'Com5',
    'Com290', 'Com490', 'Com690','spline1','spline2','spline3')
modElementsSEx=formula(paste0( dvse, '~ ', paste(ivsse, collapse=' + ') ))

gebo <- glm(formula=modElementsSEx, family=binomial(link=logit), data=extenDf2)
summary(gebo)
    
gebo.cl.se <- logit.clusterGebo(gebo)[[1]]
gebovcov <- logit.clusterGebo(gebo)[[2]]

prez <- presentation(gebo, gebo.cl.se)

############    
##Formatting the data for the coefficient plot:

coefSmallMods <- function(fitted, stderr){
    stdErr <- stderr
    upper95 <-fitted$coefficients + qt(.975, df=fitted$df.residual)*stdErr
    lower95 <- fitted$coefficients - qt(.975, df=fitted$df.residual)*stdErr

    ggData <- as.data.frame(cbind(fitted$coefficients, stdErr, upper95, lower95))
    colnames(ggData) <- c("coef", "stdErr", "Upper95", "Lower95")
                                        #rownames(ggData) <- c(ivs)
    ggData$var <- rownames(ggData)
                                        #ggData$prettyvar <- c("Intercept", ivsRR)
    return(ggData)
}

smodFullDat <- coefSmallMods(small, small.cl.se)
coefsGebo <- coefSmallMods(gebo, gebo.cl.se)

##Coefficient Plot with paperTableModel and Small Model:

##paperTable formatted plus variable for model
ggdata2 <- ggdata
ggdata2$model <- "Full Model"
smallModFullGraph <- ggdata2 #worried above overwriting ggdata2 somewhere along the lien

sModEst <- smodFullDat[2:8,]
sModEst$model <- "Limited Model"

coefsGebo <- coefSmallMods(gebo, gebo.cl.se)
coefsGebo <-  coefsGebo[2:13,]
coefsGebo$model <- "CommunityControl"
coefsGebo$prettyvar <- c('Number of Allies', 'Size Class', 'Drug Funding', 'State Sponsor',
'State Capacity', 'State Polity2', 'Log State Population',
'Community2_Period1', 'Community5_Period1', 'Community2_Period2',
'Community4_Period2', 'Community6_Period2')


datCombPlot <- rbind(sModEst, smallModFullGraph)
datCombPlot$model <- as.factor(datCombPlot$model)
datCombPlot$prettyvar <- as.factor(datCombPlot$prettyvar)
## Overlay:

cmb1=ggplot(data=datCombPlot, aes(color=model))
cmb1 <- cmb1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
cmb1 <- cmb1 + geom_pointrange(aes(x = prettyvar, y=coef, ymin = Lower95,
                                   ymax = Upper95),
                               lwd = 1, position = position_dodge(width = 1/2),
                               shape = 20)
cmb1=cmb1 + geom_hline(xintercept=0, linetype=2, color = "red")
                                        #cmb <- cmb+ theme(strip.text.x=element_text(angle=-90))
cmb1=cmb1 + xlab('Variables \n (Excluding Intercept and Splines) \n ') + ylab('Estimated Coefficient')
cmb1=cmb1 + ggtitle("Overlap in Coefficient and 95% CI Range \n Across Full and Limited Model")
cmb1=cmb1+theme_bw() + coord_flip() +theme(panel.background = element_rect(colour = "blue")) +
    scale_color_manual(values=c("#0072B2", "#009E73"))+
theme(legend.position="bottom")++ theme(legend.title=element_blank())
cmb1
                           
dev.copy(pdf, "CoefPlotsBothModels.pdf")
dev.off()

    
####coefplot for the gebo model:

cpg=ggplot(data=coefsGebo[1:11,])

cpg <- cpg+ geom_linerange(
    aes(x =prettyvar, ymin=Lower95, ymax=Upper95),
    lwd=1, size=.25, alpha=.75, color="navyblue")
cpg <- cpg + geom_point(
    aes(x =prettyvar, y = coef, ymin=Lower95, ymax=Upper95),
    cex=1, color="navyblue")
cpg=cpg + geom_hline(xintercept=0, linetype=2, color = "red")
cpg=cpg+ xlab('Variables') + ylab('Estimated Coefficient \n Excluding 90s Community 6')
cpg=cpg + ggtitle("Estimated Coefficients Controlling For Large Communities")
cpg=cpg+theme_bw() + coord_flip()
cpg

dev.copy(pdf, "WithMostCoefsCommunities.pdf")
dev.off()

### Compare Gebo and Full models
### first: combine into a plot:

dataCombPlot2 <- rbind(smallModFullGraph, coefsGebo)
dataCombPlot2$model <- as.factor(dataCombPlot2$model)
dataCombPlot2$prettyvar = factor(dataCombPlot2$prettyvar, levels=
                                       c('Number of Allies', 'Size Class', 'Religious Motivation',
                                         'Ethnic Motivation', 'Drug Funding', 'State Sponsor',
                                         'State Capacity', 'State Polity2', 'Log State Population',
                                         'Community2_Period1','Community5_Period1',
                                         'Community2_Period2', 'Community4_Period2',
                                         'Community6_Period2', 'Asia', 'M.East', 'L.America',
                                         'N.America', 'SS.Africa'))
    
dataCombPlot2$prettyvar <-  rev(dataCombPlot2$prettyvar)

cmb=ggplot(data=dataCombPlot2[1:21,], aes(
                   x=prettyvar, y=coef, ymin=Lower95,ymax=Upper95, color=model))
cmb <- cmb+ geom_linerange( lwd=1, size=1, position=position_dodge(width=.5))
cmb <- cmb + geom_point(cex=2, position=position_dodge(width=.5))
cmb=cmb + geom_hline(xintercept=0, linetype=2, color = "red")
cmb=cmb + xlab('Variables') + ylab('Estimated Coefficient \n(Robust Clustered Standard Errors)')
cmb=cmb + ggtitle("Estimated Coefficients and 95% CI Range
Phillips Model and Limited Model With Communities")
cmb=cmb+theme_bw()+ theme(axis.text.x = element_text(angle=90))
cmb= cmb +theme(panel.background = element_rect(colour = "blue")) +
    scale_color_manual(values=c("#009E73", "#0072B2"))+
theme(legend.position="bottom")+ theme(legend.title=element_blank())
cmb
    
dev.copy(pdf, "GeboandPaperTables.pdf")
dev.off()

## 2.separation plots
## graphs takes the predicted probabilities
sepData <- as.data.frame(uruz$predProbsout)
colnames(sepData) <- c('dv', 'prob', 'cut')
graphs(sepData)

### 80s communities with more than 20 constituent groups

coms80sC2 <- extenDf2[which(extenDf2$ComP1==2),] 
coms80sC5 <- extenDf2[which(extenDf2$ComP1==5),]
## 90s communities with more than 20 constituent groups

coms90sC2 <- extenDf2[which(extenDf2$ComP2==2),] 
coms90sC4 <- extenDf2[which(extenDf2$ComP2==4),]
coms90sC6 <- extenDf2[which(extenDf2$ComP2==6),]

## Model: Community 2 is (mostly) Levant
##small model: 80s Community 1
m80sC2 <- glm(modElementsRR, family=binomial(link=logit), data=coms80sC2)
cl.se.c2rr <- logit.cluster(m80sC2)[[1]]
T80sC2 <- presentation(m80sC2, cl.se.c2rr)
smd1<- coefSmallMods(m80sC2, cl.se.c2rr)

### 80s community 5. South America.
##Had 21 failures, 230 w.o failure. so better to estimate

##small model
m80sC5 <- glm(modElementsRR, family=binomial(link=logit), data=coms80sC5)
cl.se.c5rr <- logit.cluster(m80sC5)[[1]]
Table80sC5 <- presentation(m80sC5, cl.se.c5rr)
smd2<- coefSmallMods(m80sC5, cl.se.c5rr)

##nineties communities: 2, 6, 4 are the biggest

##small model.
##Community 2 is the al-Qaeda alliance.
##4 failures, 250 group years w/o failure

m90sC2 <- glm(modElementsRR, family=binomial(link=logit), data=coms90sC2)
## the above throws the error that fitted probs 0 or 1 occured
cl.se.c92rr <- logit.cluster(m90sC2)[[1]] 
Table90sC2 <- presentation(m90sC2, cl.se.c92rr)
smd3<- coefSmallMods(m90sC2, cl.se.c92rr)

### 90s Com 4- super splats.
#This is the India/Kashmir unit; only one failure
m90sC4 <- glm(modElementsRR, family=binomial(link=logit), data=coms90sC4)
                                        #this one doesn't converge
cl.se.c94rr <- logit.cluster(m90sC4)[[1]] 
Table90s42 <- presentation(m90sC4, cl.se.c94rr) ## this one == epic splatz
#smd4<- coefSmallMods(m90sC4, cl.se.c94rr)

### 90s Com 6
##Note that this is a Palestinian group nexus... so we learn that the model doesn't
## predict this group at all... because none of them have failed
m90sC6 <- glm(modElementsRR, family=binomial(link=logit), data=coms90sC6)
cl.se.c96rr <- logit.cluster(m90sC6)[[1]] 
Table90sC6 <- presentation(m90sC6, cl.se.c96rr) 
smd4<- coefSmallMods(m90sC6, cl.se.c96rr)


##### putting together for plot

smd1 <- cbind(smd1, '80s Levant Subgraph')
colnames(smd1) <- c('coef', 'stdErr', 'Upper95','Lower95','var','prettyvar','com', 'comname')

smd2 <- cbind(smd2, '80s South America Subgraph')
colnames(smd2) <- c('coef', 'stdErr', 'Upper95','Lower95','var','prettyvar','comname', 'com')

smd3 <- cbind(smd3, '90s Al-Qaeda Subgraph')
colnames(smd3) <- c('coef', 'stdErr', 'Upper95','Lower95','var','prettyvar','com','comname')

smd4 <- cbind(smd4, '90s Levant Subgraph')
colnames(smd4) <- c('coef', 'stdErr', 'Upper95','Lower95','var','prettyvar','com','comname')

smallPlot <- rbind(smd1[2:8,], smd2[2:8,], smd3[2:8,], smd4[2:8,])

 ## adding bars for the small model, to compare:
baseline <- sModEst
baseline$model <- NULL
baseline$comname <- "Baseline (Limited)"
baseline$com <- "baseline_lim"

smallPlotF <- rbind(smallPlot, baseline)

library(scales)

zp2 <- ggplot(smallPlotF, aes(color=prettyvar))
zp2 <- zp2 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp2 <- zp2 + geom_linerange(aes(x = prettyvar, ymin = Lower95,
                                ymax = Upper95),
                            lwd = 1, position = position_dodge(width = 1/2) ) +
                                scale_fill_manual(values=c("#cb4154", "#00ffff", "#ff3800", "#ba55d3"))
zp2 <- zp2 + geom_pointrange(aes(x = prettyvar, y=coef, ymin = Lower95,
                                 ymax = Upper95),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 16)
zp2 <- zp2 +theme_bw() +  theme(axis.text.x = element_text(angle=90)) +
    theme(legend.position="none") +
        guides(colour = guide_legend(nrow = 2))+ facet_wrap(~comname, scales="free_y")+
            xlab("Variables \n (Excluding Intercept and Splines)") + ylab("Estimated Coefficients and 95% CI") +
    theme(panel.background = element_rect(colour = "blue"))
zp2 <- zp2 + ggtitle("Limited Allies Model \n Applied to Four Large Alliance Communities")
print(zp2)  # The trick to these is position_dodge().

ggsave(zp2, file="smallTableCoef.pdf", width=9, height=7)


################
## Also, estimate using communities instead of geography:
## make sure it is a factor:
#################
