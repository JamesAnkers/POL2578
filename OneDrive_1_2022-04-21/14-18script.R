rm(list=ls())
tdot14_18 <- read.csv('VotesCSV14-18.csv')

warddata <- read.csv('twd.csv')

library(statnet)
library(ergm.count)
library(networkdata)
library(latentnet)

el14_18 <- matrix(data = 0, nrow = 45, ncol =45)
cn14_18 <- tdot14_18[,2]
row.names(el14_18) <- cn14_18
colnames(el14_18) <- cn14_18



# replacing abstentions with NAs
#for(i in 1:45) {
# for(j in 3:7815) {
#  if (tdot14_18[i,j] == 9) {
#   tdot14_18[i,j] <- NA
#}
#}
#}






# ergmms don't like missing data, so filling in averages for the mayor

for (i in 2:ncol(warddata)) {
  warddata[45,i] <- mean(warddata[,i], na.rm = TRUE)
}


# this creates a matrix with the % of time two councilors vote together, when both actually vote on an item. This controls for base rates and abstentions (particularly since there are some mid-term replacements in the data)
tdot14_18 <- tdot14_18[,-1] # removing ward numbers
tdot14_18 <- tdot14_18[,-1] # a second time for the names

row.names(tdot14_18) <- cn14_18
for(i in 1:45) {
  for(j in 1:45) {
    temp1 <- as.integer(tdot14_18[i,])
    temp2 <- as.integer(tdot14_18[j,])
    temp3 <- temp1 - temp2
    temp4 <- temp3[which(temp3 > -2)]
    temp4 <- temp4[which(temp4 < 2)]
    el14_18[i,j] <- sum(temp4 == 0)/length(temp4)
  }
}


# now we need to make a long list
emptylong <- matrix(data = NA, nrow = 2025, ncol = 3)
emptylong <- as.data.frame(emptylong)

for(i in 1:45) {
  for(j in 1:45) {
    emptylong[((i-1)*45)+j,3] <- el14_18[i,j]
    emptylong[((i-1)*45)+j,1] <- cn14_18[i] 
    emptylong[((i-1)*45)+j,2] <- cn14_18[j]
  }
}
# my god that's ugly! but it does the job


dyadVars <-  "votepercent"
n <- length(cn14_18)
p <- length(dyadVars)

dyadArray <- array(0, 
                   dim=c(n,n,p),
                   dimnames=list(cn14_18,cn14_18,dyadVars)
)



for(i in 1:2025) {
  a1 <- which(cn14_18 == emptylong[i,1])
  a2 <-  which(cn14_18 == emptylong[i,2])
  val <- as.numeric(emptylong[i,3])
  dyadArray[a1,a2,1] <- as.numeric(val)
}





votenet<-network(as.matrix(el14_18),directed=FALSE,matrix.type='adjacency',loops=FALSE,
                 ignore.eval = FALSE, 
                 names.eval = "votepercent")

set.network.attribute(votenet,'votepercent',dyadArray[,,'votepercent'])
as.matrix(votenet, attrname = "votepercent")[1:10, 1:10]

set.vertex.attribute(votenet,"dwnom", warddata$DW1014)
set.vertex.attribute(votenet,"denratio", warddata$DenRatio)
set.vertex.attribute(votenet,"houseincome", warddata$HouseIncome)
set.vertex.attribute(votenet,"postsec", warddata$PSPer)
set.vertex.attribute(votenet,"onefam", warddata$OneFamHomePer)
set.vertex.attribute(votenet,"oldnew", warddata$DummyOld)

y.var<-4*sd(as.matrix(el14_18), na.rm=TRUE) #Need to give a variance prior for a normal distribution.
m1<-ergmm(votenet~euclidean(d=2), 
          family="normal", 
          response="votepercent",
          control=ergmm.control(burnin=90000,sample.size= 600000,interval=5),
          verbose=1, 
          fam.par=list(
            prior.var=y.var,
            prior.var.df=1 # certainty of the prior, higher more certain
          ))
#mcmc.diagnostics(m1)


# just a test
#m2<-ergmm(votenet~nodematch("oldnew") + nodecov("denratio")+nodecov("postsec") + nodecov("houseincome") + nodecov("onefam"), response="votepercent",
#control=ergmm.control(burnin=20000,sample.size= 400000,interval=5))
#            mcmc.diagnostics(m2)


zPos = summary(m1)$'pmean'$Z
head(zPos)
bounds <- c(min(zPos[,1]), max(zPos[,1]), min(zPos[,2]), max(zPos[,2]))*1.1
nums <- c(1:45)


# plotting old/new
plot(NULL, xlim=bounds[1:2], ylim=bounds[3:4], ylab="y label", xlab="x lablel")
for (i in 1:45) {
  points(zPos[i,1], zPos[i,2], col = warddata$DummyOld[i] + 2, pch = 19, cex = 1.2)
  text(zPos[i,1], zPos[i,2] + .008, labels = cn14_18[i], cex = 0.8)
}

plot(NULL, xlim=c(-.10,-.04), ylim=c(-.025,.045), ylab="y label", xlab="x lablel")
for (i in 1:45) {
  points(zPos[i,1], zPos[i,2], col = warddata$DummyOld[i] + 2, pch = 19, cex = 1.2)
  text(zPos[i,1], zPos[i,2] + .003, labels = cn14_18[i], cex = 0.7)
}

# Plotting density
temp <- colorRampPalette(c("blue", "red"))
temp <- temp(127)

plot(NULL, xlim=bounds[1:2], ylim=bounds[3:4], ylab="y label", xlab="x lablel")
for (i in 1:45) {
  points(zPos[i,1], zPos[i,2], col = temp[warddata$DenRatio[i]], pch = 19, cex = 1.4)
  text(zPos[i,1], zPos[i,2] + .03, labels = cn14_18[i], cex = 0.8)
}

plot(NULL, xlim=c(-.05,.1), ylim=c(-.05,.05), ylab="y label", xlab="x lablel")
for (i in 1:45) {
  points(zPos[i,1], zPos[i,2], col = temp[warddata$DenRatio[i]], pch = 19, cex = 1.4)
  text(zPos[i,1], zPos[i,2] + .004, labels = cn14_18[i], cex = 0.7)
}

# ownership percent
temp <- colorRampPalette(c("blue", "red"))
temp <- temp(100)

plot(NULL, xlim=bounds[1:2], ylim=bounds[3:4], ylab="y label", xlab="x lablel")
for (i in 1:45) {
  points(zPos[i,1], zPos[i,2], col = temp[warddata$OwnPer[i]*100], pch = 19, cex = 1.4)
  text(zPos[i,1], zPos[i,2] + .03, labels = cn14_18[i], cex = 0.8)
}

plot(NULL, xlim=c(.01,.03), ylim=c(.07,.1), ylab="y label", xlab="x lablel")
for (i in 1:45) {
  points(zPos[i,1], zPos[i,2], col = temp[warddata$OwnPer[i]*100], pch = 19, cex = 1.4)
  text(zPos[i,1], zPos[i,2] + .0013, labels = cn14_18[i], cex = 0.7)
}

# income
temp <- colorRampPalette(c("blue", "red"))
temp <- temp(100)

plot(NULL, xlim=bounds[1:2], ylim=bounds[3:4], ylab="y label", xlab="x lablel")
for (i in 1:45) {
  points(zPos[i,1], zPos[i,2], col = temp[warddata$HouseIncome[i]/max(warddata$HouseIncome)*100], pch = 19, cex = 1.4)
  text(zPos[i,1], zPos[i,2] + .03, labels = cn14_18[i], cex = 0.8)
}

plot(NULL, xlim=c(.01,.03), ylim=c(.07,.1), ylab="y label", xlab="x lablel")
for (i in 1:45) {
  points(zPos[i,1], zPos[i,2], col = temp[warddata$HouseIncome[i]/max(warddata$HouseIncome)*100], pch = 19, cex = 1.4)
  text(zPos[i,1], zPos[i,2] + .0022, labels = cn14_18[i], cex = 0.7)
}


