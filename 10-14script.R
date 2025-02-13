rm(list=ls())
tdot10_14 <- read.csv('VotesCSV10-14.csv')

warddata <- read.csv('twd.csv')

library(statnet)
library(ergm.count)
library(networkdata)
library(latentnet)

el10_14 <- matrix(data = 0, nrow = 45, ncol =45)
cn10_14 <- tdot10_14[,2]
row.names(el10_14) <- cn10_14
colnames(el10_14) <- cn10_14





# ergmms don't like missing data, so filling in averages for the mayor

for (i in 2:ncol(warddata)) {
  warddata[45,i] <- mean(warddata[,i], na.rm = TRUE)
}


# this creates a matrix with the % of time two councilors vote together, when both actually vote on an item. This controls for base rates and abstentions (particularly since there are some mid-term replacements in the data)
tdot10_14 <- tdot10_14[,-1] # removing ward numbers
tdot10_14 <- tdot10_14[,-1] # a second time for the names

row.names(tdot10_14) <- cn10_14
for(i in 1:45) {
  for(j in 1:45) {
    temp1 <- as.integer(tdot10_14[i,])
    temp2 <- as.integer(tdot10_14[j,])
    temp3 <- temp1 - temp2
    temp4 <- temp3[which(temp3 > -2)]
    temp4 <- temp4[which(temp4 < 2)]
    el10_14[i,j] <- sum(temp4 == 0)/length(temp4)
  }
}


# now we need to make a long list
emptylong <- matrix(data = NA, nrow = 2025, ncol = 3)
emptylong <- as.data.frame(emptylong)

for(i in 1:45) {
  for(j in 1:45) {
    emptylong[((i-1)*45)+j,3] <- el10_14[i,j]
    emptylong[((i-1)*45)+j,1] <- cn10_14[i] 
    emptylong[((i-1)*45)+j,2] <- cn10_14[j]
  }
}
# my god that's ugly! but it does the job


dyadVars <-  "votepercent"
n <- length(cn10_14)
p <- length(dyadVars)

dyadArray <- array(0, 
                   dim=c(n,n,p),
                   dimnames=list(cn10_14,cn10_14,dyadVars)
)



for(i in 1:2025) {
  a1 <- which(cn10_14 == emptylong[i,1])
  a2 <-  which(cn10_14 == emptylong[i,2])
  val <- as.numeric(emptylong[i,3])
  dyadArray[a1,a2,1] <- as.numeric(val)
}





votenet<-network(as.matrix(el10_14),directed=FALSE,matrix.type='adjacency',loops=FALSE,
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
set.vertex.attribute(votenet,"ownper", warddata$OwnPer)

y.var<-4*sd(as.matrix(el10_14), na.rm=TRUE) #Need to give a variance prior for a normal distribution.
m1<-ergmm(votenet~nodematch("oldnew") + latentcov("denratio") + latentcov("houseincome") + latentcov("ownper") + euclidean(d=2), 
          family="normal", 
          response="votepercent",
          control=ergmm.control(burnin=20000,sample.size= 500000,interval=5),
          verbose=1, 
          fam.par=list(
            prior.var=y.var,
            prior.var.df=1 # certainty of the prior, higher more certain
          ))



#mcmc.diagnostics(m1)



zPos = summary(m1)$'pmean'$Z
head(zPos)
bounds <- c(min(zPos[,1]), max(zPos[,1]), min(zPos[,2]), max(zPos[,2]))*1.1



plot(NULL, xlim=bounds[1:2], ylim=bounds[3:4], main = "Toronto 10-14", ylab="", xlab="")
for (i in 1:45) {
  points(zPos[i,1], zPos[i,2], col = warddata$DummyOld[i] + 2, pch = 19, cex = 1.2)
  text(zPos[i,1], zPos[i,2] + .008, labels = cn10_14[i], cex = 0.8)
}

plot(NULL, xlim=c(-.065,.035), ylim=c(-.075,.068), main = "Toronto 10-14, Zoomed In", ylab="", xlab="")
for (i in 1:45) {
  points(zPos[i,1], zPos[i,2], col = warddata$DummyOld[i] + 2, pch = 19, cex = 1.2)
  text(zPos[i,1], zPos[i,2] + .003, labels = cn10_14[i], cex = 0.7)
}



