

library((HydeNet))
library(bnlearn)



data(alarm)

alarm <- alarm[,1:6]
bn <- hc(alarm)
fit <- bn.fit(bn,data=alarm,method="bayes")
#fitl <- list(fit)
# 
# orphan <- function(x=list())
# {
#   (x$parents %in% character(0)) 
#   }
# 
# sapply(fitl,FUN = orphan)


roots <- fit[(sapply(fit,function(x){length(dim(x$prob))})==1)]
non_roots <- fit[(sapply(fit,function(x){length(dim(x$prob))})!=1)]

bag <- list()
for(i in 1:length(non_roots)){
  #print(i)
  m <- non_roots[[i]]$prob
  dims <- length(dim(m)):1
  m <- aperm(m,dims)
  model <- alarm[,c(names(dimnames(m)))]
  attr(m,"model") <- model
  class(m) <- c("cpt","array")
  bag[[length(bag)+1]] <- m  
}

root_dat <- alarm[names(roots)]
for(i in names(roots)){
  #print(i)
  m <- xtabs(as.formula(paste("~",i,sep="")),data=root_dat)
  # model <- alarm[,c(names(dimnames(m)))]
  # attr(m,"model") <- model
  # class(m) <- c("cpt","array")
  bag[[length(bag)+1]] <- m  
}
  
m <- xtabs(~HIST,data=root_dat)
bag[[length(bag)+1]] <- m


m <- xtabs(~HRBP,data=root_dat)
bag[[length(bag)+1]] <- m

m <- xtabs(~PAP,data=root_dat)
bag[[length(bag)+1]] <- m

m <- xtabs(~FIO2,data=root_dat)
bag[[length(bag)+1]] <- m

m <- xtabs(~ANES,data=root_dat)
bag[[length(bag)+1]] <- m

m <- xtabs(~ERCA,data=root_dat)
bag[[length(bag)+1]] <- m

m <- xtabs(~CVP,data=root_dat)
bag[[length(bag)+1]] <- m
# m <- xtabs(~TPR,data=root_dat)
# bag[[length(bag)+1]] <- m
# 



bagNet <- HydeNetwork(bag)
writeNetworkModel(bagNet, pretty=TRUE)
plot(bagNet)


bagNet1 <- compileJagsModel(bagNet)

net <- setNode(network = bagNet, node = HIST,
               nodeType = "dbern", prob=.4)

writeNetworkModel(net, pretty=TRUE)





#####################################################
m1 <- fit$CVP$prob
m2 <- fit$PCWP$prob
m3 <- fit$HIST$prob
m4 <- fit$TPR$prob


# 
# if(length(dim(m)==1)){m <- NULL}

model1 <- alarm[,c(names(dimnames(m1)))]
attr(m1,"model") <- model1
class(m1) <- c("cpt","array")


model2 <- alarm[,c(names(dimnames(m2)))]
attr(m2,"model") <- model2
class(m2) <- c("cpt","array")


model3 <- alarm[,c(names(dimnames(m3)))]
attr(m3,"model") <- model3
class(m3) <- c("cpt","array")

model4 <- alarm[,c(names(dimnames(m4)))]
attr(m4,"model") <- model4
class(m4) <- c("cpt","array")






bag <- list(g5,g7)
bag <- list(m1,m2,m4)
bagNet <- HydeNetwork(bag)
writeNetworkModel(bagNet, pretty=TRUE)

plot(bagNet)



############################

for(i in 1:length(dropped)){
  print(i)
  m <- dropped[[i]]$prob
  dims <- 1:length(dim(m))
  dims[1] <- 2
  dims[2] <- 1
  if(length(dim(m))==2){
    m <- aperm(m,c(2,1))
    model <- alarm[,c(names(dimnames(m)))]
    attr(m,"model") <- model
    class(m) <- c("cpt","array")
    bag[[length(bag)+1]] <- m  
  }
  
  else{
    m <- aperm(m,c(2,1,3))
    model <- alarm[,c(names(dimnames(m)))]
    attr(m,"model") <- model
    class(m) <- c("cpt","array")
    bag[[length(bag)+1]] <- m  
  }
}

