library(HydeNet)
library(bnlearn)
data(alarm)
alarm <- data.frame(sapply(alarm,factor))

bn <- hc(alarm)
model <- as.character(bn)
model <- model <- gsub(model,pattern = "\\:",replacement = "*",x = model)
model <- gsub(model,pattern = "\\]\\[",replacement = "+",x = model)
model <- gsub(model,pattern = "\\]|\\[",replacement = "",x = model)
model <- as.formula(paste("~",model,sep=""))
alarmNet <- HydeNetwork(model,data=alarm)
plot(alarmNet)

DecisionNet <- setDecisionNodes(alarmNet,VTUB)
DecisionNet <- setUtilityNodes(DecisionNet, MVS)
plot(DecisionNet)




alarmNet1 <- compileJagsModel(alarmNet)

alarmNet2 <- compileJagsModel(alarmNet, data = list(HIST = "TRUE") )



Posterior <- HydeSim(alarmNet1, 
                     variable.names = c("HYP", "SAO2"), 
                     n.iter = 1000)



decisionNet <- compileDecisionModel(Net, n.chains=5)

plot(Posterior$codas[,c("SAO2")])
