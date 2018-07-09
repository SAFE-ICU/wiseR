
library(HydeNet)
library(bnlearn)
data(alarm)
bn <- hc(alarm)
alarm$MyUtilityNode <- alarm$LVV
alarm$MyUtilityNode <- gsub(pattern = "NORMAL",replacement = 0,x = alarm$MyUtilityNode)
alarm$MyUtilityNode <- gsub(pattern = "HIGH",replacement = 1,x = alarm$MyUtilityNode)
alarm$MyUtilityNode <- gsub(pattern = "LOW",replacement = -1,x = alarm$MyUtilityNode)
alarm$MyUtilityNode <- as.numeric(alarm$MyUtilityNode)

model <- as.character(bn)
model <- model <- gsub(model,pattern = "\\:",replacement = "*",x = model)
model <- gsub(model,pattern = "\\]\\[",replacement = "+",x = model)
model <- gsub(model,pattern = "\\]|\\[",replacement = "",x = model)

model <- paste(model,"+ MyUtilityNode|LVV")

model <- as.formula(paste("~",model,sep=""))

Dnet<<-HydeNetwork(model,data = alarm)
DecisionNet <- setDecisionNodes(Dnet,LVF)
DecisionNet <- setDecisionNodes(DecisionNet,PCWP)
DecisionNet <- setUtilityNodes(DecisionNet, MyUtilityNode)
plot(DecisionNet)
alarmNet <- compileJagsModel(DecisionNet)
