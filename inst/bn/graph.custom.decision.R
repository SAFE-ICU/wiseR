graph.custom.decision <- function(NetworkGraph,nodeNames,EvidenceNode,EventNode,defLeg)
{
  tryCatch({
    nodes <- data.frame(name = nodeNames)
    nodes$id <- 0:(nrow(nodes) - 1)
    nodes$group <- "Random"
    nodes$shape<-"dot"
    nodes[which(nodes$name %in% EvidenceNode),3] = "Decision"
    nodes[which(nodes$name == EventNode),3] = "Utility"
    nodes[which(nodes$name %in% EvidenceNode),4] = "square"
    nodes[which(nodes$name == EventNode),4] = "diamond"
    visNodes<- data.frame(id = nodeNames,
                          label = nodeNames,
                          group = nodes["group"],
                          shape = nodes[["shape"]])
    visEdges<- data.frame(from = NetworkGraph[,1],
                          to = NetworkGraph[,2])
    return(visNetwork(visNodes, visEdges, width = "100%") %>%
             visEdges(arrows ="to",smooth = T,color = list(color = "grey",highlight = "black",hover = "black"),scaling=list(min=0,max=1))%>%
             visGroups(groupname = "Random", color = list(background = "lightblue",highlight = 'blue', hover = "blue")) %>%
             visGroups(groupname = "Decision", color = list(background = "lightgreen",highlight = "green", hover = "green"))%>%
             visGroups(groupname = "Utility", color = list(background = "pink",highlight = "red", hover = "red")) %>%
             visLegend(width = 0.1, position = "left",enabled=defLeg)%>%
             visNodes(shape = "dot")
             )
},error=function(e){
  print(e)
  shinyalert(e)
})

}
