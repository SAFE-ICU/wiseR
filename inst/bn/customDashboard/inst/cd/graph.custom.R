graph.custom <- function(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,Ndegree,Tlayout,font)
{
  defLeg<-TRUE
  if(!(EventNode %in% nodeNames))
  {
    defLeg<-FALSE
  }
  tryCatch({
    nodes <- data.frame(name = nodeNames)
    nodes$id <- 0:(nrow(nodes) - 1)
    nodes$group <- "not in use"
    nodes[which(nodes$name %in% EvidenceNode),3] = "Evidence"
    nodes[which(nodes$name == EventNode),3] = "Event"
    visNodes<- data.frame(id = nodeNames,
                          label = nodeNames,
                          group = nodes$group,
                          shape = shapeVector,
                          font.size = font)
    visEdges<- data.frame(from = NetworkGraph$from,
                          to = NetworkGraph$to)
    return(visNetwork(visNodes, visEdges, width = "100%") %>%
             visEdges(arrows ="to",smooth = T,color = list(color = "grey",highlight = "black",hover = "black"))%>%
             visGroups(groupname = "not in use", color = list(background = "lightblue",highlight = 'blue', hover = "blue")) %>%
             visGroups(groupname = "Event", color = list(background = "lightgreen",highlight = "green", hover = "green"))%>%
             visGroups(groupname = "Evidence", color = list(background = "pink",highlight = "red", hover = "red")) %>%
             visLegend(width = 0.1, position = "left",enabled=defLeg)%>%
             visNodes(shape = "dot") %>%
             visOptions(highlightNearest = list(enabled =TRUE, degree = Ndegree,hover = T, hideColor = 'rgba(200,200,200,0)'), nodesIdSelection =
                          list(enabled = TRUE, style = 'width: 100px; height: 20px;background: #f8f8f8;border:none;outline:none;'))%>%
             visInteraction(navigationButtons = TRUE)%>%
             visIgraphLayout(layout = Tlayout)%>%
             visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                       ;}")%>%
             visExport(type = "png", name = "bayesian network",
                       float = "right", label = "Save network", background = "white", style= "")
           )
  },error=function(e){
    print(e)
    shinyalert(e)
  })

}
