graph.custom.assoc <- function(assocNetwork,nodeNames,Ndegree,Tlayout,shapeVector,font)
{
  tryCatch({
    nodes <- data.frame(name = nodeNames)
    nodes$id <- 0:(nrow(nodes) - 1)
    nodes$group <- "not in use"
    visNodes<- data.frame(id = nodeNames,
                          label = nodeNames,
                          group = nodes$group,
                          shape = shapeVector,
                          font.size = font)
    visEdges<- data.frame(from = assocNetwork[,1],
                          to = assocNetwork[,2],
                          title = assocNetwork[,3],
                          value=assocNetwork[,3])
    return(visNetwork(visNodes, visEdges, width = "100%") %>%
             visEdges(smooth = T,color = list(color = "grey",highlight = "black",hover = "black"),scaling=list(min=0,max=1))%>%
             visGroups(groupname = "not in use", color = list(background = "lightblue",highlight = 'blue', hover = "blue")) %>%
             visLegend(width = 0.1, position = "left",enabled=FALSE)%>%
             visNodes(shape = "dot") %>%
             visOptions(highlightNearest = list(enabled =TRUE, degree = Ndegree,hover = T, hideColor = 'rgba(200,200,200,0)'), nodesIdSelection =
                          list(enabled = TRUE, style = 'width: 100px; height: 20px;background: #f8f8f8;border:none;outline:none;'))%>%
             visInteraction(navigationButtons = TRUE)%>%
             visIgraphLayout(layout = Tlayout)%>%
             visExport(type = "pdf", name = "association network",
                       float = "right", label = "Save network", background = "white", style= "")%>%
             visEvents(select = "function(nodes) {
                       Shiny.onInputChange('Acurrent_node_id', nodes.nodes);
                       ;}")
           )
  },error=function(e){
    print(e)
  })

}
