custom.Modules<-function(graph,algo)
{
  lc <- linkcomm::getLinkCommunities(graph, directed = TRUE,hcmethod = algo,verbose = FALSE,dirweight = 0.8)
  num_communities <- lc$numbers[3]
  community_list <- vector(mode="list", length=num_communities)
  for(i in 1:num_communities)
  {
    community_list[[i]] <- linkcomm::getNodesIn(lc, clusterids = c(i))
  }
  return(community_list)
}
