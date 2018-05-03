# Help data for Home tab
homeHelp <-
  data.frame(
    step = c(1, 2),
    intro = c(
      "This is the sidebar menu containing three tabs. The first tab is the Home tab where you are currently present.
      The second tab is main tab. The third tab is the github link to the source code of the app.",
      "This is the Homepage of the app"
    ),
    element = c(
      "#sidebarMenu",
      "#dashboardBody"
    ),
    position = c("auto", "auto")
  )

# Help data for structure tab
dataHelp <-
  data.frame(
    step = c(1, 2, 3, 4),
    intro = c(
      "This is the Data panel",
      "Here you can choose the format and upload the data as .csv or .RData file",
      "Here you can impute the missing values in your data. MissRanger is being for the purpose",
      "Here you can discretize your data. The two available methods for discretization are quantile and interval"
    ),
    element = c(
      "#data",
      "#dataFormat",
      "#dataImpute",
      "#dataDiscretize"
    ),
    position = c("auto", "auto", "auto", "auto")
  )

# Help graph for structure tab
graphHelp <-
  data.frame(
    step = c(1, 2, 3, 4, 5),
    intro = c(
      "This is the Graph panel",
      "Here you can update network plot",
      "Select how many distance neigbour you want to see while clicking a node in the graph.
       For example, selecting chain of neighbors as 2, you will see 2-distance neighbors of the graph",
      "Select layout for the network plot. Choose from a wide range of possible layout options available.",
      "Save network plot. You can change the file name and location."
    ),
    element = c(
      "#graph",
      "#graphUpdate",
      "#graphChain",
      "#graphLayout",
      "#graphSave"
    ),
    position = c("auto", "auto", "auto", "auto", "auto")
)



