library('bnlearn')
library('shiny')
library('shinydashboard')
library('visNetwork')
library('shinyWidgets')
library("shinyBS")
library('shinyalert')
library('rintrojs')
library('igraph')
library("HydeNet")
library("rhandsontable")
source('graph.custom.decision.R')
source('error.bar.R')
source('graph.custom.R')
source('graph.custom.assoc.R')
source('custom.discretize.R')
source('check.NA.R')
source('check.discrete.R')
source('custom.association.R')
source('custom.Modules.R')
source('tooltip.R')
source('dashboardthemes.R')
source('graph.weight.R')
source('dependency.R')
source('custom.Modules.assoc.R')

shinyServer(function(input, output,session) {
  withProgress(message = "checking for dependencies... (may take longer on first installation)", value = 0, {
    dependency()
  })
  library('gRain')
  withProgress(message = "Initializing Dashboard", value = 0, {
  #Data upload limit and other options
  dependency()
  options(shiny.maxRequestSize=8000*1024^2)
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)
  #tooltips
  tooltip(session)
  #Structure Initialization
  #DiscreteData <- alarm
  #trueData<-DiscreteData
  #Sanity check
  sanity<-1
  confidence<-1
  check<-1
  reset<-1
  assocReset<-1
  simple<-1
  upload<-1
  uploadtype<-1
  type<-1
  load<-1
  exactCheck<-1
  #Initialization
  rvs <<- reactiveValues(evidence = list(),values = list(),evidenceObserve = list(),valueObserve = list())
  insertedV <- c()
  inserted <- c()
  rvs$evidence <- c()
  rvs$value <- c()
  rvs$evidenceObserve <- c()
  rvs$valueObserve <- c()
  nodeNamesB <- c()
  EventNode <- c()
  EvidenceNode <- c()
  shapeVector<- c()
  weight<-1
  value<-1
  #load<-1
  communities<-NULL
  Acommunities<-NULL
  graph<-NULL
  blacklistEdges<-c()
  whitelistEdges<-c()
  externalGraphEdges<-c()
  INTvar<-c()
  NetworkGraph <- NULL
  assocNetwork<-NULL
  predError<-NULL
  updateSelectInput(session,'event',choices = "")
  updateSelectizeInput(session,'varselect',choices = "")
  updateSelectizeInput(session,'Avarselect',choices = "")
  updateSelectInput(session,'paramSelect',choices = "")
  updateSelectInput(session,"tableName",choices = c("Bayesian Graph","blacklist edges","whitelist edges","Cross Validation Results","Nodes"))
  updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star","ellipse", "database", "text", "diamond"))
  updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star","ellipse", "database", "text", "diamond"))
  updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star","ellipse", "database", "text", "diamond"))
  updateSelectInput(session,'modGroup',choices = "")
  updateSelectInput(session,'Avarshape',choices = c( "dot","square", "triangle", "box", "circle", "star","ellipse", "database", "text", "diamond"))
  updateSelectInput(session,'Avarshape2',choices = c( "dot","square", "triangle", "box", "circle", "star","ellipse", "database", "text", "diamond"))
  updateSelectInput(session,'Avarshape3',choices = c( "dot","square", "triangle", "box", "circle", "star","ellipse", "database", "text", "diamond"))
  updateSelectInput(session,'graph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
  updateSelectInput(session,'Agraph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
  updateSelectInput(session,"moduleSelection",choices = "")
  updateSelectInput(session,"AmoduleSelection",choices = "")
  updateSelectInput(session,"neighbornodes",choices = "")
  updateSelectInput(session,"Aneighbornodes",choices = "")
  updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
  updateSelectInput(session,"fromarc",choices=c())
  updateSelectInput(session,"toarc",choices = c())
  #updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
  output$valLoss<-renderText({0})
  output$netScore<-renderText({0})
  output$assocPlot<-renderVisNetwork({validate("Explore the association network on your data")})

  output$netPlot<-renderVisNetwork({validate("Construct bayesian network for taking decision")})
  output$parameterPlot<-renderPlot({validate("Construct bayesian network for taking decision")})
  output$consensusPlot<-renderPlot({validate("Construct bayesian network(Bootstap Learning) for taking decision")})
  output$distPlot<-renderPlot({validate("Construct bayesian network for taking decision")})
  output$freqPlot<-renderPlot({validate("You have to preprocess data to built the plot")})
  output$tableOut<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
  output$datasetTable<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
  output$priorout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
  output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
  output$assocTable<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
  output$policyPlot<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'),rownames=FALSE)
  output$decisionPlot<-renderVisNetwork({validate("Build Decision Network using app")})
  tooltip(session)
  })
  #observe events
  output$pdfviewer <- renderText({
    return(paste('<iframe style="height:450px; width:100%" src="','wiseR.pdf', '"></iframe>', sep = ""))
  })
  observeEvent(input$start,{
    updateTabItems(session, "sidebarMenu", "Structure")
    tooltip(session)
  })
  observeEvent(input$loadDef,{
    if(input$defData=="Alarm")
    {
      DiscreteData<<-alarm
      trueData<<-DiscreteData
    }
    else if(input$defData=="Asia")
    {
      DiscreteData<<-asia
      trueData<<-DiscreteData
    }
    else if(input$defData=="Coronary")
    {
      DiscreteData<<-coronary
      trueData<<-DiscreteData
    }
    else if(input$defData=="Lizards")
    {
      DiscreteData<<-lizards
      trueData<<-DiscreteData
    }
    else if(input$defData=="Marks")
    {
      DiscreteData<<-marks
      trueData<<-DiscreteData
    }
    else if(input$defData=="Insurance")
    {
      DiscreteData<<-insurance
      trueData<<-DiscreteData
    }
    else if(input$defData=="Hailfinder")
    {
      DiscreteData<<-hailfinder
      trueData<<-DiscreteData
    }
    check.discrete(DiscreteData)
    check.NA(DiscreteData)
    DiscreteData<<-as.data.frame(DiscreteData)
    trueData<<-DiscreteData
    #Reset APP
    reset<<-1
    load<<-2
    assocReset<<-1
    blacklistEdges<<-c()
    whitelistEdges<<-c()
    externalGraphEdges<<-c()
    INTvar<<-c()
    output$valLoss<<-renderText({0})
    output$netScore<<-renderText({0})
    output$assocPlot<<-renderVisNetwork({validate("Explore the association network on your data")})
    output$netPlot<<-renderVisNetwork({validate("Construct bayesian network for taking decisions")})
    output$parameterPlot<<-renderPlot({validate("Construct bayesian network for taking decisions")})
    output$consensusPlot<-renderPlot({validate("Construct bayesian network(Bootstap Learning) for taking decision")})
    output$distPlot<<-renderPlot({validate("Construct bayesian network for taking decision")})
    output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
    output$priorout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
    output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
    NetworkGraph <<- NULL
    assocNetwork<<-NULL
    predError<<-NULL
    for(elem in 1:length(inserted))
    {
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', inserted[elem])
      )

    }
    inserted <<- c()
    for(elem2 in 1:length(insertedV))
    {
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', insertedV[elem2])
      )

    }
    insertedV <<- c()
    rvs$evidence <<- c()
    rvs$value <<- c()
    rvs$evidenceObserve <<- c()
    rvs$valueObserve <<- c()
    nodeNamesB <<- c()
    EventNode <<- c()
    EvidenceNode <<- c()
    shapeVector<<- c()
    weight<<-1
    value<<-1
    bn.start<<- empty.graph(names(DiscreteData))
    communities<<-NULL
    Acommunities<<-NULL
    graph<<-NULL
    updateSelectInput(session,'event',choices = "")
    updateSelectizeInput(session,'varselect',choices = "")
    updateSelectInput(session,'paramSelect',choices = "")
    updateSelectInput(session,"moduleSelection",choices = "")
    updateSelectInput(session,"neighbornodes",choices = "")
    updateSelectInput(session,"Aneighbornodes",choices = "")
    updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
    updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
    updateSelectInput(session,"delSelect",choices = names(DiscreteData))
    updateSelectInput(session,"facSelect",choices = names(DiscreteData))
    updateSelectInput(session,"numSelect",choices = names(DiscreteData))
    updateSelectInput(session,"intSelect",choices = names(DiscreteData))
    updateSelectInput(session,"fromarc",choices=c())
    updateSelectInput(session,"toarc",choices = c())
    updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
    updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star","ellipse", "database", "text", "diamond"))
    updateSelectInput(session,'modGroup',choices = "")
    updateSelectInput(session,'AmodGroup',choices = "")
    updateSelectInput(session,"AmoduleSelection",choices = "")
    updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
    output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
  })
  observeEvent(input$intervention,{
    if(load==2)
    {
      if(check.NA(DiscreteData))
      {
        shinyalert::shinyalert("Impute missing data using pre-process tab to procede",type="info")
      }
      else if(check.discrete(DiscreteData))
      {
        shinyalert::shinyalert("Discretize data using pre-process tab to proceed",type="info")
      }
      else
      {
        INTVar = sapply(c(1:(dim(DiscreteData)[2]-1)), function(x) {which(DiscreteData[[input$intSelect]] == x) })
        DiscreteData[[input$intSelect]]=NULL
        DiscreteData<<-DiscreteData
        trueData<<-DiscreteData
        nodes = names(DiscreteData)
        names(INTVar) = nodes
        check.discrete(DiscreteData)
        check.NA(DiscreteData)
        DiscreteData<<-as.data.frame(DiscreteData)
        trueData<<-DiscreteData
        #Reset APP
        reset<<-1
        load<<-2
        assocReset<<-1
        blacklistEdges<<-c()
        whitelistEdges<<-c()
        externalGraphEdges<<-c()
        output$valLoss<<-renderText({0})
        output$netScore<<-renderText({0})
        output$assocPlot<<-renderVisNetwork({validate("Explore the association network on your data")})
        output$netPlot<<-renderVisNetwork({validate("Construct bayesian network for taking decisions")})
        output$parameterPlot<<-renderPlot({validate("Construct bayesian network for taking decisions")})
        output$consensusPlot<-renderPlot({validate("Construct bayesian network(Bootstap Learning) for taking decision")})
        output$distPlot<<-renderPlot({validate("Construct bayesian network for taking decision")})
        output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
        NetworkGraph <<- NULL
        assocNetwork<<-NULL
        predError<<-NULL
        for(elem in 1:length(inserted))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[elem])
          )

        }
        inserted <<- c()
        for(elem2 in 1:length(insertedV))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', insertedV[elem2])
          )

        }
        insertedV <<- c()
        rvs$evidence <<- c()
        rvs$value <<- c()
        rvs$evidenceObserve <<- c()
        rvs$valueObserve <<- c()
        nodeNamesB <<- c()
        EventNode <<- c()
        EvidenceNode <<- c()
        shapeVector<<- c()
        weight<<-1
        value<<-1
        bn.start<<- empty.graph(names(DiscreteData))
        communities<<-NULL
        Acommunities<<-NULL
        graph<<-NULL
        updateSelectInput(session,'event',choices = "")
        updateSelectizeInput(session,'varselect',choices = "")
        updateSelectInput(session,'paramSelect',choices = "")
        updateSelectInput(session,"moduleSelection",choices = "")
        updateSelectInput(session,"neighbornodes",choices = "")
        updateSelectInput(session,"Aneighbornodes",choices = "")
        updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
        updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
        updateSelectInput(session,"delSelect",choices = names(DiscreteData))
        updateSelectInput(session,"numSelect",choices = names(DiscreteData))
        updateSelectInput(session,"facSelect",choices = names(DiscreteData))
        updateSelectInput(session,"intSelect",choices = names(DiscreteData))
        updateSelectInput(session,"fromarc",choices=c())
        updateSelectInput(session,"toarc",choices = c())
        updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
        updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star","ellipse", "database", "text", "diamond"))
        updateSelectInput(session,'modGroup',choices = "")
        updateSelectInput(session,'AmodGroup',choices = "")
        updateSelectInput(session,"AmoduleSelection",choices = "")
        output$priorout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
      }
    }
  })
  observeEvent(input$tableName,{
    if(load==2)
    {
      tryCatch({
        if(input$tableName=="Bayesian Graph")
        {
          output$tableOut<- DT::renderDataTable({NetworkGraph},options = list(scrollX = TRUE,pageLength = 10))
        }
        else if(input$tableName=="Cross Validation Results")
        {
          output$tableOut<- DT::renderDataTable({predError},options = list(scrollX = TRUE,pageLength = 10))
        }
        else if(input$tableName=="blacklist edges")
        {
          output$tableOut<- DT::renderDataTable({blacklistEdges},options = list(scrollX = TRUE,pageLength = 10))
        }
        else if(input$tableName=="whitelist edges")
        {
          output$tableOut<- DT::renderDataTable({whitelistEdges},options = list(scrollX = TRUE,pageLength = 10))
        }
        else if(input$tableName=="Nodes")
        {
          output$tableOut<- DT::renderDataTable({as.data.frame(nodeNamesB)},options = list(scrollX = TRUE,pageLength = 10))
        }
      },error=function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
      tooltip(session)
    }
  })
  observeEvent(input$listFile,{
    if(load==2)
    {
      tryCatch({
        file=input$listFile
        if(input$listType=="Blacklist")
        {
          blacklistEdges=read.csv(file$datapath,stringsAsFactors = T,na.strings = c("NA","na","Na","nA","","?","-"))
          blacklistEdges<<-as.data.frame(blacklistEdges)
          if(dim(blacklistEdges)[2]!=2)
          {
            blacklistEdges<<-c()
            shinyalert::shinyalert("Upload a .csv file containg edges in format 'from' and 'to'",type="error")
          }
          else if(!(unique(blacklistEdges[,1],blacklistEdges[,2]) %in% colnames(DiscreteData)))
          {
            blacklistEdges<<-c()
            shinyalert::shinyalert("Upload a correct file containg only nodes as observed in the data",type="error")
          }
        }
        else
        {
          whitelistEdges=read.csv(file$datapath,stringsAsFactors = T,na.strings = c("NA","na","Na","nA","","?","-"))
          whitelistEdges<<-as.data.frame(whitelistEdges)
          if(dim(whitelistEdges)[2]!=2)
          {
            whitelistEdges<<-c()
            shinyalert::shinyalert("Upload a .csv file containg edges in format 'from' and 'to'",type="error")
          }
          else if(!(unique(whitelistEdges[,1],whitelistEdges[,2]) %in% colnames(DiscreteData)))
          {
            whitelistEdges<<-c()
            shinyalert::shinyalert("Upload a correct file containg only nodes as observed in the data",type="error")
          }
        }
      },error=function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
      tooltip(session)
    }
  })
  observeEvent(input$externalGraph,{
    if(load==2)
    {
      tryCatch({
        file=input$externalGraph
        externalGraphEdges=read.csv(file$datapath,stringsAsFactors = T,na.strings = c("NA","na","Na","nA","","?","-"))
        externalGraphEdges<<-as.data.frame(externalGraphEdges)
        if(dim(externalGraphEdges)[2]!=2)
        {
          externalGraphEdges<<-c()
          shinyalert::shinyalert("Upload a .csv file containg edges in format 'from' and 'to'",type="error")
        }
        else if(!(unique(externalGraphEdges[,1],externalGraphEdges[,2]) %in% colnames(DiscreteData)))
        {
          externalGraphEdges<<-c()
          shinyalert::shinyalert("Upload a correct file containg only nodes as observed in the data",type="error")
        }
      },error=function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
      tooltip(session)
    }
  })
  observeEvent(input$priorFile,{
    if(load==2)
    {
      fileName<<- input$priorFile
      #print(fileName)
      tryCatch({
        edgeList<-read.csv(fileName$datapath,stringsAsFactors = T,na.strings = c("NA","na","Na","nA","","?","-"))
        edgeList<-as.data.frame(edgeList)
        for(i in 1:dim(edgeList)[1])
        {
          bn.start<<-set.arc(bn.start,as.character(edgeList[i,1]),as.character(edgeList[i,2]))
        }
        output$priorout<-DT::renderDataTable({bn.start$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        shinyalert::shinyalert("File Successfully Uploaded",type="success")
      },error = function(e){
        print(e)
        shinyalert::shinyalert(e,type = "error")
      })
      tooltip(session)
    }
  })
  observeEvent(input$fromarc1,{
    if(load==2)
    {
      tryCatch({
        updateSelectInput(session,"toarc1",choices = setdiff(names(DiscreteData),input$fromarc1))
      },error=function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
      tooltip(session)
    }
  })
  observeEvent(input$addarc1,{
    if(load==2)
    {
      tryCatch({
        bn.start<<-set.arc(bn.start,input$fromarc1,input$toarc1)
        output$priorout<-DT::renderDataTable({bn.start$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
      },error=function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
      tooltip(session)
    }
  })
  observeEvent(input$RemoveArc,{
    if(load==2)
    {
      tryCatch({
        if(!is.null(input$priorout_rows_selected))
        {
          bn.start<<-drop.arc(bn.start,bn.start$arcs[input$priorout_rows_selected,1],bn.start$arcs[input$priorout_rows_selected,2])
          output$priorout<-DT::renderDataTable({bn.start$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        }
      },error=function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
      tooltip(session)
    }
  })
  observeEvent(input$ReverseArc,{
    if(load==2)
    {
      tryCatch({
        if(!is.null(input$priorout_rows_selected))
        {
          bn.start<<-reverse.arc(bn.start,bn.start$arcs[input$priorout_rows_selected,1],bn.start$arcs[input$priorout_rows_selected,2])
          output$priorout<-DT::renderDataTable({bn.start$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        }
      },error=function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
      tooltip(session)
    }
  })
  observeEvent(input$threshold,{
    if(load==2)
    {
      tryCatch({
        if(assocReset==2)
        {
          if(check.NA(DiscreteData))
          {
            shinyalert::shinyalert("Impute missing data using pre-process tab to procede",type="info")
          }
          else if(check.discrete(DiscreteData))
          {
            shinyalert::shinyalert("Discretize data using pre-process tab to proceed",type="info")
          }
          else
          {
            assocNetworkprune<<- assocNetwork[which(assocNetwork[,3]>input$threshold),]
            shapeVectorAssoc<<- rep('dot',length(unique(c(assocNetworkprune[,1],assocNetworkprune[,2]))))
            updateSelectizeInput(session,'Avarselect',choices = unique(c(assocNetworkprune[,1],assocNetworkprune[,2])))
            output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),input$Adegree,input$Agraph_layout,shapeVectorAssoc,input$assocFont)})
            Agraph<<-igraph::graph_from_edgelist(as.matrix(assocNetworkprune[,1:2]),directed = F)
            updateSelectInput(session,"Aneighbornodes",choices = "")
            output$assocTable<- DT::renderDataTable({assocNetworkprune},options = list(scrollX = TRUE,pageLength = 10))
            updateSelectInput(session,"AmoduleSelection",choices = "graph")
            updateSelectInput(session,'AmodGroup',choices = "")
          }
        }
      },error=function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
      tooltip(session)
    }
  })
  observeEvent(input$association,{
    if(load==2)
    {
      withProgress(message = "Building Association Graph", value = 0, {
        tryCatch({
          if(check.NA(DiscreteData))
          {
            shinyalert::shinyalert("Impute missing data using pre-process tab to procede",type="info")
          }
          else if(check.discrete(DiscreteData))
          {
            shinyalert::shinyalert("Discretize data using pre-process tab to proceed",type="info")
          }
          else
          {
            assocNetwork<<-custom.association(DiscreteData,input$assocType)
            colnames(assocNetwork)<<-c("V1","V2","Strength of Association")
            assocNetworkprune<<- assocNetwork[which(assocNetwork[,3]>input$threshold),]
            shapeVectorAssoc<<- rep('dot',length(unique(c(assocNetworkprune[,1],assocNetworkprune[,2]))))
            updateSelectizeInput(session,'Avarselect',choices = unique(c(assocNetworkprune[,1],assocNetworkprune[,2])))
            output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),input$Adegree,input$Agraph_layout,shapeVectorAssoc,input$assocFont)})
            assocReset<<-2
            updateSelectInput(session,"Aneighbornodes",choices = "")
            Agraph<<-igraph::graph_from_edgelist(as.matrix(assocNetworkprune[,1:2]),directed = F)
            shinyalert::shinyalert("Association graph successfully built",type="success")
            output$assocTable<- DT::renderDataTable({assocNetworkprune},options = list(scrollX = TRUE,pageLength = 10))
            updateSelectInput(session,"AmoduleSelection",choices = "graph")
            updateSelectInput(session,'AmodGroup',choices = "")
          }
        },error=function(e){
          shinyalert::shinyalert(toString(e), type = "error")
        })
      })
      tooltip(session)
    }
  })
  observeEvent(input$calLoss,{
    if(load==2)
    {
      tryCatch({
        if(reset==2)
        {
          withProgress(message = "Validating Model", value = 0, {
            if(input$parallel==T)
            {
              bn.validate<<-bn.cv(DiscreteData[,nodeNamesB],bn=bn.hc.boot.average,fit = input$paramMethod3,method = input$crossFunc,cluster = cl)
              predError<<-c()
              for(n in nodeNamesB)
              {
                targetLoss<<-bn.cv(DiscreteData[,nodeNamesB],bn=bn.hc.boot.average,fit = input$paramMethod3,loss = input$lossFunc,method = input$crossFunc,loss.args = list(target = n),cluster = cl,k=10)
                predError<<-rbind(predError,targetLoss[[1]]$loss)
              }
              rownames(predError)<<-nodeNamesB
              colnames(predError)<<-"Classification Error"
              output$valLoss<<-renderText({bn.validate[[1]]$loss})
            }
            else
            {
              bn.validate<<-bn.cv(DiscreteData[,nodeNamesB],bn=bn.hc.boot.average,fit = input$paramMethod3,method = input$crossFunc,k=10)
              predError<<-c()
              for(n in nodeNamesB)
              {
                targetLoss<<-bn.cv(DiscreteData[,nodeNamesB],bn=bn.hc.boot.average,fit = input$paramMethod3,loss = input$lossFunc,method = input$crossFunc,loss.args = list(target = n),k=10)
                predError<<-rbind(predError,targetLoss[[1]]$loss)
              }
              rownames(predError)<<-nodeNamesB
              colnames(predError)<<-"Classification Error"
              output$valLoss<<-renderText({bn.validate[[1]]$loss})
            }
          })
        }
        else
        {
          shinyalert::shinyalert("Construct bayesian network for taking decisions", type = "info")
        }
      },error=function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
      tooltip(session)
    }
  })
  observeEvent(input$getScore,{
    if(load==2)
    {
      tryCatch({
        if(reset==2)
        {
          scoreVal<<- score(bn.hc.boot.average,DiscreteData,type=input$scoreAlgo)
          output$netScore<<-renderText({scoreVal})
        }
        else
        {
          shinyalert::shinyalert("Construct bayesian network for take decisions", type = "info")
        }
      },error=function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
      tooltip(session)
    }
  })
  output$downloadDataset<-downloadHandler(
    filename = function(){
      paste('dataset',".csv",sep = "")
    },
    content = function(filename){
      if(load==2)
      {
        write.csv(DiscreteData,file=filename,row.names = F)
      }
    }
  )
  output$assocDownload<-downloadHandler(
    filename = function(){
      paste('association graph',".csv",sep = "")
    },
    content = function(filename){
      if(load==2)
      {
        write.csv(assocNetwork,file=filename,row.names = F)
      }
    }
  )
  output$bSave<-downloadHandler(
    filename = function(){
      paste('bayesian graph',".html",sep = "")
    },
    content = function(filename){
      if(load==2)
      {
          bgNet <- graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)
          visSave(bgNet, file = filename)
      }
    }
  )
  output$aSave<-downloadHandler(
    filename = function(){
      paste('association graph',".html",sep = "")
    },
    content = function(filename){
      if(load==2)
      {
        agNet <- graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),input$Adegree,input$Agraph_layout,shapeVectorAssoc,input$assocFont)
        visSave(agNet, file = filename)
      }
    }
  )
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$tableName, ".csv", sep = "")
    },
    content = function(file) {
      if(load==2)
      {
        if(input$tableName=="Bayesian Graph")
        {
          write.csv(NetworkGraph, file,row.names = F)
        }
        else if(input$tableName=="Cross Validation Results")
        {
          write.csv(predError, file,row.names=F)
        }
        else if(input$tableName=="blacklist edges")
        {
          write.csv(blacklistC, file,row.names = F)
        }
        else if(input$tableName=="whitelist edges")
        {
          write.csv(whitelistC, file,row.names=F)
        }
        else if(input$tableName=="Nodes")
        {
          write.csv(nodeNamesB,file,row.names = F)
        }


      }
    }
  )
  output$saveBtn<-downloadHandler(
    filename = function() {
      paste('structure', ".RData", sep = "")
    },
    content = function(filename) {
      if(load==2)
      {
        tryCatch({
          if(reset==2)
          {
            save(bn.hc.boot.average,file=filename)
          }
          else
          {
            shinyalert::shinyalert("Construct bayesian network for taking decisions",type="info")
          }
        })
      }
    }
  )
  output$saveBtnBoot<-downloadHandler(
    filename = function() {
      paste('BootstrapObject', ".RData", sep = "")
    },
    content = function(filename) {
      if(load==2)
      {
        tryCatch({
          if(reset==2)
          {
            if(type == 2)
            {
              save(bn.hc.boot,file=filename)
            }
          }
          else
          {
            shinyalert::shinyalert("Construct bayesian network for taking decisions",type="info")
          }
        },error = function(e){
          shinyalert::shinyalert("No bootstrap object found", type = "error")
        })
      }
    }
  )
  #Data Frame From User
  observeEvent(input$dataFile,{
    inFile <- input$dataFile
    if (is.null(inFile))
    {
      shinyalert::shinyalert("Data file is empty, upload a valid data file",type = "error")
    }
    else
    {
      tryCatch({
        if(input$format==".RData")
        {
          if(tools::file_ext(inFile$datapath) == "RData")
          {
            tryCatch({
              DiscreteData <<- get(load(inFile$datapath))
              },error = function(e){
                DiscreteData<<- readRDS(inFile$datapath)
              })
          }
          else
          {
            shinyalert::shinyalert("Added file is not a .RData file.Upload a RData file.", type = "error")
          }

        }
        else if(input$format==".CSV")
        {
          if(tools::file_ext(inFile$datapath) == "csv")
          {
            DiscreteData <<- read.csv(inFile$datapath,stringsAsFactors = T,na.strings = c("NA","na","Na","nA","","?","-"))
          }
          else
          {
            shinyalert::shinyalert("Added file is not a .csv file.Upload a CSV file.", type = "error")
          }
        }
        else
        {
          tryCatch({
            if(input$format=="Comma Seperated")
            {
              DiscreteData <<- read.csv(inFile$datapath,stringsAsFactors = T,na.strings = c("NA","na","Na","nA","","?","-"),sep = ",")
            }
            else if(input$format=="Semicolon Seperated")
            {
              DiscreteData <<- read.csv(inFile$datapath,stringsAsFactors = T,na.strings = c("NA","na","Na","nA","","?","-"),sep = ";")
            }
            else if(input$format=="Space Seperated")
            {
              DiscreteData <<- read.csv(inFile$datapath,stringsAsFactors = T,na.strings = c("NA","na","Na","nA","","?","-"),sep = " ")
            }
            else
            {
              DiscreteData <<- read.csv(inFile$datapath,stringsAsFactors = T,na.strings = c("NA","na","Na","nA","","?","-"),sep = "\t")
            }
          })
        }
        DiscreteData<<-as.data.frame(DiscreteData)
        if(input$factorCheck==T)
        {
          for(nms in names(DiscreteData))
          {
            tryCatch({
              DiscreteData[,nms]<<-as.factor(DiscreteData[,nms])
              if(nlevels(DiscreteData[,nms])<2)
              {
                DiscreteData[,nms]<<-NULL
              }
              if(nlevels(DiscreteData[,nms]>52))
              {
                DiscreteData[,nms]<<-as.numeric(as.character(DiscreteData[,nms]))
              }
            },error=function(e){
              DiscreteData[,nms]<<-as.numeric(as.character(DiscreteData[,nms]))
            })
          }
        }
        check.discrete(DiscreteData)
        check.NA(DiscreteData)
        trueData<<-DiscreteData
        #Reset APP
        reset<<-1
        assocReset<<-1
        load<<-2
        blacklistEdges<<-c()
        whitelistEdges<<-c()
        externalGraphEdges<<-c()
        INTvar<<-c()
        updateSelectInput(session,"intSelect",choices = names(DiscreteData))
        output$valLoss<<-renderText({0})
        output$netScore<<-renderText({0})
        output$assocPlot<<-renderVisNetwork({validate("Explore the association network on your data")})
        output$netPlot<<-renderVisNetwork({validate("Construct a Bayesian Network for taking decisions")})
        output$parameterPlot<<-renderPlot({validate("Construct a Bayesian Network for taking decisions")})
        output$consensusPlot<-renderPlot({validate("Construct bayesian network(Bootstap Learning) for taking decision")})
        output$distPlot<<-renderPlot({validate("Construct a Bayesian Network for taking decisions")})
        output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
        NetworkGraph <<- NULL
        assocNetwork<<-NULL
        predError<<-NULL
        for(elem in 1:length(inserted))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[elem])
          )

        }
        inserted <<- c()
        for(elem2 in 1:length(insertedV))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', insertedV[elem2])
          )

        }
        insertedV <<- c()
        rvs$evidence <<- c()
        rvs$value <<- c()
        rvs$evidenceObserve <<- c()
        rvs$valueObserve <<- c()
        nodeNamesB <<- c()
        EventNode <<- c()
        EvidenceNode <<- c()
        shapeVector<<- c()
        weight<<-1
        value<<-1
        bn.start<<- empty.graph(names(DiscreteData))
        communities<<-NULL
        Acommunities<<-NULL
        graph<<-NULL
        updateSelectInput(session,'event',choices = "")
        updateSelectizeInput(session,'varselect',choices = "")
        updateSelectInput(session,'paramSelect',choices = "")
        updateSelectInput(session,"moduleSelection",choices = "")
        updateSelectInput(session,"neighbornodes",choices = "")
        updateSelectInput(session,"Aneighbornodes",choices = "")
        updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
        updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
        updateSelectInput(session,"delSelect",choices = names(DiscreteData))
        updateSelectInput(session,"facSelect",choices = names(DiscreteData))
        updateSelectInput(session,"numSelect",choices = names(DiscreteData))
        updateSelectInput(session,"fromarc",choices=c())
        updateSelectInput(session,"toarc",choices = c())
        updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
        updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star","ellipse", "database", "text", "diamond"))
        updateSelectInput(session,'modGroup',choices = "")
        updateSelectInput(session,'AmodGroup',choices = "")
        updateSelectInput(session,"AmoduleSelection",choices = "")
        output$priorout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        },error = function(e){
             shinyalert::shinyalert(c("Error in loading data: ",toString(e)), type = "error")
           })
    }
    tooltip(session)
    })
  observeEvent(input$numconv,{
    if(load==2)
    {
      print(eval(parse(text =input$numSelect2)))
      tryCatch({
        tryCatch({
          DiscreteData[,input$numSelect]<<-as.numeric(as.character(DiscreteData[,input$numSelect]))
          output$freqPlot<<-renderPlot({validate("Make sure data is complete and discretized before using this feature")})
        },error=function(e)
        {
          shinyalert("Failed to convert variable to numeric",type="error")
        })
        updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
        trueData<<-DiscreteData
        output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
        reset<<-1
        assocReset<<-1
        #shinyalert("Discretization successfull",type="success")
        weight<<-1
        value<<-1
        blacklistEdges<<-c()
        whitelistEdges<<-c()
        externalGraphEdges<<-c()
        INTvar<<-c()
        output$valLoss<<-renderText({0})
        output$netScore<<-renderText({0})
        output$assocPlot<<-renderVisNetwork({validate("Explore the association network on your data")})
        output$netPlot<<-renderVisNetwork({validate("Construct bayesian network for taking decisions")})
        output$parameterPlot<<-renderPlot({validate("Construct bayesina network for taking decisions")})
        output$consensusPlot<-renderPlot({validate("Construct bayesian network(Bootstap Learning) for taking decision")})
        output$distPlot<<-renderPlot({validate("Construct bayesian netowrk for takinng decision")})
        output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
        NetworkGraph <<- NULL
        assocNetwork<<-NULL
        predError<<-NULL
        for(elem in 1:length(inserted))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[elem])
          )

        }
        inserted <<- c()
        for(elem2 in 1:length(insertedV))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', insertedV[elem2])
          )

        }
        insertedV <<- c()
        rvs$evidence <<- c()
        rvs$value <<- c()
        rvs$evidenceObserve <<- c()
        rvs$valueObserve <<- c()
        nodeNamesB <<- c()
        EventNode <<- c()
        EvidenceNode <<- c()
        shapeVector<<- c()
        bn.start<<- empty.graph(names(DiscreteData))
        communities<<-NULL
        Acommunities<<-NULL
        graph<<-NULL
        updateSelectInput(session,'event',choices = "")
        updateSelectizeInput(session,'varselect',choices = "")
        updateSelectInput(session,'paramSelect',choices = "")
        updateSelectInput(session,"moduleSelection",choices = "")
        updateSelectInput(session,"neighbornodes",choices = "")
        updateSelectInput(session,"Aneighbornodes",choices = "")
        updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
        updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
        updateSelectInput(session,"delSelect",choices = names(DiscreteData))
        updateSelectInput(session,"facSelect",choices = names(DiscreteData))
        updateSelectInput(session,"numSelect",choices = names(DiscreteData))
        updateSelectInput(session,"fromarc",choices=c())
        updateSelectInput(session,"toarc",choices = c())
        updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
        updateSelectInput(session,'modGroup',choices = "")
        updateSelectInput(session,'AmodGroup',choices = "")
        updateSelectInput(session,"intSelect",choices = names(DiscreteData))
        updateSelectInput(session,"AmoduleSelection",choices = "")
        output$priorout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
      },error=function(e){
        shinyalert(toString(e),type = "error")
      })
    }
  })
  observeEvent(input$numconv2,{
    if(load==2)
    {
      numList<-eval(parse(text =input$numSelect2))
      tryCatch({
        tryCatch({
          for(i in numList)
          {
            DiscreteData[,i]<<-as.numeric(as.character(DiscreteData[,i]))
          }
          output$freqPlot<<-renderPlot({validate("Make sure data is complete and discretized before using this feature")})
        },error=function(e)
        {
          shinyalert("Failed to convert variable to numeric",type="error")
        })
        updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
        trueData<<-DiscreteData
        output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
        reset<<-1
        assocReset<<-1
        #shinyalert("Discretization successfull",type="success")
        weight<<-1
        value<<-1
        blacklistEdges<<-c()
        whitelistEdges<<-c()
        externalGraphEdges<<-c()
        INTvar<<-c()
        output$valLoss<<-renderText({0})
        output$netScore<<-renderText({0})
        output$assocPlot<<-renderVisNetwork({validate("Explore the association network on your data")})
        output$netPlot<<-renderVisNetwork({validate("Construct bayesian network for taking decisions")})
        output$parameterPlot<<-renderPlot({validate("Construct bayesina network for taking decisions")})
        output$consensusPlot<-renderPlot({validate("Construct bayesian network(Bootstap Learning) for taking decision")})
        output$distPlot<<-renderPlot({validate("Construct bayesian netowrk for takinng decision")})
        output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
        NetworkGraph <<- NULL
        assocNetwork<<-NULL
        predError<<-NULL
        for(elem in 1:length(inserted))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[elem])
          )

        }
        inserted <<- c()
        for(elem2 in 1:length(insertedV))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', insertedV[elem2])
          )

        }
        insertedV <<- c()
        rvs$evidence <<- c()
        rvs$value <<- c()
        rvs$evidenceObserve <<- c()
        rvs$valueObserve <<- c()
        nodeNamesB <<- c()
        EventNode <<- c()
        EvidenceNode <<- c()
        shapeVector<<- c()
        bn.start<<- empty.graph(names(DiscreteData))
        communities<<-NULL
        Acommunities<<-NULL
        graph<<-NULL
        updateSelectInput(session,'event',choices = "")
        updateSelectizeInput(session,'varselect',choices = "")
        updateSelectInput(session,'paramSelect',choices = "")
        updateSelectInput(session,"moduleSelection",choices = "")
        updateSelectInput(session,"neighbornodes",choices = "")
        updateSelectInput(session,"Aneighbornodes",choices = "")
        updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
        updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
        updateSelectInput(session,"delSelect",choices = names(DiscreteData))
        updateSelectInput(session,"facSelect",choices = names(DiscreteData))
        updateSelectInput(session,"numSelect",choices = names(DiscreteData))
        updateSelectInput(session,"fromarc",choices=c())
        updateSelectInput(session,"toarc",choices = c())
        updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
        updateSelectInput(session,'modGroup',choices = "")
        updateSelectInput(session,'AmodGroup',choices = "")
        updateSelectInput(session,"intSelect",choices = names(DiscreteData))
        updateSelectInput(session,"AmoduleSelection",choices = "")
        output$priorout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
      },error=function(e){
        shinyalert(toString(e),type = "error")
      })
    }
  })
  observeEvent(input$facconv,{
    if(load==2)
    {
      tryCatch({
        tryCatch({
          DiscreteData[,input$facSelect]<<-as.factor(DiscreteData[,input$facSelect])
          if(nlevels(DiscreteData[,input$facSelect])>52)
          {
            DiscreteData[,input$facSelect]<<-as.numeric(DiscreteData[,input$facSelect])
            output$freqPlot<<-renderPlot({validate("Make sure data is complete and discretized before using this feature")})
            shinyalert("Failed to convert variable to factor,has levels greater than 52",type="error")
          }
        },error=function(e)
        {
          shinyalert("Failed to convert variable to factor",type="error")
        })
        if(load==2)
        {
          if(check.NA(DiscreteData))
          {
            shinyalert::shinyalert("Impute missing data using pre-process tab to procede",type="info")
            output$freqPlot<<-renderPlot({validate("Make sure data is complete and discretized before using this feature")})
          }
          else if(check.discrete(DiscreteData))
          {
            shinyalert::shinyalert("Discretize data using pre-process tab to proceed",type="info")
            output$freqPlot<<-renderPlot({validate("Make sure data is complete and discretized before using this feature")})
          }
          else
          {
            tryCatch({
              val = table(DiscreteData[,input$freqSelect])/nrow(DiscreteData)
              output$freqPlot = renderPlot({par(mar=c(5,3,3,3))
                par(oma=c(5,3,3,3))
                barx <<-barplot(val,
                                col = "lightblue",
                                main = paste("Observed frequency of ",input$freqSelect),
                                border = NA,
                                xlab = "",
                                ylab = "Frequency",
                                ylim = c(0,1),
                                las=2,
                                cex.names=as.numeric(input$plotFont))
                text(x = barx,y = round(val,digits = 4),label = round(val,digits = 4), pos = 3, cex = as.numeric(input$valueFont), col = "black")})
            },error=function(e){
              if(input$freqSelect=="")
              {

              }
              else
              {
                shinyalert::shinyalert(toString(e),type="error")
              }
            })
          }
          tooltip(session)
        }
        trueData<<-DiscreteData
        output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
        reset<<-1
        assocReset<<-1
        #shinyalert("Discretization successfull",type="success")
        weight<<-1
        value<<-1
        blacklistEdges<<-c()
        whitelistEdges<<-c()
        externalGraphEdges<<-c()
        INTvar<<-c()
        output$valLoss<<-renderText({0})
        output$netScore<<-renderText({0})
        output$assocPlot<<-renderVisNetwork({validate("Explore the association network on your data")})
        output$netPlot<<-renderVisNetwork({validate("Construct bayesian network for taking decisions")})
        output$parameterPlot<<-renderPlot({validate("Construct bayesina network for taking decisions")})
        output$consensusPlot<-renderPlot({validate("Construct bayesian network(Bootstap Learning) for taking decision")})
        output$distPlot<<-renderPlot({validate("Construct bayesian netowrk for takinng decision")})
        output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
        NetworkGraph <<- NULL
        assocNetwork<<-NULL
        predError<<-NULL
        for(elem in 1:length(inserted))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[elem])
          )

        }
        inserted <<- c()
        for(elem2 in 1:length(insertedV))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', insertedV[elem2])
          )

        }
        insertedV <<- c()
        rvs$evidence <<- c()
        rvs$value <<- c()
        rvs$evidenceObserve <<- c()
        rvs$valueObserve <<- c()
        nodeNamesB <<- c()
        EventNode <<- c()
        EvidenceNode <<- c()
        shapeVector<<- c()
        bn.start<<- empty.graph(names(DiscreteData))
        communities<<-NULL
        Acommunities<<-NULL
        graph<<-NULL
        updateSelectInput(session,'event',choices = "")
        updateSelectizeInput(session,'varselect',choices = "")
        updateSelectInput(session,'paramSelect',choices = "")
        updateSelectInput(session,"moduleSelection",choices = "")
        updateSelectInput(session,"neighbornodes",choices = "")
        updateSelectInput(session,"Aneighbornodes",choices = "")
        updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
        updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
        updateSelectInput(session,"delSelect",choices = names(DiscreteData))
        updateSelectInput(session,"facSelect",choices = names(DiscreteData))
        updateSelectInput(session,"numSelect",choices = names(DiscreteData))
        updateSelectInput(session,"fromarc",choices=c())
        updateSelectInput(session,"toarc",choices = c())
        updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
        updateSelectInput(session,'modGroup',choices = "")
        updateSelectInput(session,'AmodGroup',choices = "")
        updateSelectInput(session,"intSelect",choices = names(DiscreteData))
        updateSelectInput(session,"AmoduleSelection",choices = "")
        output$priorout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
      },error=function(e){
        shinyalert(toString(e),type = "error")
      })
    }
  })
  observeEvent(input$facconv2,{
    if(load==2)
    {
      charList<-eval(parse(text =input$facSelect2))
      tryCatch({
        tryCatch({
          for(i in charList)
          {
            DiscreteData[,i]<<-as.factor(DiscreteData[,i])
            if(nlevels(DiscreteData[,i])>52)
            {
              DiscreteData[,i]<<-as.numeric(as.character(DiscreteData[,i]))
              output$freqPlot<<-renderPlot({validate("Make sure data is complete and discretized before using this feature")})
              shinyalert("Failed to convert variable to factor,has levels greater than 52",type="error")
            }
          }
        },error=function(e)
        {
          shinyalert("Failed to convert variable to factor",type="error")
        })
        if(load==2)
        {
          if(check.NA(DiscreteData))
          {
            shinyalert::shinyalert("Impute missing data using pre-process tab to procede",type="info")
            output$freqPlot<<-renderPlot({validate("Make sure data is complete and discretized before using this feature")})
          }
          else if(check.discrete(DiscreteData))
          {
            shinyalert::shinyalert("Discretize data using pre-process tab to proceed",type="info")
            output$freqPlot<<-renderPlot({validate("Make sure data is complete and discretized before using this feature")})
          }
          else
          {
            tryCatch({
              val = table(DiscreteData[,input$freqSelect])/nrow(DiscreteData)
              output$freqPlot = renderPlot({par(mar=c(5,3,3,3))
                par(oma=c(5,3,3,3))
                barx <<-barplot(val,
                                col = "lightblue",
                                main = paste("Observed frequency of ",input$freqSelect),
                                border = NA,
                                xlab = "",
                                ylab = "Frequency",
                                ylim = c(0,1),
                                las=2,
                                cex.names=as.numeric(input$plotFont))
                text(x = barx,y = round(val,digits = 4),label = round(val,digits = 4), pos = 3, cex = as.numeric(input$valueFont), col = "black")})
            },error=function(e){
              if(input$freqSelect=="")
              {

              }
              else
              {
                shinyalert::shinyalert(toString(e),type="error")
              }
            })
          }
          tooltip(session)
        }
        trueData<<-DiscreteData
        output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
        reset<<-1
        assocReset<<-1
        #shinyalert("Discretization successfull",type="success")
        weight<<-1
        value<<-1
        blacklistEdges<<-c()
        whitelistEdges<<-c()
        externalGraphEdges<<-c()
        INTvar<<-c()
        output$valLoss<<-renderText({0})
        output$netScore<<-renderText({0})
        output$assocPlot<<-renderVisNetwork({validate("Explore the association network on your data")})
        output$netPlot<<-renderVisNetwork({validate("Construct bayesian network for taking decisions")})
        output$parameterPlot<<-renderPlot({validate("Construct bayesina network for taking decisions")})
        output$consensusPlot<-renderPlot({validate("Construct bayesian network(Bootstap Learning) for taking decision")})
        output$distPlot<<-renderPlot({validate("Construct bayesian netowrk for takinng decision")})
        output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
        NetworkGraph <<- NULL
        assocNetwork<<-NULL
        predError<<-NULL
        for(elem in 1:length(inserted))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[elem])
          )

        }
        inserted <<- c()
        for(elem2 in 1:length(insertedV))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', insertedV[elem2])
          )

        }
        insertedV <<- c()
        rvs$evidence <<- c()
        rvs$value <<- c()
        rvs$evidenceObserve <<- c()
        rvs$valueObserve <<- c()
        nodeNamesB <<- c()
        EventNode <<- c()
        EvidenceNode <<- c()
        shapeVector<<- c()
        bn.start<<- empty.graph(names(DiscreteData))
        communities<<-NULL
        Acommunities<<-NULL
        graph<<-NULL
        updateSelectInput(session,'event',choices = "")
        updateSelectizeInput(session,'varselect',choices = "")
        updateSelectInput(session,'paramSelect',choices = "")
        updateSelectInput(session,"moduleSelection",choices = "")
        updateSelectInput(session,"neighbornodes",choices = "")
        updateSelectInput(session,"Aneighbornodes",choices = "")
        updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
        updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
        updateSelectInput(session,"delSelect",choices = names(DiscreteData))
        updateSelectInput(session,"facSelect",choices = names(DiscreteData))
        updateSelectInput(session,"numSelect",choices = names(DiscreteData))
        updateSelectInput(session,"fromarc",choices=c())
        updateSelectInput(session,"toarc",choices = c())
        updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
        updateSelectInput(session,'modGroup',choices = "")
        updateSelectInput(session,'AmodGroup',choices = "")
        updateSelectInput(session,"intSelect",choices = names(DiscreteData))
        updateSelectInput(session,"AmoduleSelection",choices = "")
        output$priorout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
      },error=function(e){
        shinyalert(toString(e),type = "error")
      })
    }
  })
  observeEvent(input$discretize,{
    if(load==2)
    {
      tryCatch({
        if(check.NA(DiscreteData))
        {
          shinyalert::shinyalert("Data has missing values,impute data under pre-process tab",type="info")
        }
        else
        {
          withProgress(message = "Discretizing data", value = 0, {
            tempDiscreteData <- DiscreteData
            if(input$dtype!='hartemink')
            {
              for(n in colnames(tempDiscreteData))
              {
                if(is.numeric(tempDiscreteData[,n])|| is.integer(tempDiscreteData[,n]))
                {
                  temp = custom.discretize(as.numeric(tempDiscreteData[,n]),input$dtype,1,1)
                  tempDiscreteData[,n]<-temp
                }
              }
            }
            else
            {
              if(input$ibreakH=="" || input$breakH=="")
              {
                bk = 5
                ibk = 5
              }
              else
              {
                bk= as.numeric(input$breakH)
                ibk= as.numeric(input$ibreakH)
              }
              numV<- unlist(lapply(tempDiscreteData, is.numeric))
              out <- tempDiscreteData[,numV]<-bnlearn::discretize(tempDiscreteData[,numV],method = "hartemink",breaks = bk, ibreaks = ibk, idisc = "quantile")
              if(class(out)=="try-error"){
                out <- try(bnlearn::discretize(tempDiscreteData[,numV],method="interval"))
                shinyalert("Failed to discretize all variables using the desired method, used interval discretization for them instead",type = "info")
                if(class(out)=="try-error"){
                  shinyalert("Failed to discretize some variables in the data. Try again using some other method or input a discretized data",type = "error")
                }

              }
            }
            tempDiscreteData[,which(lapply(tempDiscreteData,nlevels)<2)] = NULL
            tempDiscreteData <- droplevels(tempDiscreteData)
            DiscreteData <<-tempDiscreteData
            check.NA(DiscreteData)
            check.discrete(DiscreteData)
            trueData<<-DiscreteData
            output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
            reset<<-1
            assocReset<<-1
            #shinyalert("Discretization successfull",type="success")
            weight<<-1
            value<<-1
            blacklistEdges<<-c()
            whitelistEdges<<-c()
            externalGraphEdges<<-c()
            INTvar<<-c()
            output$valLoss<<-renderText({0})
            output$netScore<<-renderText({0})
            output$assocPlot<<-renderVisNetwork({validate("Explore the association network on your data")})
            output$netPlot<<-renderVisNetwork({validate("Construct bayesian network for taking decisions")})
            output$parameterPlot<<-renderPlot({validate("Construct bayesina network for taking decisions")})
            output$consensusPlot<-renderPlot({validate("Construct bayesian network(Bootstap Learning) for taking decision")})
            output$distPlot<<-renderPlot({validate("Construct bayesian netowrk for takinng decision")})
            output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
            NetworkGraph <<- NULL
            assocNetwork<<-NULL
            predError<<-NULL
            for(elem in 1:length(inserted))
            {
              removeUI(
                ## pass in appropriate div id
                selector = paste0('#', inserted[elem])
              )

            }
            inserted <<- c()
            for(elem2 in 1:length(insertedV))
            {
              removeUI(
                ## pass in appropriate div id
                selector = paste0('#', insertedV[elem2])
              )

            }
            insertedV <<- c()
            rvs$evidence <<- c()
            rvs$value <<- c()
            rvs$evidenceObserve <<- c()
            rvs$valueObserve <<- c()
            nodeNamesB <<- c()
            EventNode <<- c()
            EvidenceNode <<- c()
            shapeVector<<- c()
            bn.start<<- empty.graph(names(DiscreteData))
            communities<<-NULL
            Acommunities<<-NULL
            graph<<-NULL
            updateSelectInput(session,'event',choices = "")
            updateSelectizeInput(session,'varselect',choices = "")
            updateSelectInput(session,'paramSelect',choices = "")
            updateSelectInput(session,"moduleSelection",choices = "")
            updateSelectInput(session,"neighbornodes",choices = "")
            updateSelectInput(session,"Aneighbornodes",choices = "")
            updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
            updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
            updateSelectInput(session,"delSelect",choices = names(DiscreteData))
            updateSelectInput(session,"facSelect",choices = names(DiscreteData))
            updateSelectInput(session,"numSelect",choices = names(DiscreteData))
            updateSelectInput(session,"fromarc",choices=c())
            updateSelectInput(session,"toarc",choices = c())
            updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
            updateSelectInput(session,'modGroup',choices = "")
            updateSelectInput(session,'AmodGroup',choices = "")
            updateSelectInput(session,"intSelect",choices = names(DiscreteData))
            updateSelectInput(session,"AmoduleSelection",choices = "")
            output$priorout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
            output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
            output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
          })
        }
      },error = function(e){
        type <- toString(input$dtype)
        messageString <- paste(c("Error in discretising using method ", type, ". Try using other method or upload pre-discretised data."), collapse = '')
        shinyalert::shinyalert(messageString, type = "error")
      })
      tooltip(session)
    }
  })

  observeEvent(input$impute,{
    if(load==2)
    {
      tryCatch({
        withProgress(message = "Imputing missing data", value = 0, {
          for(n in colnames(DiscreteData))
          {
            if(is.character(DiscreteData[,n]))
            {
              DiscreteData[,n]<<-as.factor(DiscreteData[,n])
            }
          }
          DiscreteData <<- missRanger::missRanger(DiscreteData,maxiter = 1,num.tree = 100)
          check.discrete(DiscreteData)
          shinyalert("Imputation successfull",type="success")
          output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
          trueData<<-DiscreteData
          reset<<-1
          assocReset<<-1
          weight<<-1
          value<<-1
          blacklistEdges<<-c()
          whitelistEdges<<-c()
          externalGraphEdges<<-c()
          INTvar<<-c()
          output$valLoss<<-renderText({0})
          output$netScore<<-renderText({0})
          output$assocPlot<<-renderVisNetwork({validate("Explore the association network on your data")})
          output$netPlot<<-renderVisNetwork({validate("Construct bayesian netowrk for taking decision")})
          output$parameterPlot<<-renderPlot({validate("Construct bayesian network for taking decision")})
          output$consensusPlot<-renderPlot({validate("Construct bayesian network(Bootstap Learning) for taking decision")})
          output$distPlot<<-renderPlot({validate("Construct bayesian network for taking decision")})
          output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
          NetworkGraph <<- NULL
          assocNetwork<<-NULL
          predError<<-NULL
          for(elem in 1:length(inserted))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', inserted[elem])
            )

          }
          inserted <<- c()
          for(elem2 in 1:length(insertedV))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', insertedV[elem2])
            )

          }
          insertedV <<- c()
          rvs$evidence <<- c()
          rvs$value <<- c()
          rvs$evidenceObserve <<- c()
          rvs$valueObserve <<- c()
          nodeNamesB <<- c()
          EventNode <<- c()
          EvidenceNode <<- c()
          shapeVector<<- c()
          bn.start<<- empty.graph(names(DiscreteData))
          communities<<-NULL
          Acommunities<<-NULL
          graph<<-NULL
          updateSelectInput(session,"intSelect",choices = names(DiscreteData))
          updateSelectInput(session,'event',choices = "")
          updateSelectizeInput(session,'varselect',choices = "")
          updateSelectInput(session,'paramSelect',choices = "")
          updateSelectInput(session,"moduleSelection",choices = "")
          updateSelectInput(session,"neighbornodes",choices = "")
          updateSelectInput(session,"Aneighbornodes",choices = "")
          updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
          updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
          updateSelectInput(session,"delSelect",choices = names(DiscreteData))
          updateSelectInput(session,"numSelect",choices = names(DiscreteData))
          updateSelectInput(session,"facSelect",choices = names(DiscreteData))
          updateSelectInput(session,"fromarc",choices=c())
          updateSelectInput(session,"toarc",choices = c())
          updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
          updateSelectInput(session,'modGroup',choices = "")
          updateSelectInput(session,'AmodGroup',choices = "")
          updateSelectInput(session,"AmoduleSelection",choices = "")
          output$priorout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
          output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
          output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        })}, error = function(e){
          type <- toString(input$dtype)
          messageString <- "Error imputing missingness. Try uploading complete data."
          shinyalert::shinyalert(messageString, type = "error")
        })
      tooltip(session)
    }
  })
  observeEvent(input$delete,{
    if(load==2)
    {
      tryCatch({
        DiscreteData[,input$delSelect]=NULL
        DiscreteData<<-DiscreteData
        updateSelectInput(session,"delSelect",choices = names(DiscreteData))
        updateSelectInput(session,"facSelect",choices = names(DiscreteData))
        updateSelectInput(session,"numSelect",choices = names(DiscreteData))
        updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
        output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
        reset<<-1
        weight<<-1
        value<<-1
        assocReset<<-1
        blacklistEdges<<-c()
        whitelistEdges<<-c()
        externalGraphEdges<<-c()
        INTvar<<-c()
        output$valLoss<<-renderText({0})
        output$netScore<<-renderText({0})
        output$assocPlot<<-renderVisNetwork({validate("Explore the association network on your data")})
        output$netPlot<<-renderVisNetwork({validate("Construct bayesian netowrk for taking decision")})
        output$parameterPlot<<-renderPlot({validate("Construct bayesian network for taking decision")})
        output$consensusPlot<-renderPlot({validate("Construct bayesian network(Bootstap Learning) for taking decision")})
        output$distPlot<<-renderPlot({validate("Construct bayesian network for taking decision")})
        output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
        NetworkGraph <<- NULL
        assocNetwork<<-NULL
        predError<<-NULL
        for(elem in 1:length(inserted))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[elem])
          )

        }
        inserted <<- c()
        for(elem2 in 1:length(insertedV))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', insertedV[elem2])
          )

        }
        insertedV <<- c()
        rvs$evidence <<- c()
        rvs$value <<- c()
        rvs$evidenceObserve <<- c()
        rvs$valueObserve <<- c()
        nodeNamesB <<- c()
        EventNode <<- c()
        EvidenceNode <<- c()
        shapeVector<<- c()
        bn.start<<- empty.graph(names(DiscreteData))
        communities<<-NULL
        Acommunities<<-NULL
        graph<<-NULL
        updateSelectInput(session,'event',choices = "")
        updateSelectizeInput(session,'varselect',choices = "")
        updateSelectInput(session,'paramSelect',choices = "")
        updateSelectInput(session,"moduleSelection",choices = "")
        updateSelectInput(session,"neighbornodes",choices = "")
        updateSelectInput(session,"Aneighbornodes",choices = "")
        updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
        updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
        updateSelectInput(session,"delSelect",choices = names(DiscreteData))
        updateSelectInput(session,"facSelect",choices = names(DiscreteData))
        updateSelectInput(session,"numSelect",choices = names(DiscreteData))
        updateSelectInput(session,"fromarc",choices=c())
        updateSelectInput(session,"toarc",choices = c())
        updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
        updateSelectInput(session,'modGroup',choices = "")
        updateSelectInput(session,'AmodGroup',choices = "")
        updateSelectInput(session,"AmoduleSelection",choices = "")
        updateSelectInput(session,"intSelect",choices = names(DiscreteData))
        output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        output$priorout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        bn.start<<- empty.graph(names(DiscreteData))
        output$priorout<-DT::renderDataTable({bn.start$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
      },error=function(e){

      })
      tooltip(session)
    }
  })
  observeEvent(input$sort,{
    if(load==2)
    {
      DiscreteData<<-DiscreteData[,order(names(DiscreteData))]
      updateSelectInput(session,"delSelect",choices = names(DiscreteData))
      updateSelectInput(session,"facSelect",choices = names(DiscreteData))
      updateSelectInput(session,"numSelect",choices = names(DiscreteData))
      updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
      output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
      reset<<-1
      assocReset<<-1
      weight<<-1
      value<<-1
      blacklistEdges<<-c()
      whitelistEdges<<-c()
      externalGraphEdges<<-c()
      INTvar<<-c()
      output$valLoss<<-renderText({0})
      output$netScore<<-renderText({0})
      output$assocPlot<<-renderVisNetwork({validate("Explore the association network on your data")})
      output$netPlot<<-renderVisNetwork({validate("Construct bayesian netowrk for taking decision")})
      output$parameterPlot<<-renderPlot({validate("Construct bayesian network for taking decision")})
      output$consensusPlot<-renderPlot({validate("Construct bayesian network(Bootstap Learning) for taking decision")})
      output$distPlot<<-renderPlot({validate("Construct bayesian network for taking decision")})
      output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
      NetworkGraph <<- NULL
      assocNetwork<<-NULL
      predError<<-NULL
      for(elem in 1:length(inserted))
      {
        removeUI(
          ## pass in appropriate div id
          selector = paste0('#', inserted[elem])
        )

      }
      inserted <<- c()
      for(elem2 in 1:length(insertedV))
      {
        removeUI(
          ## pass in appropriate div id
          selector = paste0('#', insertedV[elem2])
        )

      }
      insertedV <<- c()
      rvs$evidence <<- c()
      rvs$value <<- c()
      rvs$evidenceObserve <<- c()
      rvs$valueObserve <<- c()
      nodeNamesB <<- c()
      EventNode <<- c()
      EvidenceNode <<- c()
      shapeVector<<- c()
      bn.start<<- empty.graph(names(DiscreteData))
      communities<<-NULL
      Acommunities<<-NULL
      graph<<-NULL
      updateSelectInput(session,'event',choices = "")
      updateSelectizeInput(session,'varselect',choices = "")
      updateSelectInput(session,'paramSelect',choices = "")
      updateSelectInput(session,"moduleSelection",choices = "")
      updateSelectInput(session,"neighbornodes",choices = "")
      updateSelectInput(session,"Aneighbornodes",choices = "")
      updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
      updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
      updateSelectInput(session,"delSelect",choices = names(DiscreteData))
      updateSelectInput(session,"numSelect",choices = names(DiscreteData))
      updateSelectInput(session,"facSelect",choices = names(DiscreteData))
      updateSelectInput(session,"fromarc",choices=c())
      updateSelectInput(session,"toarc",choices = c())
      updateSelectInput(session,"intSelect",choices = names(DiscreteData))
      updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
      updateSelectInput(session,'modGroup',choices = "")
      updateSelectInput(session,'AmodGroup',choices = "")
      updateSelectInput(session,"AmoduleSelection",choices = "")
      output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
      bn.start<<- empty.graph(names(DiscreteData))
      output$priorout<-DT::renderDataTable({bn.start$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
      tooltip(session)
    }
  })
  observeEvent(input$reset,{
    if(load==2)
    {
      tryCatch({
        DiscreteData<<-trueData
        updateSelectInput(session,"delSelect",choices = names(DiscreteData))
        updateSelectInput(session,"facSelect",choices = names(DiscreteData))
        updateSelectInput(session,"numSelect",choices = names(DiscreteData))
        updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
        output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
        reset<<-1
        assocReset<<-1
        weight<<-1
        value<<-1
        blacklistEdges<<-c()
        whitelistEdges<<-c()
        externalGraphEdges<<-c()
        INTvar<<-c()
        output$valLoss<<-renderText({0})
        output$netScore<<-renderText({0})
        output$assocPlot<<-renderVisNetwork({validate("Explore the association network on your data")})
        output$netPlot<<-renderVisNetwork({validate("Construct bayesian netowrk for taking decision")})
        output$parameterPlot<<-renderPlot({validate("Construct bayesian network for taking decision")})
        output$consensusPlot<-renderPlot({validate("Construct bayesian network(Bootstap Learning) for taking decision")})
        output$distPlot<<-renderPlot({validate("Construct bayesian network for taking decision")})
        output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
        NetworkGraph <<- NULL
        assocNetwork<<-NULL
        predError<<-NULL
        for(elem in 1:length(inserted))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[elem])
          )

        }
        inserted <<- c()
        for(elem2 in 1:length(insertedV))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', insertedV[elem2])
          )

        }
        insertedV <<- c()
        rvs$evidence <<- c()
        rvs$value <<- c()
        rvs$evidenceObserve <<- c()
        rvs$valueObserve <<- c()
        nodeNamesB <<- c()
        EventNode <<- c()
        EvidenceNode <<- c()
        shapeVector<<- c()
        bn.start<<- empty.graph(names(DiscreteData))
        communities<<-NULL
        Acommunities<<-NULL
        graph<<-NULL
        updateSelectInput(session,'event',choices = "")
        updateSelectizeInput(session,'varselect',choices = "")
        updateSelectInput(session,'paramSelect',choices = "")
        updateSelectInput(session,"moduleSelection",choices = "")
        updateSelectInput(session,"neighbornodes",choices = "")
        updateSelectInput(session,"Aneighbornodes",choices = "")
        updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
        updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
        updateSelectInput(session,"delSelect",choices = names(DiscreteData))
        updateSelectInput(session,"numSelect",choices = names(DiscreteData))
        updateSelectInput(session,"facSelect",choices = names(DiscreteData))
        updateSelectInput(session,"fromarc",choices=c())
        updateSelectInput(session,"toarc",choices = c())
        updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
        updateSelectInput(session,'modGroup',choices = "")
        updateSelectInput(session,'AmodGroup',choices = "")
        updateSelectInput(session,"intSelect",choices = names(DiscreteData))
        updateSelectInput(session,"AmoduleSelection",choices = "")
        output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        bn.start<<- empty.graph(names(DiscreteData))
        output$priorout<-DT::renderDataTable({bn.start$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
      },error=function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
      tooltip(session)
    }
  })
  observeEvent(input$transpose,{
    if(load==2)
    {
      if(check.NA(DiscreteData))
      {
        shinyalert::shinyalert("Impute missing data using pre-process tab to procede",type="info")
      }
      else if(check.discrete(DiscreteData))
      {
        shinyalert::shinyalert("Discretize data using pre-process tab to proceed",type="info")
      }
      else
      {
        tryCatch({
          if(dim(DiscreteData)[1]>dim(DiscreteData)[2])
          {
            shinyalert::shinyalert("Transpose is only possible for datasest with no. of variables more than no. of samples",type="info")
          }
          else
          {
            DiscreteData<<-t(DiscreteData)
            updateSelectInput(session,"delSelect",choices = names(DiscreteData))
            updateSelectInput(session,"numSelect",choices = names(DiscreteData))
            updateSelectInput(session,"facSelect",choices = names(DiscreteData))
            updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
            output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
            reset<<-1
            weight<<-1
            value<<-1
            assocReset<<-1
            blacklistEdges<<-c()
            whitelistEdges<<-c()
            externalGraphEdges<<-c()
            output$valLoss<<-renderText({0})
            output$netScore<<-renderText({0})
            output$assocPlot<<-renderVisNetwork({validate("Explore the association network on your data")})
            output$netPlot<<-renderVisNetwork({validate("Construct bayesian netowrk for taking decision")})
            output$parameterPlot<<-renderPlot({validate("Construct bayesian network for taking decision")})
            output$consensusPlot<-renderPlot({validate("Construct bayesian network(Bootstap Learning) for taking decision")})
            output$distPlot<<-renderPlot({validate("Construct bayesian network for taking decision")})
            output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
            NetworkGraph <<- NULL
            assocNetwork<<-NULL
            predError<<-NULL
            for(elem in 1:length(inserted))
            {
              removeUI(
                ## pass in appropriate div id
                selector = paste0('#', inserted[elem])
              )

            }
            inserted <<- c()
            for(elem2 in 1:length(insertedV))
            {
              removeUI(
                ## pass in appropriate div id
                selector = paste0('#', insertedV[elem2])
              )

            }
            insertedV <<- c()
            rvs$evidence <<- c()
            rvs$value <<- c()
            rvs$evidenceObserve <<- c()
            rvs$valueObserve <<- c()
            nodeNamesB <<- c()
            EventNode <<- c()
            EvidenceNode <<- c()
            shapeVector<<- c()
            bn.start<<- empty.graph(names(DiscreteData))
            communities<<-NULL
            Acommunities<<-NULL
            graph<<-NULL
            updateSelectInput(session,'event',choices = "")
            updateSelectizeInput(session,'varselect',choices = "")
            updateSelectInput(session,'paramSelect',choices = "")
            updateSelectInput(session,"moduleSelection",choices = "")
            updateSelectInput(session,"neighbornodes",choices = "")
            updateSelectInput(session,"Aneighbornodes",choices = "")
            updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
            updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
            updateSelectInput(session,"delSelect",choices = names(DiscreteData))
            updateSelectInput(session,"facSelect",choices = names(DiscreteData))
            updateSelectInput(session,"numSelect",choices = names(DiscreteData))
            updateSelectInput(session,"fromarc",choices=c())
            updateSelectInput(session,"toarc",choices = c())
            updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
            updateSelectInput(session,'modGroup',choices = "")
            updateSelectInput(session,'AmodGroup',choices = "")
            updateSelectInput(session,"intSelect",choices = names(DiscreteData))
            updateSelectInput(session,"AmoduleSelection",choices = "")
            output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
            bn.start<<- empty.graph(names(DiscreteData))
            output$priorout<-DT::renderDataTable({bn.start$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
          }
        },error=function(e){
          shinyalert::shinyalert(toString(e),type = 'error')
        })
      }
      tooltip(session)
    }
  })
  observeEvent(input$freqSelect,{
    if(load==2)
    {
      if(check.NA(DiscreteData))
      {
        shinyalert::shinyalert("Impute missing data using pre-process tab to procede",type="info")
        output$freqPlot<<-renderPlot({validate("Make sure data is complete and discretized before using this feature")})
      }
      else if(check.discrete(DiscreteData))
      {
        shinyalert::shinyalert("Discretize data using pre-process tab to proceed",type="info")
        output$freqPlot<<-renderPlot({validate("Make sure data is complete and discretized before using this feature")})
      }
      else
      {
        tryCatch({
          val = table(DiscreteData[,input$freqSelect])/nrow(DiscreteData)
          output$freqPlot = renderPlot({par(mar=c(5,3,3,3))
            par(oma=c(5,3,3,3))
            barx <<-barplot(val,
                            col = "lightblue",
                            main = paste("Observed frequency of ",input$freqSelect),
                            border = NA,
                            xlab = "",
                            ylab = "Frequency",
                            ylim = c(0,1),
                            las=2,
                            cex.names=as.numeric(input$plotFont))
            text(x = barx,y = round(val,digits = 4),label = round(val,digits = 4), pos = 3, cex = as.numeric(input$valueFont), col = "black")})
        },error=function(e){
          if(input$freqSelect=="")
          {

          }
          else
          {
            shinyalert::shinyalert(toString(e),type="error")
          }
        })
      }
      tooltip(session)
    }
  })
  # Get the data selection from user
  observeEvent(input$structFile,{# Get the uploaded file from user
    if(load==2)
    {
      if(check.NA(DiscreteData))
      {
        shinyalert::shinyalert("Impute missing data using pre-process tab to procede",type="info")
      }
      else if(check.discrete(DiscreteData))
      {
        shinyalert::shinyalert("Discretize data using pre-process tab to proceed",type="info")
      }
      else
      {
        inFile <- input$structFile
        if (is.null(inFile))
        {
          shinyalert::shinyalert("Structure File is empty",type='error')
        }
        else
        {
          if(is.null(DiscreteData))
          {
            shinyalert::shinyalert("Upload Data file first",type = 'error')
          }
          else
          {
            tryCatch({
              tryCatch({
                bn.hc.boot.average <<- get(load(inFile$datapath))
              },error = function(e){
                bn.hc.boot.average <<- readRDS(inFile$datapath)
              })
              if(input$parallel==T)
              {
                bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod,cluster = cl)
              }
              else
              {
                bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod)
              }
              shinyalert::shinyalert("Structure loaded",type = "success")
              for(elem in 1:length(inserted))
              {
                removeUI(
                  ## pass in appropriate div id
                  selector = paste0('#', inserted[elem])
                )

              }
              inserted <<- c()
              for(elem2 in 1:length(insertedV))
              {
                removeUI(
                  ## pass in appropriate div id
                  selector = paste0('#', insertedV[elem2])
                )

              }
              insertedV <<- c()
              rvs$evidence <<- c()
              rvs$value <<- c()
              rvs$evidenceObserve <<- c()
              rvs$valueObserve <<- c()
              output$distPlot <<- renderPlot(NULL)
              NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
              nodeNamesB <<- names(bn.hc.boot.average$nodes)
              EventNode <<- nodeNamesB[1]
              EvidenceNode <<- c()
              tryCatch({
                btn <<- input$insertBtn
                id <- paste0('Evidence', btn)
                idL <- paste("Evidence", btn)
                idV <- paste0('Value', btn)
                idVL <- paste("Value", btn)
                insertUI(selector = '#placeholder1',
                         ui = tags$div(selectInput(id,'Evidence',nodeNamesB),
                                       id = id
                         )
                )
                insertUI(selector = '#placeholder2',
                         ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNamesB[1]])),
                                       id = idV
                         )
                )
                inserted <<- c(id, inserted)
                insertedV <<- c(idV,insertedV)
                rvs$evidence <<- c(rvs$evidence,id)
                rvs$value <<- c(rvs$value,id)
                rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
                  tryCatch({
                    valID = insertedV[which(inserted == id)]
                    updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
                  },error = function(e){
                    shinyalert::shinyalert(toString("Construct bayesian network for taking decision"), type = "error")
                  })
                }))

              },error = function(e){
                shinyalert::shinyalert(toString(e), type = "error")
              })
              shapeVector<<- rep('dot',length(nodeNamesB))
              updateSelectInput(session,'event',choices = nodeNamesB)
              weight <<- 1
              value <<- 1
              exactCheck<<-1
              output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
              updateSelectizeInput(session,'varselect',choices = nodeNamesB)
              updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                 "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'graph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
              updateSelectInput(session,'paramSelect',choices = nodeNamesB)
              graph<<-igraph::graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
              updateSelectInput(session,"neighbornodes",choices = "")
              updateSelectInput(session,"fromarc",choices = nodeNamesB)
              updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                 "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'modGroup',choices = "")
              updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNamesB[1]]),value = nlevels(DiscreteData[,nodeNamesB[1]]))
              output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
              updateSelectInput(session,"parents",choices = nodeNamesB)
              output$decisionPlot<-renderVisNetwork({validate("Build Decision Network using app")})
              output$policyPlot<-DT::renderDataTable({NULL})
              reset<<-2
              upload<<-1
              uploadtype<<-1
              type<<-1
              tryCatch({
                if(input$tableName=="Bayesian Graph")
                {
                  output$tableOut<- DT::renderDataTable({NetworkGraph},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="Cross Validation Results")
                {
                  output$tableOut<- DT::renderDataTable({predError},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="blacklist edges")
                {
                  output$tableOut<- DT::renderDataTable({blacklistEdges},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="whitelist edges")
                {
                  output$tableOut<- DT::renderDataTable({whitelistEdges},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="Nodes")
                {
                  output$tableOut<- DT::renderDataTable({as.data.frame(nodeNamesB)},options = list(scrollX = TRUE,pageLength = 10))
                }
              },error=function(e){
                shinyalert::shinyalert(toString(e), type = "error")
              })
              save(DiscreteData,file="customDashboard/inst/cd/data.RData")
              save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
              write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
            },error = function(e){
              shinyalert::shinyalert(toString(e), type = "error")
            })
          }
        }
      }
      tooltip(session)
    }
    })

  observeEvent(input$bootFile,{
    if(load==2)
    {
      if(check.NA(DiscreteData))
      {
        shinyalert::shinyalert("Impute missing data using pre-process tab to procede",type="info")
      }
      else if(check.discrete(DiscreteData))
      {
        shinyalert::shinyalert("Discretize data using pre-process tab to proceed",type="info")
      }
      else
      {
        inFile <- input$bootFile
        if (is.null(inFile))
        {
          shinyalert::shinyalert("Structure File is empty",type='error')
        }
        else
        {
          if(is.null(DiscreteData))
          {
            shinyalert::shinyalert("Upload Data file first",type = 'error')
          }
          else
          {
            tryCatch({
              tryCatch({
                bn.hc.boot <<- get(load(inFile$datapath))
              },error = function(e){
                bn.hc.boot <<- readRDS(inFile$datapath)
              })
              bn.hc.boot.pruned <<- bn.hc.boot[bn.hc.boot$strength >= input$edgeStrengthU & bn.hc.boot$direction >= input$directionStrengthU,]
              bn.hc.boot.average <<- cextend(averaged.network(bn.hc.boot.pruned))
              if(input$parallel==T)
              {
                bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod,cluster = cl)
              }
              else
              {
                bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod)
              }
              shinyalert::shinyalert("Structure loaded",type = "success")
              for(elem in 1:length(inserted))
              {
                removeUI(
                  ## pass in appropriate div id
                  selector = paste0('#', inserted[elem])
                )

              }
              inserted <<- c()
              for(elem2 in 1:length(insertedV))
              {
                removeUI(
                  ## pass in appropriate div id
                  selector = paste0('#', insertedV[elem2])
                )

              }
              insertedV <<- c()
              rvs$evidence <<- c()
              rvs$value <<- c()
              rvs$evidenceObserve <<- c()
              rvs$valueObserve <<- c()
              output$distPlot <<- renderPlot(NULL)
              NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
              nodeNamesB <<- names(bn.hc.boot.average$nodes)
              EventNode <<- nodeNamesB[1]
              EvidenceNode <<- c()
              tryCatch({
                btn <<- input$insertBtn
                id <- paste0('Evidence', btn)
                idL <- paste("Evidence", btn)
                idV <- paste0('Value', btn)
                idVL <- paste("Value", btn)
                insertUI(selector = '#placeholder1',
                         ui = tags$div(selectInput(id,'Evidence',nodeNamesB),
                                       id = id
                         )
                )
                insertUI(selector = '#placeholder2',
                         ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNamesB[1]])),
                                       id = idV
                         )
                )
                inserted <<- c(id, inserted)
                insertedV <<- c(idV,insertedV)
                rvs$evidence <<- c(rvs$evidence,id)
                rvs$value <<- c(rvs$value,id)
                rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
                  tryCatch({
                    valID = insertedV[which(inserted == id)]
                    updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
                  },error = function(e){
                    shinyalert::shinyalert(toString("Construct bayesian network for taking decision"), type = "error")
                  })
                }))

              },error = function(e){
                shinyalert::shinyalert(toString(e), type = "error")
              })
              shapeVector<<- rep('dot',length(nodeNamesB))
              updateSelectInput(session,'event',choices = nodeNamesB)
              weight <<- graph.weight(bn.hc.boot,NetworkGraph)
              value <<- graph.weight(bn.hc.boot,NetworkGraph)
              exactCheck<<-1
              output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
              updateSelectizeInput(session,'varselect',choices = nodeNamesB)
              updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                 "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'graph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
              updateSelectInput(session,'paramSelect',choices = nodeNamesB)
              graph<<-igraph::graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
              updateSelectInput(session,"neighbornodes",choices = "")
              updateSelectInput(session,"fromarc",choices = nodeNamesB)
              updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                 "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'modGroup',choices = "")
              updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNamesB[1]]),value = nlevels(DiscreteData[,nodeNamesB[1]]))
              output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
              updateSelectInput(session,"parents",choices = nodeNamesB)
              output$decisionPlot<-renderVisNetwork({validate("Build Decision Network using app")})
              output$policyPlot<-DT::renderDataTable({NULL})
              reset<<-2
              upload<<-1
              uploadtype<<-2
              type<<-2
              tryCatch({
                if(input$tableName=="Bayesian Graph")
                {
                  output$tableOut<- DT::renderDataTable({NetworkGraph},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="Cross Validation Results")
                {
                  output$tableOut<- DT::renderDataTable({predError},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="blacklist edges")
                {
                  output$tableOut<- DT::renderDataTable({blacklistEdges},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="whitelist edges")
                {
                  output$tableOut<- DT::renderDataTable({whitelistEdges},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="Nodes")
                {
                  output$tableOut<- DT::renderDataTable({as.data.frame(nodeNamesB)},options = list(scrollX = TRUE,pageLength = 10))
                }
              },error=function(e){
                shinyalert::shinyalert(toString(e), type = "error")
              })
              save(DiscreteData,file="customDashboard/inst/cd/data.RData")
              save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
              write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
            },error = function(e){
              shinyalert::shinyalert(toString(e), type = "error")
            })
          }
        }
      }
      tooltip(session)
    }
  })
  observeEvent(input$parameterTuningU,{
    if(load==2)
    {
      if(upload==1)
      {
        if(reset==2)
        {
          if(uploadtype==1)
          {
            if(input$parallel==T)
            {
              bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod,cluster = cl)
            }
            else
            {
              bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod)
            }
            for(elem in 1:length(inserted))
            {
              removeUI(
                ## pass in appropriate div id
                selector = paste0('#', inserted[elem])
              )

            }
            inserted <<- c()
            for(elem2 in 1:length(insertedV))
            {
              removeUI(
                ## pass in appropriate div id
                selector = paste0('#', insertedV[elem2])
              )

            }
            insertedV <<- c()
            rvs$evidence <<- c()
            rvs$value <<- c()
            rvs$evidenceObserve <<- c()
            rvs$valueObserve <<- c()
            output$distPlot <<- renderPlot(NULL)
            NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
            nodeNamesB <<- names(bn.hc.boot.average$nodes)
            EventNode <<- nodeNamesB[1]
            EvidenceNode <<- c()
            tryCatch({
              btn <<- input$insertBtn
              id <- paste0('Evidence', btn)
              idL <- paste("Evidence", btn)
              idV <- paste0('Value', btn)
              idVL <- paste("Value", btn)
              insertUI(selector = '#placeholder1',
                       ui = tags$div(selectInput(id,'Evidence',nodeNamesB),
                                     id = id
                       )
              )
              insertUI(selector = '#placeholder2',
                       ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNamesB[1]])),
                                     id = idV
                       )
              )
              inserted <<- c(id, inserted)
              insertedV <<- c(idV,insertedV)
              rvs$evidence <<- c(rvs$evidence,id)
              rvs$value <<- c(rvs$value,id)
              rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
                tryCatch({
                  valID = insertedV[which(inserted == id)]
                  updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
                },error = function(e){
                  shinyalert::shinyalert(toString("Construct bayesian network for taking decision"), type = "error")
                })
              }))

            },error = function(e){
              shinyalert::shinyalert(toString(e), type = "error")
            })
            shapeVector<<- rep('dot',length(nodeNamesB))
            updateSelectInput(session,'event',choices = nodeNamesB)
            weight <<- 1
            value <<- 1
            exactCheck<<-1
            output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
            updateSelectizeInput(session,'varselect',choices = nodeNamesB)
            updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                              "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                               "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'graph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))

            updateSelectInput(session,'paramSelect',choices = nodeNamesB)
            graph<<-igraph::graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
            updateSelectInput(session,"neighbornodes",choices = "")
            updateSelectInput(session,"fromarc",choices = nodeNamesB)
            updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                               "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'modGroup',choices = "")
            updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNamesB[1]]),value = nlevels(DiscreteData[,nodeNamesB[1]]))
            output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
            updateSelectInput(session,"parents",choices = nodeNamesB)
            output$decisionPlot<-renderVisNetwork({validate("Build Decision Network using app")})
            output$policyPlot<-DT::renderDataTable({NULL})
            reset<<-2
            upload<<-1
            uploadtype<<-1
            type<<-1
            tryCatch({
              if(input$tableName=="Bayesian Graph")
              {
                output$tableOut<- DT::renderDataTable({NetworkGraph},options = list(scrollX = TRUE,pageLength = 10))
              }
              else if(input$tableName=="Cross Validation Results")
              {
                output$tableOut<- DT::renderDataTable({predError},options = list(scrollX = TRUE,pageLength = 10))
              }
              else if(input$tableName=="blacklist edges")
              {
                output$tableOut<- DT::renderDataTable({blacklistEdges},options = list(scrollX = TRUE,pageLength = 10))
              }
              else if(input$tableName=="whitelist edges")
              {
                output$tableOut<- DT::renderDataTable({whitelistEdges},options = list(scrollX = TRUE,pageLength = 10))
              }
              else if(input$tableName=="Nodes")
              {
                output$tableOut<- DT::renderDataTable({as.data.frame(nodeNamesB)},options = list(scrollX = TRUE,pageLength = 10))
              }
            },error=function(e){
              shinyalert::shinyalert(toString(e), type = "error")
            })
            save(DiscreteData,file="customDashboard/inst/cd/data.RData")
            save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
            write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
          }
          else
          {
            bn.hc.boot.pruned <<- bn.hc.boot[bn.hc.boot$strength >= input$edgeStrengthU & bn.hc.boot$direction >= input$directionStrengthU,]
            bn.hc.boot.average <<- cextend(averaged.network(bn.hc.boot.pruned))
            if(input$parallel==T)
            {
              bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod,cluster = cl)
            }
            else
            {
              bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod)
            }
            for(elem in 1:length(inserted))
            {
              removeUI(
                ## pass in appropriate div id
                selector = paste0('#', inserted[elem])
              )

            }
            inserted <<- c()
            for(elem2 in 1:length(insertedV))
            {
              removeUI(
                ## pass in appropriate div id
                selector = paste0('#', insertedV[elem2])
              )

            }
            insertedV <<- c()
            rvs$evidence <<- c()
            rvs$value <<- c()
            rvs$evidenceObserve <<- c()
            rvs$valueObserve <<- c()
            output$distPlot <<- renderPlot(NULL)
            NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
            nodeNamesB <<- names(bn.hc.boot.average$nodes)
            EventNode <<- nodeNamesB[1]
            EvidenceNode <<- c()
            tryCatch({
              btn <<- input$insertBtn
              id <- paste0('Evidence', btn)
              idL <- paste("Evidence", btn)
              idV <- paste0('Value', btn)
              idVL <- paste("Value", btn)
              insertUI(selector = '#placeholder1',
                       ui = tags$div(selectInput(id,'Evidence',nodeNamesB),
                                     id = id
                       )
              )
              insertUI(selector = '#placeholder2',
                       ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNamesB[1]])),
                                     id = idV
                       )
              )
              inserted <<- c(id, inserted)
              insertedV <<- c(idV,insertedV)
              rvs$evidence <<- c(rvs$evidence,id)
              rvs$value <<- c(rvs$value,id)
              rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
                tryCatch({
                  valID = insertedV[which(inserted == id)]
                  updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
                },error = function(e){
                  shinyalert::shinyalert(toString("Construct bayesian network for taking decision"), type = "error")
                })
              }))

            },error = function(e){
              shinyalert::shinyalert(toString(e), type = "error")
            })
            shapeVector<<- rep('dot',length(nodeNamesB))
            updateSelectInput(session,'event',choices = nodeNamesB)
            weight <<- graph.weight(bn.hc.boot,NetworkGraph)
            value <<- graph.weight(bn.hc.boot,NetworkGraph)
            exactCheck<<-1
            output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
            updateSelectizeInput(session,'varselect',choices = nodeNamesB)
            updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                              "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                               "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'graph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
            updateSelectInput(session,'paramSelect',choices = nodeNamesB)
            graph<<-igraph::graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
            updateSelectInput(session,"neighbornodes",choices = "")
            updateSelectInput(session,"fromarc",choices = nodeNamesB)
            updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                               "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'modGroup',choices = "")
            updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNamesB[1]]),value = nlevels(DiscreteData[,nodeNamesB[1]]))
            output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
            updateSelectInput(session,"parents",choices = nodeNamesB)
            output$decisionPlot<-renderVisNetwork({validate("Build Decision Network using app")})
            output$policyPlot<-DT::renderDataTable({NULL})
            reset<<-2
            upload<<-1
            uploadtype<<-2
            type<<-2
            tryCatch({
              if(input$tableName=="Bayesian Graph")
              {
                output$tableOut<- DT::renderDataTable({NetworkGraph},options = list(scrollX = TRUE,pageLength = 10))
              }
              else if(input$tableName=="Cross Validation Results")
              {
                output$tableOut<- DT::renderDataTable({predError},options = list(scrollX = TRUE,pageLength = 10))
              }
              else if(input$tableName=="blacklist edges")
              {
                output$tableOut<- DT::renderDataTable({blacklistEdges},options = list(scrollX = TRUE,pageLength = 10))
              }
              else if(input$tableName=="whitelist edges")
              {
                output$tableOut<- DT::renderDataTable({whitelistEdges},options = list(scrollX = TRUE,pageLength = 10))
              }
              else if(input$tableName=="Nodes")
              {
                output$tableOut<- DT::renderDataTable({as.data.frame(nodeNamesB)},options = list(scrollX = TRUE,pageLength = 10))
              }
            },error=function(e){
              shinyalert::shinyalert(toString(e), type = "error")
            })
            save(DiscreteData,file="customDashboard/inst/cd/data.RData")
            save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
            write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
          }
        }
      }
      tooltip(session)
    }
  })
  # Learn the structure of the network
  observeEvent(input$learnBtn, {
    if(load==2)
    {
      if(check.NA(DiscreteData))
      {
        shinyalert::shinyalert("Impute missing data using pre-process tab to procede",type="info")
      }
      else if(check.discrete(DiscreteData))
      {
        shinyalert::shinyalert("Discretize data using pre-process tab to proceed",type="info")
      }
      else
      {
        tryCatch({
          if (is.null(DiscreteData))
            return(NULL)

          # Create a Progress object
          progress <- shiny::Progress$new()

          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())
          progress$set(message = "Learning network structure", value = 0)

          # Get the selected learning algorithm from the user and learn the network
          if(input$parallel==T)
          {
            if(input$alg=="tabu")
            {
              if(input$resampling == T)
              {
                startG = random.graph(nodes = names(DiscreteData),method = 'melancon',num = input$boot,every = 100,burn.in = 10^5)
                netlist = lapply(startG, function(net) {
                  tabu(DiscreteData,score = input$algoscore,blacklist=blacklistEdges,whitelist=whitelistEdges,exp = INTvar,iss=input$iss,start = net, tabu = 50,cluster = cl)
                  })
                bn.hc.boot<<- custom.strength(netlist, nodes = names(DiscreteData))
              }
              else
              {
                bn.hc.boot <<- boot.strength(data = DiscreteData, R = input$boot, m = ceiling(nrow(DiscreteData)*input$SampleSize), algorithm = input$alg,algorithm.args=list(blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start,score = input$algoscore,iss=input$iss,exp = INTvar,tabu=50),cluster = cl)
              }

            }
            else if(input$alg =="hc")
            {
              if(input$resampling == T)
              {
                startG = random.graph(nodes = names(DiscreteData),method = 'melancon',num = input$boot,every = 100,burn.in = 10^5)
                netlist = lapply(startG, function(net) {
                  hc(DiscreteData,score = input$algoscore,blacklist=blacklistEdges,whitelist=whitelistEdges,exp = INTvar,iss=input$iss,start = net,cluster = cl)
                })
                bn.hc.boot<<- custom.strength(netlist, nodes = names(DiscreteData))
              }
              else
              {
                bn.hc.boot <<- boot.strength(data = DiscreteData, R = input$boot, m = ceiling(nrow(DiscreteData)*input$SampleSize), algorithm = input$alg,algorithm.args=list(blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start,score = input$algoscore,iss=input$iss,exp = INTvar),cluster = cl)
              }
            }
            else
            {
              bn.hc.boot <<- boot.strength(data = DiscreteData, R = input$boot, m = ceiling(nrow(DiscreteData)*input$SampleSize), algorithm = input$alg,algorithm.args=list(blacklist=blacklistEdges,whitelist=whitelistEdges),cluster = cl)
            }
            bn.hc.boot.pruned <<- bn.hc.boot[bn.hc.boot$strength >= input$edgeStrength & bn.hc.boot$direction >= input$directionStrength,]
            bn.hc.boot.average <<- cextend(averaged.network(bn.hc.boot.pruned))
            bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2,cluster = cl)
          }
          else
          {
            if(input$alg=="tabu")
            {
              if(input$resampling == T)
              {
                startG = random.graph(nodes = names(DiscreteData),method = 'melancon',num = input$boot,every = 100)
                netlist = lapply(startG, function(net) {
                  tabu(DiscreteData,score = input$algoscore,blacklist=blacklistEdges,whitelist=whitelistEdges,exp = INTvar,iss=input$iss,start = net, tabu = 50)
                })
                bn.hc.boot<<- custom.strength(netlist, nodes = names(DiscreteData))
              }
              else
              {
                bn.hc.boot <<- boot.strength(data = DiscreteData, R = input$boot, m = ceiling(nrow(DiscreteData)*input$SampleSize), algorithm = input$alg,algorithm.args=list(blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start,score = input$algoscore,iss=input$iss,exp = INTvar,tabu=50))
              }

            }
            else if(input$alg =="hc")
            {
              if(input$resampling == T)
              {
                startG = random.graph(nodes = names(DiscreteData),method = 'melancon',num = input$boot,every = 100)
                netlist = lapply(startG, function(net) {
                  hc(DiscreteData,score = input$algoscore,blacklist=blacklistEdges,whitelist=whitelistEdges,exp = INTvar,iss=input$iss,start = net)
                })
                bn.hc.boot<<- custom.strength(netlist, nodes = names(DiscreteData))
              }
              else
              {
                bn.hc.boot <<- boot.strength(data = DiscreteData, R = input$boot, m = ceiling(nrow(DiscreteData)*input$SampleSize), algorithm = input$alg,algorithm.args=list(blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start,score = input$algoscore,iss=input$iss,exp = INTvar))
              }
            }
            else
            {
              bn.hc.boot <<- boot.strength(data = DiscreteData, R = input$boot, m = ceiling(nrow(DiscreteData)*input$SampleSize), algorithm = input$alg,algorithm.args=list(blacklist=blacklistEdges,whitelist=whitelistEdges))
            }
            bn.hc.boot.pruned <<- bn.hc.boot[bn.hc.boot$strength >= input$edgeStrength & bn.hc.boot$direction >= input$directionStrength,]
            bn.hc.boot.average <<- cextend(averaged.network(bn.hc.boot.pruned))
            bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
          }

          shinyalert::shinyalert("Structure learning complete",type="success")
          simple<<-2
          upload<<-2
          for(elem in 1:length(inserted))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', inserted[elem])
            )

          }
          inserted <<- c()
          for(elem2 in 1:length(insertedV))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', insertedV[elem2])
            )

          }
          insertedV <<- c()
          rvs$evidence <<- c()
          rvs$value <<- c()
          rvs$evidenceObserve <<- c()
          rvs$valueObserve <<- c()
          output$distPlot <<- renderPlot(NULL)
          NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
          nodeNamesB <<- names(bn.hc.boot.average$nodes)
          EventNode <<- nodeNamesB[1]
          EvidenceNode <<- c()
          tryCatch({
            btn <<- input$insertBtn
            id <- paste0('Evidence', btn)
            idL <- paste("Evidence", btn)
            idV <- paste0('Value', btn)
            idVL <- paste("Value", btn)
            insertUI(selector = '#placeholder1',
                     ui = tags$div(selectInput(id,'Evidence',nodeNamesB),
                                   id = id
                     )
            )
            insertUI(selector = '#placeholder2',
                     ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNamesB[1]])),
                                   id = idV
                     )
            )
            inserted <<- c(id, inserted)
            insertedV <<- c(idV,insertedV)
            rvs$evidence <<- c(rvs$evidence,id)
            rvs$value <<- c(rvs$value,id)
            rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
              tryCatch({
                valID = insertedV[which(inserted == id)]
                updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
              },error = function(e){
                shinyalert::shinyalert(toString("Construct bayesian network for taking decision"), type = "error")
              })
            }))

          },error = function(e){
            shinyalert::shinyalert(toString(e), type = "error")
          })
          shapeVector<<- rep('dot',length(nodeNamesB))
          updateSelectInput(session,'event',choices = nodeNamesB)
          weight <<- graph.weight(bn.hc.boot,NetworkGraph)
          value <<- graph.weight(bn.hc.boot,NetworkGraph)
          exactCheck<<-1
          output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
          updateSelectInput(session,'event',choices = nodeNamesB)
          updateSelectizeInput(session,'varselect',choices = nodeNamesB)
          updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                            "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                             "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'graph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
          updateSelectInput(session,'paramSelect',choices = nodeNamesB)
          updateSelectInput(session,"moduleSelection",choices = "graph")
          graph<<-igraph::graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
          updateSelectInput(session,"neighbornodes",choices = "")
          updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                             "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'modGroup',choices = "")
          updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNamesB[1]]),value = nlevels(DiscreteData[,nodeNamesB[1]]))
          updateSelectInput(session,"parents",choices = nodeNamesB)
          output$decisionPlot<-renderVisNetwork({validate("Build Decision Network using app")})
          output$policyPlot<-DT::renderDataTable({NULL})
          reset<<-2
          type<<-2
          tryCatch({
            if(input$tableName=="Bayesian Graph")
            {
              output$tableOut<- DT::renderDataTable({NetworkGraph},options = list(scrollX = TRUE,pageLength = 10))
            }
            else if(input$tableName=="Cross Validation Results")
            {
              output$tableOut<- DT::renderDataTable({predError},options = list(scrollX = TRUE,pageLength = 10))
            }
            else if(input$tableName=="blacklist edges")
            {
              output$tableOut<- DT::renderDataTable({blacklistEdges},options = list(scrollX = TRUE,pageLength = 10))
            }
            else if(input$tableName=="whitelist edges")
            {
              output$tableOut<- DT::renderDataTable({whitelistEdges},options = list(scrollX = TRUE,pageLength = 10))
            }
            else if(input$tableName=="Nodes")
            {
              output$tableOut<- DT::renderDataTable({as.data.frame(nodeNamesB)},options = list(scrollX = TRUE,pageLength = 10))
            }
          },error=function(e){
            shinyalert::shinyalert(toString(e), type = "error")
          })
          updateSelectInput(session,"fromarc",choices = nodeNamesB)
          output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
          save(DiscreteData,file="customDashboard/inst/cd/data.RData")
          save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
          write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
        },error = function(e){
          shinyalert::shinyalert(toString(e), type = "error")
        })
      }
      tooltip(session)
    }
  })
  observeEvent(input$consensus,{
    tryCatch({
      if(reset == 2)
      {
        if(type == 2)
        {
          p = strength.plot(bn.hc.boot.average,bn.hc.boot)
          output$consensusPlot<-renderPlot({strength.plot(bn.hc.boot.average,bn.hc.boot)})
        }
      }
    },error=function(e){
      shinyalert::shinyalert("graph can't be constructed because some nodes were dropped at the set threshold", type = "error")})
  })
  observeEvent(input$externalGraphButton, {
    if(load==2)
    {
      if(check.NA(DiscreteData))
      {
        shinyalert::shinyalert("Impute missing data using pre-process tab to procede",type="info")
      }
      else if(check.discrete(DiscreteData))
      {
        shinyalert::shinyalert("Discretize data using pre-process tab to proceed",type="info")
      }
      else
      {
        tryCatch({
          if (is.null(DiscreteData))
            return(NULL)

          # Create a Progress object
          progress <- shiny::Progress$new()
          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())
          progress$set(message = "Learning network structure", value = 0)

          # Get the selected learning algorithm from the user and learn the network
          nodeCheck = unique(as.vector(as.matrix(externalGraphEdges)))
          dag= empty.graph(nodeCheck)
          for(elem in 1:nrow(externalGraphEdges))
          {
            tempEdge = as.vector(as.matrix(externalGraphEdges[elem,]))
            dag = set.arc(dag,tempEdge[1],tempEdge[2])
          }
          bn.hc.boot.average <<- dag
          bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
          shinyalert::shinyalert("Structure learning complete",type="success")
          simple<<-1
          upload<<-2
          type<<-1
          for(elem in 1:length(inserted))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', inserted[elem])
            )

          }
          inserted <<- c()
          for(elem2 in 1:length(insertedV))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', insertedV[elem2])
            )

          }
          insertedV <<- c()
          rvs$evidence <<- c()
          rvs$value <<- c()
          rvs$evidenceObserve <<- c()
          rvs$valueObserve <<- c()
          output$distPlot <<- renderPlot(NULL)
          NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
          nodeNamesB <<- names(bn.hc.boot.average$nodes)
          EventNode <<- nodeNamesB[1]
          EvidenceNode <<- c()
          tryCatch({
            btn <<- input$insertBtn
            id <- paste0('Evidence', btn)
            idL <- paste("Evidence", btn)
            idV <- paste0('Value', btn)
            idVL <- paste("Value", btn)
            insertUI(selector = '#placeholder1',
                     ui = tags$div(selectInput(id,'Evidence',nodeNamesB),
                                   id = id
                     )
            )
            insertUI(selector = '#placeholder2',
                     ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNamesB[1]])),
                                   id = idV
                     )
            )
            inserted <<- c(id, inserted)
            insertedV <<- c(idV,insertedV)
            rvs$evidence <<- c(rvs$evidence,id)
            rvs$value <<- c(rvs$value,id)
            rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
              tryCatch({
                valID = insertedV[which(inserted == id)]
                updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
              },error = function(e){
                shinyalert::shinyalert(toString("Construct bayesian network for taking decision"), type = "error")
              })
            }))

          },error = function(e){
            shinyalert::shinyalert(toString(e), type = "error")
          })
          shapeVector<<- rep('dot',length(nodeNamesB))
          updateSelectInput(session,'event',choices = nodeNamesB)
          weight <<- 1
          value <<- 1
          exactCheck<<-1
          output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
          updateSelectInput(session,'event',choices = nodeNamesB)
          updateSelectizeInput(session,'varselect',choices = nodeNamesB)
          updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                            "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                             "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'graph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
          updateSelectInput(session,'paramSelect',choices = nodeNamesB)
          updateSelectInput(session,"moduleSelection",choices = "graph")
          updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                             "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'modGroup',choices = "")
          graph<<-igraph::graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
          updateSelectInput(session,"neighbornodes",choices = "")
          updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNamesB[1]]),value = nlevels(DiscreteData[,nodeNamesB[1]]))
          updateSelectInput(session,"parents",choices = nodeNamesB)
          output$decisionPlot<-renderVisNetwork({validate("Build Decision Network using app")})
          output$policyPlot<-DT::renderDataTable({NULL})
          reset<<-2
          tryCatch({
            if(input$tableName=="Bayesian Graph")
            {
              output$tableOut<- DT::renderDataTable({NetworkGraph},options = list(scrollX = TRUE,pageLength = 10))
            }
            else if(input$tableName=="Cross Validation Results")
            {
              output$tableOut<- DT::renderDataTable({predError},options = list(scrollX = TRUE,pageLength = 10))
            }
            else if(input$tableName=="blacklist edges")
            {
              output$tableOut<- DT::renderDataTable({blacklistEdges},options = list(scrollX = TRUE,pageLength = 10))
            }
            else if(input$tableName=="whitelist edges")
            {
              output$tableOut<- DT::renderDataTable({whitelistEdges},options = list(scrollX = TRUE,pageLength = 10))
            }
            else if(input$tableName=="Nodes")
            {
              output$tableOut<- DT::renderDataTable({as.data.frame(nodeNamesB)},options = list(scrollX = TRUE,pageLength = 10))
            }
          },error=function(e){
            shinyalert::shinyalert(toString(e), type = "error")
          })
          updateSelectInput(session,"fromarc",choices = nodeNamesB)
          output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
          save(DiscreteData,file="customDashboard/inst/cd/data.RData")
          save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
          write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
        },error = function(e){
          shinyalert::shinyalert(toString(e), type = "error")
        })
      }
      tooltip(session)
    }
  })
  observeEvent(input$GNNGraphButton, {
    if(load==2)
    {
      if(check.NA(DiscreteData))
      {
        shinyalert::shinyalert("Impute missing data using pre-process tab to procede",type="info")
      }
      else if(check.discrete(DiscreteData))
      {
        shinyalert::shinyalert("Discretize data using pre-process tab to proceed",type="info")
      }
      else
      {
        tryCatch({
          if (is.null(DiscreteData))
            return(NULL)

          # Create a Progress object
          progress <- shiny::Progress$new()
          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())
          progress$set(message = "Learning network structure", value = 0)

          # Get the selected learning algorithm from the user and learn the network
          library(reticulate)
          sys.setenv(RETICULATE_PYTHON = input$pythonENV)
          use_condaenv(input$condaENV,required = T)
          #use_condaenv("wiser",required = T)
          py_config()
          df<- DiscreteData
          col<- ncol(df)
          colnames<- list(colnames(df))
          r_iter <- as.integer(no.of.bootstraps)
          reticulate::source_python('file.py')
          bootstrap(df,col,colnames,r_iter)
          externalGraphEdges=read.csv('gnn.csv',stringsAsFactors = T,na.strings = c("NA","na","Na","nA","","?","-"))
          externalGraphEdges<<-as.data.frame(externalGraphEdges)
          nodeCheck = unique(as.vector(as.matrix(externalGraphEdges)))
          dag= empty.graph(nodeCheck)
          for(elem in 1:nrow(externalGraphEdges))
          {
            tempEdge = as.vector(as.matrix(externalGraphEdges[elem,]))
            dag = set.arc(dag,tempEdge[1],tempEdge[2])
          }
          bn.hc.boot.average <<- dag
          bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
          shinyalert::shinyalert("Structure learning complete",type="success")
          simple<<-1
          upload<<-2
          type<<-1
          for(elem in 1:length(inserted))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', inserted[elem])
            )

          }
          inserted <<- c()
          for(elem2 in 1:length(insertedV))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', insertedV[elem2])
            )

          }
          insertedV <<- c()
          rvs$evidence <<- c()
          rvs$value <<- c()
          rvs$evidenceObserve <<- c()
          rvs$valueObserve <<- c()
          output$distPlot <<- renderPlot(NULL)
          NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
          nodeNamesB <<- names(bn.hc.boot.average$nodes)
          EventNode <<- nodeNamesB[1]
          EvidenceNode <<- c()
          tryCatch({
            btn <<- input$insertBtn
            id <- paste0('Evidence', btn)
            idL <- paste("Evidence", btn)
            idV <- paste0('Value', btn)
            idVL <- paste("Value", btn)
            insertUI(selector = '#placeholder1',
                     ui = tags$div(selectInput(id,'Evidence',nodeNamesB),
                                   id = id
                     )
            )
            insertUI(selector = '#placeholder2',
                     ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNamesB[1]])),
                                   id = idV
                     )
            )
            inserted <<- c(id, inserted)
            insertedV <<- c(idV,insertedV)
            rvs$evidence <<- c(rvs$evidence,id)
            rvs$value <<- c(rvs$value,id)
            rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
              tryCatch({
                valID = insertedV[which(inserted == id)]
                updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
              },error = function(e){
                shinyalert::shinyalert(toString("Construct bayesian network for taking decision"), type = "error")
              })
            }))

          },error = function(e){
            shinyalert::shinyalert(toString(e), type = "error")
          })
          shapeVector<<- rep('dot',length(nodeNamesB))
          updateSelectInput(session,'event',choices = nodeNamesB)
          weight <<- 1
          value <<- 1
          exactCheck<<-1
          output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
          updateSelectInput(session,'event',choices = nodeNamesB)
          updateSelectizeInput(session,'varselect',choices = nodeNamesB)
          updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                            "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                             "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'graph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
          updateSelectInput(session,'paramSelect',choices = nodeNamesB)
          updateSelectInput(session,"moduleSelection",choices = "graph")
          updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                             "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'modGroup',choices = "")
          graph<<-igraph::graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
          updateSelectInput(session,"neighbornodes",choices = "")
          updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNamesB[1]]),value = nlevels(DiscreteData[,nodeNamesB[1]]))
          updateSelectInput(session,"parents",choices = nodeNamesB)
          output$decisionPlot<-renderVisNetwork({validate("Build Decision Network using app")})
          output$policyPlot<-DT::renderDataTable({NULL})
          reset<<-2
          tryCatch({
            if(input$tableName=="Bayesian Graph")
            {
              output$tableOut<- DT::renderDataTable({NetworkGraph},options = list(scrollX = TRUE,pageLength = 10))
            }
            else if(input$tableName=="Cross Validation Results")
            {
              output$tableOut<- DT::renderDataTable({predError},options = list(scrollX = TRUE,pageLength = 10))
            }
            else if(input$tableName=="blacklist edges")
            {
              output$tableOut<- DT::renderDataTable({blacklistEdges},options = list(scrollX = TRUE,pageLength = 10))
            }
            else if(input$tableName=="whitelist edges")
            {
              output$tableOut<- DT::renderDataTable({whitelistEdges},options = list(scrollX = TRUE,pageLength = 10))
            }
            else if(input$tableName=="Nodes")
            {
              output$tableOut<- DT::renderDataTable({as.data.frame(nodeNamesB)},options = list(scrollX = TRUE,pageLength = 10))
            }
          },error=function(e){
            shinyalert::shinyalert(toString(e), type = "error")
          })
          updateSelectInput(session,"fromarc",choices = nodeNamesB)
          output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
          save(DiscreteData,file="customDashboard/inst/cd/data.RData")
          save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
          write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
        },error = function(e){
          shinyalert::shinyalert(toString(e), type = "error")
        })
      }
      tooltip(session)
    }
  })
  observeEvent(input$consensus,{
    tryCatch({
      if(reset == 2)
      {
        if(type == 2)
        {
          p = strength.plot(bn.hc.boot.average,bn.hc.boot)
          output$consensusPlot<-renderPlot({strength.plot(bn.hc.boot.average,bn.hc.boot)})
        }
      }
    },error=function(e){
      shinyalert::shinyalert("graph can't be constructed because some nodes were dropped at the set threshold", type = "error")})
  })
  observeEvent(input$PruneBtn,{
    if(load==2)
    {
      tryCatch({
        if(upload==2)
        {
          if(reset==2)
          {
            if(simple==1)
            {
              if(input$parallel==T)
              {
                bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2,cluster = cl)
              }
              else
              {
                bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
              }
              simple<<-1
              for(elem in 1:length(inserted))
              {
                removeUI(
                  ## pass in appropriate div id
                  selector = paste0('#', inserted[elem])
                )

              }
              inserted <<- c()
              for(elem2 in 1:length(insertedV))
              {
                removeUI(
                  ## pass in appropriate div id
                  selector = paste0('#', insertedV[elem2])
                )

              }
              insertedV <<- c()
              rvs$evidence <<- c()
              rvs$value <<- c()
              rvs$evidenceObserve <<- c()
              rvs$valueObserve <<- c()
              output$distPlot <<- renderPlot(NULL)
              NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
              nodeNamesB <<- names(bn.hc.boot.average$nodes)
              EventNode <<- nodeNamesB[1]
              EvidenceNode <<- c()
              tryCatch({
                btn <<- input$insertBtn
                id <- paste0('Evidence', btn)
                idL <- paste("Evidence", btn)
                idV <- paste0('Value', btn)
                idVL <- paste("Value", btn)
                insertUI(selector = '#placeholder1',
                         ui = tags$div(selectInput(id,'Evidence',nodeNamesB),
                                       id = id
                         )
                )
                insertUI(selector = '#placeholder2',
                         ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNamesB[1]])),
                                       id = idV
                         )
                )
                inserted <<- c(id, inserted)
                insertedV <<- c(idV,insertedV)
                rvs$evidence <<- c(rvs$evidence,id)
                rvs$value <<- c(rvs$value,id)
                rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
                  tryCatch({
                    valID = insertedV[which(inserted == id)]
                    updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
                  },error = function(e){
                    shinyalert::shinyalert(toString("Construct bayesian network for taking decision"), type = "error")
                  })
                }))

              },error = function(e){
                shinyalert::shinyalert(toString(e), type = "error")
              })
              shapeVector<<- rep('dot',length(nodeNamesB))
              updateSelectInput(session,'event',choices = nodeNamesB)
              weight <<- 1
              value <<- 1
              exactCheck<<-1
              output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
              updateSelectInput(session,'event',choices = nodeNamesB)
              updateSelectizeInput(session,'varselect',choices = nodeNamesB)
              updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                 "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'graph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
              updateSelectInput(session,'paramSelect',choices = nodeNamesB)
              updateSelectInput(session,"moduleSelection",choices = "graph")
              graph<<-igraph::graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
              updateSelectInput(session,"neighbornodes",choices = "")
              updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                 "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'modGroup',choices = "")
              updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNamesB[1]]),value = nlevels(DiscreteData[,nodeNamesB[1]]))
              updateSelectInput(session,"parents",choices = nodeNamesB)
              output$decisionPlot<-renderVisNetwork({validate("Build Decision Network using app")})
              output$policyPlot<-DT::renderDataTable({NULL})
              reset<<-2
              type<<-1
              tryCatch({
                if(input$tableName=="Bayesian Graph")
                {
                  output$tableOut<- DT::renderDataTable({NetworkGraph},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="Cross Validation Results")
                {
                  output$tableOut<- DT::renderDataTable({predError},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="blacklist edges")
                {
                  output$tableOut<- DT::renderDataTable({blacklistEdges},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="whitelist edges")
                {
                  output$tableOut<- DT::renderDataTable({whitelistEdges},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="Nodes")
                {
                  output$tableOut<- DT::renderDataTable({as.data.frame(nodeNamesB)},options = list(scrollX = TRUE,pageLength = 10))
                }
              },error=function(e){
                shinyalert::shinyalert(toString(e), type = "error")
              })
              updateSelectInput(session,"fromarc",choices = nodeNamesB)
              output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
            }
            else
            {
              if(input$parallel==T)
              {

                bn.hc.boot.pruned <<- bn.hc.boot[bn.hc.boot$strength >= input$edgeStrength & bn.hc.boot$direction >= input$directionStrength,]
                bn.hc.boot.average <<- cextend(averaged.network(bn.hc.boot.pruned))
                bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2,cluster = cl)
              }
              else
              {
                bn.hc.boot.pruned <<- bn.hc.boot[bn.hc.boot$strength >= input$edgeStrength & bn.hc.boot$direction >= input$directionStrength,]
                bn.hc.boot.average <<- cextend(averaged.network(bn.hc.boot.pruned))
                bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
              }
              simple<<-2
              for(elem in 1:length(inserted))
              {
                removeUI(
                  ## pass in appropriate div id
                  selector = paste0('#', inserted[elem])
                )

              }
              inserted <<- c()
              for(elem2 in 1:length(insertedV))
              {
                removeUI(
                  ## pass in appropriate div id
                  selector = paste0('#', insertedV[elem2])
                )

              }
              insertedV <<- c()
              rvs$evidence <<- c()
              rvs$value <<- c()
              rvs$evidenceObserve <<- c()
              rvs$valueObserve <<- c()
              output$distPlot <<- renderPlot(NULL)
              NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
              nodeNamesB <<- names(bn.hc.boot.average$nodes)
              EventNode <<- nodeNamesB[1]
              EvidenceNode <<- c()
              shapeVector<<- rep('dot',length(nodeNamesB))
              updateSelectInput(session,'event',choices = nodeNamesB)
              weight <<- graph.weight(bn.hc.boot,NetworkGraph)
              value <<- graph.weight(bn.hc.boot,NetworkGraph)
              exactCheck<<-1
              output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
              updateSelectInput(session,'event',choices = nodeNamesB)
              updateSelectizeInput(session,'varselect',choices = nodeNamesB)
              updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                 "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'graph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
              updateSelectInput(session,'paramSelect',choices = nodeNamesB)
              updateSelectInput(session,"moduleSelection",choices = "graph")
              graph<<-igraph::graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
              updateSelectInput(session,"neighbornodes",choices = "")
              updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                 "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'modGroup',choices = "")
              updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNamesB[1]]),value = nlevels(DiscreteData[,nodeNamesB[1]]))
              updateSelectInput(session,"parents",choices = nodeNamesB)
              output$decisionPlot<-renderVisNetwork({validate("Build Decision Network using app")})
              output$policyPlot<-DT::renderDataTable({NULL})
              reset<<-2
              type<<-2
              tryCatch({
                if(input$tableName=="Bayesian Graph")
                {
                  output$tableOut<- DT::renderDataTable({NetworkGraph},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="Cross Validation Results")
                {
                  output$tableOut<- DT::renderDataTable({predError},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="blacklist edges")
                {
                  output$tableOut<- DT::renderDataTable({blacklistEdges},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="whitelist edges")
                {
                  output$tableOut<- DT::renderDataTable({whitelistEdges},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="Nodes")
                {
                  output$tableOut<- DT::renderDataTable({as.data.frame(nodeNamesB)},options = list(scrollX = TRUE,pageLength = 10))
                }
              },error=function(e){
                shinyalert::shinyalert(toString(e), type = "error")
              })
              updateSelectInput(session,"fromarc",choices = nodeNamesB)
              output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
              save(DiscreteData,file="customDashboard/inst/cd/data.RData")
              save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
              write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
            }
          }
        }
      },error=function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
      tooltip(session)
    }
  })
  observeEvent(input$learnSBtn, {
    if(load==2)
    {
      if(check.NA(DiscreteData))
      {
        shinyalert::shinyalert("Impute missing data using pre-process tab to procede",type="info")
      }
      else if(check.discrete(DiscreteData))
      {
        shinyalert::shinyalert("Discretize data using pre-process tab to proceed",type="info")
      }
      else
      {
        tryCatch({
          if (is.null(DiscreteData))
            return(NULL)

          # Create a Progress object
          progress <- shiny::Progress$new()
          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())
          progress$set(message = "Learning network structure", value = 0)

          # Get the selected learning algorithm from the user and learn the network
          if(input$parallel==T)
          {
            if(input$alg == 'hc')
            {
              bn.hc.boot.average <<- cextend(bnlearn::hc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl,start=bn.start,score = input$algoscore,iss=input$iss,exp = INTvar))
            }
            else if(input$alg =="pc.stable")
            {
              bn.hc.boot.average <<- cextend(bnlearn::pc.stable(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl))
            }
            else if(input$alg == 'tabu')
            {
              bn.hc.boot.average <<- cextend(bnlearn::tabu(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl,start=bn.start,score = input$algoscore,iss=input$iss,exp = INTvar,tabu = 50))
            }
            else if(input$alg == 'gs')
            {
              bn.hc.boot.average <<- cextend(bnlearn::gs(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster=cl))
            }
            else if(input$alg == 'iamb')
            {
              bn.hc.boot.average <<- cextend(bnlearn::iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster=cl))
            }
            else if(input$alg == 'fast.iamb')
            {
              bn.hc.boot.average <<- cextend(bnlearn::fast.iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster=cl))
            }
            else if(input$alg=='inter.iamb')
            {
              bn.hc.boot.average <<- cextend(bnlearn::inter.iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster=cl))
            }
            else if(input$alg == 'mmhc')
            {
              bn.hc.boot.average <<- cextend(bnlearn::mmhc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster=cl))
            }
            else if(input$alg == 'rsmax2')
            {
              bn.hc.boot.average <<- cextend(bnlearn::rsmax2(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl))
            }
            else if(input$alg == 'mmpc')
            {
              bn.hc.boot.average <<- cextend(bnlearn::mmpc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl))
            }
            else if(input$alg == 'si.hiton.pc')
            {
              bn.hc.boot.average <<- cextend(bnlearn::si.hiton.pc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl))
            }
            else if(input$alg == 'aracne')
            {
              bn.hc.boot.average <<- cextend(bnlearn::aracne(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl))
            }
            else
            {
              bn.hc.boot.average <<- cextend(bnlearn::chow.liu(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl))
            }
            #bn.hc.boot.average <<- bnlearn::hc(DiscreteData)
            bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2,cluster=cl)
          }
          else
          {
            if(input$alg == 'hc')
            {
              bn.hc.boot.average <<- cextend(bnlearn::hc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start,score = input$algoscore,iss=input$iss,exp = INTvar))
            }
            else if(input$alg =="pc.stable")
            {
              bn.hc.boot.average <<- cextend(bnlearn::pc.stable(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
            }
            else if(input$alg == 'tabu')
            {
              bn.hc.boot.average <<- cextend(bnlearn::tabu(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start,score = input$algoscore,iss=input$iss,exp = INTvar,tabu = 50))
            }
            else if(input$alg == 'gs')
            {
              bn.hc.boot.average <<- cextend(bnlearn::gs(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
            }
            else if(input$alg == 'iamb')
            {
              bn.hc.boot.average <<- cextend(bnlearn::iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
            }
            else if(input$alg == 'fast.iamb')
            {
              bn.hc.boot.average <<- cextend(bnlearn::fast.iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
            }
            else if(input$alg=='inter.iamb')
            {
              bn.hc.boot.average <<- cextend(bnlearn::inter.iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
            }
            else if(input$alg == 'mmhc')
            {
              bn.hc.boot.average <<- cextend(bnlearn::mmhc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
            }
            else if(input$alg == 'rsmax2')
            {
              bn.hc.boot.average <<- cextend(bnlearn::rsmax2(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
            }
            else if(input$alg == 'mmpc')
            {
              bn.hc.boot.average <<- cextend(bnlearn::mmpc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
            }
            else if(input$alg == 'si.hiton.pc')
            {
              bn.hc.boot.average <<- cextend(bnlearn::si.hiton.pc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
            }
            else if(input$alg == 'aracne')
            {
              bn.hc.boot.average <<- cextend(bnlearn::aracne(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
            }
            else
            {
              bn.hc.boot.average <<- cextend(bnlearn::chow.liu(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
            }
            #bn.hc.boot.average <<- bnlearn::hc(DiscreteData)
            bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
          }
          shinyalert::shinyalert("Structure learning complete",type="success")
          simple<<-1
          upload<<-2
          type<<-1
          for(elem in 1:length(inserted))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', inserted[elem])
            )

          }
          inserted <<- c()
          for(elem2 in 1:length(insertedV))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', insertedV[elem2])
            )

          }
          insertedV <<- c()
          rvs$evidence <<- c()
          rvs$value <<- c()
          rvs$evidenceObserve <<- c()
          rvs$valueObserve <<- c()
          output$distPlot <<- renderPlot(NULL)
          NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
          nodeNamesB <<- names(bn.hc.boot.average$nodes)
          EventNode <<- nodeNamesB[1]
          EvidenceNode <<- c()
          tryCatch({
            btn <<- input$insertBtn
            id <- paste0('Evidence', btn)
            idL <- paste("Evidence", btn)
            idV <- paste0('Value', btn)
            idVL <- paste("Value", btn)
            insertUI(selector = '#placeholder1',
                     ui = tags$div(selectInput(id,'Evidence',nodeNamesB),
                                   id = id
                     )
            )
            insertUI(selector = '#placeholder2',
                     ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNamesB[1]])),
                                   id = idV
                     )
            )
            inserted <<- c(id, inserted)
            insertedV <<- c(idV,insertedV)
            rvs$evidence <<- c(rvs$evidence,id)
            rvs$value <<- c(rvs$value,id)
            rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
              tryCatch({
                valID = insertedV[which(inserted == id)]
                updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
              },error = function(e){
                shinyalert::shinyalert(toString("Construct bayesian network for taking decision"), type = "error")
              })
            }))

          },error = function(e){
            shinyalert::shinyalert(toString(e), type = "error")
          })
          shapeVector<<- rep('dot',length(nodeNamesB))
          updateSelectInput(session,'event',choices = nodeNamesB)
          weight <<- 1
          value <<- 1
          exactCheck<<-1
          output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
          updateSelectInput(session,'event',choices = nodeNamesB)
          updateSelectizeInput(session,'varselect',choices = nodeNamesB)
          updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                            "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                             "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'graph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
          updateSelectInput(session,'paramSelect',choices = nodeNamesB)
          updateSelectInput(session,"moduleSelection",choices = "graph")
          updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                             "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'modGroup',choices = "")
          graph<<-igraph::graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
          updateSelectInput(session,"neighbornodes",choices = "")
          updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNamesB[1]]),value = nlevels(DiscreteData[,nodeNamesB[1]]))
          updateSelectInput(session,"parents",choices = nodeNamesB)
          output$decisionPlot<-renderVisNetwork({validate("Build Decision Network using app")})
          output$policyPlot<-DT::renderDataTable({NULL})
          reset<<-2
          tryCatch({
            if(input$tableName=="Bayesian Graph")
            {
              output$tableOut<- DT::renderDataTable({NetworkGraph},options = list(scrollX = TRUE,pageLength = 10))
            }
            else if(input$tableName=="Cross Validation Results")
            {
              output$tableOut<- DT::renderDataTable({predError},options = list(scrollX = TRUE,pageLength = 10))
            }
            else if(input$tableName=="blacklist edges")
            {
              output$tableOut<- DT::renderDataTable({blacklistEdges},options = list(scrollX = TRUE,pageLength = 10))
            }
            else if(input$tableName=="whitelist edges")
            {
              output$tableOut<- DT::renderDataTable({whitelistEdges},options = list(scrollX = TRUE,pageLength = 10))
            }
            else if(input$tableName=="Nodes")
            {
              output$tableOut<- DT::renderDataTable({as.data.frame(nodeNamesB)},options = list(scrollX = TRUE,pageLength = 10))
            }
          },error=function(e){
            shinyalert::shinyalert(toString(e), type = "error")
          })
          updateSelectInput(session,"fromarc",choices = nodeNamesB)
          output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
          save(DiscreteData,file="customDashboard/inst/cd/data.RData")
          save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
          write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
        },error = function(e){
          shinyalert::shinyalert(toString(e), type = "error")
        })
      }
      tooltip(session)
    }
  })
  observeEvent(input$fromarc,{
    tryCatch({
      if(reset==2)
      {
        updateSelectInput(session,"toarc",choices = setdiff(nodeNamesB,input$fromarc))
      }
    },error = function(e){
      shinyalert::shinyalert(toString(e), type = "error")
    })
    tooltip(session)
  })
  observeEvent(input$addarc,{
    tryCatch({
      if(input$parallel==T)
      {
        bn.hc.boot.average<<-set.arc(bn.hc.boot.average,input$fromarc,input$toarc)
        bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2,cluster = cl)
      }
      else
      {
        bn.hc.boot.average<<-set.arc(bn.hc.boot.average,input$fromarc,input$toarc)
        bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
      }
      for(elem in 1:length(inserted))
      {
        removeUI(
          ## pass in appropriate div id
          selector = paste0('#', inserted[elem])
        )

      }
      inserted <<- c()
      for(elem2 in 1:length(insertedV))
      {
        removeUI(
          ## pass in appropriate div id
          selector = paste0('#', insertedV[elem2])
        )

      }
      insertedV <<- c()
      rvs$evidence <<- c()
      rvs$value <<- c()
      rvs$evidenceObserve <<- c()
      rvs$valueObserve <<- c()
      output$distPlot <<- renderPlot(NULL)
      NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
      nodeNamesB <<- names(bn.hc.boot.average$nodes)
      EventNode <<- nodeNamesB[1]
      EvidenceNode <<- c()
      tryCatch({
        btn <<- input$insertBtn
        id <- paste0('Evidence', btn)
        idL <- paste("Evidence", btn)
        idV <- paste0('Value', btn)
        idVL <- paste("Value", btn)
        insertUI(selector = '#placeholder1',
                 ui = tags$div(selectInput(id,'Evidence',nodeNamesB),
                               id = id
                 )
        )
        insertUI(selector = '#placeholder2',
                 ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNamesB[1]])),
                               id = idV
                 )
        )
        inserted <<- c(id, inserted)
        insertedV <<- c(idV,insertedV)
        rvs$evidence <<- c(rvs$evidence,id)
        rvs$value <<- c(rvs$value,id)
        rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
          tryCatch({
            valID = insertedV[which(inserted == id)]
            updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
          },error = function(e){
            shinyalert::shinyalert(toString("Construct bayesian network for taking decision"), type = "error")
          })
        }))

      },error = function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
      type<<-1
      shapeVector<<- rep('dot',length(nodeNamesB))
      updateSelectInput(session,'event',choices = nodeNamesB)
      weight <<- 1
      value <<- 1
      exactCheck<<-1
      output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
      updateSelectInput(session,'event',choices = nodeNamesB)
      updateSelectizeInput(session,'varselect',choices = nodeNamesB)
      updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                        "ellipse", "database", "text", "diamond"))
      updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                         "ellipse", "database", "text", "diamond"))
      updateSelectInput(session,'graph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
      updateSelectInput(session,'paramSelect',choices = nodeNamesB)
      updateSelectInput(session,"moduleSelection",choices = "graph")
      updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                         "ellipse", "database", "text", "diamond"))
      updateSelectInput(session,'modGroup',choices = "")
      graph<<-igraph::graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
      updateSelectInput(session,"neighbornodes",choices = "")
      updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNamesB[1]]),value = nlevels(DiscreteData[,nodeNamesB[1]]))
      updateSelectInput(session,"parents",choices = nodeNamesB)
      output$decisionPlot<-renderVisNetwork({validate("Build Decision Network using app")})
      output$policyPlot<-DT::renderDataTable({NULL})
      reset<<-2
      tryCatch({
        if(input$tableName=="Bayesian Graph")
        {
          output$tableOut<- DT::renderDataTable({NetworkGraph},options = list(scrollX = TRUE,pageLength = 10))
        }
        else if(input$tableName=="Cross Validation Results")
        {
          output$tableOut<- DT::renderDataTable({predError},options = list(scrollX = TRUE,pageLength = 10))
        }
        else if(input$tableName=="blacklist edges")
        {
          output$tableOut<- DT::renderDataTable({blacklistEdges},options = list(scrollX = TRUE,pageLength = 10))
        }
        else if(input$tableName=="whitelist edges")
        {
          output$tableOut<- DT::renderDataTable({whitelistEdges},options = list(scrollX = TRUE,pageLength = 10))
        }
        else if(input$tableName=="Nodes")
        {
          output$tableOut<- DT::renderDataTable({as.data.frame(nodeNamesB)},options = list(scrollX = TRUE,pageLength = 10))
        }
      },error=function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
      updateSelectInput(session,"fromarc",choices = nodeNamesB)
      output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
      save(DiscreteData,file="customDashboard/inst/cd/data.RData")
      save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
      write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
    },error = function(e){
      shinyalert::shinyalert(toString(e), type = "error")
    })
    tooltip(session)
  })
  observeEvent(input$RemoveArc2,{
    #print(input$postout__rows_selected)
    if(load==2)
    {
      tryCatch({
        if(reset==2)
        {
          if(!is.null(input$postout_rows_selected))
          {
            bn.hc.boot.average<<-drop.arc(bn.hc.boot.average,bn.hc.boot.average$arcs[input$postout_rows_selected,1],bn.hc.boot.average$arcs[input$postout_rows_selected,2])
            if(input$parallel==T)
            {
              bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2,cluster = cl)
            }
            else
            {
              bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
            }
            for(elem in 1:length(inserted))
            {
              removeUI(
                ## pass in appropriate div id
                selector = paste0('#', inserted[elem])
              )

            }
            inserted <<- c()
            for(elem2 in 1:length(insertedV))
            {
              removeUI(
                ## pass in appropriate div id
                selector = paste0('#', insertedV[elem2])
              )

            }
            insertedV <<- c()
            rvs$evidence <<- c()
            rvs$value <<- c()
            rvs$evidenceObserve <<- c()
            rvs$valueObserve <<- c()
            output$distPlot <<- renderPlot(NULL)
            NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
            nodeNamesB <<- names(bn.hc.boot.average$nodes)
            EventNode <<- nodeNamesB[1]
            EvidenceNode <<- c()
            tryCatch({
              btn <<- input$insertBtn
              id <- paste0('Evidence', btn)
              idL <- paste("Evidence", btn)
              idV <- paste0('Value', btn)
              idVL <- paste("Value", btn)
              insertUI(selector = '#placeholder1',
                       ui = tags$div(selectInput(id,'Evidence',nodeNamesB),
                                     id = id
                       )
              )
              insertUI(selector = '#placeholder2',
                       ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNamesB[1]])),
                                     id = idV
                       )
              )
              inserted <<- c(id, inserted)
              insertedV <<- c(idV,insertedV)
              rvs$evidence <<- c(rvs$evidence,id)
              rvs$value <<- c(rvs$value,id)
              rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
                tryCatch({
                  valID = insertedV[which(inserted == id)]
                  updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
                },error = function(e){
                  shinyalert::shinyalert(toString("Construct bayesian network for taking decision"), type = "error")
                })
              }))

            },error = function(e){
              shinyalert::shinyalert(toString(e), type = "error")
            })
            shapeVector<<- rep('dot',length(nodeNamesB))
            updateSelectInput(session,'event',choices = nodeNamesB)
            weight <<- 1
            value <<- 1
            type<<-1
            exactCheck<<-1
            tryCatch({
              if(input$tableName=="Bayesian Graph")
              {
                output$tableOut<- DT::renderDataTable({NetworkGraph},options = list(scrollX = TRUE,pageLength = 10))
              }
              else if(input$tableName=="Cross Validation Results")
              {
                output$tableOut<- DT::renderDataTable({predError},options = list(scrollX = TRUE,pageLength = 10))
              }
              else if(input$tableName=="blacklist edges")
              {
                output$tableOut<- DT::renderDataTable({blacklistEdges},options = list(scrollX = TRUE,pageLength = 10))
              }
              else if(input$tableName=="whitelist edges")
              {
                output$tableOut<- DT::renderDataTable({whitelistEdges},options = list(scrollX = TRUE,pageLength = 10))
              }
              else if(input$tableName=="Nodes")
              {
                output$tableOut<- DT::renderDataTable({as.data.frame(nodeNamesB)},options = list(scrollX = TRUE,pageLength = 10))
              }
            },error=function(e){
              shinyalert::shinyalert(toString(e), type = "error")
            })
            output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
            updateSelectInput(session,'event',choices = nodeNamesB)
            updateSelectizeInput(session,'varselect',choices = nodeNamesB)
            updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                              "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                               "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'graph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
            updateSelectInput(session,'paramSelect',choices = nodeNamesB)
            updateSelectInput(session,"moduleSelection",choices = "graph")
            graph<<-igraph::graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
            updateSelectInput(session,"neighbornodes",choices = "")
            updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNamesB[1]]),value = nlevels(DiscreteData[,nodeNamesB[1]]))
            updateSelectInput(session,"parents",choices = nodeNamesB)
            output$decisionPlot<-renderVisNetwork({validate("Build Decision Network using app")})
            output$policyPlot<-DT::renderDataTable({NULL})
            reset<<-2
            updateSelectInput(session,"fromarc",choices = nodeNamesB)
            updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                               "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'modGroup',choices = "")
            output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
            save(DiscreteData,file="customDashboard/inst/cd/data.RData")
            save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
            write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
          }
        }
      },error = function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
      tooltip(session)
    }
  })
  observeEvent(input$ReverseArc2,{
    #print(input$postout_rows_selected)
    if(load==2)
    {
      tryCatch({
        if(reset==2)
        {
          if(!is.null(input$postout_rows_selected))
          {
            bn.hc.boot.average<<-reverse.arc(bn.hc.boot.average,bn.hc.boot.average$arcs[input$postout_rows_selected,1],bn.hc.boot.average$arcs[input$postout_rows_selected,2])
            if(input$parallel==T)
            {
              bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2,cluster = cl)
            }
            else
            {
              bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
            }
            for(elem in 1:length(inserted))
            {
              removeUI(
                ## pass in appropriate div id
                selector = paste0('#', inserted[elem])
              )

            }
            inserted <<- c()
            for(elem2 in 1:length(insertedV))
            {
              removeUI(
                ## pass in appropriate div id
                selector = paste0('#', insertedV[elem2])
              )

            }
            insertedV <<- c()
            rvs$evidence <<- c()
            rvs$value <<- c()
            rvs$evidenceObserve <<- c()
            rvs$valueObserve <<- c()
            output$distPlot <<- renderPlot(NULL)
            NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
            nodeNamesB <<- names(bn.hc.boot.average$nodes)
            EventNode <<- nodeNamesB[1]
            EvidenceNode <<- c()
            tryCatch({
              btn <<- input$insertBtn
              id <- paste0('Evidence', btn)
              idL <- paste("Evidence", btn)
              idV <- paste0('Value', btn)
              idVL <- paste("Value", btn)
              insertUI(selector = '#placeholder1',
                       ui = tags$div(selectInput(id,'Evidence',nodeNamesB),
                                     id = id
                       )
              )
              insertUI(selector = '#placeholder2',
                       ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNamesB[1]])),
                                     id = idV
                       )
              )
              inserted <<- c(id, inserted)
              insertedV <<- c(idV,insertedV)
              rvs$evidence <<- c(rvs$evidence,id)
              rvs$value <<- c(rvs$value,id)
              rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
                tryCatch({
                  valID = insertedV[which(inserted == id)]
                  updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
                },error = function(e){
                  shinyalert::shinyalert(toString("Construct bayesian network for taking decision"), type = "error")
                })
              }))

            },error = function(e){
              shinyalert::shinyalert(toString(e), type = "error")
            })
            shapeVector<<- rep('dot',length(nodeNamesB))
            updateSelectInput(session,'event',choices = nodeNamesB)
            type<<-1
            weight <<- 1
            value <<- 1
            exactCheck<<-1
            output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
            updateSelectInput(session,'event',choices = nodeNamesB)
            updateSelectizeInput(session,'varselect',choices = nodeNamesB)
            updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                              "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                               "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'graph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
            updateSelectInput(session,'paramSelect',choices = nodeNamesB)
            updateSelectInput(session,"moduleSelection",choices = "graph")
            graph<<-igraph::graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
            updateSelectInput(session,"neighbornodes",choices = "")
            updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNamesB[1]]),value = nlevels(DiscreteData[,nodeNamesB[1]]))
            updateSelectInput(session,"parents",choices = nodeNamesB)
            output$decisionPlot<-renderVisNetwork({validate("Build Decision Network using app")})
            output$policyPlot<-DT::renderDataTable({NULL})
            reset<<-2
            tryCatch({
              if(input$tableName=="Bayesian Graph")
              {
                output$tableOut<- DT::renderDataTable({NetworkGraph},options = list(scrollX = TRUE,pageLength = 10))
              }
              else if(input$tableName=="Cross Validation Results")
              {
                output$tableOut<- DT::renderDataTable({predError},options = list(scrollX = TRUE,pageLength = 10))
              }
              else if(input$tableName=="blacklist edges")
              {
                output$tableOut<- DT::renderDataTable({blacklistEdges},options = list(scrollX = TRUE,pageLength = 10))
              }
              else if(input$tableName=="whitelist edges")
              {
                output$tableOut<- DT::renderDataTable({whitelistEdges},options = list(scrollX = TRUE,pageLength = 10))
              }
              else if(input$tableName=="Nodes")
              {
                output$tableOut<- DT::renderDataTable({as.data.frame(nodeNamesB)},options = list(scrollX = TRUE,pageLength = 10))
              }
            },error=function(e){
              shinyalert::shinyalert(toString(e), type = "error")
            })
            updateSelectInput(session,"fromarc",choices = nodeNamesB)
            updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                               "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'modGroup',choices = "")
            output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
            save(DiscreteData,file="customDashboard/inst/cd/data.RData")
            save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
            write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
          }
        }
      },error = function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
      tooltip(session)
    }
  })
  observeEvent(input$paramSelect,{
    if(load==2)
    {
      if(reset==2)
      {
        tryCatch({
          output$parameterPlot<-renderPlot({bn.fit.barchart(bn.hc.boot.fit[[input$paramSelect]])})
        },error = function(e){
          shinyalert::shinyalert(toString(e), type = "error")
        })
      }
      tooltip(session)
    }
  })
  observeEvent(input$parallel,{
    withProgress(message = "Building Clusters", value = 0, {
      tryCatch({
        if(input$parallel==TRUE)
        {
          check<<-2
          cl <<- parallel::makeCluster(strtoi(input$clusters), type = "SOCK")
          shinyalert::shinyalert("Parallel computing enabled",type="success")
        }
        else
        {
          if(check==2)
          {
            parallel::stopCluster(cl)
            ckeck<<-1
          }
        }
      },error=function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
    })
    tooltip(session)
  })
  observeEvent(input$insertBtn, {
    if(load==2)
    {
      withProgress(message = "Inserting Evidence", value = 0, {
        if(reset==2)
        {
          tryCatch({
            btn <<- input$insertBtn
            id <- paste0('Evidence', btn)
            idL <- paste("Evidence", btn)
            idV <- paste0('Value', btn)
            idVL <- paste("Value", btn)
            insertUI(selector = '#placeholder1',
                     ui = tags$div(selectInput(id,'Evidence',nodeNamesB),
                                   id = id
                     )
            )
            insertUI(selector = '#placeholder2',
                     ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNamesB[1]])),
                                   id = idV
                     )
            )
            inserted <<- c(id, inserted)
            insertedV <<- c(idV,insertedV)
            rvs$evidence <<- c(rvs$evidence,id)
            rvs$value <<- c(rvs$value,id)
            rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
              tryCatch({
                valID = insertedV[which(inserted == id)]
                updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
              },error = function(e){
                shinyalert::shinyalert(toString("Construct bayesian network for taking decision"), type = "error")
              })
            }))

          },error = function(e){
            shinyalert::shinyalert(toString(e), type = "error")
          })
        }
      })
      tooltip(session)
    }
  })

  observeEvent(input$removeBtn, {
    if(load==2)
    {
      if(reset==2)
      {
        tryCatch({
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[length(inserted)])
          )
          inserted <<- inserted[-length(inserted)]
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', insertedV[length(insertedV)])
          )
          insertedV <<- insertedV[-length(insertedV)]
          rvs$evidence <<- rvs$evidence[-length(inserted)]
          rvs$value <<- rvs$value[-length(insertedV)]
          rvs$evidenceObserve <<- rvs$evidenceObserve[-length(inserted)]
          rvs$valueObserve <<- rvs$valueObserve[-length(insertedV)]
        },error=function(e){
          shinyalert::shinyalert(toString(e), type = "error")
        })
      }
      tooltip(session)
    }
  })
  observeEvent(input$event,{
    if(load==2)
    {
      if(reset==2)
      {
        tryCatch({
          if(input$event=="")
          {
            updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNamesB[1]]),value = nlevels(DiscreteData[,nodeNamesB[1]]))
          }
          else
          {
            updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,input$event]),value = nlevels(DiscreteData[,input$event]))
          }
          EventNode <<- input$event
        },error=function(e){
          shinyalert::shinyalert(toString(e), type = "error")
        })
      }
      tooltip(session)
    }
  })
  observeEvent(input$exactInference,{
    tryCatch({
      withProgress(message = "Learning Exact Inference", value = 0, {
        if(load==2)
        {
          if(reset==2)
          {
            bn.jtree <<- compile(as.grain(bn.hc.boot.fit))
            exactCheck<<-2
            shinyalert("Exact inferences learned",type = "success")
          }
        }})
    },error=function(e)
      {
      exactCheck<<-1
      shinyalert(e,type="error")
    })
  })
  observeEvent(input$plotBtn,{
    if(load==2)
    {
      withProgress(message = "Learning Inference", value = 0, {
        if(reset==2)
        {
          tryCatch({
            confidence<<-1
            str1 <<- ""
            str2<<-""
            str3<<-""
            count =1
            for(elem in inserted)
            {
              vid = insertedV[which(inserted == elem)]
              str1 <<- paste0(str1,"(", input[[elem]], "=='", input[[vid]], "')")
              str2<<-paste(str2,input[[elem]],", ")
              str3<<-paste0(str3, input[[elem]], "='", input[[vid]], "'")
              if(count!=length(inserted))
              {
                str1 <<- paste0(str1," & ")
                str3 <<- paste0(str3,",")
              }
              count = count + 1
            }
            str3<<-paste0("list(",str3,")")
            if(input$exact==T)
            {
              if(exactCheck==2)
              {
                evidenceV = setEvidence(bn.jtree, evidence=eval(parse(text = str3)))
                probs = ((querygrain(evidenceV,nodes=input$event))[[input$event]])[1:input$NumBar]
              }
              else
              {
                shinyalert("Learn exact inferences on the new or modified structure",type = "info")
              }
            }
            else
            {
              probs = prop.table(table(cpdist(bn.hc.boot.fit,input$event,evidence = eval(parse(text = str1)))))[1:input$NumBar]
            }
            output$distPlot = renderPlot({par(mar=c(5,3,3,3))
              par(oma=c(5,3,3,3))
              barx<<-barplot(probs,
                             col = "lightblue",
                             main = paste("probability of ",input$event," conditioned on ",substr(str2,1,(nchar(str2)-2))),
                             border = NA,
                             xlab = "",
                             ylab = "Probabilities",
                             ylim = c(0,1),
                             las=2,
                             cex.names=as.numeric(input$plotFont))
              text(x = barx,y = round(probs,digits = 4),label = round(probs,digits = 4), pos = 3, cex = as.numeric(input$valueFont), col = "black")
            })
            updateRadioGroupButtons(session,'bayesianOption',selected = "Infer Decisions")
          },error = function(e){
            shinyalert::shinyalert(toString(e), type = "error")
          })
        }
      })
      tooltip(session)
    }
  })
  observeEvent(input$plotStrengthBtn,{
    if(load==2)
    {
      withProgress(message = "Learning Inference", value = 0, {
        if(reset==2)
        {
          tryCatch({
            confidence<<-2
            probT = c()
            for(i in 1:input$plotStrengthBtn)
            {
              str1 <<- ""
              str2<<-""
              str3<<-""
              count =1
              for(elem in inserted)
              {
                vid = insertedV[which(inserted == elem)]
                str1 <<- paste0(str1,"(", input[[elem]], "=='", input[[vid]], "')")
                str2<<-paste(str2,input[[elem]],", ")
                str3<<-paste0(str3, input[[elem]], "='", input[[vid]], "'")
                if(count!=length(inserted))
                {
                  str1 <<- paste0(str1," & ")
                  str3 <<- paste0(str3,",")
                }
                count = count + 1
              }
              str3<<-paste0("list(",str3,")")
              if(input$exact==T)
              {
                if(exactCheck==2)
                {
                  evidenceV = setEvidence(bn.jtree, evidence=eval(parse(text = str3)))
                  probs = ((querygrain(evidenceV,nodes=input$event))[[input$event]])
                }
                else
                {
                  shinyalert("Learn exact inferences on the new or modified structure",type = "info")
                }
              }
              else
              {
                probs = prop.table(table(cpdist(bn.hc.boot.fit,input$event,evidence = eval(parse(text = str1)))))
              }
              probT = rbind(probT,probs)
            }
            ee = 1
            ee$mean = colMeans(probT)
            ee$sd = apply(probT, 2, sd)
            output$distPlot = renderPlot({par(mar=c(5,3,3,3))
              par(oma=c(5,3,3,3))
              barx <<-barplot(ee$mean[1:input$NumBar],
                              col = "lightblue",
                              main = paste("probability of ",input$event," conditioned on ",substr(str2,1,(nchar(str2)-2))),
                              border = NA,
                              xlab = "",
                              ylab = "Probabilities",
                              ylim = c(0,1),
                              las=2,
                              cex.names=as.numeric(input$plotFont))
              text(x = barx,y = round(ee$mean[1:input$NumBar],digits = 4),label = round(ee$mean[1:input$NumBar],digits = 4), pos = 3, cex = as.numeric(input$valueFont), col = "black")
              error.bar(barx,ee$mean[1:input$NumBar], 1.96*ee$sd[1:input$NumBar]/sqrt(input$plotStrengthBtn))})
            updateRadioGroupButtons(session,'bayesianOption',selected = "Infer Decisions")

          },error = function(e){
            shinyalert::shinyalert(toString(e), type = "error")
          })
        }
      })
      tooltip(session)
    }
  })
  observeEvent(input$sortPlot,{
    if(load==2)
    {
      withProgress(message = "Learning Inference", value = 0, {
        if(reset==2)
        {
          if(confidence==1)
          {
            tryCatch({
              confidence<<-1
              str1 <<- ""
              str2<<-""
              str3<<-""
              count =1
              for(elem in inserted)
              {
                vid = insertedV[which(inserted == elem)]
                str1 <<- paste0(str1,"(", input[[elem]], "=='", input[[vid]], "')")
                str2<<-paste(str2,input[[elem]],", ")
                str3<<-paste0(str3, input[[elem]], "='", input[[vid]], "'")
                if(count!=length(inserted))
                {
                  str1 <<- paste0(str1," & ")
                  str3 <<- paste0(str3,",")
                }
                count = count + 1
              }
              str3<<-paste0("list(",str3,")")
              if(input$exact==T)
              {
                if(exactCheck==2)
                {
                  evidenceV = setEvidence(bn.jtree, evidence=eval(parse(text = str3)))
                  probs = sort(((querygrain(evidenceV,nodes=input$event))[[input$event]]),decreasing = T)[1:input$NumBar]
                }
                else
                {
                  shinyalert("Learn exact inferences on the new or modified structure",type = "info")
                }
              }
              else
              {
                probs = sort(prop.table(table(cpdist(bn.hc.boot.fit,input$event,evidence = eval(parse(text = str1))))),decreasing = T)[1:input$NumBar]
              }
              output$distPlot = renderPlot({par(mar=c(5,3,3,3))
                par(oma=c(5,3,3,3))
                barx<<-barplot(probs,
                               col = "lightblue",
                               main = paste("probability of ",input$event," conditioned on ",substr(str2,1,(nchar(str2)-2))),
                               border = NA,
                               xlab = "",
                               ylab = "Probabilities",
                               ylim = c(0,1),
                               las=2,
                               cex.names=as.numeric(input$plotFont))
                text(x = barx,y = round(probs,digits = 4),label = round(probs,digits = 4), pos = 3, cex = as.numeric(input$valueFont), col = "black")
              })
              updateRadioGroupButtons(session,'bayesianOption',selected = "Infer Decisions")

            },error = function(e){
              shinyalert::shinyalert(toString(e), type = "error")
            })
          }
          else
          {
            tryCatch({
              confidence<<-2
              probT = c()
              for(i in 1:input$plotStrengthBtn)
              {
                str1 <<- ""
                str2<<-""
                str3<<-""
                count =1
                for(elem in inserted)
                {
                  vid = insertedV[which(inserted == elem)]
                  str1 <<- paste0(str1,"(", input[[elem]], "=='", input[[vid]], "')")
                  str2<<-paste(str2,input[[elem]],", ")
                  str3<<-paste0(str3, input[[elem]], "='", input[[vid]], "'")
                  if(count!=length(inserted))
                  {
                    str1 <<- paste0(str1," & ")
                    str3 <<- paste0(str3,",")
                  }
                  count = count + 1
                }
                if(input$exact==T)
                {
                  if(exactCheck==2)
                  {
                    evidenceV = setEvidence(bn.jtree, evidence=eval(parse(text = str3)))
                    probs = ((querygrain(evidenceV,nodes=input$event))[[input$event]])
                  }
                  else
                  {
                    shinyalert("Learn exact inferences on the new or modified structure",type = "info")
                  }
                }
                else
                {
                  probs = prop.table(table(cpdist(bn.hc.boot.fit,input$event,evidence = eval(parse(text = str1)))))
                }
                probT = rbind(probT,probs)
              }
              ee = 1
              ee$mean = colMeans(probT)
              ee$sd = apply(probT, 2, sd)
              nm = names(sort(ee$mean,decreasing = T))[1:input$NumBar]
              output$distPlot = renderPlot({par(mar=c(5,3,3,3))
                par(oma=c(5,3,3,3))
                barx <<-barplot(ee$mean[nm],
                                col = "lightblue",
                                main = paste("probability of ",input$event," conditioned on ",substr(str2,1,(nchar(str2)-2))),
                                border = NA,
                                xlab = "",
                                ylab = "Probabilities",
                                ylim = c(0,1),
                                las=2,
                                cex.names=as.numeric(input$plotFont))
                text(x = barx,y = round(ee$mean[nm],digits = 4),label = round(ee$mean[nm],digits = 4), pos = 3, cex = as.numeric(input$valueFont), col = "black")
                error.bar(barx,ee$mean[nm], 1.96*ee$sd[nm]/sqrt(input$plotStrengthBtn))})
              updateRadioGroupButtons(session,'bayesianOption',selected = "Infer Decisions")
            },error = function(e){
              shinyalert::shinyalert(toString(e), type = "error")
            })
          }
        }
      })
      tooltip(session)
    }
  })
  observeEvent(input$moduleSelection,{
    if(load==2)
    {
      withProgress(message = "Loading Module", value = 0, {
        if(reset==2)
        {
          tryCatch({
            if(input$moduleSelection!='graph')
            {
              selectedNodes<<-communities[[lengthCom[input$moduleSelection]]]
              from<-c()
              to<-c()
              for(i in 1:length(data.frame(directed.arcs(bn.hc.boot.average))[,1]))
              {
                if(is.element(data.frame(directed.arcs(bn.hc.boot.average))[i,1],selectedNodes))
                {
                  from<-c(from,i)
                }
                if(is.element(data.frame(directed.arcs(bn.hc.boot.average))[i,2],selectedNodes))
                {
                  to<-c(to,i)
                }
              }
              pruneGraph<<-data.frame(directed.arcs(bn.hc.boot.average))[intersect(from,to),]
              NetworkGraph<<-pruneGraph
              shapeVector<<-rep('dot',length(communities[[input$moduleSelection]]))
              for(elem in 1:length(inserted))
              {
                removeUI(
                  selector = paste0('#', inserted[elem])
                )

              }
              inserted <<- c()
              for(elem2 in 1:length(insertedV))
              {
                removeUI(
                  selector = paste0('#', insertedV[elem2])
                )

              }
              insertedV <<- c()
              rvs$evidence <<- c()
              rvs$value <<- c()
              rvs$evidenceObserve <<- c()
              rvs$valueObserve <<- c()
              output$distPlot <<- renderPlot(NULL)
              nodeNamesB <<- selectedNodes
              EventNode <<- nodeNamesB[1]
              EvidenceNode <<- c()
              tryCatch({
                btn <<- input$insertBtn
                id <- paste0('Evidence', btn)
                idL <- paste("Evidence", btn)
                idV <- paste0('Value', btn)
                idVL <- paste("Value", btn)
                insertUI(selector = '#placeholder1',
                         ui = tags$div(selectInput(id,'Evidence',nodeNamesB),
                                       id = id
                         )
                )
                insertUI(selector = '#placeholder2',
                         ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNamesB[1]])),
                                       id = idV
                         )
                )
                inserted <<- c(id, inserted)
                insertedV <<- c(idV,insertedV)
                rvs$evidence <<- c(rvs$evidence,id)
                rvs$value <<- c(rvs$value,id)
                rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
                  tryCatch({
                    valID = insertedV[which(inserted == id)]
                    updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
                  },error = function(e){
                    shinyalert::shinyalert(toString("Construct bayesian network for taking decision"), type = "error")
                  })
                }))

              },error = function(e){
                shinyalert::shinyalert(toString(e), type = "error")
              })
              shapeVector<<- rep('dot',length(nodeNamesB))
              updateSelectInput(session,'event',choices = nodeNamesB)
              if(type==2)
              {
                weight <<- graph.weight(bn.hc.boot,NetworkGraph)
                value <<- graph.weight(bn.hc.boot,NetworkGraph)
                output$netPlot<-renderVisNetwork({graph.custom(pruneGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
              }
              else
              {
                weight <<- 1
                value <<- 1
                output$netPlot<-renderVisNetwork({graph.custom(pruneGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
              }
              tryCatch({
                if(input$tableName=="Bayesian Graph")
                {
                  output$tableOut<- DT::renderDataTable({NetworkGraph},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="Cross Validation Results")
                {
                  output$tableOut<- DT::renderDataTable({predError},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="blacklist edges")
                {
                  output$tableOut<- DT::renderDataTable({blacklistEdges},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="whitelist edges")
                {
                  output$tableOut<- DT::renderDataTable({whitelistEdges},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="Nodes")
                {
                  output$tableOut<- DT::renderDataTable({as.data.frame(nodeNamesB)},options = list(scrollX = TRUE,pageLength = 10))
                }
              },error=function(e){
                shinyalert::shinyalert(toString(e), type = "error")
              })
              updateSelectizeInput(session,'varselect',choices = nodeNamesB)
              updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                 "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'graph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
              updateSelectInput(session,'paramSelect',choices = nodeNamesB)
              graph<<-igraph::graph_from_edgelist(as.matrix(pruneGraph),directed = TRUE)
              updateSelectInput(session,"neighbornodes",choices = "")
              updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                 "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'modGroup',choices = input$moduleSelection)
              updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNamesB[1]]),value = nlevels(DiscreteData[,nodeNamesB[1]]))
            }
            else
            {
              for(elem in 1:length(inserted))
              {
                removeUI(
                  selector = paste0('#', inserted[elem])
                )

              }
              inserted <<- c()
              for(elem2 in 1:length(insertedV))
              {
                removeUI(
                  selector = paste0('#', insertedV[elem2])
                )

              }
              insertedV <<- c()
              rvs$evidence <<- c()
              rvs$value <<- c()
              rvs$evidenceObserve <<- c()
              rvs$valueObserve <<- c()
              output$distPlot <<- renderPlot(NULL)
              NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
              nodeNamesB <<- names(bn.hc.boot.average$nodes)
              EventNode <<- nodeNamesB[1]
              EvidenceNode <<- c()
              tryCatch({
                btn <<- input$insertBtn
                id <- paste0('Evidence', btn)
                idL <- paste("Evidence", btn)
                idV <- paste0('Value', btn)
                idVL <- paste("Value", btn)
                insertUI(selector = '#placeholder1',
                         ui = tags$div(selectInput(id,'Evidence',nodeNamesB),
                                       id = id
                         )
                )
                insertUI(selector = '#placeholder2',
                         ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNamesB[1]])),
                                       id = idV
                         )
                )
                inserted <<- c(id, inserted)
                insertedV <<- c(idV,insertedV)
                rvs$evidence <<- c(rvs$evidence,id)
                rvs$value <<- c(rvs$value,id)
                rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
                  tryCatch({
                    valID = insertedV[which(inserted == id)]
                    updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
                  },error = function(e){
                    shinyalert::shinyalert(toString("Construct bayesian network for taking decision"), type = "error")
                  })
                }))

              },error = function(e){
                shinyalert::shinyalert(toString(e), type = "error")
              })
              shapeVector<<- rep('dot',length(nodeNamesB))
              updateSelectInput(session,'event',choices = nodeNamesB)
              if(type==2)
              {
                weight <<- graph.weight(bn.hc.boot,NetworkGraph)
                value <<- graph.weight(bn.hc.boot,NetworkGraph)
                output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
              }
              else
              {
                weight <<- 1
                value <<- 1
                output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
              }
              tryCatch({
                if(input$tableName=="Bayesian Graph")
                {
                  output$tableOut<- DT::renderDataTable({NetworkGraph},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="Cross Validation Results")
                {
                  output$tableOut<- DT::renderDataTable({predError},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="blacklist edges")
                {
                  output$tableOut<- DT::renderDataTable({blacklistEdges},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="whitelist edges")
                {
                  output$tableOut<- DT::renderDataTable({whitelistEdges},options = list(scrollX = TRUE,pageLength = 10))
                }
                else if(input$tableName=="Nodes")
                {
                  output$tableOut<- DT::renderDataTable({as.data.frame(nodeNamesB)},options = list(scrollX = TRUE,pageLength = 10))
                }
              },error=function(e){
                shinyalert::shinyalert(toString(e), type = "error")
              })
              updateSelectInput(session,'event',choices = nodeNamesB)
              updateSelectizeInput(session,'varselect',choices = nodeNamesB)
              updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                 "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'graph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
              updateSelectInput(session,'paramSelect',choices = nodeNamesB)
              graph<<-igraph::graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
              updateSelectInput(session,"neighbornodes",choices = "")
              updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                 "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'modGroup',choices = names(communities))
              updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNamesB[1]]),value = nlevels(DiscreteData[,nodeNamesB[1]]))
            }
          },error=function(e){
            shinyalert::shinyalert(toString(e), type = "error")
          })
        }
      })
      tooltip(session)
    }
  })
  observeEvent(input$current_node_id,{
    if(reset==2)
    {
      tryCatch({
        if(!is.null(input$current_node_id))
        {
          if(input$degreeN>1)
          {
            nlist<<-ego(graph,input$degreeN,nodes = input$current_node_id, mode = c("all", "out", "in"),mindist = 0)
            nlistP<<-ego(graph,input$degreeN-1,nodes = input$current_node_id, mode = c("all", "out", "in"),mindist = 0)
            diffList<<-setdiff(nlist[[1]]$name,nlistP[[1]]$name)
            updateSelectInput(session,"neighbornodes",choices = diffList)
          }
          else
          {
            nlist<<-ego(graph,input$degreeN,nodes = input$current_node_id, mode = c("all", "out", "in"),mindist = 0)
            updateSelectInput(session,"neighbornodes",choices = setdiff(nlist[[1]]$name,input$current_node_id))
          }

        }
      },error=function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
    }
    tooltip(session)
  })
  observeEvent(input$Acurrent_node_id,{
    if(load==2)
    {
      if(assocReset==2)
      {
        tryCatch({
          if(!is.null(input$Acurrent_node_id))
          {
            if(input$AdegreeN>1)
            {
              nlist<<-ego(Agraph,input$AdegreeN,nodes = input$Acurrent_node_id, mode = c("all", "out", "in"),mindist = 0)
              nlistP<<-ego(Agraph,input$AdegreeN-1,nodes = input$Acurrent_node_id, mode = c("all", "out", "in"),mindist = 0)
              diffList<<-setdiff(nlist[[1]]$name,nlistP[[1]]$name)
              updateSelectInput(session,"Aneighbornodes",choices = diffList)
            }
            else
            {
              nlist<<-ego(Agraph,input$AdegreeN,nodes = input$Acurrent_node_id, mode = c("all", "out", "in"),mindist = 0)
              updateSelectInput(session,"Aneighbornodes",choices = setdiff(nlist[[1]]$name,input$Acurrent_node_id))
            }

          }
        },error=function(e){
          shinyalert::shinyalert(toString(e), type = "error")
        })
      }
      tooltip(session)
    }
  })
  observeEvent(input$Bcommunities,{
    if(load==2)
    {
      tryCatch({
        if(reset==2)
        {
          communities<<-custom.Modules(NetworkGraph,input$moduleAlgo)
          names(communities)<<-paste("Module",c(1:length(communities)),sep=" ")
          lengthCom<<-c()
          for(n in names(communities))
          {
            lengthCom<<-c(lengthCom,length(communities[[n]]))
          }
          lengthCom<<-order(lengthCom,decreasing = T)
          names(lengthCom)<<-paste("Module",c(1:length(communities)),sep=" ")
          updateSelectInput(session,"moduleSelection",choices = c("graph",names(communities)))
          updateSelectInput(session,'modGroup',choices = names(communities))
          shinyalert::shinyalert("Module detection successfull",type="success")
        }
      },error=function(e){
        shinyalert::shinyalert("Module detection failed",type="error")
        updateSelectInput(session,"moduleSelection",choices = "graph")
        updateSelectInput(session,'modGroup',choices = "")
      })
      tooltip(session)
    }
  })
  observeEvent(input$Acommunities,{
    if(load==2)
    {
      tryCatch({
        if(assocReset==2)
        {
          Acommunities<<-custom.Modules(assocNetworkprune,input$AmoduleAlgo)
          names(Acommunities)<<-paste("Module",c(1:length(Acommunities)),sep=" ")
          AlengthCom<<-c()
          for(n in names(Acommunities))
          {
            AlengthCom<<-c(AlengthCom,length(Acommunities[[n]]))
          }
          AlengthCom<<-order(AlengthCom,decreasing = T)
          names(AlengthCom)<<-paste("Module",c(1:length(Acommunities)),sep=" ")
          updateSelectInput(session,"AmoduleSelection",choices = c("graph",names(Acommunities)))
          updateSelectInput(session,'AmodGroup',choices = names(Acommunities))
          shinyalert::shinyalert("Module detection successfull",type="success")
        }
      },error=function(e){
        shinyalert::shinyalert("Module detection failed",type="error")
        updateSelectInput(session,"AmoduleSelection",choices = "graph")
        updateSelectInput(session,'AmodGroup',choices = "")
      })
      tooltip(session)
    }
  })
  observeEvent(input$AmoduleSelection,{
    if(load==2)
    {
      withProgress(message = "Loading Module", value = 0, {
        if(assocReset==2)
        {
          tryCatch({
            if(input$AmoduleSelection!='graph')
            {
              AselectedNodes<<-Acommunities[[AlengthCom[input$AmoduleSelection]]]
              from<-c()
              to<-c()
              for(i in 1:dim(data.frame(assocNetwork[which(assocNetwork[,3]>input$threshold),]))[1])
              {
                if(is.element(data.frame(assocNetwork[which(assocNetwork[,3]>input$threshold),])[i,1],AselectedNodes))
                {
                  from<-c(from,i)
                }
                if(is.element(data.frame(assocNetwork[which(assocNetwork[,3]>input$threshold),])[i,2],AselectedNodes))
                {
                  to<-c(to,i)
                }
              }
              assocNetworkprune<<-assocNetwork[which(assocNetwork[,3]>input$threshold),]
              assocNetworkprune<<-assocNetworkprune[intersect(from,to),]
              shapeVectorAssoc<<-rep('dot',length(unique(c(assocNetworkprune[,1],assocNetworkprune[,2]))))
              updateSelectizeInput(session,'Avarselect',choices = unique(c(assocNetworkprune[,1],assocNetworkprune[,2])))
              updateSelectInput(session,'Avarshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'Avarshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                 "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'Agraph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
              updateSelectInput(session,"Aneighbornodes",choices = "")
              updateSelectInput(session,'Avarshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                 "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'AmodGroup',choices = input$AmoduleSelection)
              output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),input$Adegree,input$Agraph_layout,shapeVectorAssoc,input$assocFont)})
              updateSelectInput(session,"Aneighbornodes",choices = "")
              output$assocTable<- DT::renderDataTable({assocNetworkprune},options = list(scrollX = TRUE,pageLength = 10))
            }
            else
            {
              assocNetworkprune<<-assocNetwork[which(assocNetwork[,3]>input$threshold),]
              shapeVectorAssoc<<- rep('dot',length(unique(c(assocNetworkprune[,1],assocNetworkprune[,2]))))
              updateSelectizeInput(session,'Avarselect',choices = unique(c(assocNetworkprune[,1],assocNetworkprune[,2])))
              updateSelectInput(session,'Avarshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'Avarshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                 "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'Agraph_layout',choices = c("layout_nicely (Recommended)"="layout_nicely","layout_as_star","layout_as_tree (Recommended)"="layout_as_tree","layout_in_circle","layout_with_sugiyama (Recommended)"="layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds (Recommended)"="layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
              updateSelectInput(session,"Aneighbornodes",choices = "")
              updateSelectInput(session,'Avarshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                 "ellipse", "database", "text", "diamond"))
              updateSelectInput(session,'AmodGroup',choices = names(Acommunities))
              output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),input$Adegree,input$Agraph_layout,shapeVectorAssoc,input$assocFont)})
              updateSelectInput(session,"Aneighbornodes",choices = "")
              output$assocTable<- DT::renderDataTable({assocNetworkprune},options = list(scrollX = TRUE,pageLength = 10))
            }
          },error=function(e){
            shinyalert::shinyalert(toString(e), type = "error")
          })
        }
      })
      tooltip(session)
    }
  })
  observeEvent(input$Agroup3,{
    if(load==2)
    {
      if(assocReset==2)
      {
        if(input$AmodGroup!="")
        {
          tryCatch({
            AselectedNodes<<-Acommunities[[AlengthCom[input$AmodGroup]]]
            shapeVectorAssoc[which(unique(c(assocNetworkprune[,1],assocNetworkprune[,2])) %in% AselectedNodes)] <<- input$Avarshape3
            output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),input$Adegree,input$Agraph_layout,shapeVectorAssoc,input$assocFont)})
            updateSelectInput(session,"Aneighbornodes",choices = "")
          },error = function(e){
            shinyalert::shinyalert(toString(e), type = "error")

          })
        }
      }
      tooltip(session)
    }
  })
  observeEvent(input$group3,{
    if(load==2)
    {
      if(reset==2)
      {
        if(input$modGroup!="")
        {
          tryCatch({
            selectedNodes<<-communities[[lengthCom[input$modGroup]]]
            shapeVector[which(nodeNamesB %in% selectedNodes)] <<- input$varshape3
            for(elem in inserted)
            {
              EvidenceNode = c(EvidenceNode,input[[elem]])
            }
            EventNode = input$event
            output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
            updateSelectInput(session,"neighbornodes",choices = "")
          },error = function(e){
            shinyalert::shinyalert(toString(e), type = "error")

          })
        }
      }
      tooltip(session)
    }
  })
  observeEvent(input$degree,{
    if(load==2)
    {
      if(reset==2)
      {
        tryCatch({
          for(elem in inserted)
          {
            EvidenceNode = c(EvidenceNode,input[[elem]])
          }
          EventNode = input$event
          output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
          updateSelectInput(session,"neighbornodes",choices = "")
        },error = function(e){
          shinyalert::shinyalert(toString(e), type = "error")

        })
      }
      tooltip(session)
    }
  })
  observeEvent(input$Adegree,{
    if(load==2)
    {
      if(assocReset==2)
      {
        output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),input$Adegree,input$Agraph_layout,shapeVectorAssoc,input$assocFont)})
        updateSelectInput(session,"Aneighbornodes",choices = "")
      }
      tooltip(session)
    }
  })
  observeEvent(input$graph_layout,{
    if(load==2)
    {
      if(reset==2)
      {
        tryCatch({
          for(elem in inserted)
          {
            EvidenceNode = c(EvidenceNode,input[[elem]])
          }
          EventNode<<-input$event
          output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
          updateSelectInput(session,"neighbornodes",choices = "")
        },error = function(e){
          shinyalert::shinyalert(toString(e), type = "error")

        })
      }
      tooltip(session)
    }
  })
  observeEvent(input$Agraph_layout,{
    if(load==2)
    {
      if(assocReset==2)
      {
        output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),input$Adegree,input$Agraph_layout,shapeVectorAssoc,input$assocFont)})
        updateSelectInput(session,"Aneighbornodes",choices = "")
      }
      tooltip(session)
    }
  })
  observeEvent(input$graphBtn,{
    if(load==2)
    {
      if(reset==2)
      {
        tryCatch({
          for(elem in inserted)
          {
            EvidenceNode = c(EvidenceNode,input[[elem]])
          }
          EventNode<<-input$event
          output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
          updateSelectInput(session,"neighbornodes",choices = "")
        },error = function(e){
          shinyalert::shinyalert(toString(e), type = "error")

        })
      }
      tooltip(session)
    }
  })
  observeEvent(input$graphBtn2,{
    if(load==2)
    {
      if(assocReset==2)
      {
        tryCatch({
          assocNetworkprune<<- assocNetwork[which(assocNetwork[,3]>input$threshold),]
          output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),input$Adegree,input$Agraph_layout,shapeVectorAssoc,input$assocFont)})
          updateSelectInput(session,"Aneighbornodes",choices = "")
        },error = function(e){
          shinyalert::shinyalert(toString(e), type = "error")

        })
      }
      tooltip(session)
    }
  })
  observeEvent(input$group,{
    if(load==2)
    {
      if(reset==2)
      {
        tryCatch({
          shapeVector[which(nodeNamesB %in% input$varselect)] <<- input$varshape
          for(elem in inserted)
          {
            EvidenceNode = c(EvidenceNode,input[[elem]])
          }
          EventNode<<-input$event
          output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
          updateSelectInput(session,"neighbornodes",choices = "")
        },error = function(e){
          shinyalert::shinyalert(toString(e), type = "error")

        })
      }
      tooltip(session)
    }
  })
  observeEvent(input$Agroup,{
    if(load==2)
    {
      if(assocReset==2)
      {
        shapeVectorAssoc[which(unique(c(assocNetworkprune[,1],assocNetworkprune[,2])) %in% input$Avarselect)] <<- input$Avarshape
        output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),input$Adegree,input$Agraph_layout,shapeVectorAssoc,input$assocFont)})
        updateSelectInput(session,"Aneighbornodes",choices = "")
      }
      tooltip(session)
    }
  })
  observeEvent(input$Agroup2,{
    if(load==2)
    {
      if(assocReset==2)
      {
        shapeVectorAssoc<<-shapeVectorAssoc[1:length(unique(c(assocNetworkprune[,1],assocNetworkprune[,2])))]
        shapeVectorAssoc[eval(parse(text = input$Avarselectvector))] <<- input$Avarshape2
        output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),input$Adegree,input$Agraph_layout,shapeVectorAssoc,input$assocFont)})
        updateSelectInput(session,"Aneighbornodes",choices = "")
      }
      tooltip(session)
    }
  })
  observeEvent(input$group2,{
    if(load==2)
    {
      if(reset==2)
      {
        tryCatch({
          shapeVector<<-shapeVector[1:length(nodeNamesB)]
          shapeVector[eval(parse(text = input$varselectvector))] <<- input$varshape2
          for(elem in inserted)
          {
            EvidenceNode = c(EvidenceNode,input[[elem]])
          }
          EventNode<<-input$event
          output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNamesB,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value,input$bayesFont)})
          updateSelectInput(session,"neighbornodes",choices = "")
        },error = function(e){
          shinyalert::shinyalert(toString(e), type = "error")

        })
      }
      tooltip(session)
    }
  })


  #homeIntroduction Event
  observeEvent(input$homeIntro,{
      print(input$sidebarMenu)
      if(input$sidebarMenu == "Home")
      {introjs(session, options = list(steps = homeHelp))}
      else if(input$sidebarMenu == "Structure")
      {
        print(input$control_tabs)
        if(input$control_tabs == "Data")
        {
          introjs(session, options = list(steps = dataHelp))
        }
        else if(input$control_tabs == "Graph")
        introjs(session, options = list(steps = graphHelp))

      }
    })
  output$dashboard<-downloadHandler(
    filename = "customDashboard.tar.gz",
    content = function(filename){
      if(load==2)
      {
        if(reset==2)
        {
          tar(filename,"customDashboard", compression = 'gzip', tar=Sys.getenv("tar"))
        }
      }
    }
  )
  observeEvent(input$parents,{
    tryCatch({
      if(reset==2)
      {
        DF<<- data.frame(Variable_states = levels(DiscreteData[,input$parents]),payoff = 1)
        output$payoff <- renderRHandsontable({
          rhandsontable(DF, stretchH = "all")
        })

      }
      output$policyPlot<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'),rownames=FALSE)
    },error=function(e){
      print(e)
    })

  })
  observeEvent(input$buildDecisionNet2,{
    tryCatch({
      if(reset==2 && input$parents!="")
      {
        model <- as.character(bn.hc.boot.average)
        model <- model <- gsub(model,pattern = "\\:",replacement = "*",x = model)
        model <- gsub(model,pattern = "\\]\\[",replacement = "+",x = model)
        model <- gsub(model,pattern = "\\]|\\[",replacement = "",x = model)
        model <- as.formula(paste("~",model,sep=""))
        strAdd = paste("Payoff"," | ",input$parents,sep = "")
        model <- as.formula(paste(paste(as.character(model)[1],as.character(model)[2],sep = ""),strAdd,sep = " + "))
        newData<<-DiscreteData
        newData[["Payoff"]] = 1
        DF<<-hot_to_r(input$payoff)
        for(l in levels(DiscreteData[[input$parents]]))
        {
          ind = which(DiscreteData[[input$parents]]==l)
          ind2 = which(DF$Variable_states == l)
          newData[ind,ncol(newData)] = as.numeric(DF[ind2,2])
        }
        newData<<-newData
        #print(newData$Payoff)
        Dnet<<-HydeNetwork(model,data = newData)
        decisionNodes <<-c()
        utilityNodes <<-c()
        updateSelectInput(session,"decisionNode",choices = c(nodeNamesB,"Payoff"))
        updateSelectInput(session,"utilityNode",choices = c(nodeNamesB,"Payoff"))
        updateSelectInput(session,"policyNode",choices = c(nodeNamesB,"Payoff"))
        netName<<-c(nodeNamesB,"Payoff")
        netGraph<<-directed.arcs(bn.hc.boot.average)
        netGraph<<- rbind(netGraph,c(input$parents,"Payoff"))
        netGraph<<-netGraph
        utilityNodes<<-c(utilityNodes,"Payoff")
        Dnet[["nodeUtility"]][as.character("Payoff")]<<-TRUE
        utilityVar<<-"Payoff"
        Dnet<<-Dnet
        output$decisionPlot<<-renderVisNetwork({graph.custom.decision(netGraph,netName,decisionNodes,utilityNodes,TRUE)})
      }

    },error=function(e){
      print(e)
    })

  })
  observeEvent(input$set_decision,{
    if(reset==2)
    {
      decisionNodes<<-c(decisionNodes,input$decisionNode)
      Dnet[["nodeDecision"]][as.character(input$decisionNode)]<<-TRUE
      Dnet<<-Dnet
      output$decisionPlot<<-renderVisNetwork({graph.custom.decision(netGraph,netName,decisionNodes,utilityNodes,TRUE)})
    }
  })
  observeEvent(input$set_policy,{
    withProgress(message = "Building Policy table", value = 0, {
      tryCatch({
        policyVars <<- utilityVar
        policies<<-policyMatrix(Dnet)
        invisible(CNets <- compileDecisionModel(Dnet, policyMatrix = policies))
        samples <- lapply(CNets,HydeSim,variable.names = policyVars,n.iter=1000, trace=F)
        inference <<-lapply(samples, function(l) mean(as.numeric(l[[utilityVar]])))
        inference<<-unlist(inference)
        tabP = as.data.frame(policies)
        colnames(tabP) = colnames(policies)
        for(i in 1:ncol(tabP))
        {
          for(j in 1:nrow(tabP))
          {
            v = as.numeric(tabP[j,i])
            tabP[j,i] <- levels(DiscreteData[[colnames(tabP)[[i]]]])[v]
            tabP<<-tabP
          }
        }
        tabP<<-tabP
        tabP$payoff<-inference
        tabP<<-tabP
        sortOrder = order(tabP$payoff,decreasing = T)
        tabP = dplyr::arrange(tabP,dplyr::desc(tabP$payoff))
        tabP<<-tabP
        output$policyPlot<-DT::renderDataTable({tabP},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'),rownames=FALSE)
        updateRadioGroupButtons(session,"decisionOption",selected = "Policy Table")
      },error=function(e){
        print(e)
        shinyalert::shinyalert(e,type = "error")
      })
    })
  })
  observeEvent(input$cytoBayes,{
    tryCatch({
      if(reset==2)
      {
        mygraphNEL <- graph::ftM2graphNEL(as.matrix(NetworkGraph), edgemode="directed")
        cytoscapePing()
        createNetworkFromGraph(mygraphNEL,"myGraph")
        shinyalert::shinyalert("Successfull",type = "success")
      }

    },error=function(e){
      print(e)
      shinyalert::shinyalert(e,type = "error")
    })
  })
  observeEvent(input$cytoAssoc,{
    tryCatch({
      if(assocReset==2)
      {
        mygraphNEL <- graph::ftM2graphNEL(assocNetworkprune[,1:2], edgemode="undirected")
        cytoscapePing()
        createNetworkFromGraph(mygraphNEL,"myGraph")
        shinyalert::shinyalert("Successfull",type = "success")
      }

    },error=function(e){
      print(e)
      shinyalert::shinyalert(e,type = "error")
    })
  })
  observeEvent(input$build,{
    if(load==2)
    {
      if(input$name!="")
      {
        if(reset==2)
        {
          do.call(file.remove, list(list.files("customDashboard/R/", full.names = TRUE)))
          write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
          write.csv(input$theme,file="customDashboard/inst/cd/theme.txt",row.names = FALSE)
          customDashboardName <- input$name
          customFileName <- paste("customDashboard/R/",customDashboardName, ".R", sep = "")
          sink(customFileName)
          customString <- paste(customDashboardName ," <- function(){
                                shiny::runApp(appDir = system.file('cd',package = 'customDashboard'),launch.browser = TRUE)
        }", sep = "")
      cat(customString)
      sink()
      fileName <- "customDashboard/NAMESPACE"
      customString <- paste("exportPattern(", customDashboardName, ")", sep = "")
      sink(fileName)
      cat(customString)
      sink()
      if(file.exists("customDashboard/Temp")==TRUE)
      {
        print("test")
        file.rename(from = "customDashboard/Temp",to = "customDashboard/.Rbuildignore")
      }
      shinyalert::shinyalert("Custom Dashboard successfully built. You can now download it as an R package",type="success")
      }
      }
      else
      {
        shinyalert::shinyalert("Enter a dashboard name",type="error")
      }
    }
  })
})
