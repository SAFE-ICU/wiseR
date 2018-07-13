library('bnlearn')
library('rhandsontable')
library('shiny')
library('shinydashboard')
library('dplyr')
library('visNetwork')
library('shinyWidgets')
library('tools')
library('shinyalert')
library('shinycssloaders')
library('rintrojs')
library('arules')
library('psych')
library("DT")
library("linkcomm")
library('igraph')
library("shinyBS")
library("HydeNet")
source('error.bar.R')
source('graph.custom.R')
source('custom.Modules.R')
source('dashboardthemes.R')

nm<-read.csv("name.txt")
th<-read.csv("theme.txt")

myDashboardHeader <- function (..., title = NULL, titleWidth = NULL, disable = FALSE,
                               .list = NULL) {
  items <- c(list(...), .list)
  # lapply(items, tagAssert, type = "li", class = "dropdown")
  titleWidth <- validateCssUnit(titleWidth)
  custom_css <- NULL
  if (!is.null(titleWidth)) {
    custom_css <- tags$head(tags$style(HTML(gsub("_WIDTH_",
                                                 titleWidth, fixed = TRUE, "\n      @media (min-width: 768px) {\n .main-header > .navbar {\n  margin-left: _WIDTH_;text-align: left;\n }\n        .main-header .logo {\n          width: _WIDTH_;\n        }\n      }\n    "))))
  }
  tags$header(class = "main-header", custom_css, style = if (disable)
    "display: none;", span(class = "logo", title), tags$nav(class = "navbar navbar-static-top",
                                                            role = "navigation", span(shiny::icon("bars"), style = "display:none;"),
                                                           # a(href = "#", class = "sidebar-toggle", `data-toggle` = "offcanvas",
                                                           #  role = "button", span(class = "sr-only", "Toggle navigation")),
                                                            div(class = "navbar-custom-menu", tags$ul(class = "nav navbar-nav",
                                                                                                      items))))
}

dashboardPage(skin = "blue",
              myDashboardHeader(title = nm$x,
                                titleWidth = "400"
                                #,tags$li(class = "dropdown", bsButton("homeIntro", label = NULL, icon = icon("question-circle", lib="font-awesome"), style = "primary", size = "large"))
              ),
              dashboardSidebar(width = 50,
                               sidebarMenu(id = "sidebarMenu",
                                           menuItem(text = "",
                                                    icon = shiny::icon("globe"),
                                                    tabName = "Structure"
                                           )
                                          )
                               ),
              dashboardBody(id ="dashboardBody",
                            # Include shinyalert Ui
                            useShinyalert(),
                            shinyDashboardThemes(
                              theme = th$x
                            ),
                            # Include introjs UI
                            rintrojs::introjsUI(),
                            #shinythemes::themeSelector(),
                            #theme = shinytheme("united"),
                            tags$script(HTML("$('body').addClass('fixed');")),
                            shinydashboard::tabItems(
                              shinydashboard::tabItem(tabName = "Structure",
                                                          tabBox(id = "visula_tabs",
                                                                 width = 12,
                                                                 tabPanel("Bayesian Network",
                                                                          fluidPage(
                                                                            shiny::fluidRow(
                                                                              shiny::column(2, dropdownButton(
                                                                                fluidRow(column(6,actionButton("exactInference","Learn Exact Inference",class="butt")),column(6,materialSwitch(inputId = "exact", label = "Enable Exact Inferences", status = "primary", right = TRUE))),
                                                                                hr(),
                                                                                h4("Select evidence to add to the model"),
                                                                                shiny::fluidRow(shiny::column(6,actionButton('insertBtn', 'Insert', class = "butt")),
                                                                                                shiny::column(6,actionButton('removeBtn', 'Remove', class = "butt"))
                                                                                ),
                                                                                shiny::fluidRow(shiny::column(6,tags$div(id = 'placeholder1')),
                                                                                                shiny::column(6,tags$div(id = 'placeholder2'))
                                                                                ),
                                                                                hr(),
                                                                                h4("Select an event of interest"),
                                                                                shiny::h5("Event Node:"),
                                                                                shiny::selectInput("event",
                                                                                                   label = NULL,
                                                                                                   ""),
                                                                                shiny::h4("Display inference plot"),
                                                                                shiny::fluidRow(shiny::column(5,actionButton('plotBtn', 'without error bars', class = "butt")),shiny::column(4,actionButton('plotStrengthBtn', 'with error bars', class = "butt"))),
                                                                                hr(),
                                                                                shiny::h4("No. of resampling iterations for error bars"),
                                                                                textInput("numInterval", label = NULL,placeholder = 25),
                                                                                selectInput('plotFont',label = "axis label font size",choices = c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10),selected = 1.5),
                                                                                selectInput('valueFont',label = "plot value font size",choices = c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10),selected = 1.5),
                                                                                label = "Inference Learning",circle = F, status = "primary", icon = icon("bar-chart-o"), width = "500px",tooltip = tooltipOptions(title = "Learn Inferences")
                                                                              )),
                                                                              shiny::column(9,shinyWidgets::radioGroupButtons(inputId = "bayesianOption",
                                                                                                                              choices = c("Bayesian Network","Fitted Local Distributions", "Infer Decisions"),
                                                                                                                              selected = "Bayesian Network",
                                                                                                                              justified = FALSE
                                                                              ))
                                                                              ),
                                                                            shiny::conditionalPanel(
                                                                              "input.bayesianOption=='Bayesian Network'",
                                                                              shiny::column(11,
                                                                                            shiny::fluidRow(

                                                                                              shiny::column(2,
                                                                                                            div(
                                                                                                                h5("Nth Neigbors(of selection)"))),

                                                                                              shiny::column(2,style="padding-right:0px",
                                                                                                            shiny::selectInput("neighbornodes",label = NULL,choices = "")),
                                                                                              shiny::column(1,
                                                                                                            div(style = "position:absolute;right:0em;",
                                                                                                                h5("Modules:"))),
                                                                                              shiny::column(1,style="padding-right:0px",
                                                                                                            shiny::selectInput("moduleSelection",label = NULL,"graph")),
                                                                                              shiny::column(2,style="margin-right:20px",dropdownButton(
                                                                                                shiny::fluidRow(shiny::column(6,selectInput('moduleAlgo',label = NULL,choices = c("ward.D","ward.D2", "single", "complete", "average", "mcquitty", "median","centroid"))),shiny::column(1,bsButton("Bcommunities","Build Modules", style="primary"))),
                                                                                                label = "Module Detection",circle = F, status = "primary", width = "300px",tooltip = tooltipOptions(title = "Build modules in the graph")
                                                                                              )),
                                                                                              shiny::column(2,style = "margin-right:8px",
                                                                                                            dropdownButton(
                                                                                                              div(id="Bgraph",
                                                                                                                  h4('Group of variables:'),
                                                                                                                  shiny::fluidRow(shiny::column(6,selectizeInput('varselect',label = "Variables","",multiple = T)),
                                                                                                                                  shiny::column(3,selectInput('varshape',label = "Shape","")),
                                                                                                                                  shiny::column(3, actionButton('group','Group', style="margin-top:25px;"))

                                                                                                                  ),

                                                                                                                  hr(),
                                                                                                                  h4('Vector of index:'),
                                                                                                                  shiny::fluidRow(shiny::column(6,textInput('varselectvector',label = "Variables")),
                                                                                                                                  shiny::column(3,selectInput('varshape2',label = "Shape","")),
                                                                                                                                  shiny::column(3, actionButton('group2','Group', style="margin-top:25px;"))
                                                                                                                  ),
                                                                                                                  shiny::fluidRow(shiny::column(6,selectInput('modGroup',label = "modules",choices = "")),
                                                                                                                                  shiny::column(3,selectInput('varshape3',label = "Shape","")),
                                                                                                                                  shiny::column(3, actionButton('group3','Group', style="margin-top:25px;"))
                                                                                                                  ),
                                                                                                                  shiny::fluidRow(shiny::column(6,h4('Visible Neighbors'),div(id = "graphChain",
                                                                                                                                                                                    sliderInput("degree", label = NULL,
                                                                                                                                                                                                min = 1, max = 10,
                                                                                                                                                                                                value = 2
                                                                                                                                                                                    ))),
                                                                                                                                  shiny::column(6,h4('Nth Neighbors'), div(id = "NChain",
                                                                                                                                                                           sliderInput("degreeN", label = NULL,
                                                                                                                                                                                       min = 1, max = 10,
                                                                                                                                                                                       value = 2
                                                                                                                                                                           )))

                                                                                                                  ),
                                                                                                                  hr(),
                                                                                                                  div(id="graphLayout",
                                                                                                                      h4("Select Graph Layout"),
                                                                                                                      shiny::selectInput('graph_layout',label = NULL,"layout_nicely")),
                                                                                                                  selectInput('bayesFont',label = "Node Font",choices = c(1:100),selected = 20)
                                                                                                              ),
                                                                                                              label = "Visual Settings",circle = F, status = "primary", icon = icon("gear"), width = "400px",tooltip = tooltipOptions(title = "graph settings")
                                                                                                            )
                                                                                              ),
                                                                                              shiny::column(1, bsButton('graphBtn', 'Refresh', icon = icon("refresh"),style = "primary"))),

                                                                                            withSpinner(visNetworkOutput("netPlot",height = "480px"), color= "#2E86C1")
                                                                                            )
                                                                              ),
                                                                            shiny::conditionalPanel(
                                                                              "input.bayesianOption=='Infer Decisions'",
                                                                              dropdownButton(
                                                                                sliderInput("NumBar", label = "No. of bars",min = 0, max = 1,value = 1,step=1),
                                                                                actionButton("sortPlot","Sort X-axis"),
                                                                                label = "Plot",circle = F, status = "primary", icon = icon("gear"), width = "400px",tooltip = tooltipOptions(title = "plot settings")
                                                                              ),
                                                                              withSpinner(plotOutput("distPlot",height = "450px"), color="#2E86C1")
                                                                            ),
                                                                            shiny::conditionalPanel(
                                                                              "input.bayesianOption=='Fitted Local Distributions'",
                                                                              selectInput("paramSelect",label = "Variable",""),
                                                                              withSpinner(plotOutput("parameterPlot",height = "450px"),color="#2E86C1")
                                                                            )
                                                                            )
                                                                         ),
                                                                 tabPanel("Decision Networks",
                                                                          shinyWidgets::radioGroupButtons(inputId = "decisionOption",
                                                                                                          choices = c("Decision Network","Policy Table"),
                                                                                                          selected = "Decision Network",
                                                                                                          justified = FALSE
                                                                          ),
                                                                          conditionalPanel(
                                                                            "input.decisionOption=='Decision Network'",
                                                                            shiny::fluidRow(
                                                                              shiny::column(2,dropdownButton(
                                                                                shiny::fluidRow(shiny::column(6,selectInput("parents",label = "Create payoff Node For:",choices = "",multiple = F))),
                                                                                shiny::fluidRow(shiny::column(10,rHandsontableOutput("payoff"))),
                                                                                br(),
                                                                                shiny::fluidRow(shiny::column(6,actionButton("buildDecisionNet2",'build decision net', class = "butt"))),
                                                                                h5("Set Decision Node"),
                                                                                shiny::fluidRow(shiny::column(6,selectInput("decisionNode",label = NULL,choices = c())),shiny::column(6,actionButton("set_decision","Set Node",class = "butt"))),
                                                                                br(),
                                                                                shiny::fluidRow(shiny::column(6,actionButton("set_policy","Best Policy",class="butt"))),
                                                                                br(),

                                                                                label = "Build Network",circle = F, status = "primary", icon = icon("gear"), width = "500px",tooltip = tooltipOptions(title = "Build Network")
                                                                              ))),
                                                                            shinycssloaders::withSpinner(visNetworkOutput("decisionPlot",height = "450px"),color="#2E86C1")
                                                                          ),
                                                                          conditionalPanel(
                                                                            "input.decisionOption=='Policy Table'",
                                                                            shinycssloaders::withSpinner(DT::dataTableOutput("policyPlot",height = "150px"),color="#2E86C1")
                                                                          )
                                                                 )
                                                                 )



                              )
)
)



)
