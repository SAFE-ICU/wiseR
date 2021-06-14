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
source('error.bar.R')
source('graph.custom.R')
source('graph.custom.assoc.R')
source('custom.discretize.R')
source('check.NA.R')
source('check.discrete.R')
source('custom.association.R')
source('custom.Modules.R')
source('dashboardthemes.R')
source('dependency.R')



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
              myDashboardHeader(title = "wiseR",
                                titleWidth = "400"
                                #,tags$li(class = "dropdown", bsButton("homeIntro", label = NULL, icon = icon("question-circle", lib="font-awesome"), style = "primary", size = "large"))
              ),
              dashboardSidebar(width = 50,
                               sidebarMenu(id = "sidebarMenu",
                                           menuItem(text = "",
                                                    tabName = "Home",
                                                    icon = icon("home")
                                           ),
                                           menuItem(text = "",
                                                    icon = shiny::icon("globe"),
                                                    tabName = "Structure"
                                           ),
                                           menuItem(text = "",
                                                    icon = shiny::icon("github"),
                                                    href = "https://github.com/SAFE-ICU/wiseR"),
                                          menuItem(text = "",
                                                    icon = shiny::icon("info"),
                                                  tabName = "About")
                                          )
                               ),
              dashboardBody(id ="dashboardBody",
                            # Include shinyalert Ui
                            shinyalert::useShinyalert(),
                            shinyDashboardThemes(
                              theme = "grey_light"
                            ),
                            # Include introjs UI
                            rintrojs::introjsUI(),
                            #shinythemes::themeSelector(),
                            #theme = shinytheme("united"),
                            tags$script(HTML("$('body').addClass('fixed');")),
                            shinydashboard::tabItems(
                            shinydashboard::tabItem(tabName = "Home",
                                                      fluidRow(box(#title = "",
                                                                   status = "primary",
                                                                   width = 12,
                                                                   div(style="text-align:center",
                                                                       shiny::img(src = "wiseR_HomePage.png",height = 400,width = 1250)
                                                                   ),
                                                                   fluidRow(
                                                                     style = "margin-left:40px;padding:10px;",
                                                                     column(width=3, align = "center", h4('Discover Deep Knowledge')),
                                                                     column(width=1, align = "center", img(src = "arrow.png",height = 40,width = 60)),
                                                                     column(width=3, align = "center", h4('Assess Impact')),
                                                                     column(width=1, align = "center", img(src = "arrow.png",height = 40,width = 60)),
                                                                     column(width=3, align = "center", h4('Take Decisions'))
                                                                   ),
                                                                   hr(),
                                                                   div(style="text-align:center",
                                                                       actionButton("start", "Start Analyzing", style  = "background-color:#2E86C1;color:white;height:50px;font-size:20px", width = '300px', align = "center")
                                                                   )
                                                               )

                                                      )),
                              shinydashboard::tabItem(tabName = "Structure",
                                                          tabBox(id = "visual_tabs",
                                                                 width = 12,
                                                                 tabPanel("App Settings",
                                                                          shiny::fluidRow(
                                                                            column(3,materialSwitch(inputId = "parallel", label = "Enable Parallel Computing", status = "primary", right = TRUE), style="margin:30px;"),
                                                                            column(3,selectInput("clusters",choices = c(1:20),label = "Number of clusters")))
                                                                 ),
                                                                 tabPanel('Data',

                                                                          shiny::fluidRow(
                                                                            shiny::column(3,
                                                                                          shinyWidgets::radioGroupButtons(inputId = "dataoption",
                                                                                                                          choices = c("Dataset","Explore"),
                                                                                                                          selected = "Dataset",
                                                                                                                          justified = FALSE
                                                                                          )
                                                                                          )
                                                                          ),
                                                                          conditionalPanel(
                                                                            "input.dataoption =='Dataset'",
                                                                            fluidRow(style="padding:0px",
                                                                              shiny::column(2, dropdownButton(
                                                                                h5('Choose default dataset'),
                                                                                fluidRow(column(9,selectInput('defData',label = NULL,choices = c("Alarm","Asia","Coronary","Lizards","Marks","Insurance","Hailfinder"))),column(3,actionButton('loadDef','load', class = "butt"))),
                                                                                h5('Data Format:'),
                                                                                shiny::selectInput('format',label = NULL,c(".CSV",".RData","Comma Separated","Semicolon Separated","Tab Separated","Space Separated")),
                                                                                h5('variables as Factor:'),
                                                                                checkboxInput("factorCheck", label = NULL, value = TRUE, width = NULL),
                                                                                h5('File Input:'),
                                                                                shiny::fileInput('dataFile',
                                                                                                 label = NULL,
                                                                                                 accept = c('.RData','.csv','.txt')
                                                                                ),
                                                                                label = "upload",circle = F, status = "primary", icon = icon("upload"), width = "500px",tooltip = tooltipOptions(title = "upload data as csv or RData")
                                                                              )),
                                                                              shiny::column(2, dropdownButton(
                                                                                div(id="dataNumeric",
                                                                                    shiny::h4("Convert Variables to Numeric"),
                                                                                    shiny::fluidRow(shiny::column(6,selectInput('numSelect',label = NULL,"")),shiny::column(3,actionButton('numconv','Convert', class = "butt"))),
                                                                                    shiny::fluidRow(shiny::column(6,textInput('numSelect2',label = NULL,placeholder = "Array of Variables")),shiny::column(3,actionButton('numconv2','Convert', class = "butt")))
                                                                                ),
                                                                                div(id="dataFactor",
                                                                                    shiny::h4("Convert Variables to Factor"),
                                                                                    shiny::fluidRow(shiny::column(6,selectInput('facSelect',label = NULL,"")),shiny::column(3,actionButton('facconv','Convert', class = "butt"))),
                                                                                    shiny::fluidRow(shiny::column(6,textInput('facSelect2',label = NULL,placeholder = "Array of Variables")),shiny::column(3,actionButton('facconv2','Convert', class = "butt")))
                                                                                ),
                                                                                div(id="dataImpute",
                                                                                    shiny::h4("Impute Missing Data:"),
                                                                                    actionButton('impute','Impute', class = "butt")),
                                                                                div(id="dataDiscretize",
                                                                                    shiny::h4('Discretize Data'),
                                                                                    h5('Discretization Type:'),
                                                                                    shiny::fluidRow(column(9,shiny::selectInput('dtype',label = NULL,c("hybrid discretization(Recommended)"="hybrid","hartemink discretization(Recommended)"="hartemink","interval discretization"="interval","quantile discretization"="quantile","frequency discretization"="frequency","K-means clustering"="cluster"))),column(3,actionButton('discretize',"Discretize", class = "butt"))),
                                                                                    h5('Hartemink Discritization Parameters:'),
                                                                                    shiny::fluidRow(column(4,shiny::textInput('breakH',label = 'breaks',placeholder = 5)),column(4,shiny::textInput('ibreakH',label = 'ibreaks',placeholder = 5)))

                                                                                ),
                                                                                div(id="dataTranspose",
                                                                                    shiny::h4("Transpose data frame:"),
                                                                                    actionButton('transpose','Transpose', class = "butt")),
                                                                                div(id="dataSort",
                                                                                    shiny::h4("Sort data frame:"),
                                                                                    actionButton('sort','Arrange Columns', class = "butt")),
                                                                                div(id="dataDelete",
                                                                                    shiny::h4("Delete variables"),
                                                                                    shiny::fluidRow(shiny::column(6,selectInput('delSelect',label = NULL,"",multiple = T)),shiny::column(3,actionButton('delete','Delete', class = "butt")),shiny::column(3,actionButton('reset','Reset', class = "butt")))
                                                                                ),
                                                                                div(id="dataIntervention",
                                                                                    shiny::h4("Specify Intervention Variable"),
                                                                                    shiny::fluidRow(shiny::column(4,selectInput('intSelect',label = NULL,"")),shiny::column(8,actionButton('intervention','Select', class = "butt")))
                                                                                ),
                                                                                label = "Pre-Process",circle = F, status = "primary", icon = icon("edit"), width = "500px",tooltip = tooltipOptions(title = "prepare data for bayesian network analysis")
                                                                              )),
                                                                              shiny::column(2, downloadButton("downloadDataset", "Download", class = "butt"))),
                                                                              tags$head(tags$style(".butt{background-color:#2E86C1;} .butt{color:white;} .butt{border:#2E86C1;}")

                                                                            ),
                                                                            hr(),
                                                                            shinycssloaders::withSpinner(DT::dataTableOutput("datasetTable"),color = "#2E86C1")
                                                                          ),
                                                                          conditionalPanel(
                                                                            "input.dataoption=='Explore'",
                                                                            selectInput("freqSelect",label = "Variable",""),
                                                                            shinycssloaders::withSpinner(plotOutput("freqPlot",height = "600px"),color="#2E86C1")
                                                                            )
                                                                          ),
                                                                 tabPanel("Association Network",
                                                                          shiny::fluidRow(
                                                                            column(5,h5("")),
                                                                            column(4,shinyWidgets::radioGroupButtons(inputId = "assocOption",
                                                                                                                     choices = c("Association Network","Export Table"),
                                                                                                                     selected = "Association Network",
                                                                                                                     justified = FALSE
                                                                            ))
                                                                          ),
                                                                          conditionalPanel(
                                                                            "input.assocOption=='Association Network'",
                                                                            shiny::fluidRow(
                                                                              shiny::column(1,
                                                                                            div(style="width: 500px;",
                                                                                                dropdownButton(
                                                                                                  h5("Association Network"),
                                                                                                  shiny::fluidRow(column(8,shiny::selectInput('assocType',label = NULL,c("cramer's V (Recommended)"="cramer's V","Cohen's D","Goodman Kruskal lambda","Tschuprow's T"))),column(4,actionButton('association',"Build", class = "butt"))),
                                                                                                  sliderInput("threshold", label = "Association Threshold",min = 0, max = 1,value = 0.75),

                                                                                                  label = "` Build",circle = F, status = "primary", icon = icon("glyphicon glyphicon-wrench",lib = "glyphicon"), width = "400px",tooltip = tooltipOptions(title = "Build association Network")
                                                                                                ))
                                                                              ),
                                                                              shiny::column(2, style='margin-right:0px;',
                                                                                            dropdownButton(
                                                                                              div(id="Agraph",
                                                                                                  h4('Highlight Variables'),
                                                                                                  shiny::fluidRow(shiny::column(6,selectInput('Avarselect',label = "Variables Names","",multiple = T)),
                                                                                                                  shiny::column(3,selectInput('Avarshape',label = "Shape","")),
                                                                                                                  shiny::column(3,actionButton('Agroup','Group', style="margin-top:25px;", class = "butt"))


                                                                                                  ),
                                                                                                  h4("Or"),
                                                                                                  shiny::fluidRow(shiny::column(6,textInput('Avarselectvector',label = "Column indices")),
                                                                                                                  shiny::column(3,selectInput('Avarshape2',label = "Shape","")),
                                                                                                                  shiny::column(3,actionButton('Agroup2','Group', style="margin-top:25px;", class = "butt"))
                                                                                                  ),
                                                                                                  h3("Or"),
                                                                                                  shiny::fluidRow(shiny::column(6,selectInput('AmodGroup',label = "Module",choices = "")),
                                                                                                                  shiny::column(3,selectInput('Avarshape3',label = "Shape","")),
                                                                                                                  shiny::column(3, actionButton('Agroup3','Group', style="margin-top:25px;", class = "butt"))
                                                                                                  ),
                                                                                                  hr(),
                                                                                                  shiny::fluidRow(shiny::column(6,h4('Visible Neighbors chain'),div(id = "AgraphChain",
                                                                                                                                                                    sliderInput("Adegree", label = NULL,
                                                                                                                                                                                min = 1, max = 10,
                                                                                                                                                                                value = 1
                                                                                                                                                                    ))),
                                                                                                                  shiny::column(6,h4('Nth Order Neighbors'), div(id = "ANChain",
                                                                                                                                                           sliderInput("AdegreeN", label = NULL,
                                                                                                                                                                       min = 1, max = 10,
                                                                                                                                                                       value = 1
                                                                                                                                                           )))

                                                                                                  ),
                                                                                                  hr(),
                                                                                                  div(id="AgraphLayout",
                                                                                                      h4("Select Graph Layout"),
                                                                                                      shiny::selectInput('Agraph_layout',label = NULL,"layout_nicely")),
                                                                                                  selectInput('assocFont',label = "Node Font",choices = c(1:100),selected = 20)
                                                                                              ),
                                                                                              shiny::fluidRow(shiny::column(6,downloadButton("aSave","Save Graph as html")),shiny::column(6,actionButton("cytoAssoc","Export to Cytoscape",class="butt"))),
                                                                                              label = "Visual Settings",circle = F, status = "primary", icon = icon("gear"), width = "500px",tooltip = tooltipOptions(title = "graph visualization settings")
                                                                                            )
                                                                              ),
                                                                              shiny::column(1,bsButton('graphBtn2', 'Refresh', icon = icon("refresh"),style = "primary")),
                                                                              shiny::column(2,
                                                                                            div(style = "position:absolute;right:0.1em;",
                                                                                                h5("Nth neighbors(of selection):"))),

                                                                              shiny::column(2,

                                                                                            shiny::selectInput("Aneighbornodes",label = NULL,choices = "")
                                                                              ),
                                                                              shiny::column(1,style="padding-right:0px",
                                                                                            shiny::selectInput("AmoduleSelection",label = NULL,"graph")),
                                                                              shiny::column(2,style="margin-right:20px",dropdownButton(
                                                                                shiny::fluidRow(shiny::column(6,selectInput('AmoduleAlgo',label = NULL,choices = c("ward.D","ward.D2", "single", "complete", "average", "mcquitty", "median","centroid"))),shiny::column(1,bsButton("Acommunities","Build Modules", style="primary"))),
                                                                                label = "Detect Modules",circle = F, status = "primary", width = "300px",tooltip = tooltipOptions(title = "Build modules in the graph")
                                                                              ))
                                                                            ),
                                                                            br(),
                                                                            shinycssloaders::withSpinner(visNetworkOutput("assocPlot",height = "550px"), color= "#2E86C1")
                                                                          ),
                                                                          conditionalPanel(
                                                                            "input.assocOption=='Export Table'",
                                                                            downloadButton('assocDownload','Download', class = "butt"),
                                                                            br(),
                                                                            shinycssloaders::withSpinner(DT::dataTableOutput("assocTable"),color = "#2E86C1")
                                                                          )
                                                                 ),
                                                                 tabPanel("Bayesian Network",
                                                                          fluidPage(
                                                                            shiny::fluidRow(
                                                                              shiny::column(2,dropdownButton(
                                                                                shinyWidgets::radioGroupButtons(inputId = "structureOption",
                                                                                                                choices = c("Initialize Structure (optional)","Learn Structure","Upload Pre-learnt Structure","Edit Structure (optional)","External Graph","Graph Neural Networks","Validate Structure"),
                                                                                                                selected = "Initialize Structure (optional)",
                                                                                                                justified = FALSE
                                                                                ),
                                                                                shiny::conditionalPanel(
                                                                                  "input.structureOption=='Upload Pre-learnt Structure'",
                                                                                  h5("parameter learning algorithm"),
                                                                                  selectizeInput('paramMethod',label = NULL,choices = c("Bayesian parameter estimation" = "bayes","Maximum Likelihood parameter estimation" = "mle")),
                                                                                  hr(),
                                                                                  shinyWidgets::radioGroupButtons(inputId = "uploadOption",
                                                                                                                  choices = c("Averaged Network","Bootstrapped Network"),
                                                                                                                  selected = "Averaged Network",
                                                                                                                  justified = FALSE
                                                                                  ),
                                                                                  shiny::conditionalPanel(
                                                                                    "input.uploadOption=='Averaged Network'",
                                                                                    div(
                                                                                      # File input
                                                                                      shiny::p("Note: Upload .RData file"),
                                                                                      shiny::fileInput(
                                                                                        'structFile',
                                                                                        strong('File Input:'),
                                                                                        accept = c('.RData')
                                                                                      )

                                                                                    )
                                                                                  ),
                                                                                  shiny::conditionalPanel(
                                                                                    "input.uploadOption=='Bootstrapped Network'",
                                                                                    div(
                                                                                      # File input
                                                                                      shiny::p("Note: Upload .RData file"),
                                                                                      shiny::fileInput(
                                                                                        'bootFile',
                                                                                        strong('File Input:'),
                                                                                        accept = c('.RData')
                                                                                      ),
                                                                                      fluidRow(
                                                                                        column(6,h5("Edge Strength"),
                                                                                               sliderInput("edgeStrengthU", label = NULL,
                                                                                                           min = 0, max = 1,
                                                                                                           value = 0.5)),
                                                                                        column(6,h5("Direction Confidence:"),
                                                                                               sliderInput("directionStrengthU", label = NULL,
                                                                                                           min = 0, max = 1,
                                                                                                           value = 0.5))
                                                                                      )
                                                                                    )
                                                                                  ),
                                                                                  actionButton("parameterTuningU","Parameter Tuning", class = "butt")
                                                                                ),
                                                                                shiny::conditionalPanel(
                                                                                  "input.structureOption=='Initialize Structure (optional)'",
                                                                                  shiny::fluidRow(column(5,h5("Upload list of prior known edges (as .CSV)"))),
                                                                                  shiny::fluidRow(column(5,shiny::fileInput('priorFile',label = NULL,accept = c('.CSV')))),
                                                                                  shiny::fluidRow(shiny::column(3,h5("from")),shiny::column(3,h5("to")),shiny::column(3,h5("")),shiny::column(3,h5("Select from table"))),
                                                                                  shiny::fluidRow(shiny::column(3,selectInput("fromarc1",label = NULL,choices=c())),shiny::column(3,selectInput("toarc1",label = NULL,choices=c())),column(3,actionButton("addarc1","Add", class = "butt")),actionButton("RemoveArc","Remove", class = "butt"),actionButton("ReverseArc","Reverse", class = "butt")),
                                                                                  shinycssloaders::withSpinner(DT::dataTableOutput("priorout"),color = "#2E86C1")
                                                                                ),
                                                                                shiny::conditionalPanel(
                                                                                  "input.structureOption=='Learn Structure'",
                                                                                  div(style ='overflow-y:scroll;height:600px;padding-right:20px;',

                                                                                      # Structural learning algorithm input select
                                                                                      shiny::fluidRow(
                                                                                        shiny::column(6,
                                                                                                      shiny::selectizeInput(
                                                                                                        inputId = "alg",
                                                                                                        label="Learning Algorithm",
                                                                                                        choices = list(
                                                                                                          "Score-based Learning" =
                                                                                                            c("Hill Climbing (Recommended)" = "hc",
                                                                                                              "Tabu (Recommended)" = "tabu"),
                                                                                                          "Constraint-based Learning" =
                                                                                                            c("Grow-Shrink" = "gs",
                                                                                                              "Incremental Association" = "iamb",
                                                                                                              "Fast IAMB" = "fast.iamb",
                                                                                                              "Inter IAMB" = "inter.iamb",
                                                                                                              "PC" = "pc.stable"
                                                                                                            ),
                                                                                                          "Hybrid Learning" =
                                                                                                            c("Max-Min Hill Climbing" = "mmhc",
                                                                                                              "2-phase Restricted Maximization" = 'rsmax2'
                                                                                                            ),
                                                                                                          "Local Discovery Learning" =
                                                                                                            c("Max-Min Parents and Children" = 'mmpc',
                                                                                                              "Semi-Interleaved HITON-PC" = "si.hiton.pc",
                                                                                                              "ARACNE" = "aracne",
                                                                                                              "Chow-Liu" = "chow.liu"
                                                                                                            )
                                                                                                        )
                                                                                                      )
                                                                                        ),
                                                                                        shiny::column(6,
                                                                                                      selectizeInput("paramMethod2",label = "Parameter fitting algorithm",choices = c("Bayesian parameter estimation" = "bayes","Maximum Likelihood parameter estimation" = "mle"))
                                                                                        )

                                                                                      ),
                                                                                      shiny::fluidRow(
                                                                                        shiny::column(6,selectInput("algoscore",label = "Network Score",choices = c("Bayesian Information Criterion"="bic","Bayesian Dirichlet Equivalent"="bde","modified Bayesian Dirichlet Equivalent"="mbde","log-likelihood"="loglik","Akaike Information Criterion"="aic","Bayesian Dirichlet Sparse"="bds","Locally Averaged Bayesian Dirichlet"="bdla"))),
                                                                                        shiny::column(6,sliderInput("iss", label = "Imaginary sample size",
                                                                                                                    min = 1, max = 1000,
                                                                                                                    value = 7))
                                                                                      ),
                                                                                      h5("Use Expert Knowledge by Forcing/Prohibiting Edges"),
                                                                                      shiny::fluidRow(shiny::column(6,selectInput("listType",label = NULL,choices = c("Blacklist","Whitelist"))),shiny::column(6,shiny::fileInput('listFile',label = NULL,accept = c('.csv')))),
                                                                                      hr("Bootstrap without resampling is available only for score-based learning"),
                                                                                      fluidRow(column(4,materialSwitch(inputId = "resampling", label = "Disable resampling in bootstrap", status = "primary", right = F), style="margin:30px;")),
                                                                                      fluidRow(
                                                                                        column(6, h5("Bootstrap replicates"),
                                                                                               sliderInput("boot", label = NULL,
                                                                                                           min = 1, max = 1000,
                                                                                                           value = 11)),
                                                                                        column(6, h5("Proportion of sample for Bootstrap:"),
                                                                                               sliderInput("SampleSize", label = NULL,
                                                                                                           min = 0, max = 1,
                                                                                                           value = 1))
                                                                                      ),

                                                                                      hr(),
                                                                                      fluidRow(
                                                                                        column(6,h5("Edge Strength"),
                                                                                               sliderInput("edgeStrength", label = NULL,
                                                                                                           min = 0, max = 1,
                                                                                                           value = 0.5)),
                                                                                        column(6,h5("Direction Confidence:"),
                                                                                               sliderInput("directionStrength", label = NULL,
                                                                                                           min = 0, max = 1,
                                                                                                           value = 0.5))
                                                                                      ),
                                                                                      actionButton('learnBtn', 'Bootstrap', class = "butt"),
                                                                                      actionButton('learnSBtn','One-time', class = "butt"),
                                                                                      actionButton('PruneBtn','Parameter Tuning', class = "butt"),
                                                                                      hr(),
                                                                                      shiny::h5("Save learned structure"),
                                                                                      downloadButton('saveBtn','Save', class = "butt"),
                                                                                      downloadButton('saveBtnBoot','Save Bootstrap Object', class = "butt")
                                                                                  )
                                                                                ),
                                                                                shiny::conditionalPanel(
                                                                                  "input.structureOption=='Edit Structure (optional)'",
                                                                                  shiny::fluidRow(shiny::column(3,h5("from")),shiny::column(3,h5("to")),shiny::column(3,h5("")),shiny::column(3,h5("Select from table"))),
                                                                                  shiny::fluidRow(shiny::column(3,selectInput("fromarc",label = NULL,choices=c())),shiny::column(3,selectInput("toarc",label = NULL,choices=c())),column(3,actionButton("addarc","Add", class = "butt")),actionButton("RemoveArc2","Remove", class = "butt"),actionButton("ReverseArc2","Reverse", class = "butt")),
                                                                                  shinycssloaders::withSpinner(DT::dataTableOutput("postout"),color = "#2E86C1")
                                                                                ),
                                                                                shiny::conditionalPanel(
                                                                                  "input.structureOption=='External Graph'",
                                                                                  shiny::fluidRow(column(5,h5("Upload list of edges of external graph (as .CSV)"))),
                                                                                  shiny::fluidRow(column(5,shiny::fileInput('externalGraph',label = NULL,accept = c('.CSV')))),
                                                                                  actionButton('externalGraphButton','Train', class = "butt")
                                                                                ),
                                                                                shiny::conditionalPanel(
                                                                                  "input.structureOption=='Graph Neural Networks'",
                                                                                  shiny::fluidRow(column(6,shiny::textInput('pythonENV',label = 'Python Path',placeholder = "Please provide python env path in linux format")),column(6,shiny::textInput('condaENV',label = 'Conda Path',placeholder = "Please provide conda env path in linux format"))),
                                                                                  shiny::fluidRow(column(5, h5("Bootstrap replicates"),
                                                                                                         sliderInput("bootGNN", label = NULL,
                                                                                                                     min = 1, max = 1000,
                                                                                                                     value = 11))),
                                                                                  actionButton('GNNGraphButton','Train', class = "butt")
                                                                                ),
                                                                                shiny::conditionalPanel(
                                                                                  "input.structureOption=='Validate Structure'",
                                                                                  shiny::fluidRow(shiny::column(6,shiny::selectInput('crossFunc',label = "Validation Method",choices = c("10-fold"="k-fold","hold-out"))),shiny::column(6,shiny::selectInput('lossFunc',label = "Loss Function",choices = c("pred","pred-lw")))),
                                                                                  h5("Parameter Fitting Method"),
                                                                                  shiny::fluidRow(shiny::column(8,shiny::selectInput('paramMethod3',label = NULL,choices = c("Bayesian parameter estimation" = "bayes","Maximum Likelihood parameter estimation" = "mle"))),shiny::column(4,shiny::actionButton("calLoss","Cross Validate", class = "butt"))),
                                                                                  h5("Log-Likelihood Loss of the learned model"),
                                                                                  shiny::verbatimTextOutput("valLoss"),
                                                                                  h5("Network Score"),
                                                                                  shiny::fluidRow(shiny::column(6,selectInput("scoreAlgo",label = NULL,choices = c("modified Bayesian Dirichlet equivalent"="mbde","log-likelihood"="loglik","Akaike Information Criterion"="aic","Bayesian Information Criterion"="bic","Bayesian Dirichlet equivalent"="bde","Bayesian Dirichlet sparse"="bds","locally averaged Bayesian Dirichlet"="bdla"))),shiny::column(2,actionButton("getScore","Score", class = "butt")),shiny::column(4,shiny::verbatimTextOutput("netScore")))
                                                                                ),
                                                                                label = "Structure Learning",circle = F, status = "primary", icon = icon("wrench"), width = "800px",tooltip = tooltipOptions(title = "Upload structure")
                                                                              )),
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
                                                                              shiny::column(7,shinyWidgets::radioGroupButtons(inputId = "bayesianOption",
                                                                                                                              choices = c("Bayesian Network","Consensus Plot","Fitted Local Distributions", "Infer Decisions","Export Tables"),
                                                                                                                              selected = "Bayesian Network",
                                                                                                                              justified = FALSE
                                                                              ))
                                                                              ),
                                                                            shiny::conditionalPanel(
                                                                              "input.bayesianOption=='Bayesian Network'",
                                                                              shiny::column(12,
                                                                                            shiny::fluidRow(

                                                                                              shiny::column(2,
                                                                                                            div(
                                                                                                                h5("Nth Neighbors(of selection):"))),

                                                                                              shiny::column(2,style="padding-right:0px",
                                                                                                            shiny::selectInput("neighbornodes",label = NULL,choices = "")),
                                                                                              shiny::column(1,
                                                                                                            div(style = "position:absolute;right:0em;",
                                                                                                                h5("Modules:"))),
                                                                                              shiny::column(1,style="padding-right:0px",
                                                                                                            shiny::selectInput("moduleSelection",label = NULL,"graph")),
                                                                                              shiny::column(2,style="margin-right:20px",dropdownButton(
                                                                                                shiny::fluidRow(shiny::column(6,selectInput('moduleAlgo',label = NULL,choices = c("ward.D","ward.D2", "single", "complete", "average", "mcquitty", "median","centroid"))),shiny::column(1,bsButton("Bcommunities","Build Modules", style="primary"))),
                                                                                                label = "Detect Modules",circle = F, status = "primary", width = "300px",tooltip = tooltipOptions(title = "Build modules in the graph")
                                                                                              )),
                                                                                              shiny::column(2,style = "margin-right:8px",
                                                                                                            dropdownButton(
                                                                                                              div(id="Bgraph",
                                                                                                                  h4('Highlight variables:'),
                                                                                                                  shiny::fluidRow(shiny::column(6,selectizeInput('varselect',label = "Variable names","",multiple = T)),
                                                                                                                                  shiny::column(3,selectInput('varshape',label = "Shape","")),
                                                                                                                                  shiny::column(3, actionButton('group','Group', style="margin-top:25px;", class = "butt"))

                                                                                                                  ),
                                                                                                                  h3("Or"),
                                                                                                                  shiny::fluidRow(shiny::column(6,textInput('varselectvector',label = "Column indices")),
                                                                                                                                  shiny::column(3,selectInput('varshape2',label = "Shape","")),
                                                                                                                                  shiny::column(3, actionButton('group2','Group', style="margin-top:25px;", class = "butt"))
                                                                                                                  ),
                                                                                                                  h3("Or"),
                                                                                                                  shiny::fluidRow(shiny::column(6,selectInput('modGroup',label = "Module",choices = "")),
                                                                                                                                  shiny::column(3,selectInput('varshape3',label = "Shape","")),
                                                                                                                                  shiny::column(3, actionButton('group3','Group', style="margin-top:25px;", class = "butt"))
                                                                                                                  ),
                                                                                                                  shiny::fluidRow(shiny::column(6,h4('Visible Neighbors'),div(id = "graphChain",
                                                                                                                                                                                    sliderInput("degree", label = NULL,
                                                                                                                                                                                                min = 1, max = 10,
                                                                                                                                                                                                value = 1
                                                                                                                                                                                    ))),
                                                                                                                                  shiny::column(6,h4('Nth Neighbors'), div(id = "NChain",
                                                                                                                                                                           sliderInput("degreeN", label = NULL,
                                                                                                                                                                                       min = 1, max = 10,
                                                                                                                                                                                       value = 1
                                                                                                                                                                           )))

                                                                                                                  ),
                                                                                                                  hr(),
                                                                                                                  div(id="graphLayout",
                                                                                                                      h4("Select Graph Layout"),
                                                                                                                      shiny::selectInput('graph_layout',label = NULL,"layout_nicely")),
                                                                                                                  selectInput('bayesFont',label = "Node Font",choices = c(1:100),selected = 20)
                                                                                                              ),
                                                                                                              shiny::fluidRow(shiny::column(6,downloadButton("bSave","Save Graph as html")),shiny::column(6,actionButton("cytoBayes","Export to Cytoscape",class="butt"))),
                                                                                                              label = "Visual Settings",circle = F, status = "primary", icon = icon("gear"), width = "400px",tooltip = tooltipOptions(title = "graph settings")
                                                                                                            )
                                                                                              ),
                                                                                              shiny::column(1, bsButton('graphBtn', 'Refresh', icon = icon("refresh"),style = "primary"))),

                                                                                            shinycssloaders::withSpinner(visNetworkOutput("netPlot",height = "480px"), color= "#2E86C1")
                                                                                            )
                                                                              ),
                                                                            shiny::conditionalPanel(
                                                                              "input.bayesianOption=='Infer Decisions'",
                                                                              dropdownButton(
                                                                                sliderInput("NumBar", label = "No. of bars",min = 0, max = 1,value = 1,step=1),
                                                                                actionButton("sortPlot","Sort X-axis", class = "butt"),
                                                                                label = "Plot",circle = F, status = "primary", icon = icon("gear"), width = "400px",tooltip = tooltipOptions(title = "plot settings")
                                                                              ),
                                                                              shinycssloaders::withSpinner(plotOutput("distPlot",height = "450px"), color="#2E86C1")
                                                                            ),
                                                                            shiny::conditionalPanel(
                                                                              "input.bayesianOption=='Fitted Local Distributions'",
                                                                              selectInput("paramSelect",label = "Variable",""),
                                                                              shinycssloaders::withSpinner(plotOutput("parameterPlot",height = "450px"),color="#2E86C1")
                                                                            ),
                                                                            conditionalPanel(
                                                                              "input.bayesianOption=='Export Tables'",
                                                                              shiny::fluidRow(shiny::column(4,selectInput("tableName",label = NULL,"")),shiny::column(1,downloadButton("downloadData", "Download", class = "butt"))),
                                                                              shinycssloaders::withSpinner(DT::dataTableOutput("tableOut"),color = "#2E86C1")
                                                                            ),
                                                                            conditionalPanel(
                                                                              "input.bayesianOption=='Consensus Plot'",
                                                                              shiny::fluidRow(shiny::column(1,actionButton("consensus", "Build Plot", class = "butt"))),
                                                                              shinycssloaders::withSpinner(plotOutput("consensusPlot",height = "450px"),color="#2E86C1")
                                                                            )
                                                                            )
                                                                         ),
                                                                 tabPanel("Decision Network",
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
                                                                          ),
                                                                 tabPanel("Publish your dashboard",
                                                                          shiny::fluidRow(
                                                                            column(3,h5("Name")),
                                                                            column(3,h5("Theme"))),
                                                                          shiny::fluidRow(
                                                                            column(3,textInput("name",placeholder = NULL,label = NULL)),
                                                                            column(3,selectInput("theme",label = NULL,choices = c("Blue gradient"="blue_gradient","BoE website"="boe_website","Grey light"="grey_light","Grey dark"="grey_dark","OneNote"="onenote","Poor man's Flatly"="poor_mans_flatly","Purple gradient"="purple_gradient"))),
                                                                            column(2,actionButton("build",'build', class = "butt")),
                                                                            column(3,downloadButton('dashboard','Download', class = "butt")))
                                                                 ),
                                                                 tabPanel("App Tutorial",
                                                                          htmlOutput('pdfviewer'))
                                                                 )



                              ),
                              tabItem(tabName = "About",
                                      fluidRow(box(
                                        status = "primary",
                                        width = 12,
                                        div(style="text-align:center",
                                            p(h2(em("wiseR"),"Authors"))
                                        ),
                                        fluidRow(
                                          style = "margin-left:10px;padding:10px;",
                                          column(3, align = "center",
                                                 img(src = "tps.jpg", style = "max-width: 50%; width: 50%; height: auto;")
                                                 ),
                                          column(4,
                                                 h4('Tavpritesh Sethi'),
                                                 h5('Visiting Assistant Professor, Stanford Medicine'),
                                                 h5('Assistant Professor, IIIT-Delhi'),
                                                 h5('tavsethi@stanford.edu | tavpriteshsethi@iiitd.ac.in'),
                                                 fluidRow(width = 12,
                                                          column(width=2, a(img(src = "email.png", style = "margin:5px; width: 20px; height: 20px"), href = "mailto:tavsethi@stanford.edu"), target = "_blank"),
                                                          column(width=2, a(img(src = "email.png", style = "margin:5px; width: 20px; height: 20px"), href = "mailto:tavpriteshsethi@iiitd.ac.in"), target = "_blank"),
                                                          column(width=2, a(img(src = "github.png", width = '30px', height = '30px'), href = "https://github.com/SAFE-ICU?tab=repositories"), target = "_blank"),
                                                          column(width=2, a(img(src = "facebook.png", style = "margin:5px; width: 20px; height: 20px"), href = "https://www.facebook.com/tavpritesh.sethi"), target = "_blank"),
                                                          column(width=2, a(img(src = "linkedin.png", style = "margin:5px; width: 20px; height: 20px"), href = "https://in.linkedin.com/in/tavpritesh"), target = "_blank"),
                                                          column(width=2, a(img(src = "twitter.png", style = "margin:6px; width: 18px; height: 18px"), href = "https://twitter.com/tavpritesh"), target = "_blank")
                                                 )
                                                 )
                                        ),
                                        fluidRow(
                                          style = "margin-left:10px;padding:10px;",
                                          column(3, align = "center",
                                                 img(src = "shubham.jpg",style = "max-width: 50%; width: 50%; height: auto")
                                          ),
                                          column(4,
                                                 h4('Shubham Maheshwari'),
                                                 h5('B.Tech Computer Science, IIIT-Delhi'),
                                                 h5('shubham14101@iiitd.ac.in'),
                                                 h5("(looking for opportunities to collaborate on projects in AI and its Applications)"),
                                                 fluidRow(width = 12,
                                                          column(width=2, a(img(src = "email.png", style = "margin:5px; width: 20px; height: 20px"), href = "mailto:shubham14101@iiitd.ac.in"), target = "_blank"),
                                                          column(width=2, a(img(src = "github.png", width = '30px', height = '30px'), href = "https://github.com/shubham14101"),target = "_blank"),

                                                          column(width=2, a(img(src = "linkedin.png", style = "margin:5px; width: 20px; height: 20px"), href = "https://www.linkedin.com/in/shubham-maheshwari-93a35b108/"),target = "_blank"),
                                                          column(width=2, a(img(src = "twitter.png", style = "margin:6px; width: 18px; height: 18px"), href = "https://twitter.com/real_SM96"),target = "_blank")
                                                 )

                                          )
                                        ),
                                        hr(),
                                            div(style="text-align:center",
                                                h3("Contributors"),
                                                h4('Anant Mittal'),
                                                h5('B.Tech Computer Science, IIIT-Delhi'),
                                                h5('anant14015@iiitd.ac.in'),
                                                a(img(src = "email.png", style = "margin:5px; width: 20px; height: 20px"), href = "mailto:anant14015@iiitd.ac.in"),
                                                a(img(src = "github.png", width = '30px', height = '30px'), href = "https://github.com/anant15"),
                                                hr(),
                                                h3('Reference'),
                                                h5('wiseR: A Framework for Learning and Deploying Decisions With Probabilistic Graphical Models'),
                                                hr(),
                                                h3('Acknowlegments'),
                                                h5("Rakesh Lodha, Professor (Pediatrics), All India Institute of Medical Sciences, New Delhi, India"),
                                                h5("Nigam Shah, Associate Professor (Biomedical Informatics), Stanford University, USA"),
                                                h5('Funding Support: The Wellcome Trust/DBT India Alliance Early Career Award IA/CPHE/14/1/501504'),
                                                hr(),
                                                h3('Work with us'),
                                                a(h5("Contact:  tavpriteshsethi@iiitd.ac.in"), href = "mailto:tavpriteshsethi@iiitd.ac.in")


                                        )



                                      )

                                      )
                                    )
),
tags$footer("Funding Support: The Wellcome Trust/DBT India Alliance grant IA/CPHE/14/1/501504", align = "center", style = "
position:absolute;
            bottom:0;
            width:100%;
            height:30px;
            padding:5px;
            display:inline-block;
            background-color: white;z-index:1200;")
                           )


)
