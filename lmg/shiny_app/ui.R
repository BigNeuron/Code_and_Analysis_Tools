library(shiny)
library(plotly)
library(shinycustomloader)
library(rgl)
options(shiny.maxRequestSize = 30*1024^2)

shinyUI(fluidPage(
  titlePanel("Gold163 analysis"),
  fluidRow(
    column(2,     
      selectInput("variablegroups",label = h5(strong("Select for coloring")),"",selected="variablegroups"),
      h5(strong("Choose variables for PCA:")),
      checkboxGroupInput("variablemorph", "Tree morphology","",selected="variablemorph"),
      actionLink("selectall","Select All"),
      checkboxGroupInput("variabledist", "Reconstruction quality","",selected="variabledist"),
      actionLink("selectall2","Select All"),
      checkboxGroupInput("variableiq", "Image quality","",selected="variableiq"),
      actionLink("selectall3","Select All"),
      h5(""),
      actionLink("selectvoliq","Select only volumes IQ")
    #selectInput("variable",label = h5("Variables for PCA"),"",multiple=T)
    ),
    column(10,
  #   # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("PCA", 
                           selectInput("variableclust",label = h5(strong("Select for cluster polygons")),"",selected="variableclust"),
                           withLoader(plotlyOutput("PCA",height = "800px", width = "100%")),
                           downloadLink("downloadPlot", "Download Plot"),
                           fluidRow(splitLayout(style = "border: 1px solid silver:", cellWidths = c("40%","40%"),
                                                plotlyOutput("PCAdim1"),
                                                plotlyOutput("PCAdim2"))),
                           fluidRow(splitLayout(style = "border: 1px solid silver:", cellWidths = c("40%","40%"),
                                                downloadLink("downloadPlotdim1", "Download Plot"),
                                                downloadLink("downloadPlotdim2", "Download Plot")))
                           ),
                  tabPanel("t-SNE",  
                           selectInput("variableclust2",label = h5(strong("Select for cluster polygons")),"",selected="variableclust2"),
                           withLoader(plotlyOutput("tSNE",height = "800px", width = "100%"),type="text",
                                                loader = list(marquee("Please be patient, this can take up to 60 seconds"))),
                           downloadLink("downloadPlot2", "Download Plot")
                           ),
                  tabPanel("Clustering",
                           withLoader(
                   plotlyOutput("Clustering",height = "1200px", width = "100%")),
                   downloadLink("downloadPlot3", "Download Plot"),
                   fluidRow(splitLayout(style = "border: 1px solid silver:", cellWidths = c("40%","40%"),
                                        plotOutput("DendClust",height = "2400px"),
                                        plotOutput("DendIm",height = "2400px"))),
                   fluidRow(splitLayout(style = "border: 1px solid silver:", cellWidths = c("40%","40%"),
                                        downloadLink("downloadPlot4", "Download Plot"),
                                        downloadLink("downloadPlot5", "Download Plot"))),
                   fluidRow(plotOutput("DendBoth",height = "2400px")),
                   fluidRow(downloadLink("downloadPlot7", "Download Plot")),
                   fluidRow(selectInput("clustset",'Choose a set:',"",selected="clustset")),
                   fluidRow(splitLayout(style = "border: 1px solid silver:", cellWidths = c("40%","40%"),
                                        selectInput("dendclust",'Choose a cluster:',"",selected="dendclust",selectize = F,size=3),
                                        selectInput("recgroup",'Choose a group of auto reconstructions:',"",selected="recgroup",selectize = F,size=3))),
                   fluidRow(splitLayout(style = "border: 1px solid silver:", cellWidths = c("40%","40%"),
                                        selectInput("dendplot",'Choose a Gold Standard tree for plotting:',"",selected="dendplot",selectize = F,size=3),
                                        selectInput("recalg",'Choose algorithm:',"",selected="recalg",selectize = F,size=3))),
                   fluidRow(splitLayout(style = "border: 1px solid silver:", cellWidths = c("40%","40%"),
                                        plotOutput("DendPlot",height = "400px"),
                                        plotOutput("DendPlot2",height = "400px"))),
                   fluidRow(splitLayout(style = "border: 1px solid silver:", cellWidths = c("40%","40%"),
                                        plotOutput("ImPlot",height = "600px")))),
                 tabPanel("Sholl",
                          plotlyOutput("ShollPlot",height="500px"),
                          fluidRow(downloadLink("downloadPlot8", "Download Plot")),
                          fluidRow(splitLayout(style = "border: 1px solid silver:", cellWidths = c("40%","40%"),
                                               plotlyOutput("DensPlot",height = "400px", width = "100%"),
                                               plotlyOutput("VolPlot",height = "400px", width = "100%"))),
                          fluidRow(splitLayout(style = "border: 1px solid silver:", cellWidths = c("40%","40%"),
                                               downloadLink("downloadPlot9", "Download Plot"),
                                               downloadLink("downloadPlot10", "Download Plot")))),
                 
                          # fluidRow(plotOutput("VolPlot",height = "400px"))),
                 tabPanel("Persistent Homology",
                          h5(strong("Click on any data point in the heatmap to see Persistent Homology plots for pairs of neurons.")),
                          #splitLayout(style = "border: 1px solid silver:", cellWidths = c("40%","40%"),
                          fluidRow(
                                       selectInput("xlabtmdmap",label = h5(strong("Select for x axis labels")),"",selected="xlabtmdmap"),
                                       selectInput("ylabtmdmap",label = h5(strong("Select for y axis labels")),"",selected="ylabtmdmap")),
                          fluidRow(withLoader(plotlyOutput("ClusterMap",height="1000px"))),
                          fluidRow(splitLayout(style = "border: 1px solid silver:",
                                               cellWidths = c("40%","40%"),
                                               verbatimTextOutput("Neuronnm1"),
                                               verbatimTextOutput("Neuronnm2"))),
                          fluidRow(splitLayout(style = "border: 1px solid silver:", cellWidths = c("40%","40%"),
                                               plotOutput("DiagramPlot",height = "400px", width = "100%"),
                                               plotOutput("DiagramPlot2",height = "400px", width = "100%"))),
                          fluidRow(splitLayout(style = "border: 1px solid silver:", cellWidths = c("40%","40%"),
                                               plotOutput("BarcodePlot",height = "400px", width = "100%"),
                                               plotOutput("BarcodePlot2",height = "400px", width = "100%"))),
                          fluidRow(splitLayout(style = "border: 1px solid silver:", cellWidths = c("40%","40%"),
                                               plotOutput("ImagePlot",height = "400px", width = "100%"),
                                               plotOutput("ImagePlot2",height = "400px", width = "100%"))),
                          fluidRow(splitLayout(style = "border: 1px solid silver:", cellWidths = c("40%","40%"),
                                               rglwidgetOutput("TreePlot",height = "400px", width = "100%"),
                                               rglwidgetOutput("TreePlot2",height = "400px", width = "100%"))),
                          fluidRow(tableOutput("TreesMetadata"))),
                 tabPanel("Distances",
                          selectInput("recgroup2",'Choose a group of auto reconstructions:',"",selected="recgroup2",selectize = F,size=2),
                          selectInput("variableclust3",'Choose a set:',"",selected="variableclust3"),
                          selectInput("distclust",'Choose a cluster:',"",selected="distclust",selectize = F,size=3),
                          plotlyOutput("Distances",height = "600px", width = "100%"),
                          downloadLink("downloadPlot6", "Download Plot"))
      )
    ),
    column(1),
    column(2,
           checkboxGroupInput("variablealg", "Choose algorithm(s)","",selected="variablealg",inline=TRUE),
           actionLink("selectall4","Select All")
    ),
    column(4,
           checkboxGroupInput("variabledat", "Choose dataset(s)","",selected="variabledat",inline=TRUE),
           actionLink("selectall5","Select All")
    ),
    column(2,
           selectInput("filtermetrics",'Choose a metric for filtering:',"",selected="filtermetrics"),
           sliderInput("range", "Select Range (% of max)",
                       min = 0, max = 100, value = c(0, 100))
           )
  ),
  fluidRow(
    column(1),
    
    column(4,
           
          # Input: Text
          textInput("inputalg","Specify uploaded reconstruction algorithm name"),
          # Horizontal line ----
          tags$hr(),
          # Input: Select a file ----
          fileInput("file1", "Choose SWC and their associated IQ files",
              multiple = TRUE,
              accept = c(".swc",".eswc",".txt"))),

          # # Input: Checkbox 
          # checkboxGroupInput("variableinpdat", "Choose images of uploaded reconstructions","",selected="variableinpdat",inline=TRUE),
          # actionLink("selectallinp","Select All"))
    column(6,
           verbatimTextOutput("console"))
)))