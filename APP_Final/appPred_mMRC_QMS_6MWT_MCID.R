library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(philentropy)
library(ggpubr)
library(cowplot)
library(leaflet)

load("data/App_Final_Data.RData")

min_f1=-2.856
max_f1=1.114
min_f2=-27.134
max_f2=17.107
min_f3=-148.41
max_f3=80.01    
    
b <- cbind(App_Data_Norm$f1[App_Data_Norm$group2==1],
           App_Data_Norm$f2[App_Data_Norm$group2==1],
           App_Data_Norm$f3[App_Data_Norm$group2==1])

FuncOptAllOriginalData_3_13<-function (x) {
    if (is.null(dim(x))) {x <- matrix(x, nrow = 1)}
    b1 <- as.numeric(x[, 1])
    b2 <- as.numeric(x[, 2])
    
    b5 <- x[, 3]
    if (x[, 4]==1) {b6 <- 0
    b7<-0}
    if (x[, 4]==2) {b6 <- 1
    b7<-0}
    if (x[, 4]==3) {b6 <- 0
    b7<-1}
    b9 <- as.numeric(x[, 5])
    b11 <- x[, 6]
    b16 <- x[, 7]
    b18 <- x[, 8]
    b19 <- x[, 9]
    b20 <- x[, 10]
    
    f1<- 2.2477-0.6646*b1-0.0035*b5+0.4992*b9-0.6450*b16-0.0028*b18+0.0184*b19
    
    f2<- -0.5405-4.9981*b1-6.1571*b6-4.9780*b7-0.3736*b11+1.5448*b16-0.0169*b18+0.6241*b20
    
    f3<--81.2005-46.3837*b1-23.4841*b2+0.2637*b5-59.0927*b6-24.2149*b7+33.5306*b9+11.5711*b16+0.2047*b18
    
    return(cbind(f1,f2,f3))
}



ui <- dashboardPagePlus(#theme="bootstrap.min.css", 
    skin = "green",
    
    # Header ####  
    dashboardHeaderPlus(title = tagList(
        span(class = "logo-lg", "COPD"), 
        img(src = "LAr3_xs.png")),titleWidth = 350,
        dropdownMenu(type = "messages",
                     messageItem(
                         from = "Admin",
                         message = "Welcome.",
                         icon = icon("grin"),
                         time = ""
                     )),
        dropdownMenu(type = "notifications"
        ),
        dropdownMenu(type = "tasks"
        ), enable_rightsidebar = TRUE, rightSidebarIcon = "gears"
        
        #,left_menu = tagList(
            # dropdownBlock(
            #     id = "mydropdown",
            #     title = "Linear Regression",
            #     icon = "sliders",
            #     selectInput("Step", h5(strong("LinReg:")),
            #                 c("OLS" = "none",
            #                   "Robust" = "R",
            #                   "VIF Backward" = "VIF",
            #                   "AIC Backward" = "AIC"),selected = "none"#,
            #                 #inline = FALSE,width = '100%'
            #     ),
            #     prettyToggle(
            #         inputId = "Outliers",
            #         label_on = "Remove outliers",
            #         label_off = "Remove outliers",
            #         icon_on = icon("check"),
            #         icon_off = icon("remove"),
            #         animation = "smooth"
            #     )
            # )
            #)
    ),
    
    # SideBar ####
    dashboardSidebar(width = 350,
                     sidebarMenu(
                         menuItem("Home", tabName = "home", icon = icon("house-user")),
                         
                         menuItem("Distance Predicted", tabName = "distPred", icon = icon("chart-line")),
                         
                         menuItem("mMRC difference predicted", tabName = "dmMRC", icon = icon("lungs")),
                         
                         menuItem("QMS difference predicted", tabName = "dQMS", icon = icon("dumbbell")),
                         
                         menuItem("6MWT difference predicted", tabName = "d6MWT", icon = icon("walking")),
                         
                         menuItem("Help", tabName = "Help", icon = icon("question-circle")),
                         
                         menuItem("About", tabName = "About", icon = icon("info-circle"))
                         
                     ) # end sidebarMenu
                     
    ), #end dashboardSidebar
    
    
    # Body ####                         
    dashboardBody(
        
         
        
        tabItems(
            #### home ####
            tabItem(tabName = "home",
                    br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                    fluidRow( column(6,
                                     div(tags$a(img(src = "biomath.png"),href="https://sites.google.com/view/ltbiomath/home"), style="text-align: center;")),
                              column(6, div(tags$a(img(src = "LAr3_a.png"),href="https://www.ua.pt/pt/lab3r"), style="text-align: center;")
                                     )),
                    br(),br(),br(),br(),br(),br(),br(),
                    fluidRow( 
                              column(12,
                                     div(tags$a(img(src = "logo.png"),href="https://www.ua.pt"), style="text-align: center;")
                              ))
            ),
            
            #### dist Pred ####
            tabItem(tabName = "distPred",
                    fluidRow(column(6,
                    fluidRow(column(2),
                                    column(2,
                                    knobInput(
                                               inputId = "mMRC",
                                               label = "mMRC",
                                               value = 1,
                                               min = 0,
                                               max = 4,
                                               rotation = c("clockwise"),
                                               bgColor = "white",
                                               immediate = FALSE,
                                               displayPrevious = FALSE,
                                               displayInput = TRUE,
                                               lineCap = "round",
                                               fgColor = "green",
                                               inputColor = "#5D478B",
                                               width = 75,
                                               height = 75)),
                                      column(2,
                                             knobInput(
                                               inputId = "SixMWT",
                                               label = "6MWT",
                                               value = 552,
                                               min = 75,
                                               max = 652,
                                               rotation = c("clockwise"),
                                               bgColor = "white",
                                               immediate = FALSE,
                                               displayPrevious = FALSE,
                                               displayInput = TRUE,
                                               lineCap = "round",
                                               fgColor = "green",
                                               inputColor = "#5D478B",
                                               width = 75,
                                               height = 75)),
                                      column(2,
                                             knobInput(
                                               inputId = "PY",
                                               label = "Pack Years",
                                               value = 0,
                                               min = 0,
                                               max = 180,
                                               rotation = c("clockwise"),
                                               bgColor = "white",
                                               immediate = FALSE,
                                               displayPrevious = FALSE,
                                               displayInput = TRUE,
                                               lineCap = "round",
                                               fgColor = "green",
                                               inputColor = "#5D478B",
                                               width = 75,
                                               height = 75)),
                                      column(2,
                                             knobInput(
                                               inputId = "LTOT",
                                               label = "LTOT",
                                               value = 0,
                                               min = 0,
                                               max = 1,
                                               step=1,
                                               rotation = c("clockwise"),
                                               bgColor = "white",
                                               immediate = FALSE,
                                               displayPrevious = FALSE,
                                               displayInput = FALSE,
                                               lineCap = "round",
                                               fgColor = "green",
                                               inputColor = "#5D478B",
                                               width = 75,
                                               height = 75)),
                             column(2,
                                    knobInput(
                                      inputId = "CAT",
                                      label = "CAT",
                                      value = 9,
                                      min = 2,
                                      max = 35,
                                      rotation = c("clockwise"),
                                      bgColor = "white",
                                      immediate = FALSE,
                                      displayPrevious = FALSE,
                                      displayInput = TRUE,
                                      lineCap = "round",
                                      fgColor = "green",
                                      inputColor = "#5D478B",
                                      width = 75,
                                      height = 75)))),
                                    column(6,fluidRow(
                                      
                             column(2,
                                    knobInput(
                                      inputId = "BMI",
                                      label = "BMI",
                                      value = 26.6,
                                      min = 17.6,
                                      max = 38.2,
                                      step = .1,
                                      rotation = c("clockwise"),
                                      bgColor = "white",
                                      immediate = FALSE,
                                      displayPrevious = FALSE,
                                      displayInput = TRUE,
                                      lineCap = "round",
                                      fgColor = "green",
                                      inputColor = "#5D478B",
                                      width = 75,
                                      height = 75)),
                             column(2,
                                    knobInput(
                                      inputId = "QMS",
                                      label = "QMS",
                                      value = 37.1,
                                      min = 13.3,
                                      max = 55.6,
                                      step = 0.1,
                                      rotation = c("clockwise"),
                                      bgColor = "white",
                                      immediate = FALSE,
                                      displayPrevious = FALSE,
                                      displayInput = TRUE,
                                      lineCap = "round",
                                      fgColor = "green",
                                      inputColor = "#5D478B",
                                      width = 75,
                                      height = 75)),
                             column(2,
                                    knobInput(
                                      inputId = "SmokSt",
                                      label = "SmokSt",
                                      value = 1,
                                      min = 1,
                                      max = 3,
                                      step = 1,
                                      rotation = c("clockwise"),
                                      bgColor = "white",
                                      immediate = FALSE,
                                      displayPrevious = FALSE,
                                      displayInput = TRUE,
                                      lineCap = "round",
                                      fgColor = "green",
                                      inputColor = "#5D478B",
                                      width = 75,
                                      height = 75)),
                             column(2,
                                    knobInput(
                                      inputId = "Gender",
                                      label = "Female",
                                      value = 1,
                                      min = 0,
                                      max = 1,
                                      step=1,
                                      rotation = c("clockwise"),
                                      bgColor = "white",
                                      immediate = FALSE,
                                      displayPrevious = FALSE,
                                      displayInput = FALSE,
                                      lineCap = "round",
                                      fgColor = "green",
                                      inputColor = "#5D478B",
                                      width = 75,
                                      height = 75)),
                             column(2,br(),br(),actionButton("refresh", "",width="100%",icon = icon("redo"))),
                             column(2)))),
                    
                             
                    fluidRow(column(4,box(title="Pareto front",status="success",background = "green",solidHeader = TRUE,collapsible = TRUE, width = 12,
                                          plotlyOutput("PlotPareto"))),
                             column(2,
                                    valueBoxOutput("dmMRC",width = 12),
                                    valueBoxOutput("dQMS",width = 12),
                                    valueBoxOutput("dSixMWT",width = 12)),
                             column(4,
                                    boxPlus(title="Distance",
                                            status="success",
                                            solidHeader = TRUE,
                                            collapsible = TRUE, 
                                            background = "green",
                                            width = 12,
                                            enable_sidebar = TRUE,
                                            sidebar_width = 25,
                                            side_bar_title = "Options",
                                            sidebar_start_open = FALSE,
                                            sidebar_content = list(
                                                prettyToggle(
                                                    inputId = "Observed",
                                                    label_on = "Include observed",
                                                    label_off = "Include observed",
                                                    icon_on = icon("check"),
                                                    icon_off = icon("remove"),
                                                    animation = "smooth",
                                                    value=FALSE
                                                ),
                                            prettyToggle(
                                                inputId = "Pareto",
                                                label_on = "Include Pareto",
                                                label_off = "Include Pareto",
                                                icon_on = icon("check"),
                                                icon_off = icon("remove"),
                                                animation = "smooth",
                                                value=FALSE
                                            ),
                                            br(),
                                            prettyToggle(
                                            inputId = "DensityHist",
                                            label_on = "Density curve",
                                            label_off = "Density curve",
                                            icon_on = icon("check"),
                                            icon_off = icon("remove"),
                                            animation = "smooth",
                                            value=TRUE
                                            ),
                                            br(),
                                            prettyToggle(
                                                inputId = "CountHist",
                                                label_on = "Count values",
                                                label_off = "Count values",
                                                icon_on = icon("check"),
                                                icon_off = icon("remove"),
                                                animation = "smooth",
                                                value=TRUE
                                            ),
                                            knobInput(
                                                inputId = "binsPred",
                                                label = "Bins",
                                                value = 16,
                                                min = 12,
                                                max = 40,
                                                rotation = c("clockwise"),
                                                bgColor = "white",
                                                immediate = TRUE,
                                                displayPrevious = FALSE,
                                                displayInput = FALSE,
                                                lineCap = "round",
                                                fgColor = "green",
                                                inputColor = "#5D478B",
                                                width = 100,
                                                height = 100)),
                                                
                                            plotOutput("DistHist"))),
                                    
                             
                             column(2,valueBoxOutput("distance",width = 12),
                                    valueBoxOutput("dist_Perc",width = 12))),
                    fluidRow(column(4,box(title="Pareto front - Projection",status="success",background = "green",solidHeader = TRUE,collapsible = TRUE, width = 12,
                                          plotlyOutput("PlotParetof1f2"))),
                             column(4,box(title="Pareto front - Projection",status="success",background = "green",solidHeader = TRUE,collapsible = TRUE, width = 12,
                                          plotlyOutput("PlotParetof1f3"))),
                             column(4,box(title="Pareto front - Projection",status="success",background = "green",solidHeader = TRUE,collapsible = TRUE, width = 12,
                                          plotlyOutput("PlotParetof2f3")))),
            ),
            
            ####  dmMRC ####
            tabItem(tabName = "dmMRC",
                    
                    fluidRow(column(12,#box(title="Variables",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,
                                        fluidRow(
                                          column(2,
                                          knobInput(
                                            inputId = "mMRCf1",
                                            label = "mMRC",
                                            value = 1,
                                            min = 0,
                                            max = 4,
                                            rotation = c("clockwise"),
                                            bgColor = "white",
                                            immediate = TRUE,
                                            displayPrevious = FALSE,
                                            displayInput = TRUE,
                                            lineCap = "round",
                                            fgColor = "green",
                                            inputColor = "#5D478B",
                                            width = 100,
                                            height = 100)),
                                          column(2,
                                                 knobInput(
                                                   inputId = "SixMWTf1",
                                                   label = "6MWT",
                                                   value = 552,
                                                   min = 75,
                                                   max = 652,
                                                   rotation = c("clockwise"),
                                                   bgColor = "white",
                                                   immediate = TRUE,
                                                   displayPrevious = FALSE,
                                                   displayInput = TRUE,
                                                   lineCap = "round",
                                                   fgColor = "green",
                                                   inputColor = "#5D478B",
                                                   width = 100,
                                                   height = 100)),
                                          column(2,
                                                 knobInput(
                                                   inputId = "PYf1",
                                                   label = "Pack Years",
                                                   value = 0,
                                                   min = 0,
                                                   max = 180,
                                                   rotation = c("clockwise"),
                                                   bgColor = "white",
                                                   immediate = TRUE,
                                                   displayPrevious = FALSE,
                                                   displayInput = TRUE,
                                                   lineCap = "round",
                                                   fgColor = "green",
                                                   inputColor = "#5D478B",
                                                   width = 100,
                                                   height = 100)),
                                          column(2,
                                                 knobInput(
                                                   inputId = "LTOTf1",
                                                   label = "LTOT",
                                                   value = 0,
                                                   min = 0,
                                                   max = 1,
                                                   step=1,
                                                   rotation = c("clockwise"),
                                                   bgColor = "white",
                                                   immediate = TRUE,
                                                   displayPrevious = FALSE,
                                                   displayInput = FALSE,
                                                   lineCap = "round",
                                                   fgColor = "green",
                                                   inputColor = "#5D478B",
                                                   width = 100,
                                                   height = 100)),
                                          column(2,
                                          knobInput(
                                            inputId = "CATf1",
                                            label = "CAT",
                                            value = 9,
                                            min = 2,
                                            max = 35,
                                            rotation = c("clockwise"),
                                            bgColor = "white",
                                            immediate = TRUE,
                                            displayPrevious = FALSE,
                                            displayInput = TRUE,
                                            lineCap = "round",
                                            fgColor = "green",
                                            inputColor = "#5D478B",
                                            width = 100,
                                            height = 100)),
                                          column(2,
                                          knobInput(
                                            inputId = "PRf1",
                                            label = "PR",
                                            value = 1,
                                            min = 0,
                                            max = 1,
                                            step=1,
                                            rotation = c("clockwise"),
                                            bgColor = "white",
                                            immediate = TRUE,
                                            displayPrevious = FALSE,
                                            displayInput = FALSE,
                                            lineCap = "round",
                                            fgColor = "green",
                                            inputColor = "#5D478B",
                                            width = 100,
                                            height = 100,
                                            readOnly = FALSE))))),
                    fluidRow(column(3,
                                    boxPlus(title="Variation",
                                            status="success",
                                            solidHeader = TRUE,
                                            collapsible = TRUE, 
                                            background = "green",
                                            width = 12,
                                            enable_sidebar = TRUE,
                                            sidebar_width = 25,
                                            side_bar_title = "Options",
                                            sidebar_start_open = FALSE,
                                            sidebar_content = prettyRadioButtons(inputId = "PTypef1",
                                                                                                  label = h5(strong("Type:")),
                                                                                                  choices = c("Bar" = "1",
                                                                                                              "Donut" = "2"
                                                                                                  ),
                                                                                                  selected = "1",
                                                                                                  shape = "curve",
                                                                                                  fill=TRUE,
                                                                                                  icon = icon("check"),
                                                                                                  animation = "smooth",
                                                                                                  inline = FALSE,
                                                                                                  outline = TRUE,
                                                                                                  width = "100%"),
                                                     plotOutput("donutf1"))),
                             column(2,
                                    valueBoxOutput("dmMRCf1",width = 12),
                                    valueBoxOutput("dmMRCf1_Perc",width = 12)),
                             column(7,
                                    boxPlus(title="Histogram",
                                            status="success",
                                            solidHeader = TRUE,
                                            collapsible = TRUE,
                                            background = "green",
                                            width = 12,
                                            enable_sidebar = TRUE,
                                            sidebar_width = 25,
                                            side_bar_title = "Options",
                                            sidebar_start_open = FALSE,
                                            sidebar_content = list(
                                              knobInput(
                                               inputId = "binsmMRC",
                                               label = "Bins",
                                               value = 16,
                                               min = 12,
                                               max = 40,
                                               rotation = c("clockwise"),
                                               bgColor = "white",
                                               immediate = TRUE,
                                               displayPrevious = FALSE,
                                               displayInput = FALSE,
                                               lineCap = "round",
                                               fgColor = "green",
                                               inputColor = "#5D478B",
                                               width = 100,
                                               height = 100),
                                              prettyRadioButtons(inputId = "HistTypemMRC",
                                                                 label = h5(strong("Histogram:")),
                                                                 choices = c("Type 1" = "1",
                                                                             "Type 2" = "2"
                                                                   ),
                                                                 selected = "1",
                                                                 shape = "curve",
                                                                 fill=TRUE,
                                                                 icon = icon("check"),
                                                                 animation = "smooth",
                                                                 inline = FALSE,
                                                                 outline = TRUE,
                                                                 width = "100%"),
                                              prettyRadioButtons(inputId = "IntervalTypemMRC",
                                                                 label = h5(strong("Interval:")),
                                                                 choices = c("Prediction" = "prediction",
                                                                             "Confidence" = "confidence"
                                                                 ),
                                                                 selected = "prediction",
                                                                 shape = "curve",
                                                                 fill=TRUE,
                                                                 icon = icon("check"),
                                                                 animation = "smooth",
                                                                 inline = FALSE,
                                                                 outline = TRUE,
                                                                 width = "100%")),
                                            
                                            plotOutput("HistmMRC")))
            ),
            fluidRow(column(2,box(title="mMRC",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                  plotOutput("BoxPlotf1_mMRC"))),
                     column(2,box(title="6MWT",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                  plotOutput("BoxPlotf1_6MWT"))),
                     column(2,box(title="Pack Years",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                  plotOutput("BoxPlotf1_PY"))),
                     column(2,box(title="LTOT",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                  plotOutput("BoxPlotf1_LTOT"))),
                     column(2,box(title="CAT",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                  plotOutput("BoxPlotf1_CAT"))),
                     column(2,box(title="",status="success",solidHeader = TRUE,collapsible = TRUE, collapsed = TRUE, width = 12,background = "green"
                     )))),
            #### dQMS ####
            tabItem(tabName = "dQMS",
                    
                    fluidRow(column(12,
                                    fluidRow(
                                      
                                      column(2,
                                             knobInput(
                                               inputId = "mMRCf2",
                                               label = "mMRC",
                                               value = 1,
                                               min = 0,
                                               max = 4,
                                               step = 1,
                                               rotation = c("clockwise"),
                                               bgColor = "white",
                                               immediate = TRUE,
                                               displayPrevious = FALSE,
                                               displayInput = TRUE,
                                               lineCap = "round",
                                               fgColor = "green",
                                               inputColor = "#5D478B",
                                               width = 100,
                                               height = 100)),
                                      column(2,
                                             knobInput(
                                               inputId = "SixMWTf2",
                                               label = "6MWT",
                                               value = 552,
                                               min = 75,
                                               max = 652,
                                               step = 1,
                                               rotation = c("clockwise"),
                                               bgColor = "white",
                                               immediate = TRUE,
                                               displayPrevious = FALSE,
                                               displayInput = TRUE,
                                               lineCap = "round",
                                               fgColor = "green",
                                               inputColor = "#5D478B",
                                               width = 100,
                                               height = 100)),
                                      
                                      column(2,
                                             knobInput(
                                               inputId = "BMIf2",
                                               label = "BMI",
                                               value = 26.6,
                                               min = 17.6,
                                               max = 38.2,
                                               step = .1,
                                               rotation = c("clockwise"),
                                               bgColor = "white",
                                               immediate = TRUE,
                                               displayPrevious = FALSE,
                                               displayInput = TRUE,
                                               lineCap = "round",
                                               fgColor = "green",
                                               inputColor = "#5D478B",
                                               width = 100,
                                               height = 100)),
                                      column(2,
                                             knobInput(
                                               inputId = "QMSf2",
                                               label = "QMS",
                                               value = 37.1,
                                               min = 13.3,
                                               max = 55.6,
                                               step = 0.1,
                                               rotation = c("clockwise"),
                                               bgColor = "white",
                                               immediate = TRUE,
                                               displayPrevious = FALSE,
                                               displayInput = TRUE,
                                               lineCap = "round",
                                               fgColor = "green",
                                               inputColor = "#5D478B",
                                               width = 100,
                                               height = 100)),
                                      column(2,
                                             knobInput(
                                               inputId = "SmokStf2",
                                               label = "SmokSt",
                                               value = 1,
                                               min = 1,
                                               max = 3,
                                               step = 1,
                                               rotation = c("clockwise"),
                                               bgColor = "white",
                                               immediate = TRUE,
                                               displayPrevious = FALSE,
                                               displayInput = TRUE,
                                               lineCap = "round",
                                               fgColor = "green",
                                               inputColor = "#5D478B",
                                               width = 100,
                                               height = 100)),
                                      column(2,
                                    knobInput(
                                            inputId = "PRf2",
                                            label = "PR",
                                            value = 1,
                                            min = 0,
                                            max = 1,
                                            step=1,
                                            rotation = c("clockwise"),
                                            bgColor = "white",
                                            immediate = TRUE,
                                            displayPrevious = FALSE,
                                            displayInput = FALSE,
                                            lineCap = "round",
                                            fgColor = "green",
                                            inputColor = "#5D478B",
                                            width = 100,
                                            height = 100,
                                            readOnly = FALSE))
                    ))#,
            #column(2,
            #       valueBoxOutput("dQMSf2",width = 12)),
            #column(2,valueBoxOutput("dQMSf2_Perc",width = 12))
            ),
                    fluidRow(column(3,
                                                    boxPlus(title="Variation",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,
                                                            enable_sidebar = TRUE,
                                                            sidebar_width = 25,
                                                            background = "green",
                                                            side_bar_title = "Options",
                                                            sidebar_start_open = FALSE,
                                                            sidebar_content = prettyRadioButtons(inputId = "PTypef2",
                                                                                                 label = h5(strong("Type:")),
                                                                                                 choices = c("Bar" = "1",
                                                                                                             "Donut" = "2"
                                                                                                 ),
                                                                                                 selected = "1",
                                                                                                 shape = "curve",
                                                                                                 fill=TRUE,
                                                                                                 icon = icon("check"),
                                                                                                 animation = "smooth",
                                                                                                 inline = FALSE,
                                                                                                 outline = TRUE,
                                                                                                 width = "100%"),
                                                            plotOutput("donutf2"))),
                             column(2,
                                    valueBoxOutput("dQMSf2",width = 12),
                                    valueBoxOutput("dQMSf2_Perc",width = 12)),
                                      column(7,
                                             boxPlus(title="Histogram",
                                                     status="success",
                                                     solidHeader = TRUE,
                                                     collapsible = TRUE,
                                                     background = "green",
                                                     width = 12,
                                                     enable_sidebar = TRUE,
                                                     sidebar_width = 25,
                                                     side_bar_title = "Options",
                                                     sidebar_start_open = FALSE,
                                                     sidebar_content = list(
                                                       knobInput(
                                                         inputId = "binsQMS",
                                                         label = "Bins",
                                                         value = 16,
                                                         min = 12,
                                                         max = 40,
                                                         rotation = c("clockwise"),
                                                         bgColor = "white",
                                                         immediate = TRUE,
                                                         displayPrevious = FALSE,
                                                         displayInput = FALSE,
                                                         lineCap = "round",
                                                         fgColor = "green",
                                                         inputColor = "#5D478B",
                                                         width = 100,
                                                         height = 100),
                                                       prettyRadioButtons(inputId = "HistTypeQMS",
                                                                          label = h5(strong("Histogram:")),
                                                                          choices = c("Type 1" = "1",
                                                                                      "Type 2" = "2"
                                                                          ),
                                                                          selected = "1",
                                                                          shape = "curve",
                                                                          fill=TRUE,
                                                                          icon = icon("check"),
                                                                          animation = "smooth",
                                                                          inline = FALSE,
                                                                          outline = TRUE,
                                                                          width = "100%"),
                                                       prettyRadioButtons(inputId = "IntervalTypeQMS",
                                                                          label = h5(strong("Interval:")),
                                                                          choices = c("Prediction" = "prediction",
                                                                                      "Confidence" = "confidence"
                                                                          ),
                                                                          selected = "prediction",
                                                                          shape = "curve",
                                                                          fill=TRUE,
                                                                          icon = icon("check"),
                                                                          animation = "smooth",
                                                                          inline = FALSE,
                                                                          outline = TRUE,
                                                                          width = "100%")),
                                                     
                                                     plotOutput("HistQMS")))
                             ),
                             fluidRow(
                             column(2,box(title="mMRC",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                            plotOutput("BoxPlotf2_mMRC"))),
                             column(2,box(title="6MWT",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                          plotOutput("BoxPlotf2_6MWT"))),
                             column(2,box(title="BMI",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                          plotOutput("BoxPlotf2_BMI"))),
                             column(2,box(title="QMS",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                          plotOutput("BoxPlotf2_QMS"))),
                             column(2,box(title="SmokSt",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                          plotOutput("BoxPlotf2_SmokSt"))),
                             column(2,box(title="",status="success",solidHeader = TRUE,collapsible = TRUE, collapsed = TRUE, width = 12,background = "green"
                             )))),
            
            #### d6MWT ####
            tabItem(tabName = "d6MWT",
                    
                    fluidRow(column(10,#box(title="Variables",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,
                                    fluidRow(column(2,
                                             knobInput(
                                               inputId = "mMRCf3",
                                               label = "mMRC",
                                               value = 1,
                                               min = 0,
                                               max = 4,
                                               step = 1,
                                               rotation = c("clockwise"),
                                               bgColor = "white",
                                               immediate = TRUE,
                                               displayPrevious = FALSE,
                                               displayInput = TRUE,
                                               lineCap = "round",
                                               fgColor = "green",
                                               inputColor = "#5D478B",
                                               width = 100,
                                               height = 100)),
                                      column(2,
                                             knobInput(
                                               inputId = "SixMWTf3",
                                               label = "6MWT",
                                               value = 552,
                                               min = 75,
                                               max = 652,
                                               rotation = c("clockwise"),
                                               bgColor = "white",
                                               immediate = TRUE,
                                               displayPrevious = FALSE,
                                               displayInput = TRUE,
                                               lineCap = "round",
                                               fgColor = "green",
                                               inputColor = "#5D478B",
                                               width = 100,
                                               height = 100)),
                                      column(2,
                                             knobInput(
                                               inputId = "PYf3",
                                               label = "Pack Years",
                                               value = 0,
                                               min = 0,
                                               max = 180,
                                               rotation = c("clockwise"),
                                               bgColor = "white",
                                               immediate = TRUE,
                                               displayPrevious = FALSE,
                                               displayInput = TRUE,
                                               lineCap = "round",
                                               fgColor = "green",
                                               inputColor = "#5D478B",
                                               width = 100,
                                               height = 100)),
                                      column(2,
                                             knobInput(
                                               inputId = "LTOTf3",
                                               label = "LTOT",
                                               value = 0,
                                               min = 0,
                                               max = 1,
                                               step=1,
                                               rotation = c("clockwise"),
                                               bgColor = "white",
                                               immediate = TRUE,
                                               displayPrevious = FALSE,
                                               displayInput = FALSE,
                                               lineCap = "round",
                                               fgColor = "green",
                                               inputColor = "#5D478B",
                                               width = 100,
                                               height = 100)),
                                      column(2,
                                             knobInput(
                                               inputId = "SmokStf3",
                                               label = "SmokSt",
                                               value = 1,
                                               min = 1,
                                               max = 3,
                                               step = 1,
                                               rotation = c("clockwise"),
                                               bgColor = "white",
                                               immediate = TRUE,
                                               displayPrevious = FALSE,
                                               displayInput = TRUE,
                                               lineCap = "round",
                                               fgColor = "green",
                                               inputColor = "#5D478B",
                                               width = 100,
                                               height = 100)),
                                      column(2,
                                             knobInput(
                                               inputId = "Genderf3",
                                               label = "Female",
                                               value = 1,
                                               min = 0,
                                               max = 1,
                                               step=1,
                                               rotation = c("clockwise"),
                                               bgColor = "white",
                                               immediate = TRUE,
                                               displayPrevious = FALSE,
                                               displayInput = FALSE,
                                               lineCap = "round",
                                               fgColor = "green",
                                               inputColor = "#5D478B",
                                               width = 100,
                                               height = 100)))),
                                      column(2,
                                             knobInput(
                                                 inputId = "PRf3",
                                                 label = "PR",
                                                 value = 1,
                                                 min = 0,
                                                 max = 1,
                                                 step=1,
                                                 rotation = c("clockwise"),
                                                 bgColor = "white",
                                                 immediate = TRUE,
                                                 displayPrevious = FALSE,
                                                 displayInput = FALSE,
                                                 lineCap = "round",
                                                 fgColor = "green",
                                                 inputColor = "#5D478B",
                                                 width = 100,
                                                 height = 100,
                                                 readOnly = FALSE))),
                    fluidRow(column(3,
                                                    boxPlus(title="Variation",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,
                                                            enable_sidebar = TRUE,
                                                            sidebar_width = 25,
                                                            side_bar_title = "Options",
                                                            background = "green",
                                                            sidebar_start_open = FALSE,
                                                            sidebar_content = prettyRadioButtons(inputId = "PTypef3",
                                                                                                 label = h5(strong("Type:")),
                                                                                                 choices = c("Bar" = "1",
                                                                                                             "Donut" = "2"
                                                                                                 ),
                                                                                                 selected = "1",
                                                                                                 shape = "curve",
                                                                                                 fill=TRUE,
                                                                                                 icon = icon("check"),
                                                                                                 animation = "smooth",
                                                                                                 inline = FALSE,
                                                                                                 outline = TRUE,
                                                                                                 width = "100%"),
                                                            
                                                            plotOutput("donutf3"))),
                             column(2,
                                    valueBoxOutput("d6MWTf3",width = 12),
                                    valueBoxOutput("d6MWTf3_Perc",width = 12)),                
                             column(7,
                                                    boxPlus(title="Histogram",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,
                                                            enable_sidebar = TRUE,
                                                            background = "green",
                                                            sidebar_width = 25,
                                                            side_bar_title = "Options",
                                                            sidebar_start_open = FALSE,
                                                            sidebar_content = list(sliderInput("bins6MWT", "Bins:",
                                                                                               min = 8,
                                                                                               max = 40,
                                                                                               step = 1,
                                                                                               value = 18),
                                                                                   radioButtons("HistType6MWT", h5(strong("Histogram:")),
                                                                                                c("Type 1" = "1",
                                                                                                  "Type 2" = "2"
                                                                                                ),selected = "1",
                                                                                                inline = FALSE,width = '100%'),
                                                                                   prettyRadioButtons(inputId = "IntervalType6MWT",
                                                                                                      label = h5(strong("Interval:")),
                                                                                                      choices = c("Prediction" = "prediction",
                                                                                                                  "Confidence" = "confidence"
                                                                                                      ),
                                                                                                      selected = "prediction",
                                                                                                      shape = "curve",
                                                                                                      fill=TRUE,
                                                                                                      icon = icon("check"),
                                                                                                      animation = "smooth",
                                                                                                      inline = FALSE,
                                                                                                      outline = TRUE,
                                                                                                      width = "100%")),
                                                            
                                                            plotOutput("Hist6MWT")))
                                    ),
                    fluidRow(
                      column(2,box(title="mMRC",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                   plotOutput("BoxPlotf3_mMRC"))),
                      column(2,box(title="6MWT",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                   plotOutput("BoxPlotf3_6MWT"))),
                      column(2,box(title="Pack Years",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                   plotOutput("BoxPlotf3_PY"))),
                      column(2,box(title="LTOT",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                   plotOutput("BoxPlotf3_LTOT"))),
                      column(2,box(title="SmokSt",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                          plotOutput("BoxPlotf3_SmokSt"))),
                      column(2,box(title="Gender",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                          plotOutput("BoxPlotf3_Gender")))
                             )),
            #### Help ####
            tabItem(tabName = "Help",
                    h1("Help"),
                    br(),
                    fluidRow(
                        column(12,box(title="Main Menu",status="success",solidHeader = TRUE,collapsible = TRUE,collapsed = T, width = 12,background = "green",
                                      fluidRow(
                                          column(8,
                                                 p("COPD pulmonary rehabilitation outcomes dashboard is divided in 7 tabs."),
                        p(span("Home tab", style = "color:orange"), "is the opening tab and provides links to University of Aveiro, CIDMA and 3R webpages"),
                        p(span("Distance predicted", style = "color:orange"), "tab lets you set the characteristics of an individivual and calculates the distance to the Pareto front. "),
                        p(span("mMRC difference predicted", style = "color:orange"), "tab lets you set the characteristics of an individivual and calculates the predicted change in mMRC."),
                        p(span("QMS difference predicted", style = "color:orange"), "tab lets you set the characteristics of an individivual and calculates the predicted change in QMS."),
                        p(span("6MWD difference predicted", style = "color:orange"), "tab lets you set the characteristics of an individivual and calculates the predicted change in 6MWD."),
                        p(span("Help", style = "color:orange"), "tab aids in the use of the dashbord."),
                        p(span("About", style = "color:orange"), "tab provides a short introduction to 3R and CIDMA.")),
                        column(4,tags$a(img(src = "Menu.png"))))))),
                    fluidRow(
                        column(12,box(title="Settings Menu",status="success",solidHeader = TRUE,collapsible = TRUE,collapsed = T, width = 12,background = "green",
                                      tags$a(img(src = "settings.png"))
                                      ))),
                    fluidRow(
                    column(12,box(title=span("Distance predicted tab", style = "color:orange"),collapsed = T,status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                    fluidRow(
                        column(12,box(title=span("Inputs", style = "color:white"),status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",collapsed = T,
                                      br(),
                                      p("On the top of the Distance predicted tab you will find several round slider inputs that allows you to set some necessary characteristics of the individual that you want to predict."),
                                        tags$a(img(src = "SliderInput01.png")),
                                      br(),
                                      br(),
                                      p(span("mMRC -", style = "color:black"), "modified Medical Research Council Questionnaire. Ranges from 0 to 4."),
                                      p(span("6MWT -", style = "color:black"), " Six-minutes walk test distance. Ranges from 75 to 652 metres."),
                                      p(span("PY -", style = "color:black"), " Pack-years (PY), a product of the average number of packs of cigarettes smoked a day and smoking duration in years. Ranges from 0 to 180."),
                                      p(span("LTOT -", style = "color:black"), " Long term oxygen therapy. 0 - No (white), 1 - Yes (green)."),
                                      p(span("CAT -", style = "color:black"), " COPD assessment test. Ranges from 2 to 35."),
                                      p(span("BMI -", style = "color:black"), " Body mass index, body weight in kilograms divided by height in meters squared. Range from 17.6 to 38.2."),
                                      p(span("QMS -", style = "color:black"), " Hand-held dynamometer quadriceps muscle strength in kgf. Ranges from 13.3 to 55.6."),
                                      p(span("SmokSt -", style = "color:black"), " Smoking status. 1 - Non-smoker, 2 - Ex-smoker, 3 - Actual smoker."),
                                      p(span("Female -", style = "color:black"), " Female gender. 0 - No (white), 1 - Yes (green).")
                                      ))),
                    fluidRow(
                        column(12,box(title=span("Pareto front plot, predictions and histogram", style = "color:white"),status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",collapsed = T,
                                      br(),
                                      p("Distance predicted tab shows the Pareto front and the relation of the individual with that set."),
                                      tags$a(img(src = "PredDist01.png"))))),
                    fluidRow(
                        column(12,box(title=span("Pareto front 2D projections", style = "color:white"),status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",collapsed = T,
                                      br(),
                                      p("Distance predicted tab showns 2D projections of the Pareto front, the new individual and the individuals included in the original study."),
                                      tags$a(img(src = "PredDist02.png")))))))),
                    
                    fluidRow(
                        column(12,box(title=span("mMRC/QMS/6MWT difference predicted tab", style = "color:orange"),collapsed = T,status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                      fluidRow(
                                          column(12,box(title=span("Inputs", style = "color:white"),status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",collapsed = T,
                                                        br(),
                                                        tags$a(img(src = "SliderInput02.png")),
                                                        br(),
                                                        br(),
                                                        tags$a(img(src = "SliderInput03.png")),
                                                        br(),
                                                        br(),
                                                        tags$a(img(src = "SliderInput04.png")),
                                                        br(),
                                                        br(),
                                                        p("Green colour in the outcome difference means that the minimal clinically important difference (MCID) was achieved."),
                                                        p("Yellow colour in the outcome difference means that the value is greater than zero but the MCID was not achieved."),
                                                        p("Red colour in the outcome difference means that we expect a worst value os the outcome at the end of the PR programme."),
                                                        br(),
                                                        p("Green colour in the Percentile means that the estimated value is greater than the 3rd quantile of the original distribution."),
                                                        p("Yellow colour in the Percentile means that the estimated value is between the 1st and the 3rd quantile of the original distribution."),
                                                        p("Red colour in the in the Percentile means that the estimated value is lesser than the 1st quantile of the original distribution."),
                                                        
                                                        
                                          ))),
                                      fluidRow(
                                          column(12,box(title=span("Variation", style = "color:white"),status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",collapsed = T,
                                                        br(),
                                                        p(""),
                                                        tags$a(img(src = "Barplot.png"))))),
                                      fluidRow(
                                          column(12,box(title=span("Histogram", style = "color:white"),status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",collapsed = T,
                                                        br(),
                                                        p(""),
                                                        tags$a(img(src = "Outcome01.png"))))),
                                      fluidRow(
                                          column(12,box(title=span("Barplot and Boxplot", style = "color:white"),status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",collapsed = T,
                                                        br(),
                                                        p(""),
                                                        tags$a(img(src = "Outcome02.png"))))),
                                      )))
                    ),
            #### About ####
            tabItem(tabName = "About",
                    h1("About"),
                    br(),
                    fluidRow(
                        column(6,box(title="Lab3R",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                    tags$a(img(src = "LAr3_b.png"),href="https://www.ua.pt/pt/lab3r"),
                    p("Respiratory Research and Rehabilitation Laboratory - Lab 3R. The development of innovative approaches in the evaluation and management of exercise capacity and respiratory rehabilitation to maximize functionality, quality of life and well-being of people with problems (or conditions) and respiratory families throughout the course of life are the main interests of this lab. Also intends to assess the burden of treatment and the disease itself and explore strategies to promote self-management of the disease and treatment adherence and healthy lifestyles. Another concern is the development and validation of new measurement tools and building extensive database, with special interest in respiratory acoustics, in order to increase knowledge about the normal and pathological behaviour of clinical measures and its response to intervention.
The laboratory, as we know it today, was created in September 2013 and since then has achieved some remarkable results, such as obtaining financing and external collaborations at national and international level, as well as recognition of the work developed in the laboratory by International prestigious institutions (for example, the European Respiratory Society and the American Thoracic Society).
The clinical nature of research conducted in the laboratory has captured the interest of students. They have the opportunity to interact with the equipment available and take an active role in the implementation of some projects in progress, allowing them to acquire skills in the context of clinical practice and research. The laboratory also provides students with an interest in research topics above the opportunity to develop their degree projects, Masters and PhD, which can be self-financed or supported through scholarships.
The wide range of research activities carried out in the laboratory, together with the excellent facilities and complementarity of teachers and researchers, fellows and PhD fellows and recognized experience project consultants, provides a stimulating environment for students to develop their graduate and post-graduate work."))),
                    
                        column(6,box(title="CIDMA",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                        tags$a(img(src = "Cidma02.png"),href="https://cidma.ua.pt/"),
                    p("The Center for Research & Development in Mathematics and Applications (CIDMA) is a research team hosted at the Department of Mathematics of the University of Aveiro. It was created in 2010 as a fusion between the research centers MA and CEOC, continuing the main goal of carrying out fundamental and applied research in several domains of Mathematics.
CIDMA also aims to contribute for the scientific development of young researchers as well as for the preparation of new researchers through post-graduate and advanced education.

The CIDMA is one of the largest mathematical research centers in Portugal having over one hundred members and collaborators, and its scientific activities are organized in 8 research groups:

Algebra and Geometry Group (AGG)
Complex and Hypercomplex Analysis Group (CHAG)
Functional Analysis and Applications Group (FAAG)
Gravitational Geometry and Dynamics Group (GGDG)
History of Mathematics and Mathematical Education Group (HMG)
Optimization, Graph Theory and Combinatorics Group (OGTCG)
Probability and Statistics Group (PSG)
Systems and Control Group (SCG)
In order to achieve its goals, the CIDMA promotes activities such as: regular talks, courses and seminars; dissemination of the scientific production of its team through the participation in scientific meetings; international cooperation (by means of visits to foreign countries and invitation of foreign researchers); organization of workshops and congresses; post-graduate and advanced research. In order to increase its contribution to social and economic development, the CIDMA also strives for an active participation in multidisciplinary projects in cooperation with other research teams.

On September 25, 2013, the Scientific Council of CIDMA adopted the Code of Practice of the European Mathematical Society.

CIDMA is supported by Portuguese funds through the Portuguese Foundation for Science and Technology, references UIDB/04106/2020 and UIDP/04106/2020."
                    )))),
                    fluidRow(
                    column(6,box(title="Video",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                           tags$iframe(width="700", height="400", src="https://www.youtube.com/embed/RObym8BOTDI", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                                           )),
                    column(6,box(title="Pdf",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                 tags$a(img(src = "BiomathResp.png"),href="https://drive.google.com/file/d/1DvFxiPK1Q34d1FRh4Ul7ZvbSBc_TEQa9/view"),
                        ))),
                    fluidRow(column(12,box(title="Map",status="success",solidHeader = TRUE,collapsible = TRUE, width = 12,background = "green",
                                     leafletOutput("map"))),
                        column(3),
                        column(6,tags$a(img(src = "Funding.png")))))
                             
        ) #end tabitems
        
    ), #end dashboardBody                        
    
    #### rightSidebar ####
    
    rightSidebar(
        background = "dark",
        
        rightSidebarTabContent(
            id = 1,
            icon = "palette",
            title = "Visual",
            fluidRow(column(4,knobInput(
                inputId = "Red",
                label = "Red:",
                value = 140,
                min = 0,
                max = 255,
                displayPrevious = FALSE,
                displayInput = FALSE,
                lineCap = "round",
                fgColor = "red",
                inputColor = "lightred",
                width = 45,
                height = 45
            )),
            
            column(4,knobInput(
                inputId = "Green",
                label = "Green:",
                value = 70,
                min = 0,
                max = 255,
                displayPrevious = FALSE,
                displayInput = FALSE,
                lineCap = "round",
                fgColor = "green",
                inputColor = "red",
                width = 45,
                height = 45
            )),
            
            column(4,knobInput(
                inputId = "Blue",
                label = "Blue:",
                value = 70,
                min = 0,
                max = 255,
                displayPrevious = FALSE,
                displayInput = FALSE,
                lineCap = "round",
                fgColor = "blue",
                inputColor = "blue",
                width = 45,
                height = 45
            ))),
            knobInput(
              inputId = "Size",
              label = "Size:",
              value = 3,
              min = 0,
              max = 10,
              displayPrevious = FALSE,
              displayInput = FALSE,
              lineCap = "round",
              fgColor = "orange",
              inputColor = "orange",
              width = 150,
              height = 150
            ),
            knobInput(
              inputId = "Transparency",
              label = "Transparency:",
              value = 0.4,
              min = 0,
              max = 1,
              step=.1,
              displayPrevious = FALSE,
              displayInput = FALSE,
              lineCap = "round",
              fgColor = "orange",
              inputColor = "orange",
              width = 150,
              height = 150
            )),
        rightSidebarTabContent(
          id = 2,
          icon = "bullseye",
          title = "MCID",
          fluidRow(column(4,knobInput(
            inputId = "MCIDmMRC",
            label = "mMRC:",
            value = 1,
            min = 0,
            max = 3,
            step=0.1,
            displayPrevious = FALSE,
            displayInput = TRUE,
            lineCap = "round",
            fgColor = "orange",
            inputColor = "orange",
            width = 45,
            height = 45
          )),
          
          column(4,knobInput(
            inputId = "MCIDQMS",
            label = "QMS:",
            value = 5.2,
            min = 0,
            max = 10,
            step=.1,
            displayPrevious = FALSE,
            displayInput = TRUE,
            lineCap = "round",
            fgColor = "orange",
            inputColor = "orange",
            width = 45,
            height = 45
          )),
          
          column(4,knobInput(
            inputId = "MCID6MWT",
            label = "6MWT:",
            value = 25,
            min = 0,
            max = 50,
            displayPrevious = FALSE,
            displayInput = TRUE,
            lineCap = "round",
            fgColor = "orange",
            inputColor = "orange",
            width = 45,
            height = 45
          ))))
        
        
    ), # end rightSidebar
    
    #### footer #### 
    footer = dashboardFooter(
        left_text = "Jorge Cabral",
        right_text = "University of Aveiro, 2020"
    ) # end footer 
    
) # end dashboardPage


server = function(input, output) {
    
    output$map <- renderLeaflet({
        
        leaflet() %>% 
            addProviderTiles(providers$Esri.WorldImagery) %>%
            addMiniMap(tiles = providers$Esri.WorldStreetMap,
                       toggleDisplay = TRUE) %>%
            addMeasure(
                position = "bottomleft",
                primaryLengthUnit = "meters",
                primaryAreaUnit = "sqmeters") %>%
            addMarkers(lng=-8.6572794, lat=40.6231332, popup="3R") %>%
            addMarkers(lng=-8.6583321, lat=40.6303413, popup="CIDMA")
        
    })
    
  observeEvent(input$refresh, {  
  
    VectorPatient <- reactive({
        FuncOptAllOriginalData_3_13(c(as.numeric(1),
                                      as.numeric(input$Gender),
                                      input$PY,
                                      input$SmokSt,
                                      as.numeric(input$LTOT),
                                      input$BMI,
                                      input$mMRC,
                                      input$SixMWT,
                                      input$CAT,
                                      input$QMS))
    })
    
     MatrixDist <- reactive({
    
         for (j in 1:(sum(App_Data_Norm$group2==1))) {
             M_dis_100_LR3_Norm<-as.data.frame(matrix(nrow = 1,ncol = sum(App_Data_Norm$group2==1)))
             M_dis_100_LR3_Norm[1,j] <- distance(rbind(c((VectorPatient()[1]-min_f1)/(max_f1-min_f1),
                                                         (VectorPatient()[2]-min_f2)/(max_f2-min_f2),
                                                         (VectorPatient()[3]-min_f3)/(max_f3-min_f3)),b[j,]),
                                                 test.na = FALSE)}
         round(mean(as.numeric(M_dis_100_LR3_Norm[1,])),2)
         })
    
  
     
    #### Paretof1f2 #### 
    output$PlotParetof1f2 = renderPlotly({
    
        Dataf1f2f3 <- App_Data
        Dataf1f2f3[nrow(Dataf1f2f3)+1,]<-c(VectorPatient()[1],
                                           VectorPatient()[2],
                                           VectorPatient()[3],
                                           "Pareto 100% - Positive",
                                           4)
        Dataf1f2f3$f1<-as.numeric(Dataf1f2f3$f1)
        Dataf1f2f3$f2<-as.numeric(Dataf1f2f3$f2)
        Dataf1f2f3$f3<-as.numeric(Dataf1f2f3$f3)
        
        dat <- data.frame(x = numeric(0), y = numeric(0))
        
        withProgress(message = 'Making plot', value = 0, {
          # Number of times we'll go through the loop
          n <- 10
          
          for (i in 1:n) {
            # Each time through the loop, add another row of data. This is
            # a stand-in for a long-running computation.
            dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
            
            # Increment the progress bar, and update the detail text.
            incProgress(1/n, detail = paste("Doing part", i))
            
            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
          }
        })
    
            
        plot_ly(data = Dataf1f2f3,
                x = ~f1,
                y = ~f2,
                type = 'scatter',
                mode = 'markers',
                color = ~factor(group2),
                colors = c(rgb(input$Red/256,input$Green/256,input$Blue/256,1),"darkgreen","green","red"),
                marker = list(size = 3.5+input$Size/2)) %>%
                layout(showlegend=FALSE)
    })
     
    
     #### Paretof1f3 ####
   output$PlotParetof1f3 = renderPlotly({
        
        Dataf1f2f3 <- App_Data
        Dataf1f2f3[nrow(Dataf1f2f3)+1,]<-c(VectorPatient()[1],
                                           VectorPatient()[2],
                                           VectorPatient()[3],
                                           "Pareto 100% - Positive",
                                           4)
        Dataf1f2f3$f1<-as.numeric(Dataf1f2f3$f1)
        Dataf1f2f3$f2<-as.numeric(Dataf1f2f3$f2)
        Dataf1f2f3$f3<-as.numeric(Dataf1f2f3$f3)
        
        dat <- data.frame(x = numeric(0), y = numeric(0))
        
        withProgress(message = 'Making plot', value = 0, {
          # Number of times we'll go through the loop
          n <- 10
          
          for (i in 1:n) {
            # Each time through the loop, add another row of data. This is
            # a stand-in for a long-running computation.
            dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
            
            # Increment the progress bar, and update the detail text.
            incProgress(1/n, detail = paste("Doing part", i))
            
            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
          }
        })
        
        plot_ly(data = Dataf1f2f3,
                x = ~f1,
                y = ~f3,
                type = 'scatter',
                mode = 'markers',
                color = ~factor(group2),
                colors = c(rgb(input$Red/256,input$Green/256,input$Blue/256,1),"darkgreen","green","red"),
                marker = list(size = 3.5+input$Size/2)) %>%
            layout(showlegend=FALSE)
    })
    
    #### Paretof2f3 ####
    output$PlotParetof2f3 = renderPlotly({
        
        Dataf1f2f3 <- App_Data
        Dataf1f2f3[nrow(Dataf1f2f3)+1,]<-c(VectorPatient()[1],
                                           VectorPatient()[2],
                                           VectorPatient()[3],
                                           "Pareto 100% - Positive",
                                           4)
        Dataf1f2f3$f1<-as.numeric(Dataf1f2f3$f1)
        Dataf1f2f3$f2<-as.numeric(Dataf1f2f3$f2)
        Dataf1f2f3$f3<-as.numeric(Dataf1f2f3$f3)
        
        dat <- data.frame(x = numeric(0), y = numeric(0))
        
        withProgress(message = 'Making plot', value = 0, {
          # Number of times we'll go through the loop
          n <- 10
          
          for (i in 1:n) {
            # Each time through the loop, add another row of data. This is
            # a stand-in for a long-running computation.
            dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
            
            # Increment the progress bar, and update the detail text.
            incProgress(1/n, detail = paste("Doing part", i))
            
            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
          }
        })
        
        
        plot_ly(data = Dataf1f2f3,
                x = ~f2,
                y = ~f3,
                type = 'scatter',
                mode = 'markers',
                color = ~factor(group2),
                colors = c(rgb(input$Red/256,input$Green/256,input$Blue/256,1),"darkgreen","green","red"),
                marker = list(size =  3.5+input$Size/2)) %>%
            layout(showlegend=FALSE)
    })
      
     #### Pareto ####
     output$PlotPareto = renderPlotly({
        
    min_Pareto<-App_Data_Norm[App_Data_Norm$group2==1,][which.min(long_dis_P$Distance[long_dis_P$Type=="Pareto"]),]
    min_Pareto$group2<-3
    df_Pareto<-rbind(App_Data_Norm[App_Data_Norm$group2==1,],
                     c((VectorPatient()[1]-min_f1)/(max_f1-min_f1),
                       (VectorPatient()[2]-min_f2)/(max_f2-min_f2),
                       (VectorPatient()[3]-min_f3)/(max_f3-min_f3),
                       "Pareto 100% - Positive",
                       2),min_Pareto)
    App_Data_Norm[App_Data_Norm$group2==1,][which.min(long_dis_P$Distance[long_dis_P$Type=="Pareto"]),]
    
    # View(df_Pareto)<-rbind(App_Data_Norm[App_Data_Norm$group2==1,],
    #                  c((1-min_f1)/(max_f1-min_f1),
    #                    (1-min_f2)/(max_f2-min_f2),
    #                    (1-min_f3)/(max_f3-min_f3),
    #                    "Pareto 100% - Positive",
    #                    2))
    # df_Pareto$f1<-data.frame(df_Pareto)
    # df_Pareto$f1=-as.numeric(df_Pareto$f1)
    # df_Pareto$f2=-as.numeirc(df_Pareto$f2)
    # df_Pareto$f3=-as.numeric(df_Pareto$f3)
    
        fig_All_D_3_13_LR_Norm <- plot_ly(df_Pareto,
                                          x = ~f1,
                                          y = ~f2,
                                          z = ~f3,
                                          type = 'scatter3d',
                                          mode ="markers",
                                          marker = list(size =  2.5+input$Size/2)
                                          ,color = ~factor(group2),
                                          colors = c(rgb(input$Red/256,input$Green/256,input$Blue/256,1),"red","black")
        ) %>%
            layout(scene = list(xaxis = list(title = 'dif_mMRC',
                                             backgroundcolor="#E2E2E2",
                                             gridcolor="rgb(255,255,255)",
                                             showbackground=TRUE,
                                             zerolinecolor="rgb(255,0,255",
                                             showticklabels = FALSE),
                                yaxis = list(title = 'dif_QMS',
                                             backgroundcolor="#E2E2E2",
                                             gridcolor="rgb(255,255,255)",
                                             showbackground=TRUE,
                                             zerolinecolor="rgb(255,0,255",
                                             showticklabels = FALSE),
                                zaxis = list(title = 'dif_6MWT',
                                             backgroundcolor="#E2E2E2",
                                             gridcolor="rgb(255,255,255)",
                                             showbackground=TRUE,
                                             zerolinecolor="rgb(255,0,255",
                                             showticklabels = FALSE)))  %>%
            layout( showlegend=FALSE)  
        
        dat <- data.frame(x = numeric(0), y = numeric(0))
        
        withProgress(message = 'Making plot', value = 0, {
            # Number of times we'll go through the loop
            n <- 10
            
            for (i in 1:n) {
                # Each time through the loop, add another row of data. This is
                # a stand-in for a long-running computation.
                dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                
                # Increment the progress bar, and update the detail text.
                incProgress(1/n, detail = paste("Doing part", i))
                
                # Pause for 0.1 seconds to simulate a long computation.
                Sys.sleep(0.1)
            }
        })
        
        fig_All_D_3_13_LR_Norm
        
    })
        
        output$dmMRC <- renderValueBox({
            
            if (round(-VectorPatient()[1],2)>=1) {a1="thumbs-up"; b1="green"}
            if (round(-VectorPatient()[1],2)>=0 & round(-VectorPatient()[1],2)<1) {a1="hand-paper"; b1="yellow"}
            if (round(-VectorPatient()[1],2)<0) {a1="thumbs-down"; b1="red"}
            valueBox(round(-VectorPatient()[1],2), "mMRC", icon = icon(a1),
                     color = b1)
        })
        
        output$dQMS <- renderValueBox({
            
            if (round(-VectorPatient()[2],2)>=5.2) {a2="thumbs-up"; b2="green"}
            if (round(-VectorPatient()[2],2)>=0 & round(-VectorPatient()[2],2)<5.2) {a2="hand-paper"; b2="yellow"}
            if (round(-VectorPatient()[2],2)<0) {a2="thumbs-down"; b2="red"}
            valueBox(round(-VectorPatient()[2],2), "QMS", icon = icon(a2),
                     color = b2)
        })
        
        output$dSixMWT <- renderValueBox({
            
            if (round(-VectorPatient()[3],2)>=25) {a3="thumbs-up"; b3="green"}
            if (round(-VectorPatient()[3],2)>=0 & round(-VectorPatient()[3],2)<25) {a3="hand-paper"; b3="yellow"}
            if (round(-VectorPatient()[3],2)<0) {a3="thumbs-down"; b3="red"}
            valueBox(round(-VectorPatient()[3],2), "6MWT", icon = icon(a3),
                     color = b3)
        })
        
        
        output$distance <- renderValueBox({
            
            M_dis_100_LR3_Norm<-as.data.frame(matrix(nrow = 1,ncol = sum(App_Data_Norm$group2==1)))
            for (j in 1:(sum(App_Data_Norm$group2==1))) {
                M_dis_100_LR3_Norm[1,j] <- distance(rbind(c((VectorPatient()[1]-min_f1)/(max_f1-min_f1),
                                                            (VectorPatient()[2]-min_f2)/(max_f2-min_f2),
                                                            (VectorPatient()[3]-min_f3)/(max_f3-min_f3)),b[j,]),
                                                    test.na = FALSE)}
            
            M_dist<-round(mean(as.numeric(M_dis_100_LR3_Norm[1,])),2)
        
            if (M_dist<quantile(long_dis$Distance[long_dis$Type=="Predicted"],0.25)) {a1d="thumbs-up"; b1d="green"}
            if ((M_dist>=quantile(long_dis$Distance[long_dis$Type=="Predicted"],0.25)) & (M_dist<=quantile(long_dis$Distance[long_dis$Type=="Predicted"],0.75))) {a1d="hand-paper"; b1d="yellow"}
            if (M_dist>quantile(long_dis$Distance[long_dis$Type=="Predicted"],0.75)) {a1d="thumbs-down"; b1d="red"}
            
            #M_dist<-1.6
            quantil_new_data<-data.frame(all=c(long_dis$Distance[long_dis$Type=="Predicted"],M_dist))
            quantil_new_data<-quantil_new_data$all[order(quantil_new_data$all)]
            Plotquantil<-100*which(quantil_new_data==M_dist)/96
            
            output$dist_Perc <- renderValueBox({
              
              if (Plotquantil<25) {a1p="thumbs-up"; b1p="green"}
              if (Plotquantil<75 & Plotquantil>=25) {a1p="hand-paper"; b1p="yellow"}
              if (Plotquantil>=75) {a1p="thumbs-down"; b1p="red"}
              
              valueBox(round(Plotquantil,2), "Percentile", icon = icon(a1p),
                       color = b1p)
            }) 
            
            output$DistHist = renderPlot({
                
                vline<-data.frame(Median=c(median(long_dis_P$Distance[long_dis_P$Type=="Predicted"]),
                                           median(long_dis_P$Distance[long_dis_P$Type=="Observed"]),
                                           median(long_dis_P$Distance[long_dis_P$Type=="Pareto"])),
                                  Type=c("Predicted","Observed","Pareto"))
                
                if (input$Observed & input$Pareto){ 
                    dataHist<-long_dis_P
                    palHist<-c("darkblue","darkred","darkgreen")} else {if(input$Observed){
                        dataHist<-long_dis_P[long_dis_P$Type %in% c("Predicted","Observed"),]
                        palHist<-c("darkblue","darkgreen")} else {if (input$Pareto) {
                            dataHist<-long_dis_P[long_dis_P$Type %in% c("Predicted","Pareto"),]
                            palHist<-c("darkred","darkgreen")} else {
                                dataHist<-long_dis_P[long_dis_P$Type %in% c("Predicted"),]
                                palHist<-c("darkgreen")
                            }    
                        }
                    }    
                    
                    
                
                
                if (input$CountHist) {
                phist <- gghistogram(
                        dataHist, x = "Distance", #y = "..density..",
                        add = "median", rug = TRUE,
                        size=1,
                        fill = "Type",  bins=input$binsPred, palette = palHist,
                        font.axis = list(size = 15, color = "black"),) +
                        theme(text = element_text(size=15)) +
                        geom_vline(xintercept = M_dist, linetype="dotted",color = b1d, size=1.3)} else {
                            phist <- gghistogram(
                                dataHist, x = "Distance", y = "..density..",
                                add = "median", rug = TRUE,
                                size=1,
                                fill = "Type",  bins=input$binsPred, palette = palHist,
                                font.axis = list(size = 15, color = "black"),) +
                                theme(text = element_text(size=15)) +
                                geom_vline(xintercept = M_dist, linetype="dotted",color = b1d, size=1.3)
                        }
                    
                        
                # 2. Create the density plot with y-axis on the right
                # Remove x axis elements
                pdensity <- ggdensity(
                    dataHist, x = "Distance", 
                    color= "Type", palette = palHist,
                    alpha = 0,
                    size=1.4
                ) +
                    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), position = "right")  +
                    theme_half_open(15, rel_small = 1) +
                    rremove("x.axis")+
                    rremove("xlab") +
                    rremove("x.text") +
                    rremove("x.ticks") +
                    rremove("legend")
                
                # 3. Align the two plots and then overlay them.
                aligned_plots <- align_plots(phist, pdensity, align="hv", axis="tblr")
                
                
                
                dat <- data.frame(x = numeric(0), y = numeric(0))
                
                withProgress(message = 'Making plot', value = 0, {
                  # Number of times we'll go through the loop
                  n <- 10
                  
                  for (i in 1:n) {
                    # Each time through the loop, add another row of data. This is
                    # a stand-in for a long-running computation.
                    dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                    
                    # Increment the progress bar, and update the detail text.
                    incProgress(1/n, detail = paste("Doing part", i))
                    
                    # Pause for 0.1 seconds to simulate a long computation.
                    Sys.sleep(0.1)
                  }
                })
                
                if (input$DensityHist) { 
                ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]]) } else {
                    phist}
                
                
            })
            
            
                
            valueBox(M_dist, "Distance", icon = icon(a1d),
                      color = b1d)
        })
        
  })
         ####  dmMRCf1 ####
        output$dmMRCf1 <- renderValueBox({
            
            f1<- 2.2477-0.6646*as.numeric(input$PRf1)-0.0035*as.numeric(input$PYf1)+0.4992*as.numeric(input$LTOTf1)-0.6450*as.numeric(input$mMRCf1)-0.0028*as.numeric(input$SixMWTf1)+0.0184*as.numeric(input$CATf1)
            
            df_pred_mMRC<-data.frame(Group=factor(as.numeric(input$PRf1)+1,levels = c(1,2)),
                       Pack_Years=as.numeric(input$PYf1),
                       LTOT_Baseline=factor(as.numeric(input$LTOTf1)+1,levels=c(1,2)),
                       mMRC=as.numeric(input$mMRCf1),
                       SixMWT_Baseline=as.numeric(input$SixMWTf1),
                       CAT_total_Baseline=as.numeric(input$CATf1))



            pred_one_mMRC=-predict(m01_step,newdata =df_pred_mMRC,
                              interval = input$IntervalTypemMRC)

            if (round(-f1,2)>=input$MCIDmMRC) {a1f1t="thumbs-up"; b1f1t="green";b1f1g="darkgreen" }
            if (round(-f1,2)<input$MCIDmMRC & round(-f1,2)>=0) {a1f1t="hand-paper"; b1f1t="yellow";b1f1g="gold" }
            if (round(-f1,2)<0) {a1f1t="thumbs-down"; b1f1t="red"; b1f1g="darkred" }
            
            
            
            output$donutf1 <- renderPlot({
              
              
              dfBarChartmMRC <- data.frame(varmMRC=c("mMRC","mMRC"),
                                           conditionmMRC = c("A" , "B"),
                                           valuemMRC = c(round((round(-f1,2)+2.52)/(3.42+2.52),2),
                                                         round(1-(round(-f1,2)+2.52)/(3.42+2.52),2)))
              
              
              
              if (input$PTypef1==1) {
                # min -2.52 max 3.42
                ggplot(dfBarChartmMRC, aes(fill=conditionmMRC, y=valuemMRC, x=varmMRC)) + 
                  geom_bar(position="fill", stat="identity",fill = c(b1f1g,"white")) + 
                  geom_hline(yintercept = round((round(input$MCIDmMRC,2)+2.52)/(3.42+2.52),2),
                             size=1.5,
                             ?linetype="solid") +
                  geom_hline(yintercept = round((round(0,2)+2.52)/(3.42+2.52),2),
                             size=1.5,
                             linetype="dotted") +
                  geom_hline(yintercept = round((round(-min(App_Data$f1[App_Data$group2!=1]),2)+2.52)/(3.42+2.52),2),
                             size=1.5,
                             linetype="dashed") +
                  geom_hline(yintercept = round((round(-max(App_Data$f1[App_Data$group2!=1]),2)+2.52)/(3.42+2.52),2),
                             size=1.5,
                             linetype="dashed") +
                  theme(legend.position = "none",
                        axis.title.x=element_blank(),
                        axis.text.x=element_blank(),
                        axis.ticks.x=element_blank(),
                        axis.text.y=element_text(size=14+input$Size),
                        axis.title.y=element_blank())
                
              } else {
                
                
                ggdonutchart(dfBarChartmMRC, "valuemMRC", label = "valuemMRC",
                             fill = "conditionmMRC", color = "white",
                             palette = c(b1f1g, "grey95"),ggtheme = theme_minimal() ) +
                  theme(legend.position = "none",
                        axis.text.x=element_text(size=12+input$Size))
               
              }
              
            })
            
            
            output$HistmMRC <- renderPlot({

              
              df<-data.frame(f1=-App_Data$f1[App_Data$group2!=1])
              quantil_new_data_mMRC<-data.frame(rbind(df,-f1))
              quantil_new_data_mMRC<-quantil_new_data_mMRC$f1[order(quantil_new_data_mMRC$f1)]
              PlotquantilmMRC<-100*mean(which(quantil_new_data_mMRC==-f1))/96
              
              output$dmMRCf1_Perc <- renderValueBox({
                
                if (PlotquantilmMRC>=75) {a1f1="thumbs-up"; b1f1="green"}
                if (PlotquantilmMRC<75 & PlotquantilmMRC>=25) {a1f1="hand-paper"; b1f1="yellow"}
                if (PlotquantilmMRC<25) {a1f1="thumbs-down"; b1f1="red"}
                
                valueBox(round(PlotquantilmMRC,2), "Percentile", icon = icon(a1f1),
                         color = b1f1)
              })  
              
              #f1<--1
              #b1f1g<-"darkgreen"
              #PlotquantilmMRC<-23.2
              if (input$HistTypemMRC==1) {
              ggplot(df, aes(x=f1)) + 
                geom_histogram(aes(y=..density..),
                               bins = input$binsmMRC,
                               colour= rgb(input$Red/256,input$Green/256,input$Blue/256,1),#"mediumpurple4",
                               fill= rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),#"mediumpurple1",
                               size=1.5) +
                theme(axis.text=element_text(size=14+input$Size),
                      axis.title=element_blank()) + 
                geom_density(alpha=0,
                             fill="brown3",
                             size=1.5) +
                scale_x_continuous(limits=c(-5,6)) +
                geom_vline(xintercept = -f1,
                           colour = b1f1g,
                           size=2 ) +
                # geom_vline(xintercept = pred_one_mMRC[1],
                #            colour = b1f1g,
                #            size=2 ) +
                geom_vline(xintercept = pred_one_mMRC[2],
                           colour = b1f1g,
                           size=2,
                           linetype="dotted") +
                geom_vline(xintercept = pred_one_mMRC[3],
                           colour = b1f1g,
                           size=2,
                           linetype="dotted") +
                  geom_vline(xintercept = input$MCIDmMRC,
                             colour = "black",
                             size=1.5,
                             linetype="solid")

                } else {

                  
                  hcum <- h <- hist(df$f1,
                                    plot=FALSE,
                                    breaks = seq(min(df$f1), max(df$f1), length.out = input$binsmMRC + 1) )
                  hcum$counts <- cumsum(hcum$counts)/length(df$f1)*100
                  plot(hcum,
                       main="",
                       xlab = "",
                       ylab="",
                       xlim=c(-5,6),
                       ylim = c(0,100),
                       col=rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                       las=1,
                       cex.axis=1 + input$Size/8)
                  grid(col="grey")
                  plot(hcum,
                       add=T,
                       main="",
                       xlab = "",
                       xlim=c(-5,6),
                       ylim = c(0,100),
                       col=rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                       las=1,
                       cex.axis=1 + input$Size/8)
                  plot(h,
                       add=T,
                       col=rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                       xlim=c(-5,6),
                       ylim = c(0,100),
                       las=1,
                       cex.axis=1 + input$Size/8)
                  
                  ## Plot the density and cumulative density
                  d <- density(df$f1)
                  lines(x = d$x,
                        y = d$y *  diff(h$breaks)[1] * 100,
                        lwd = 2,
                        cex.axis=1 + input$Size)
                  lines(x = d$x,
                        y = cumsum(d$y)/max(cumsum(d$y)) * 100,
                        lwd = 2)    
                  abline(v=-f1,
                         lwd=3,
                         lty=1,
                         col=b1f1g)
                  abline(v=pred_one_mMRC[2],
                         lwd=3,
                         lty=2,
                         col=b1f1g)
                  abline(v=pred_one_mMRC[3],
                         lwd=3,
                         lty=2,
                         col=b1f1g)
                  abline(v=input$MCIDmMRC,
                         lwd=2,
                         lty=1,
                         col="black")
              
                  }
            })
            
            
            output$BoxPlotf1_PY <- renderPlot({
            
            bxp(list(stats=matrix(c(0,12.10,35.25,62,180),5,1),n=95),
                border = rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                #staplecol="red",
                #boxcol="red,
                boxfill=rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                las=1,
                lwd=2,
                cex.axis=1+input$Size/8)
            abline(h=input$PYf1,lwd=3, lty=3)
            })
              
            output$BoxPlotf1_6MWT <- renderPlot({
              
              bxp(list(stats=matrix(c(75,336.4,419.6,494.9,652),5,1),n=95),
                  border = rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                  #staplecol="red",
                  #boxcol="red,
                  boxfill=rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                  las=1,
                  lwd=2,
                  cex.axis=1 + input$Size/8)
              abline(h=input$SixMWTf1,lwd=3, lty=3)
            
            })
              
            
            output$BoxPlotf1_CAT <- renderPlot({
              
              bxp(list(stats=matrix(c(2,10,14,21,35),5,1),n=95),
                  border = rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                  #staplecol="red",
                  #boxcol="red,
                  boxfill=rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                  las=1,
                  lwd=2,
                  cex.axis=1 + input$Size/8)
              abline(h=input$CATf1,lwd=3, lty=3)
            })
            
            
            output$BoxPlotf1_mMRC <- renderPlot({
              
              if (input$mMRCf1==0) {
              barplot(c(9,36,21,26,3)/95,names = 0:4,ylim=c(0,0.4),
                      cex.axis=1 + input$Size/8,las=2,
                      cex.names=1+input$Size/8,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                                                                               rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                               rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                               rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                               rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))} else {
                if (input$mMRCf1==1) {barplot(c(9,36,21,26,3)/95,names = 0:4,ylim=c(0,0.4),
                                              cex.axis=1 + input$Size/8,las=2,
                                              cex.names=1+input$Size/8,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                       rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                                                                                                       rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                       rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                       rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))}
                  else {if (input$mMRCf1==2) {barplot(c(9,36,21,26,3)/95,ylim=c(0,0.4),
                                                      cex.axis=1 + input$Size/8,names = 0:4,las=2,
                                                      cex.names=1+input$Size/8,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                               rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                               rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                                                                                                               rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                               rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))}
                    else {if (input$mMRCf1==3) {barplot(c(9,36,21,26,3)/95,ylim=c(0,0.4),
                                                        cex.axis=1 + input$Size/8,names = 0:4,las=2,
                                                        cex.names =1+input$Size/8,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                 rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                 rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                 rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                                                                                                                 rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))}  
                        else {barplot(c(9,36,21,26,3)/95,ylim=c(0,0.4),
                                      cex.axis=1 + input$Size/8,names = 0:4,las=2,
                                      cex.names=1+input$Size/8,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                               rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                               rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                               rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                               rgb(input$Red/256,input$Green/256,input$Blue/256,1)))}}}}
            })
            
            
            output$BoxPlotf1_LTOT <- renderPlot({
              
              if (input$LTOTf1) {
                barplot(c(81,11)/95,
                        names = c("No","Yes"),
                        las=2,
                        ylim=c(0,1),
                        cex.axis=1 + input$Size/8,
                        cex.names=1+input$Size/8,
                        col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),rgb(input$Red/256,input$Green/256,input$Blue/256,1)))
                } else {
                  barplot(c(81,11)/95,
                          names = c("No","Yes"),
                          las=2,
                          ylim=c(0,1),
                          cex.axis=1 + input$Size/8,
                          cex.names=1+input$Size/8,
                          col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,1),rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))
                  }
            })
            
            
            
            valueBox(round(-f1,2), "dmMRC", icon = icon(a1f1t),
                     color = b1f1t)
        })
        
        
        
        ####  dQMSf2 ####
        
        output$dQMSf2 <- renderValueBox({
            
            if (as.numeric(input$SmokStf2)==1) {b6f2=0
                                                b7f2=0} else {if (as.numeric(input$SmokStf2)==2){b6f2=1;b7f2=0} else {b6f2=0;b7f2=1}}
            
           f2<- 0.5405 + 4.9981*as.numeric(input$PRf2) + 6.1571*b6f2 + 4.9780*b7f2 + 0.3736*as.numeric(input$BMIf2) - 1.5448*as.numeric(input$mMRCf2) + 0.0169*as.numeric(input$SixMWTf2) - 0.6241*as.numeric(input$QMSf2)  
                
           df_pred_QMS<-data.frame(Group=factor(as.numeric(input$PRf2)+1,levels = c(1,2)),
                                   Smoking_Status=factor(as.numeric(input$SmokStf2),levels = c(1,2,3)),
                                   BMI_Baseline=as.numeric(input$BMIf2),
                                   mMRC=as.numeric(input$mMRCf2),
                                   SixMWT_Baseline=as.numeric(input$SixMWTf2),
                                   HHD_QMS_Baseline=as.numeric(input$QMSf2))
           
           
           
           pred_one_QMS=predict(m04_step,newdata = df_pred_QMS,
                                  interval = input$IntervalTypeQMS)
           
           
            if (round(f2,2)>=input$MCIDQMS) {a2f2t="thumbs-up"; b2f2t="green";b2f2g="darkgreen" }
            if (round(f2,2)<input$MCIDQMS & round(f2,2)>=0) {a2f2t="hand-paper"; b2f2t="yellow";b2f2g="gold" }
            if (round(f2,2)<0) {a2f2t="thumbs-down"; b2f2t="red"; b2f2g="darkred" }
                        

            output$donutf2 <- renderPlot({
              
              dfBarChartQMS <- data.frame(varQMS=c("QMS","QMS"),
                                           conditionQMS = c("A" , "B"),
                                           valueQMS = c(round((round(f2,2)+27.5)/(28.69+27.5),2),
                                                         round(1-(round(f2,2)+27.5)/(28.69+27.5),2)))
              
              
              if (input$PTypef2==1) {
                # min -27.5 max 28.69
                ggplot(dfBarChartQMS, aes(fill=conditionQMS, y=valueQMS, x=varQMS)) + 
                  geom_bar(position="fill", stat="identity",fill = c(b2f2g,"white")) + 
                  geom_hline(yintercept = round((round(input$MCIDQMS,2)+27.5)/(28.69+27.5),2),
                             size=1.5,
                             ?linetype="solid") +
                  geom_hline(yintercept = round((round(0,2)+27.5)/(28.69+27.5),2),
                             size=1.5,
                             linetype="dotted") +
                  geom_hline(yintercept = round((round(-min(App_Data$f2[App_Data$group2!=1]),2)+27.5)/(28.69+27.5),2),
                             size=1.5,
                             linetype="dashed") +
                  geom_hline(yintercept = round((round(-max(App_Data$f2[App_Data$group2!=1]),2)+27.5)/(28.69+27.5),2),
                             size=1.5,
                             linetype="dashed") +
                  theme(legend.position = "none",
                        axis.title.x=element_blank(),
                        axis.text.x=element_blank(),
                        axis.ticks.x=element_blank(),
                        axis.text.y=element_text(size=14+input$Size),
                        axis.title.y=element_blank())
                
              } else {
                
                
                ggdonutchart(dfBarChartQMS, "valueQMS", label = "valueQMS",
                             fill = "conditionQMS", color = "white",
                             palette = c(b2f2g, "grey95"),ggtheme = theme_minimal() ) +
                  theme(legend.position = "none",
                        axis.text.x=element_text(size=12+input$Size))
                
              }
              
              
            })
            
            
            output$HistQMS <- renderPlot({
              
    
              
              df_QMS<-data.frame(f2=-App_Data$f2[App_Data$group2!=1])
              quantil_new_data_QMS<-data.frame(rbind(df_QMS,f2))
              quantil_new_data_QMS<-quantil_new_data_QMS$f2[order(quantil_new_data_QMS$f2)]
              PlotquantilQMS<-100*mean(which(quantil_new_data_QMS==f2))/96
              
              output$dQMSf2_Perc <- renderValueBox({
                
                if (PlotquantilQMS>=75) {a2f2="thumbs-up"; b2f2="green"}
                if (PlotquantilQMS<75 & PlotquantilQMS>=25) {a2f2="hand-paper"; b2f2="yellow"}
                if (PlotquantilQMS<25) {a2f2="thumbs-down"; b2f2="red"}
                
                valueBox(round(PlotquantilQMS,2), "Percentile", icon = icon(a2f2),
                         color = b2f2)
              })
              
              #f2<--1
              #b2f2g<-"darkgreen"
              #PlotquantilQMS<-23.2
              if (input$HistTypeQMS==1) {
                ggplot(df_QMS, aes(x=f2)) + 
                  geom_histogram(aes(y=..density..),
                                 bins = input$binsQMS,
                                 colour=rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                                 fill=rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                 size=1.5)+
                      theme(axis.text=element_text(size=14+input$Size),
                            axis.title=element_blank()) +  
                  geom_density(alpha=0,
                               fill="brown3",
                               size=1.5) +
                  scale_x_continuous(limits=c(-50,50)) +
                  geom_vline(xintercept = f2,
                             colour = b2f2g,
                             size=2 ) +
                geom_vline(xintercept = pred_one_QMS[2],
                           colour = b2f2g,
                           size=2,
                           linetype="dotted") +
                  geom_vline(xintercept = pred_one_QMS[3],
                             colour = b2f2g,
                             size=2,
                             linetype="dotted") +
                  geom_vline(xintercept = input$MCIDQMS,
                             colour = "black",
                             size=1.5,
                             linetype="solid")
                  
                
              } else {
                
                
                hcum_QMS <- h_QMS <- hist(df_QMS$f2,
                                  plot=FALSE,
                                  breaks = seq(min(df_QMS$f2), max(df_QMS$f2), length.out = input$binsQMS + 1) )
                hcum_QMS$counts <- cumsum(hcum_QMS$counts)/length(df_QMS$f2)*100
                plot(hcum_QMS,
                     main="",
                     xlab = "",
                     ylab="",
                     xlim=c(-50,50),
                     ylim = c(0,100),
                     col=rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                     las=1,
                     cex.axis=1 + input$Size/8)
                grid(col="grey")
                plot(hcum_QMS,
                     add=T,
                     main="",
                     xlab = "",
                     xlim=c(-50,50),
                     ylim = c(0,100),
                     col=rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                     las=1)
                plot(h_QMS,
                     add=T,
                     col=rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                     xlim=c(-50,50),
                     ylim = c(0,100),
                     las=1,
                     cex.axis=1 + input$Size/8)
                
                ## Plot the density and cumulative density
                d_QMS <- density(df_QMS$f2)
                lines(x = d_QMS$x,
                      y = d_QMS$y * 100 * diff(h_QMS$breaks)[1],
                      lwd = 2,
                      cex.axis=1 + input$Size/8)
                lines(x = d_QMS$x,
                      y = cumsum(d_QMS$y)/max(cumsum(d_QMS$y)) * 100,
                      lwd = 2)    
                abline(v=f2,
                       lwd=3,
                       lty=1,
                       col=b2f2g)
                abline(v=pred_one_QMS[2],
                       lwd=3,
                       lty=2,
                       col=b2f2g)
                abline(v=pred_one_QMS[3],
                       lwd=3,
                       lty=2,
                       col=b2f2g)
                abline(v=input$MCIDQMS,
                       lwd=2,
                       lty=1,
                       col="black")
                
                
              }
            })
            
            
            output$BoxPlotf2_BMI <- renderPlot({
              
              bxp(list(stats=matrix(c(17.63,23.44,25.97,29.23,38.24),5,1),n=95),
                  border = rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                  #staplecol="red",
                  #boxcol="red,
                  boxfill=rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                  las=1,
                  lwd=2,
                  cex.axis=1+input$Size/8)
              abline(h=input$BMIf2,lwd=3, lty=3)
            })
            
            output$BoxPlotf2_6MWT <- renderPlot({
              
              bxp(list(stats=matrix(c(75,336.4,419.6,494.9,652),5,1),n=95),
                  border = rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                  #staplecol="red",
                  #boxcol="red,
                  boxfill=rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                  las=1,
                  lwd=2,
                  cex.axis=1+input$Size/8)
              abline(h=input$SixMWTf2,lwd=3, lty=3)
            })
            
            
            output$BoxPlotf2_QMS <- renderPlot({
              
              bxp(list(stats=matrix(c(13.3,26.4,31.2,36.75,55.6),5,1),n=95),
                  border = rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                  #staplecol="red",
                  #boxcol="red,
                  boxfill=rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                  las=1,
                  lwd=2,
                  cex.axis=1+input$Size/8)
              abline(h=input$QMSf2,lwd=3, lty=3)
            })
            
            
            output$BoxPlotf2_mMRC <- renderPlot({
              
              if (input$mMRCf2==0) {
                barplot(c(9,36,21,26,3)/95,
                        cex.names=1+input$Size/8,
                        cex.axis=1+input$Size/8,names = 0:4,ylim=c(0,0.4),las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                                                                                 rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                 rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                 rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                 rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))} else {
                                                                                   if (input$mMRCf2==1) {barplot(c(9,36,21,26,3)/95,cex.axis=1+input$Size/8,
                                                                                                                 cex.names=1+input$Size/8,names = 0:4,ylim=c(0,0.4),las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                                          rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                                                                                                                                                                          rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                                          rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                                          rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))}
                                                                                   else {if (input$mMRCf2==2) {barplot(c(9,36,21,26,3)/95,cex.axis=1+input$Size/8,
                                                                                                                       cex.names=1+input$Size/8,ylim=c(0,0.4),names = 0:4,las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                                                rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                                                rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                                                                                                                                                                                rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                                                rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))}
                                                                                     else {if (input$mMRCf2==3) {barplot(c(9,36,21,26,3)/95,cex.axis=1+input$Size/8,
                                                                                                                         cex.names=1+input$Size/8,ylim=c(0,0.4),names = 0:4,las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                                                  rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                                                  rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                                                  rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                                                                                                                                                                                  rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))}  
                                                                                       else {barplot(c(9,36,21,26,3)/95,cex.axis=1+input$Size/8,
                                                                                                     cex.names=1+input$Size/8,ylim=c(0,0.4),names = 0:4,las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                              rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                              rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                              rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                              rgb(input$Red/256,input$Green/256,input$Blue/256,1)))}}}}
            })
            
            
            output$BoxPlotf2_SmokSt <- renderPlot({
              
              if (input$SmokStf2==1) {
                barplot(c(15,64,16)/95,cex.axis=1+input$Size/8,
                        cex.names=1+input$Size/8,ylim=c(0,0.8),names = c("Non-Smoker","Ex-Smoker","Smoker"),las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                                                                                                              rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                              rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))} else {
                  if (input$SmokStf2==2) {barplot(c(15,64,16)/95,cex.axis=1+input$Size/8,
                                                  cex.names=1+input$Size/8,ylim=c(0,0.8),names = c("Non-Smoker","Ex-Smoker","Smoker"),las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                        rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                                                                                                                                        rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))} else 
                                                                                                                                        {barplot(c(15,64,16)/95,ylim=c(0,0.8),cex.axis=1+input$Size/8,
                                                                                                                                                 cex.names=1+input$Size/8,names = c("Non-Smoker","Ex-Smoker","Smoker"),las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),rgb(input$Red/256,input$Green/256,input$Blue/256,1))) }}
            })
            
            
            valueBox(round(f2,2), "dQMS", icon = icon(a2f2t),
                     color = b2f2t)
        })
    
        ####  d6MWTf3 ####
        output$d6MWTf3 <- renderValueBox({

            if (as.numeric(input$SmokStf3)==1) {b6f3=0;b7f3=0} else {if (as.numeric(input$SmokStf3)==2){b6f3=1;b7f3=0} else {b6f3=0;b7f3=1}}
            
            f3<--81.2005-46.3837*as.numeric(input$PRf3)-23.4841*as.numeric(input$Genderf3)+0.2637*as.numeric(input$PYf3)-59.0927*b6f3-24.2149*b7f3+33.5306*as.numeric(input$LTOTf3)+11.5711*as.numeric(input$mMRCf3)+0.2047*as.numeric(input$SixMWTf3)
        
                
            df_pred_6MWT<-data.frame(Group=factor(as.numeric(input$PRf3)+1,levels = c(1,2)),
                                     Sex=factor(as.numeric(input$Genderf3)+1,levels = c(1,2)),
                                     Pack_Years=as.numeric(input$PYf3),
                                     Smoking_Status=factor(as.numeric(input$SmokStf3),levels = c(1,2,3)),
                                     LTOT_Baseline=factor(as.numeric(input$LTOTf3)+1,levels=c(1,2)),
                                     mMRC=as.numeric(input$mMRCf3),
                                     SixMWT_Baseline=as.numeric(input$SixMWTf3))
            
            
            
            pred_one_6MWT=predict(m03_step,newdata = df_pred_6MWT,
                                 interval = input$IntervalType6MWT)
            
            
            if (round(-f3,2)>=input$MCID6MWT) {a3f3t="thumbs-up"; b3f3t="green"; b3f3g="darkgreen"}
            if (round(-f3,2)<input$MCID6MWT & round(-f3,2)>=0) {a3f3t="hand-paper"; b3f3t="yellow"; b3f3g="gold"}
            if (round(-f3,2)<0) {a3f3t="thumbs-down"; b3f3t="red"; b3f3g="darkred"}
            
            output$donutf3 <- renderPlot({
            
              
              dfBarChart6MWT <- data.frame(var6MWT=c("6MWT","6MWT"),
                                           condition6MWT = c("A" , "B"),
                                           value6MWT = c(round((round(-f3,2)+133.16)/(194.81+133.16),2),
                                                         round(1-(round(-f3,2)+133.16)/(194.81+133.16),2)))
              
              
              if (input$PTypef3==1) {
                # min -2.52 max 3.42
                ggplot(dfBarChart6MWT, aes(fill=condition6MWT, y=value6MWT, x=var6MWT)) + 
                  geom_bar(position="fill", stat="identity",fill = c(b3f3g,"white")) + 
                  geom_hline(yintercept = round((round(input$MCID6MWT,2)+133.16)/(194.81+133.16),2),
                             size=1.5,
                             ?linetype="solid") +
                  geom_hline(yintercept = round((round(0,2)+133.16)/(194.81+133.16),2),
                             size=1.5,
                             linetype="dotted") +
                  geom_hline(yintercept = round((round(-min(App_Data$f3[App_Data$group2!=1]),2)+133.16)/(194.81+133.16),2),
                             size=1.5,
                             linetype="dashed") +
                  geom_hline(yintercept = round((round(-max(App_Data$f3[App_Data$group2!=1]),2)+133.16)/(194.81+133.16),2),
                             size=1.5,
                             linetype="dashed") +
                  theme(legend.position = "none",
                        axis.title.x=element_blank(),
                        axis.text.x=element_blank(),
                        axis.ticks.x=element_blank(),
                        axis.text.y=element_text(size=14+input$Size),
                        axis.title.y=element_blank())
                
              } else {
                
                
                ggdonutchart(dfBarChart6MWT, "value6MWT", label = "value6MWT",
                             fill = "condition6MWT", color = "white",
                             palette = c(b3f3g, "grey95"),ggtheme = theme_minimal() ) +
                  theme(legend.position = "none",
                        axis.text.x=element_text(size=12+input$Size))
                
              }
              
            })
            
            
            
            output$Hist6MWT <- renderPlot({
              
              
              df_6MWT<-data.frame(f3=-App_Data$f3[App_Data$group2!=1])
              quantil_new_data_6MWT<-data.frame(rbind(df_6MWT,-f3))
              quantil_new_data_6MWT<-quantil_new_data_6MWT$f3[order(quantil_new_data_6MWT$f3)]
              Plotquantil6MWT<-100*mean(which(quantil_new_data_6MWT==-f3))/96
              
              output$d6MWTf3_Perc <- renderValueBox({
                
                if (Plotquantil6MWT>=75) {a3f3="thumbs-up"; b3f3="green"}
                if (Plotquantil6MWT<75 & Plotquantil6MWT>=25) {a3f3="hand-paper"; b3f3="yellow"}
                if (Plotquantil6MWT<25) {a3f3="thumbs-down"; b3f3="red"}
                
                valueBox(round(Plotquantil6MWT,2), "Percentile", icon = icon(a3f3),
                         color = b3f3)
              })
              
              
              if (input$HistType6MWT==1) {
                ggplot(df_6MWT, aes(x=f3)) + 
                  geom_histogram(aes(y=..density..),
                                 bins = input$bins6MWT,
                                 colour=rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                                 fill=rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                 size=1.5)+
                      theme(axis.text=element_text(size=14+input$Size),
                            axis.title=element_blank()) +  
                  geom_density(alpha=0,
                               fill="brown3",
                               size=1.5) +
                  scale_x_continuous(limits=c(-250,320)) +
                  geom_vline(xintercept = -f3,
                             colour = b3f3g,
                             size=2 ) +
                  geom_vline(xintercept = pred_one_6MWT[2],
                             colour = b3f3g,
                             size=2,
                             linetype="dotted") +
                  geom_vline(xintercept = pred_one_6MWT[3],
                             colour = b3f3g,
                             size=2,
                             linetype="dotted") +
                  geom_vline(xintercept = input$MCID6MWT,
                             colour = "black",
                             size=1.5,
                             linetype="solid")
                
              } else {
                
                
                hcum_6MWT <- h_6MWT <- hist(df_6MWT$f3,
                                          plot=FALSE,
                                          breaks = seq(min(df_6MWT$f3), max(df_6MWT$f3), length.out = input$bins6MWT + 1) )
                hcum_6MWT$counts <- cumsum(hcum_6MWT$counts)/length(df_6MWT$f3)*100
                plot(hcum_6MWT,
                     main="",
                     xlab = "",
                     ylab="",
                     xlim=c(-250,320),
                     ylim = c(0,100),
                     col=rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                     las=1,
                     cex.axis=1 + input$Size/8)
                grid(col="grey")
                plot(hcum_6MWT,
                     add=T,
                     main="",
                     xlab = "",
                     xlim=c(-250,320),
                     ylim = c(0,100),
                     col=rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                     las=1,
                     cex.axis=1 + input$Size/8)
                plot(h_6MWT,
                     add=T,
                     col=rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                     xlim=c(-250,320),
                     ylim = c(0,100),
                     las=1,
                     cex.axis=1 + input$Size/8)
                
                ## Plot the density and cumulative density
                d_6MWT <- density(df_6MWT$f3)
                lines(x = d_6MWT$x,
                      y = d_6MWT$y * 100 * diff(h_6MWT$breaks)[1],
                      lwd = 2)
                lines(x = d_6MWT$x,
                      y = cumsum(d_6MWT$y)/max(cumsum(d_6MWT$y)) * 100,
                      lwd = 2)    
                abline(v=-f3,
                       lwd=3,
                       lty=1,
                       col=b3f3g)
                abline(v=pred_one_6MWT[2],
                       lwd=3,
                       lty=2,
                       col=b3f3g)
                abline(v=pred_one_6MWT[3],
                       lwd=3,
                       lty=2,
                       col=b3f3g)
                abline(v=input$MCID6MWT,
                       lwd=2,
                       lty=1,
                       col="black")
                
              }
            })
            
            
            output$BoxPlotf3_LTOT <- renderPlot({
              
              if (input$LTOTf3) {
                barplot(c(81,11)/95,ylim=c(0,1),
                        cex.axis=1 + input$Size/8,
                        cex.names=1+input$Size/8,names = c("No","Yes"),las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),rgb(input$Red/256,input$Green/256,input$Blue/256,1)))} else {
                  barplot(c(81,11)/95,ylim=c(0,1),
                          cex.axis=1 + input$Size/8,
                          cex.names=1+input$Size/8,names = c("No","Yes"),las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,1),rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))}
            })
            
            
            output$BoxPlotf3_Gender <- renderPlot({
              
              if (input$Genderf3) {
                barplot(c(76,19)/95,ylim=c(0,1),
                        cex.axis=1 + input$Size/8,
                        cex.names=1+input$Size/8,names = c("Male","Female"),las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),rgb(input$Red/256,input$Green/256,input$Blue/256,1)))} else {
                  barplot(c(76,19)/95,ylim=c(0,1),
                          cex.axis=1 + input$Size/8,
                          cex.names=1+input$Size/8,names = c("Male","Female"),las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,1),rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))}
            })
            
            output$BoxPlotf3_6MWT <- renderPlot({
              
              bxp(list(stats=matrix(c(75,336.4,419.6,494.9,652),5,1),n=95),
                  border = rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                  #staplecol="red",
                  #boxcol="red,
                  boxfill=rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                  las=1,
                  lwd=2,
                  cex.axis=1+input$Size/8)
              abline(h=input$SixMWTf3,lwd=3, lty=3)
            })
            
            
            output$BoxPlotf3_PY <- renderPlot({
              
              bxp(list(stats=matrix(c(0,12.1,35.25,62,180),5,1),n=95),
                  border = rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                  #staplecol="red",
                  #boxcol="red,
                  boxfill=rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                  las=1,
                  lwd=2,
                  cex.axis=1+input$Size/8)
              abline(h=input$PYf3,lwd=3, lty=3)
            })
            
            
            output$BoxPlotf3_mMRC <- renderPlot({
              
              if (input$mMRCf3==0) {
                barplot(c(9,36,21,26,3)/95,
                        cex.axis=1 + input$Size/8,
                        cex.names=1+input$Size/8,names = 0:4,ylim=c(0,0.4),las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                                                                                 rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                 rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                 rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                 rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))} else {
                                                                                   if (input$mMRCf3==1) {barplot(c(9,36,21,26,3)/95,
                                                                                                                 cex.axis=1 + input$Size/8,
                                                                                                                 cex.names=1+input$Size/8,names = 0:4,ylim=c(0,0.4),las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                                          rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                                                                                                                                                                          rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                                          rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                                          rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))}
                                                                                   else {if (input$mMRCf3==2) {barplot(c(9,36,21,26,3)/95,
                                                                                                                       cex.axis=1 + input$Size/8,
                                                                                                                       cex.names=1+input$Size/8,ylim=c(0,0.4),names = 0:4,las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                                                rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                                                rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                                                                                                                                                                                rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                                                rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))}
                                                                                     else {if (input$mMRCf3==3) {barplot(c(9,36,21,26,3)/95,
                                                                                                                         cex.axis=1 + input$Size/8,
                                                                                                                         cex.names=1+input$Size/8,ylim=c(0,0.4),names = 0:4,las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                                                  rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                                                  rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                                                  rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                                                                                                                                                                                  rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))}  
                                                                                       else {barplot(c(9,36,21,26,3)/95,
                                                                                                     cex.axis=1 + input$Size/8,
                                                                                                     cex.names=1+input$Size/8,ylim=c(0,0.4),names = 0:4,las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                              rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                              rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                              rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                                              rgb(input$Red/256,input$Green/256,input$Blue/256,1)))}}}}
            })
            
            
            output$BoxPlotf3_SmokSt <- renderPlot({
              
              if (input$SmokStf3==1) {
                barplot(c(15,64,16)/95,
                        cex.axis=1 + input$Size/8,
                        cex.names=1+input$Size/8,ylim=c(0,0.8),names = c("Non-Smoker","Ex-Smoker","Smoker"),las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                                                                                                              rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                              rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))} else {
                  if (input$SmokStf3==2) {barplot(c(15,64,16)/95,
                                                  cex.axis=1 + input$Size/8,
                                                  cex.names=1+input$Size/8,ylim=c(0,0.8),names = c("Non-Smoker","Ex-Smoker","Smoker"),las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),
                                                                                                                                        rgb(input$Red/256,input$Green/256,input$Blue/256,1),
                                                                                                                                        rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency)))} else 
                                                                                                                                        {barplot(c(15,64,16)/95,
                                                                                                                                                 cex.axis=1 + input$Size/8,
                                                                                                                                                 cex.names=1+input$Size/8,ylim=c(0,0.8),names = c("Non-Smoker","Ex-Smoker","Smoker"),las=2,col=c(rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),rgb(input$Red/256,input$Green/256,input$Blue/256,input$Transparency),rgb(input$Red/256,input$Green/256,input$Blue/256,1))) }}
            })
            
            
      
            valueBox(round(-f3,2), "d6MWT", icon = icon(a3f3t),
                 color = b3f3t)
            
        })
        
        
        
}


# Run the application 
shinyApp(ui = ui, server = server)
