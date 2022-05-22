library(shiny)
library(ggExtra)
library(shinyWidgets)
library(shinyjqui)
library(markdown)
library(shinyBS)
library(shinythemes)
library(colourpicker)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(grid)
ui=tagList(
  shinythemes::themeSelector(),
  navbarPage(
    title = "shinylollipop",
   tabPanel("lollipop",
              sidebarPanel(width = 4,h5("Upload input data",
                        bsButton("bs00", label="", icon=icon("question"), style="info", size="extra-small"), 
                        bsPopover("bs00", 'Select Input data type', "The dataset should contain three columns , the first column is name , the second column and third column are value.", trigger = "focus")),
                        fileInput("Upload_data", label="",multiple = FALSE) ,
                    
                    
                checkboxInput("lollipoptitle", "Add plot title", FALSE) ,
                         
                   conditionalPanel(condition = "input.lollipoptitle",
                         textInput("lollipop_Title", "Plot  title:",value = c("plot")),
                         textInput("lollipop_xTitle","Plot x axis title:",value = c("X axis")),
                         textInput("lollipop_yTitle","Plot y axis title:",value = c("Y axis")),),
                
                
                checkboxInput("lollipopcolor", "Choose color", FALSE) ,
                
                 conditionalPanel(condition = "input.lollipopcolor",
                
                         radioButtons("lollipop_color","",
                                               choices = list("Default color" = "1", "Customize color" = "2"), selected="1"),
                 conditionalPanel(condition="input.lollipop_color=='1'"
                                  ),
                 conditionalPanel(condition="input.lollipop_color=='2'",
                         colourInput(inputId = "inputcolor1",label="The color of line ",value="#FFFFFF"),
                         colourInput(inputId = "inputcolor2",label="The color of point 1",value="#FFFFFF"),
                         colourInput(inputId = "inputcolor3",label="The color of point 2",value="#FFFFFF"),             
                 )),
                 
                checkboxInput("lollipopSize", "Adjust plot parameter", FALSE),
                 conditionalPanel(condition = "input.lollipopSize",
                         selectInput("line_Type",h5("Type of line:",bsButton("bs04", label = "", icon = icon("question"), style = "info", size = "extra-small"),
                                bsPopover("bs04",'The type of line ',"Change the type of connection line in the lollipop diagram. There are five types to choose", trigger = "focus")),
                                                      choices = list("solid"=1,
                                                                               "dashed"=2,
                                                                               "dotted"=3,
                                                                               "dotdash"=4,
                                                                               "longdash"=5,
                                                                               "twodash"=6)),
                         sliderInput("point_Size1", "The size of the second point:",
                                 min = 0, max = 8,
                                 value = 3,step = 0.5),
                         sliderInput("point_Size2", "The size of the first point:",
                                 min = 0, max = 8,
                                 value = 3,step = 0.5)),
                     
                 
                checkboxInput("downloadratio", "Adjust the image download ratio", FALSE),
                 conditionalPanel(condition = "input.downloadratio",
                        numericInput("lollipopHeight", "Plot download height", value="6"),
                        numericInput("lollipopWidth", "Plot download width", value="13")) ,
                 actionButton("submit1", strong("Submit!"), styleclass = "success", width='100%')) ,
                 
            
            mainPanel(
                   downloadButton("downloadlollipop.pdf", "Download PDF File"),
                   downloadButton("downloadlollipop.svg", "Download SVG File"),
                   br(),
                   br(),
                   jqui_resizable(plotOutput("lollipopplot", width = "100%", height = "400px")))
             ),
   
   tabPanel("Help",
                includeMarkdown("REDME.md"))
   
   )                                    
 )     
