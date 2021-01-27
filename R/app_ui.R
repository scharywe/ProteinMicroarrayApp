#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinyFiles EBImage
#' @importFrom shinythemes shinytheme
#' @importFrom DT DTOutput renderDT datatable
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      theme = shinytheme("yeti"),
      shinyjs::useShinyjs(),
      
      titlePanel("Protein Array Shiny App"),
      tags$style(type='text/css', "#stop { float:right; }"),
      actionButton("stop", "EXIT"), 
      tabsetPanel(id = "tabs", 
                  ## Start of Tab Image Editor
                  tabPanel("Image Editor", value = "tab1",
                           sidebarLayout(
                             sidebarPanel(
                               dateInput("testdate", label = "Date of test:", value = NULL),
                               radioButtons("upload", 
                                            label = ("Upload Image or Sample"), 
                                            choices = list("Upload Image" = 1, 
                                                           "Sample" = 2), 
                                            selected = 1),
                               conditionalPanel(
                                 condition = "input.upload == 1",
                                 fileInput(inputId = 'file1',
                                           label = 'Upload Image',
                                           placeholder = 'JPEG, PNG, and TIFF are supported',
                                           accept = c(
                                             "image/jpeg",
                                             "image/x-png",
                                             "image/tiff",
                                             ".jpg",
                                             ".png",
                                             ".tiff")
                                 )
                               ),
                               
                               uiOutput("rotatePanel"),
                               
                               sliderInput("hor", "Number of horizontal cells:", min = 10, max = 30,  value = 19),
                               sliderInput("ver", "Number of vertical cells:", min = 10, max = 30,  value = 19)
                             ), # END OF SIDEBAR PANEL
                             
                             mainPanel(
                               h3('Cropping and Segmentation', align = "center"),
                               plotOutput("plot1",
                                          click = "plot_click",
                                          dblclick = "plot_dblclick",
                                          hover = "plot_hover",
                                          brush = "plot_brush"),
                               column(6,
                                      actionButton("reset", label = "Reset"),
                                      tags$style(type='text/css', "#reset { display: block; width:30%; margin-left: auto; margin-right:auto;}"),
                               ),
                               column(6, shinyjs::disabled(
                                 actionButton("segmentation", label = "Apply Segmentation")),
                                 tags$style(type='text/css', "#segmentation { display: block; width:30%; margin-left: auto; margin-right:auto;}"),
                               )
                             ) # END OF MAIN PANEL
                           ) # END OF SIDEBAR LAYOUT
                  ), # END OF TAB PANEL
                  
                  ## Start of Tab Background Correction
                  tabPanel("Background Correction", value = "tab2",
                           sidebarLayout(
                             sidebarPanel(
                               radioButtons("colorImage",
                                            label = ("Color image?"),
                                            choices = list("No" = 1,
                                                           "Yes" = 2),
                                            selected = 1),
                               
                               conditionalPanel(
                                 condition = "input.colorImage == 2",
                                 radioButtons("channel",
                                              label = ("Conversion mode"),
                                              choices = list("luminance",
                                                             "gray",
                                                             "red",
                                                             "green",
                                                             "blue"),
                                              selected = "luminance")
                               ),
                               
                               checkboxInput("invert", "Invert colors", value=FALSE),
                               
                               radioButtons("thresh", 
                                            label = ("Threshold Method"), 
                                            choices = list("Otsu" = 1,
                                                           "Quantile" = 2
                                            ), 
                                            selected = 1),
                               
                               conditionalPanel(
                                 condition = "input.thresh == 2",
                                 numericInput(inputId = "quantile1",
                                              label = "Probability [%]:",
                                              value = 99,
                                              min = 0,
                                              max = 100,
                                              step = 0.1,
                                              width = NULL)
                               ),
                               
                               conditionalPanel(
                                 condition = "input.thresh == 3",
                                 numericInput(inputId = "tri_offset",
                                              label = "Offset:",
                                              value = 0.2,
                                              min = 0,
                                              max = 1,
                                              step = 0.01,
                                              width = NULL)
                               ),
                               
                               actionButton("threshold", label = "Apply Threshold"),
                               tags$style(type='text/css', "#threshold { display: block; width:30%;}"),
                               br(), br(),
                               actionButton("data", label = "Add To Data"),
                               tags$style(type='text/css', "#data { display: block; width:30%;}"),
                               br(), br(),
                               actionButton("showIntensData", label = "Show Intensity Data"),
                               tags$style(type='text/css', "#showIntensData { display: block; width:30%;}"),
                               br(), br(),
                             ), # END OF SIDEBAR PANEL
                             mainPanel(
                               HTML(
                                 paste(
                                   h3('Background Correction', align = "center"),
                                   verbatimTextOutput("thresh"),br(),
                                   h4('Signal Intensity Above Background and Signal after Background Correction', align = "center"),
                                   splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot3"), plotOutput("plot4")),
                                   verbatimTextOutput("meanIntens"),
                                   verbatimTextOutput("medianIntens"),
                                   '<br/>','<br/>'
                                 )
                               ),
                               width = 8
                             ) # END OF MAIN PANEL
                           ) # END OF SIDEBAR LAYOUT
                  ), # END OF TAB PANEL
                  ## Start of Tab Data
                  tabPanel("Intensity Data", value = "tab3",
                           sidebarLayout(
                             sidebarPanel(
                               actionButton("refreshData", label = "Refresh Data"),
                               tags$style(type='text/css', "#refreshData { display: block; width:30%;}"),
                               br(), br(),
                               downloadButton("downloadData", "Download Data"),
                               tags$style(type='text/css', "#downloadData { display: block; width:30%;}"),
                               br(), br(),
                               actionButton("deleteData", label = "Delete Data"),
                               tags$style(type='text/css', "#deleteData { display: block; width:30%;}"),
                               br(), br(),
                               
                             ),
                             mainPanel(
                               DTOutput("intens")
                             )# END OF MAIN PANEL
                           )# END OF SIDEBAR LAYOUT
                  ), # END OF TAB PANEL
                  ## Start of Tab Data
                  tabPanel("Timeseries", value = "tab4",
                           sidebarLayout(
                             sidebarPanel(
                               h5("You can also upload existing intensity data and go to 3)", style="font-weight:bold"),
                               fileInput("intensFile", "Select CSV file",
                                         multiple = FALSE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")), hr(style="border-color: black"),
                               actionButton("readIntensData", "Read Intensity Data"),
                               tags$style(type='text/css', "#read { display: block; width:30%;}"),
                               br(), br(),
                               
                               actionButton("plotme", "Plot timeseries"),
                               tags$style(type='text/css', "#plotme { display: block; width:30%;}"),
                               br(),br(),
                             ),
                             mainPanel(
                               uiOutput("subtabs")
                             )# END OF MAIN PANEL
                           )# END OF SIDEBAR LAYOUT
                  ),# END OF TAB PANEL
                  tabPanel("Timeseries plot", value = "plotly",
                           sidebarLayout(
                             sidebarPanel(
                               h3("Sample tab"),
                               p("Here goes all inputs, sliders etc.")
                             ),
                             mainPanel(
                               h3("Plot Output"),
                               plotly::plotlyOutput("plotly")
                             )# END OF MAIN PANEL
                           )# END OF SIDEBAR LAYOUT
                  )# END OF TAB PANEL
      )# END OF TAB SET PANEL
    ) # END OF UI
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'ProteinMicroarrayApp'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}
