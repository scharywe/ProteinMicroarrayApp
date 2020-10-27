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
                               radioButtons("radio", 
                                            label = ("Upload Image or Sample"), 
                                            choices = list("Upload Image" = 1, 
                                                           "Sample" = 2), 
                                            selected = 1),
                               conditionalPanel(
                                 condition = "input.radio == 1",
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
                               #radioButtons("color", label = ("Color Mode"), choices = list("Color" = 1, "Greyscale" = 2), selected = 1),
                               
                               sliderInput("hor", "Number of horizontal cells:", min = 10, max = 30,  value = 19),
                               sliderInput("ver", "Number of vertical cells:", min = 10, max = 30,  value = 19)
                             ), # END OF SIDEBAR PANEL
                             mainPanel(
                               HTML(
                                 paste(
                                   h3('Image Editor and Segmentation', align = "center"),
                                   plotOutput("plot1",
                                              click = "plot_click",
                                              dblclick = "plot_dblclick",
                                              hover = "plot_hover",
                                              brush = "plot_brush"),
                                   '<br/>',
                                   
                                   column(6, shinyjs::hidden(actionButton("crop", label = "Cropping"))),
                                   tags$style(type='text/css', "#crop { display: block; width:60%; margin-left: auto; margin-right:auto;}"),
                                   
                                   column(6, shinyjs::hidden(actionButton("segmentation", label = "Segmentation"))),
                                   tags$style(type='text/css', "#segmentation { display: block; width:60%; margin-left: auto; margin-right:auto;}"),
                                   
                                   '<br/>','<br/>',
                                   
                                   h3('Preview Crop', align = "center"),
                                   h6('Click and drag where you would like to crop the photo. To keep the cropped version, press Crop', align = "center"),
                                   '<br/>',
                                   plotOutput("plot2"),
                                   
                                   verbatimTextOutput("info")
                                 )
                               ),
                               width = 8
                             ) # END OF MAIN PANEL
                           ) # END OF SIDEBAR LAYOUT
                  ), # END OF TAB PANEL
                  
                  ## Start of Tab Background Correction
                  tabPanel("Background Correction", value = "tab2",
                           sidebarLayout(
                             sidebarPanel(
                               radioButtons("channel", 
                                            label = ("Conversion model"), 
                                            choices = list("luminance" = 1, 
                                                           "gray" = 2), 
                                            selected = 1),
                               
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
                               actionButton("threshold", label = "Apply Threshold"),
                               br(), br(),
                               actionButton("data", label = "Add To Data"), 
                               br(), br(),
                               actionButton("showIntensData", label = "Show Intensity Data"),
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
                               br(), br(),
                               downloadButton("downloadData", "Download Data"), 
                               br(), br(),
                               actionButton("deleteData", label = "Delete Data"), 
                               br(), br(),
                               
                             ),
                             mainPanel(
                               DTOutput("intens")
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

