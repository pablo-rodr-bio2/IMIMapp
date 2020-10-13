ui <- dashboardPage(
  ########## Header  ########## 
  dashboardHeader(title= "ANALYSING CLINICAL CANCER DATA", titleWidth = 400),
  
  ########## Sidebar #########
  dashboardSidebar(
    useShinyjs(),
    width = 320,
    sidebarMenu(
      id="sidebar",
      menuItem("Upload & Settings", tabName = "upload", icon = icon("upload")),
      menuItem("Plots & Data", tabName = "plots_data", icon = icon("chart-bar")),
      uiOutput("dates"),
      uiOutput("age"),
      checkboxInput("if_x", "X axis", FALSE),
      uiOutput("X"),
      checkboxInput("if_y", "Y axis", FALSE),
      uiOutput("Y"),
      checkboxInput("if_col", "Grouping Variable", FALSE),
      uiOutput("COL")
    )
    
  ),
  ######## Body #############
  dashboardBody(
    
    conditionalPanel(
      condition = "input.sidebar == 'upload'",
      fluidPage(
        fluidRow(
          box(id = "box_excel", title = "Import Data", status = "primary", solidHeader = TRUE,
              width = 12,
              fileInput("inputExcel", HTML("Choose your file"), width = "50%")
          )),
        fluidRow(
          uiOutput("input_dates")
        ),
        fluidRow(
          uiOutput("input_age"),
          uiOutput("input_columns")
        ),
        br(),
        br(),
        fluidRow(
          uiOutput("cont_select")
        ),
        fluidRow(
          uiOutput("Databtn")
        )
      ) 
    ),
    conditionalPanel(
      condition = "input.sidebar == 'plots_data'",
      fluidRow(
        box(id="cat_checks", title="Categoric Filters", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = 12, 
            uiOutput("stage_select"))
      ),
      tabsetPanel(type="tabs",
                  tabPanel("Plots",
                           fluidRow(
                             column(6, 
                                    tableOutput("tableby"),
                                    uiOutput("downTable_by"),
                                    br(),
                                    
                                    DT::dataTableOutput("dt"),
                                    downloadButton('downloadDT', 'Download DataTable',
                                                   class="btn btn-success")),
                             column(6, 
                                    plotOutput("plot_area", height=600),
                                    downloadButton('downloadPlot', 'Download Plot',
                                                   class="btn btn-success")))),
                  tabPanel("Surv",
                           fluidRow(
                             box(plotOutput("curvsurv", height=700),
                                 downloadButton('downloadCurve', 'Download Curve Plot'))))
      )
    )
  )
)