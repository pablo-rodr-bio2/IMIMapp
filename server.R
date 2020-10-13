server <- function(input, output, session) {
  
  # session$onSessionEnded(stopApp)
  
  ############################## UI SETTINGS ###############################
  
  ##### ActionButton controller ######## 
  observeEvent(input$goToData, {
    newtab <- switch(input$sidebar, "upload" = "plots_data", "plots_data" = "upload")
    updateTabItems(session, "sidebar", newtab)
  })
  
  ### Selectinput For Dates ####
  output$input_dates <- renderUI({
    req(input$inputExcel)
    box(id="date_setup", title="Dates Setup", status = "primary", solidHeader = TRUE, width = 12, 
        column(4,
               selectInput("start_date", "Select Starting or Diagnosis Date:", choices=my_data$colnames,
                           selected = "DATA_DIAG")
        ),
        column(4,
               selectInput("exit_date", "Select Exit Date:", choices=my_data$colnames,
                           selected = "DATA_UC")
        ),
        column(4,
               selectInput("exit_event", "Select Exit Event:", choices=my_data$colnames,
                           selected = "ESTAT_UC")
        )
    )
  })
  
  ### SelectInput for Age Column ####
  output$input_age <- renderUI({
    req(input$inputExcel)
    box(id = "box_ages", title = "Age Column", status = "primary", solidHeader = TRUE, width = 4,
        selectInput("Age", "Select Column for Age: ", choices=my_data$colnames,
                    selected = "EDAT"))
    
  })
  
  #### Picker for Categoric and Continuos values ####
  output$input_columns <- renderUI({
    req(input$inputExcel)
    box(id = "box_columns", title = "Types of Parameters", status = "primary", solidHeader = TRUE,
        width = 8, 
        column(6,
               pickerInput(inputId = "box_categoric", label = "Categoric Parameters ",
                           choices= my_data$colnames, multiple = TRUE,
                           options = pickerOptions(actionsBox = TRUE, dropupAuto =FALSE))
        ),
        column(6,
               pickerInput(inputId = "box_continuos", label = "Continuos Parameters ",
                           choices= my_data$colnames, multiple = TRUE,
                           options = pickerOptions(actionsBox = TRUE, dropupAuto =FALSE))
        )
    )
  })
  
  #### Discretizing Continuos Options #########
  output$cont_select <- renderUI({
    req(input$box_continuos)
    box(id="cont_checks", title="Continuos Parameters Discretization", status = "primary",
        solidHeader = TRUE, collapsible = TRUE, width = 12,
        fluidRow(
          column(3,
                 selectInput(inputId = "discretize_cont", label = "Choose Continuos parameter",
                             choices = input$box_continuos)
          ),
          column(3,
                 selectInput(inputId = "discretize_option", label = "Choose Type of discretization",
                             choices = c("Fixed number", "Median", "Quartils"))
          ),
          column(3,
                 textInput(inputId = "disc_col_name", label = "Please insert New discrete column name")
          ),
          column(3,
                 uiOutput("fix_num")
          )
        ),
        actionBttn(inputId = "add_discrete_param", label = "Add discrete param",
                   style = "gradient", color = "primary"))
  })
  
  #### Generate Add discrete fixed number input #### 
  output$fix_num <- renderUI({
    if(input$discretize_option == "Fixed number"){
      textInput(inputId = "disc_fix_num", label = "Please insert fixed number")
    } 
  })
  
  ### UI for discretizing Values ####
  observeEvent(input$add_discrete_param, {
    param <- input$discretize_cont
    name <- input$disc_col_name
    oper <- input$discretize_option
    fixed_number <-  ifelse(is.null(input$disc_fix_num) | is.na(input$disc_fix_num), 1, input$disc_fix_num)
    
    switch(oper,
           "Median"={
             my_data$dt <- my_data$dt %>% 
               mutate(!!name := case_when( .data[[param]]>median(.data[[param]])~"above", TRUE~"below"))
             
           },
           "Quartil"={
             my_data$dt <- my_data$dt %>% 
               mutate(!!name := case_when( .data[[param]] > quantile(.data[[param]], 0.75)~"Fourth quart",
                                           .data[[param]] <= quantile(.data[[param]], 0.25)~"First quart",
                                           .data[[param]] > median(.data[[param]]) ~"Third quart",
                                           TRUE~"Second quart"))
           },
           "Fixed number"={
             my_data$dt <- my_data$dt %>% 
               mutate(!!name := case_when( .data[[param]]>fixed_numer~"above",
                                           TRUE~below))
           })
    my_data$Categoric <- append(my_data$Categoric, name)
    print(my_data$Categoric)
    
  })
  
  #### Generate Go Plot Button ####
  output$Databtn <- renderUI({
    req(my_data$dt)
    actionBttn(inputId = "goToData", label = "Go to data", style = "gradient", color = "royal",
               size = "lg", block = TRUE)
  })
  
  ##### DateRangeInput ##########
  # since date columns can be imported as POSIXCT, there can be a lot of troubles
  # to get max and min values. We choose to use magrittr's pipeline and create a 
  # dataframe with summarize with range in order to get min and max values.
  # For further updates, in case someone wants to use R base subsettings, you
  # can try something like this:
  # max(as.Date(dt[,dates][[1]]))
  
  output$dates <- renderUI({
    req(input$start_date, input$exit_date)
    dates <- input$start_date
    max.min.dates <- my_data$dt %>% summarize(max = range(!!ensym(dates))[2],
                                              min = range(!!ensym(dates))[1])
    dateRangeInput("diag_date", "Select diagnosis dates: ",
                   start = max.min.dates$min,
                   end = max.min.dates$max,
                   min = max.min.dates$min,
                   max = max.min.dates$max)
  })
  
  ##### Age slider ####
  output$age <- renderUI({
    req(input$Age)
    age <- input$Age
    my_data$dt <- to.numeric(my_data$dt, age)
    min1 <- my_data$dt %>% select(!!ensym(age)) %>% min(.,na.rm=TRUE)
    max1 <- my_data$dt %>% select(!!ensym(age)) %>% max(.,na.rm=TRUE)
    sliderInput("age", "Age", min = min1, max = max1,
                value = c(min1,max1))
  })
  
  ####  X values for selectInput ####
  output$X <- renderUI({
    req(input$if_x)
    Categoric <- input$box_categoric
    Continuos <- input$box_continuos
    selectInput("x_axis", "Select value for x axis: ", 
                choices=list("Categoric"=Categoric, "Continuos"=Continuos))
  })
  
  #### Y values for selectInput ####
  output$Y <- renderUI({
    req(input$if_y)
    Categoric <- input$box_categoric
    Continuos <- input$box_continuos
    selectInput("y_axis", "Select value for y axis: ", 
                choices=list("Categoric"=Categoric, "Continuos"=Continuos))
  })
  
  #### COLOUR values for selectInput ####
  output$COL <- renderUI({
    req(input$if_col)
    Categoric <- input$box_categoric
    selectInput("colours", "Group by: ", 
                choices=list("Categoric"=Categoric))
  })
  
  
  ####### Hide checkboxs and download button ###########
  
  observe({
    if(input$sidebar== 'plots_data' && !is.null(input$inputExcel)){
      shinyjs::show("if_x")
      shinyjs::show("if_y")
      shinyjs::show("checks")
      shinyjs::show("diag_date")
      shinyjs::show("age")
      shinyjs::show("downloadDT")
      shinyjs::show("if_col")
    } else {
      shinyjs::hide("if_x")
      shinyjs::hide("if_y")
      shinyjs::hide("checks")
      shinyjs::show("diag_date")
      shinyjs::hide("age")
      shinyjs::hide("downloadDT")
      shinyjs::hide("if_col")
    }
  })
  
  ###### Hide x_Axis ############
  observe({
    if(input$if_x){
      shinyjs::show("x_axis")
      shinyjs::show("downloadPlot")
    } else {
      shinyjs::hide("x_axis")
      shinyjs::hide("downloadPlot")
    }
  })
  
  ###### Hide y_Axis ############
  observe({
    if(input$if_y){
      shinyjs::show("y_axis")
    } else {
      shinyjs::hide("y_axis")
    }
  })
  
  ###### Hide col ############
  observe({
    if(input$if_col){
      shinyjs::show("colours")
    } else {
      shinyjs::hide("colours")
    }
  })
  
  
  # ##### Hide download Table_by button #####
  # output$downTable_by <- renderUI({
  #   req(my_data())
  #   downloadButton('downloadTable', 'Download Table', class="btn btn-success")
  # })
  
  
  ###### Filter Categoric Options #########
  output$stage_select <- renderUI({
    filters <- my_data$Categoric %>% 
      map(~list(inputId = .,
                label = .,
                choices = levels(my_data$dt[[.]]),
                multiple = TRUE, options = pickerOptions(actionsBox = TRUE),
                selected = levels(my_data$dt[[.]])))
    map(filters, ~do.call(what = pickerInput, .))
    
  })
  
  
  ################################### DATA FORMATTING AND FUNCTIONS ######################################
  
  my_data <- reactiveValues(dt=NULL)
  
  # function to transform columns to numeric
  to.numeric <- function(df, x){
    df %>% 
      mutate_at(x, funs(as.numeric))
  }
  
  # function to transform columns to factor
  to.factor <- function(df,x){
    df %>% 
      mutate_at(x, funs(as.factor))
  }
  
  ######## Import Data  ######## 
  observeEvent(input$inputExcel, {
    my_data$dt <- read_excel(input$inputExcel$datapath)
    my_data$colnames <- colnames(my_data$dt)
  })
  
  ###### Reactive values for Continuos and Categoric values ######
  observeEvent(input$box_continuos,{
    my_data$Continuos <- input$box_continuos
    my_data$dt <- to.numeric(my_data$dt, as.vector(my_data$Continuos))
  })
  observeEvent(input$box_categoric,{
    my_data$Categoric <- input$box_categoric
    my_data$dt <- to.factor(my_data$dt, as.vector(my_data$Categoric))
  })
  
  ##### Filtering Data   ##### 
  # Fist, filters by dates, then by age, then by Categoric values
  # filters are age slider and daterangerinput
  # note: in case of needing to remove rows with na, see "complete.cases"
  
  filtered_data <- reactive ({
    req(my_data$dt)
    isolate(dt <- my_data$dt)
    DATA_DIAG <- input$start_date
    EDAT <- input$Age
    f <- input$exit_date
    s <- input$start_date
    event <- input$exit_event
    
    dt <- dt %>%
      filter(!!ensym(DATA_DIAG)>=input$diag_date[1] & !!ensym(DATA_DIAG)<=input$diag_date[2]) %>% # by date
      filter(!!ensym(EDAT)>=input$age[1] & !!ensym(EDAT)<=input$age[2])
    
    # disclaimer: I had a lot of problems retrieving data from dynamically 
    # generated ui inputs, specially since their id is dynamically
    # generated, so this part of the code was shamelessly copied 
    # from this Stackoverflow thread: 
    # https://stackoverflow.com/questions/45899713/filtering-dataframe-rows-from-dynamic-variables-within-shiny
    
    dfs <- lapply(as.vector(my_data$Categoric), function(d) {
      dt[dt[[d]] %in% input[[d]],]
    })
    dt <- Reduce(function(...) merge(..., all=FALSE), dfs)
    
    # new columns for survfit object
    dt <- dt %>%
      mutate("Days_to_Event" = difftime(!!ensym(f), !!ensym(s),
                                        units=c("days")))
    
    dt <- dt %>% mutate(!!event := case_when(.data[[event]]=="E"~1,
                                             TRUE ~0))
    dt
  })
  
  #####  Filtering for data table   #####
  ## since only the categoric values are factors in filtered_data,
  # we only need to group by factor
  
  filtered_table <- reactive ({
    req(filtered_data())
    d<- filtered_data()
    d <- d %>% 
      group_by_if(is.factor) %>% 
      summarize(Total =n())
  })
  
  #### Surv fit object ####
  fit1 <- reactive({
    req(input$if_col, filtered_data())
    time_event <- "Days_to_Event"
    exit_event <- input$exit_event
    grouping <- input$colours
    dt <- filtered_data()
    formu <- as.formula(paste("Surv(",time_event,",",exit_event,")~",grouping))
    fit <- survfit(formu, data = dt)
    fit$call$formula <- formu
    fit
  })
  
  
  ###################### PLOT FUNCTIONS  ######################
  
  ##### Generic plot function ######
  # Kind of plot "process request"
  # depending of user actions, it will decide
  # what kind of graph to plot
  
  plotGraphs <- reactive({
    req(filtered_data())
    data <- filtered_data()
    x1 <- input$x_axis
    y1 <- input$y_axis
    col <- input$colours
    
    if(!input$if_y){                 # only X axis
      if(!input$if_col){             # no de-aggregation
        df <- data %>%
          filter(!!ensym(x1) != "NA") %>% 
          group_by(!!ensym(x1)) %>% 
          summarise(counts=n())
        plot_bar(df, x1)
      } 
      else {                               # with aggregation
        df <- data %>% 
          filter(!!ensym(x1) != "NA") %>% 
          drop_na(!!ensym(col)) %>%
          group_by(!!ensym(x1), !!ensym(col)) %>% 
          summarise(counts=n())
        plot_bar(df,x1,col)
      }
    } else {                               # x and y axis
      if(!input$if_col){             # no de-aggregation
        df <- data %>% 
          filter(!!ensym(x1) != "NA") %>% 
          drop_na(!!ensym(y1)) %>%
          group_by(!!ensym(x1), !!ensym(y1))
        plot_xy(df, x1, y1)
      } else {                             # with aggregation
        df <- data %>% 
          filter(!!ensym(x1) != "NA") %>% 
          drop_na(!!ensym(y1), !!ensym(col)) %>%
          group_by(!!ensym(x1), !!ensym(y1), !!ensym(col))
        plot_xy(df, x1, y1, col)
      }
    }
  })
  
  #### Bar plot function #####
  
  plot_bar <- function(data, x1, col){
    if (missing(col)){
      g <- ggbarplot(data = data, x = x1, y="counts", fill=x1) + ylab("N of patients")
    } else {
      g <- ggbarplot(data = data, x = x1, y="counts", fill=col) + ylab("N of patients")
    }
    return(g)
  }
  
  #### Box and scatter plot function #####
  
  plot_xy <- function(data,x1,y1, col){
    
    if(missing(col)){
      if(x1 %in% my_data$Categoric){
        g <- ggboxplot(data, x=x1, y=y1, fill=x1, title="Plot") +
          stat_compare_means(size=8) + 
          theme(legend.position="bottom")
      }
      else {
        g <- ggscatter(data, x=x1, y=y1, color=x1, add="reg.line", 
                       conf.int = TRUE,cor.coef = TRUE)
      }
    } else {
      if(x1 %in% my_data$Categoric){
        g <- ggboxplot(data, x=x1, y=y1, fill=col, title="Plot") +
          stat_compare_means(size=8) + 
          theme(legend.position="bottom")
      }
      else {
        g <- ggscatter(data, x=x1, y=y1, color = col, add="reg.line",
                       conf.int = TRUE,cor.coef = TRUE)
      }
    }
    return(g)
  }
  
  #### Tableby function #####
  # depending of user inputs, tableby will be generated
  # one wayr or another: 
  # 1. if x and y axis are continuos, doesn't do anything
  # 2. if no axis, uses both Continuos and Categoric as variables and no group
  # 3. If only x axis, variable = x
  # 4. if x in Categoric and y in Continuos, group = y, variable = x
  
  plotTableBy <- function(){
    f <- filtered_data()
    isolate(Continuos <- my_data$Continuos)
    isolate(Categoric <- my_data$Categoric)
    y <- input$y_axis
    x <- input$x_axis
    
    if(!input$if_x && !input$if_y){
      tab1 <- f %>%
        select(!!Continuos, !!Categoric) %>%
        tableby(~., .)
      return(tab1)
    }
    
    if(input$if_x && !input$if_y){
      tab1 <- tableby(update(~., reformulate(x)), data=f)
      return(tab1)
    }
    
    if((x %in% Continuos && y %in% Continuos)||(x %in% Categoric && y %in% Categoric))
      return(NA)
    
    if(x %in% Categoric && y %in% Continuos){
      tab1 <-  tableby(update(.~., reformulate(y,x)), data=f)
      return(tab1)
    }
  }
  
  #### plotCurve() ####
  # function for plotting surfvfit object with min and max values
  # Since there is only one plot, I didn't find
  # useful to make a "process request" separation-like layer
  
  plotCurve <- reactive({
    req(fit1())
    dt <- filtered_data()
    min_y <- min(fit1()$surv)
    max_y <- max(fit1()$surv)
    g <- ggsurvplot(fit1(), pval=TRUE, risk.table = TRUE, ylim=c(min_y-0.2,max_y), data = dt)
    g
  })
  
  ########### RENDERS ###########
  
  ##### Plot ##### 
  output$plot_area <- renderPlot({
    req(filtered_data(), input$if_x)
    plotGraphs()
  })
  
  #####  Tableby ######
  output$tableby <- renderTable({
    req(filtered_data())
    result <- plotTableBy()
    if(is.na(result) || is.null(result))
      return("Error: Tableby function will not work if both params are Continuos or Categoric")
    as.data.frame(summary(result, text="html"))
  }, striped = TRUE, bordered = TRUE, sanitize.text.function = function(x) x)
  
  #####  Datatable   ##### 
  output$dt <- DT::renderDataTable(
    filtered_table()
  )
  
  #### Survfit curve ######
  output$curvsurv <- renderPlot({
    plotCurve()
  })
  
  
  ########################  DOWNLOAD BUTTONS ##############################
  
  ##### Download plot #####
  output$downloadPlot <- downloadHandler(
    filename = "plot.png",
    content = function(file) {
      ggsave(file, plot = plotGraphs(), device = "png")
    }
  )
  
  ##### Download Curve plot ######
  # Warining!
  # ggsurvplot() returns a list of ggplots containing survival curves and 
  # optionally the risk table. You can't directly save the list using ggsave(),
  # but you can save the output of print(ggsurvplot)
  
  output$downloadCurve <- downloadHandler(
    filename = "curve-plot.png",
    content = function(file) {
      ggsave(file, plot = print(plotCurve()), device = "png")
    }
  )
  
  ##### Download Table By #####
  # note: tableby returns a table with some elements as lists
  # as.data.frame(tableby_object) wont work because of that, be aware
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), ".csv", sep = "")
    }, 
    content = function(file) {
      result <- plotTableBy()
      result <- as.data.frame(summary(result, text=TRUE))
      write.csv(result, file, row.names=FALSE)
    }
  )
  
  ##### Download Datatable ##### 
  output$downloadDT <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), ".csv", sep = "")
    }, 
    content = function(file) {
      write.csv(filtered_table(), file, row.names=FALSE)
    }
  )
}