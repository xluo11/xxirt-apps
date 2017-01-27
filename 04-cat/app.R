library(xxIRT)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(
    title = "CAT"
  ),
  
  dashboardSidebar(
    disable = TRUE
  ),
  
  dashboardBody(
    fluidRow(
      # Output Area
      column(9,
             # Alert
             conditionalPanel(condition="$('html').hasClass('shiny-busy')", 
                              tags$div(class="alert alert-warning",
                                       tags$b("Simulation in progress ... "),
                                       HTML("<i class='fa fa-spinner fa-pulse fa-1x fa-fw'></i>"))
             ),
             
             tabBox(width = 6, title = "",
                    tabPanel("Job", htmlOutput("job_summary")),
                    tabPanel("Constraints", DT::dataTableOutput("constraint_table"))
             ),
             
             tabBox(width = 6, title = "",
                    tabPanel("Theta", plotOutput("mass_theta_plot")),
                    tabPanel("Length", plotOutput("mass_length_plot"))
             ),
             
             box(width = 12, title = "Results", status = "success",
                 HTML('<div class="pull-right">
                      Simulation #: &nbsp;<input type="number" id="index" value="1" style="width:50px">
                      </div>'),
                 DT::dataTableOutput("cat_table"),
                 downloadButton("cat_download", "Save")
                 ),
             
             box(width = 12, title = "Constraints", status = "success",
                 plotOutput("cat_plot")
             )
             ),
      
      # Input Area
      column(3, 
             box(width = NULL, title = "Controls", status = "warning",
                 # items and length
                 fileInput("item_file", "Upload item parameters", accept=c("text/plain", "text/csv")),
                 fluidRow(
                   column(6, numericInput("min_len", "Min. Length:", 30, min=1)),
                   column(6, numericInput("max_len", "Max. Length:", 30, min=1))
                 ),
                 hr(),
                 
                 # theta
                 selectInput("theta_method", "Simulation type:", list("Single Simulation"="single", "Mass Simulation"="mass")),
                 conditionalPanel("input.theta_method == 'single'",
                                  numericInput("theta_value", "Theta", value=0)),
                 conditionalPanel("input.theta_method == 'mass'",
                                  fileInput("theta_file", "Upload people parameters:", accept=c("text/plain", "text/csv"))),
                 hr(),
                 
                 # selection rule
                 selectInput("select_rule", "Item selection rule:", 
                             list("Max. Info."="default", "C-CAT"="ccat", "Shadow Test"="shadow")),
                 numericInput("randomesque", "Randomesque:", 1, min=1),
                 conditionalPanel("input.select_rule == 'ccat'",
                                  uiOutput("ccat_ui")),
                 conditionalPanel("input.select_rule == 'shadow'",
                                  uiOutput("shadow_ui")),
                 hr(),
                 
                 # estimation rule
                 selectInput("estimate_rule", "Ability estimation rule:", 
                             list("Default"="default")),
                 hr(),
                 
                 # stopping rule
                 selectInput("stop_rule", "Stopping rule:",
                             list("Standard Error"="se", "Min. Info."="mi", "Cut Score"="cut")),
                 conditionalPanel("input.stop_rule == 'se'",
                                  numericInput("stop_se", "SE threshold: ", .3)),
                 conditionalPanel("input.stop_rule == 'mi'",
                                  numericInput("stop_mi", "MI threshold: ", .6)),
                 conditionalPanel("input.stop_rule == 'cut'",
                                  numericInput("stop_cut", "Cut score: ", 0)),
                 hr(),
                 
                 # button
                 actionButton("simulate", "Simulate", icon=icon("laptop"))
             )
             
      )
      
    )
    
  )
)

# server
server <- function(input, output, session){
  # reactive values
  rv <- reactiveValues(opts=list(), results=NULL, cons=NULL, job="")
  
  # items data
  items <- reactive({
    req(input$item_file)
    validate(need(input$item_file$size <= 500000, "File is too large"))
    x <- read.csv(input$item_file$datapath, header=TRUE, as.is=TRUE)
    colnames(x) <- tolower(colnames(x))
    req(all(c("a", "b", "c") %in% colnames(x)))
    x
  })
  
  # people data 
  people <- reactive({
    if(input$theta_method == "single") {
      x <- input$theta_value
    } else if(input$theta_method == "mass") {
      req(input$theta_file)
      validate(need(input$theta_file$size <= 500000, "File is too large"))
      x <- read.csv(input$theta_file$datapath, header=TRUE, as.is=TRUE)
      colnames(x) <- tolower(colnames(x))
      req("theta" %in% colnames(x))
      x <- x$theta
    }
    x
  })
  
  # selection rule
  select_rule <- reactive({
    if(input$select_rule == 'default') {
      x <- cat_select_default
    } else if(input$select_rule == 'ccat') {
      x <- cat_select_ccat
    } else if(input$select_rule == 'shadow') {
      x <- cat_select_shadow
    }
    x
  })
  
  # estimation rule
  estimate_rule <- reactive({
    if(input$estimate_rule == 'default') {
      x <- cat_estimate_default
    }
    x
  })
  
  # stopping rule
  stop_rule <- reactive({
    if(input$stop_rule == 'se') {
      x <- cat_stop_default
      rv$opts$stop.se <- input$stop_se
    } else if(input$stop_rule == 'mi') {
      x <- cat_stop_default
      rv$opts$stop.mi <- input$stop_mi
    } else if(input$stop_rule == 'cut') {
      x <- cat_stop_default
      rv$opts$stop.cut <- input$stop_cut
    }
    x
  })
  
  # render ccat ui
  output$ccat_ui <- renderUI({
    req(items())
    req("content" %in% colnames(items()))
    x <- items()[, "content"]
    x <- unique(x)
    x <- sort(x)
    
    tagList(
      lapply(x, function(x) {
        id <- paste("ccat_target", x, sep="")
        label <- paste("Percent in Content", x)
        numericInput(id, label, 0, min=0)
      }),
      numericInput("ccat_random","Initial random areas:", 0, min=0)
    )
  })
  
  ccat_target <- reactive({
    key <- unique(items()$content)
    key <- sort(key)
    val <- rep(NA, length(key))
    for(i in 1:length(key))
      val[i] <- input[[paste("ccat_target", key[i], sep="")]]
    
    req(sum(val) == 100)
    val <- round(val / 100, 2)
    rv$opts$ccat.target <- val
    rv$cons <- rbind(rv$cons,
                     data.frame(name="content", level=key, value=val, stringsAsFactors=FALSE))
  })
  
  # render shadow ui
  output$shadow_ui <- renderUI({
    req(items())
    fluidRow(
      column(6, selectInput("shadow_name", "Variable:", colnames(items()))),
      column(6, textInput("shadow_level", "Level:", "")),
      column(6, numericInput("shadow_min", "Min.:", value=0)),
      column(6, numericInput("shadow_max", "Max:", value=0)),
      column(12, actionButton("shadow_btn", "Add Constraint", icon("plus")))
    )
  })
  
  observeEvent(input$shadow_btn, {
    name <- input$shadow_name
    level <- input$shadow_level
    min <- input$shadow_min
    max <- input$shadow_max
    x <- data.frame(name=name, level=level, min=min, max=max, stringsAsFactors=TRUE)
    rv$opts$shadow.constraints <- rbind(rv$opts$shadow.constraints, x)
    rv$cons <- rbind(rv$cons, x)
  })
  
  
  #--------------------------------------------- simulation
  observeEvent(input$simulate, {
    rv$opts$min <- input$min_len
    rv$opts$max <- input$max_len
    rv$opts$randomesque <- input$randomesque
    
    results <- list()
    theta <- people()
    
    if(input$select_rule == 'ccat') {
      ccat_target()
      rv$opts$ccat.random <- input$ccat_random
    } 
    
    for(i in 1:length(theta)) 
      results[[i]] <- cat_sim(theta[i], items(), rv$opts, 
                              cat.select = select_rule(),
                              cat.estimate = estimate_rule(),
                              cat.stop = stop_rule())
    
    rv$results <- results
    
    rv$job <- paste("The pool has ", nrow(items())," items.<br>",
                    "Conduct ", length(theta), " simulation(s).<br>",
                    "Simulation finished!", sep="")
  })
  
  #---------------------------------------------- Output
  output$job_summary <- renderText({
    rv$job
  })
  
  output$constraint_table <- DT::renderDataTable({
    validate(need(rv$cons, "No constraints."))
    rv$cons
  }, rownames=FALSE, options=list(pageLength=10, dom='tip'))
  
  output$mass_theta_plot <- renderPlot({
    validate(need(length(rv$results) > 1, "No results for a single simulation."))
    t.true <- sapply(rv$results, function(x) x$true)
    t.est  <- sapply(rv$results, function(x) x$est)
    t.se <- sapply(rv$results, function(x) x$stats[,"se"][x$len])
    t <- data.frame(true=t.true, est=t.est, se=t.se)
    ggplot(t, aes_string(x="true", y="est")) + 
      geom_point(aes_string(color=1, alpha=.3, size="se")) + geom_smooth(linetype=2) +
      xlab(expression(paste("True ", theta))) + ylab(expression(paste("Est. ", theta))) + 
      coord_cartesian(xlim=c(-4, 4), ylim=c(-4, 4)) + guides(color=FALSE, alpha=FALSE, size=FALSE) + 
      theme_bw() + theme(legend.key=element_blank()) 
  })
  
  output$mass_length_plot <- renderPlot({
    validate(need(length(rv$results) > 1, "No results for a single simulation."))
    t.true <- sapply(rv$results, function(x) x$true)
    t.len <- sapply(rv$results, function(x) x$len)
    t.se <- sapply(rv$results, function(x) x$stats[,"se"][x$len])
    t <- data.frame(true=t.true, len=t.len, se=t.se)
    ggplot(t, aes_string(x="true", y="len")) + 
      geom_point(aes_string(color=1, alpha=.5, size="se")) + geom_smooth(linetype=2) +
      xlab(expression(paste("True ", theta))) + ylab("Test Length") + 
      coord_cartesian(xlim=c(-4, 4)) + guides(color=FALSE, alpha=FALSE, size=FALSE) + 
      theme_bw() + theme(legend.key=element_blank())
  })
  
  output$cat_table <- DT::renderDataTable({
    req(rv$results)
    validate(need(input$index >=1 && input$index <= length(rv$results), "Index is out of bound."))
    x <- rv$results[[input$index]]$admin
    round(x, 2)
  }, rownames=FALSE, options=list(pageLength=10, dom='tip'))
  
  output$cat_plot <- renderPlot({
    req(rv$results)
    validate(need(input$index >=1 && input$index <= length(rv$results), "Index is out of bound."))
    x <- rv$results[[input$index]]
    plot(x)
  })
  
}


# shinyApp
shinyApp(ui=ui, server=server)
  
