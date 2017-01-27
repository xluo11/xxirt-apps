library(xxIRT)
library(shiny)
library(shinydashboard)
library(DT)


# UI
ui <- dashboardPage(
  dashboardHeader(
    title = "ATA"
  ),
  
  dashboardSidebar(
    disable = TRUE
  ),
  
  dashboardBody(
    fluidRow(
      # Output Area
      column(width = 9,
             # Alert
             conditionalPanel(condition="$('html').hasClass('shiny-busy')", 
                              tags$div(class="alert alert-warning",
                                       tags$b("ATA in progress ... "),
                                       HTML("<i class='fa fa-spinner fa-pulse fa-1x fa-fw'></i>"))
             ),
             
             box(width = 6, title = "Job Summary", status = "primary",
                 htmlOutput("job_summary")
             ),
             
             box(width = 6, title = "Constraints", status = "primary",
                 DT::dataTableOutput("cons_table")
             ),
             
             box(width = 6, title = "Items", status = "success",
                 DT::dataTableOutput("items_table"),
                 downloadButton("items_download", "Save")
             ),
             
             tabBox(width = 6, title = "Analysis",
                    tabPanel("TIFs", plotOutput("tif_plot")),
                    tabPanel("Variables", DT::dataTableOutput("var_table"))
             )
      ),
      
      # Input Area
      column(width = 3,
             box(width = NULL, title = "Controls", status = "warning",
                 # test level configs
                 fileInput("item_file", "Upload item parameters:", accept=c("text/plain", "test/csv")),
                 numericInput("nform", "Number of forms:", value=1, min=1),
                 fluidRow(
                   column(6, numericInput("min_len", "Min. length:", value=10, min=1)),
                   column(6, numericInput("max_len", "Max. length:", value=10, min=1))
                 ),
                 hr(),
                 
                 # objectives
                 selectInput("obj_type", "Add objective To:", list("Optimize Values"="relative", "Approach Targets"="absolute")),
                 fluidRow(
                   column(6, selectInput("obj_coef_method", "Coefficient:", list("Theta"="theta", "Variable"="var"))),
                   column(6, 
                          conditionalPanel("input.obj_coef_method == 'theta'", textInput("obj_coef_theta", "Value:", "")),
                          conditionalPanel("input.obj_coef_method == 'var'", uiOutput("obj_var_ui")))
                 ),
                 
                 conditionalPanel("input.obj_type == 'relative'", 
                                  selectInput("obj_mode", "Optimization mode:", list("Maximize"="max", "Minimize"="min")),
                                  checkboxInput("obj_negative", "Expect a negative optimal value?"), value=FALSE),
                 conditionalPanel("input.obj_type == 'absolute'", 
                                  numericInput("obj_target", "Target value:", 0, step=0.1)),
                 actionButton("obj_btn", "Add Objective", icon=icon("plus")),
                 hr(),
                 
                 # constraints
                 selectInput("cons_method", "Coefficient type:", list("Variable"="var", "Constant"="const")),
                 conditionalPanel("input.cons_method == 'var'",
                                  uiOutput("cons_var_ui")),
                 conditionalPanel("input.cons_method == 'const'",
                                  numericInput("cons_coef_const", "Constant:", value=0)),
                 fluidRow(
                   column(6, numericInput("cons_min", "Min.", NULL)),
                   column(6, numericInput("cons_max", "Max.", NULL))
                 ),
                 actionButton("cons_btn", "Add Constraint", icon=icon("plus")),
                 hr(),
                 
                 # assemble button
                 actionButton("assemble_btn", "Assemble", icon=icon("bathtub"))
             )
      )
    )
  )
  
)


# Server
server <- function(input, output, session){
  rv <- reactiveValues(summary="", ata=NULL, cons=NULL, items=NULL)
  
  # Import item parameters
  items <- reactive({
    req(input$item_file)
    validate(need(input$item_file$size <= 500000, "File is too large"))
    x <- read.csv(input$item_file$datapath, header=TRUE, as.is=TRUE)
    colnames(x) <- tolower(colnames(x))
    validate(need(all(c("a", "b", "c") %in% colnames(x)), "a, b, c parameters are not all found"))
    x$a <- round(x$a, 2)
    x$b <- round(x$b, 2)
    x$c <- round(x$c, 2)
    x
  })
  
  # Render objective variables UI
  output$obj_var_ui <- renderUI({
    req(items())
    selectInput("obj_coef_var", "Value:", colnames(items()))
  })
  
  # Render constraint variables UI
  output$cons_var_ui <- renderUI({
    req(items())
    fluidRow(
      column(6, selectInput("cons_coef_var", "Variable:", colnames(items()))),
      column(6, textInput("cons_coef_level", "Level:", ""))
    )
  })
  
  
  # Create ATA
  observe({
    rv$ata <- ata(items(), input$nform, len=c(input$min_len, input$max_len), maxselect=1)
    rv$cons <- NULL
    rv$items <- NULL
  })
  
  # Add objective
  observeEvent(input$obj_btn, {
    req(rv$ata)
    
    if(input$obj_coef_method == "theta") {
      coef <- input$obj_coef_theta
      coef <- as.numeric(unlist(strsplit(coef, ",")))
      req(all(!is.na(coef)))
    } else if(input$obj_coef_method == "var") {
      coef <- input$obj_coef_var
    }
    
    if(input$obj_type == "relative") {
      rv$ata <- ata_obj_relative(rv$ata, coef, mode=input$obj_mode, negative=input$obj_negative)
    } else if(input$obj_type == "absolute") {
      rv$ata <- ata_obj_absolute(rv$ata, coef, target=input$obj_target)
    }
    
    rv$cons <- rbind(rv$cons, 
                     data.frame(type="objective", name=coef, 
                                level=ifelse(input$obj_type=="relative", input$obj_mode, input$obj_target), 
                                min=NA, max=NA, stringsAsFactors=FALSE))
  })
  
  # Add constraint
  observeEvent(input$cons_btn, {
    req(rv$ata)
    
    if(input$cons_method == "var") {
      coef <- input$cons_coef_var
      level <- as.numeric(input$cons_coef_level)
    } else if(input$cons_method == "const") {
      coef <- input$cons_coef_const
      level <- NA
    }
    
    min <- input$cons_min
    max <- input$cons_max
    req((!is.na(min) || !is.na(max)))
    
    rv$ata <- ata_constraint(rv$ata, coef, min=min, max=max, level=level)
    rv$cons <- rbind(rv$cons, 
                     data.frame(type="constraint", name=coef, 
                                level=level, min=min, max=max, 
                                stringsAsFactors=FALSE))
  })
  
  # Assemble
  observeEvent(input$assemble_btn, {
    req(rv$ata, rv$cons)
    rv$ata <- ata_solve(rv$ata, tiemout=60)
    if(is.null(rv$ata$result)) {
      rv$summary <- paste(rv$summary, "<br>", "No solution :-<")
    } else {
      rv$items <- ata_get_items(rv$ata)
    }
  })
  
  
  # Output: job summary
  observeEvent(c(rv$ata, rv$cons), {
    x1 <- ifelse(is.null(rv$ata), 
                 "Import items to create an ATA job", 
                 paste("Use a ", nrow(rv$ata$pool), "-item pool to assemble ", 
                       input$nform, " form(s).<br>Each form has at least ", 
                       input$min_len, " and at most ", input$max_len, " items.", 
                       sep="")) 
    
    x2 <- ifelse(is.null(rv$cons), 
                 "There is no objective or constraint.",
                 paste("There are", sum(rv$cons$type=="objective"), "objectives and", 
                       sum(rv$cons$type=="constraint"), "constraints"))
    
    rv$summary <- paste(x1, "<br>", x2)
  })
  output$job_summary <- renderText({
    rv$summary
  })
  
  # Output: constraints
  output$cons_table <- DT::renderDataTable({
    rv$cons
  }, rownames=FALSE, options=list(pageLength=10, dom='tip'))
  
  # Output: items
  output$items_table <- DT::renderDataTable({
    req(rv$items)
    rv$items
  }, rownames=FALSE, options=list(pageLength=10, dom='tip'))
  
  # Output: tif
  output$tif_plot <- renderPlot({
    req(rv$items)
    plot(rv$ata)
  })
  
  # Output: variable summary
  output$var_table <- DT::renderDataTable({
    req(rv$items, rv$cons)
    
    out <- NULL
    for(i in 1:nrow(rv$cons)) {
      type <- rv$cons[i, "type"]
      name <- rv$cons[i, "name"]
      level <- rv$cons[i, "level"]
      
      if(type == "objective") {
        if(name %in% colnames(rv$items)) {
          x <- rv$items[, name]
        } else {
          t <- as.numeric(name)
          x <- irt_stats(irt_model("3pl", theta=t, items=rv$items), "info")[1, ]
        }
      } else if(type == "constraint") {
        if(is.na(level)) {
          x <- rv$items[, name]
        } else {
          x <- as.character(rv$items[, name]) == level
        }
      }
      
      x <- aggregate(x, list(rv$items$form), sum)
      out <- rbind(out,data.frame(type=type, name=name, level=level, form=x[,1], 
                                  value=round(x[,2], 1), stringsAsFactors=FALSE))
    }
    
    out
  }, rownames=FALSE, options=list(pageLength=10, dom='tip'))
  
  # Download
  output$items_download <- downloadHandler(
    filename=function(){
      paste("xxirt_ata_", gsub("-","", Sys.Date()), ".txt", sep="")
    }, content=function(file){
      req(rv$items)
      write.csv(rv$items, file, quote=FALSE, row.names=FALSE)
    }
  )
  
}


# shinyApp
shinyApp(ui=ui, server=server)
  
