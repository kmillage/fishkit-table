
# Move to own functions file

rating_stars <- function(rating, max_rating = 5) {
  star_icon <- function(empty = FALSE) {
    tagAppendAttributes(shiny::icon("star"),
                        style = paste("color:", if (empty) grey_color else "orange"),
                        "aria-hidden" = "true"
    )
  }
  rounded_rating <- floor(rating + 0.5)  # always round up
  stars <- lapply(seq_len(max_rating), function(i) {
    if (i <= rounded_rating) star_icon() else star_icon(empty = TRUE)
  })
  label <- sprintf("%s out of %s stars", rating, max_rating)
  div(title = label, role = "img", stars)
}

grey_color <- "#EEEEEE"
orange_color <- "#E88727"


interactiveTable <- function(){
  fluidPage(
    
    # Modal for facilitator inputs
    bsModal("inputModal", title = "", trigger = "table_click", size = "large",
            tagList(
              # Reactive title
              uiOutput("selected_row"),
              
              tags$br(),
              
              # Widgets
              fluidRow(
                column(8,
                       # Notes to add to selected row
                       textInput("Notes",
                                 label = NULL,
                                 value = "",
                                 placeholder = "Input notes here",
                                 width = "100%")
                ),
                column(4,
                       
                       pickerInput("Ranking",
                                   label = NULL,
                                   choices = c(1,2,3,4,5),
                                   choicesOpt = list(
                                     content = c(paste(rating_stars(1)),
                                                 paste(rating_stars(2)),
                                                 paste(rating_stars(3)),
                                                 paste(rating_stars(4)),
                                                 paste(rating_stars(5)))
                                     )
                       )
                )
              ),
              fluidRow(
                column(2, offset = 6,
                       actionBttn(
                         "addNotes",
                         strong("Add notes"),
                         size = "sm",
                         block = TRUE,
                         color = "warning",
                         style = "gradient"
                       )
                ),
                column(2, offset = 2,
                       actionBttn(
                         "addRanking",
                         strong("Add ranking"),
                         size = "sm",
                         block = TRUE,
                         color = "warning",
                         style = "gradient"
                       )
                )
              ),
              tags$hr(),
              fluidRow(
                column(2, offset = 10,
                       # Delete selected row button
                       actionBttn(
                         "deleteRow",
                         strong("Delete"),
                         size = "sm",
                         block = TRUE,
                         color = "default",
                         style = "gradient"
                       )
                )
              )
            )
    ),
    
    tags$h3("Examples of interactive tables for retaining user-defined scenarios"),
    tags$br(),
    
    # This would be the button that retains selected options in the current Size Limit Builder - for this demo, we're going to use the button to randomly draw an entry from a CSV with fake data. 
    fluidRow(
      column(
        4,
        div(
          style = "margin: 0px 20px 47px 20px;",
          actionBttn(
            "addRow",
            strong("Retain option:"),
            size = "sm",
            block = TRUE,
            color = "warning",
            style = "gradient"
          )
        )
      ),
      column(5,
             "This button simulates adding data by randomly drawing from a file of possible outcomes"
      )
      # column(2, offset = 1,
      #        downloadButton("testReport", label = "Download Report", width = "100%")
      # )
    ),
    # Interactive Table of retained size limits - Option #1
    fluidRow(
      tabBox(
        width = 12,
        height = "500px",
        type = "tabs",
        collapsible = FALSE,
        
        # Example #1
        tabPanel(
          title = h6(strong("Example #1")),
          collapsible = FALSE, 
          width = 12,
          solidHeader = FALSE,
          status = NULL,
          boxPad(
            color = "white",
            
            # Table
            reactableOutput("exampleTable1"),
            tags$br()
          )
        ),
        
        # Example #2
        tabPanel(
          title = h6(strong("Example #2")),
          collapsible = FALSE, 
          width = 12,
          solidHeader = FALSE,
          status = NULL,
          boxPad(
            color = "white",
            
            # Table
            reactableOutput("exampleTable2"),
            tags$br()
          )
        ),
        
        # Example #3
        tabPanel(
          title = h6(strong("Example #3")),
          collapsible = FALSE, 
          width = 12,
          solidHeader = FALSE,
          status = NULL,
          boxPad(
            color = "white",
            
            # Table
            reactableOutput("exampleTable3"),
            tags$br()
          )
        ),
        
        # Example #4
        tabPanel(
          title = h6(strong("Example #4")),
          collapsible = FALSE, 
          width = 12,
          solidHeader = FALSE,
          status = NULL,
          boxPad(
            color = "white",
            
            # Table
            reactableOutput("exampleTable4"),
            tags$br()
            
          )
        ),
        
        # Example #5
        tabPanel(
          title = h6(strong("Example #5")),
          collapsible = FALSE, 
          width = 12,
          solidHeader = FALSE,
          status = NULL,
          boxPad(
            color = "white",
            
            # Table
            reactableOutput("exampleTable5"),
            tags$br()
            
          )
        )
      )
    )
  )
}