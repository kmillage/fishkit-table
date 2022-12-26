
interactiveTable <- function(){
  fluidPage(
    
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
            tags$br(),
            
            fluidRow(
              column(3, offset = 7,
                     # Notes to add to selected row
                     textInput("Notes",
                               label = NULL,
                               value = "",
                               placeholder = "Input notes here")
              ),
              column(2,
                     # Ranking to add to selected row
                     numericInput("Ranking",
                                  label = NULL,
                                  value = 1,
                                  min = 0,
                                  max = 5,
                                  step = 1)
              )
            ),
            fluidRow(
              column(7, 
                     # Delete selected row button
                     actionBttn(
                       "deleteRow",
                       strong("Delete selected row:"),
                       size = "sm",
                       block = TRUE,
                       color = "warning",
                       style = "gradient"
                     )
              ),
              column(3,
                     actionBttn(
                       "addNotes",
                       strong("Add notes to selected row:"),
                       size = "sm",
                       block = TRUE,
                       color = "warning",
                       style = "gradient"
                     )
              ),
              column(2,
                     actionBttn(
                       "addRanking",
                       strong("Add rank to selected row:"),
                       size = "sm",
                       block = TRUE,
                       color = "warning",
                       style = "gradient"
                     )
              )
            )
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