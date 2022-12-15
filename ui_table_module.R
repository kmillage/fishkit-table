
interactiveTable <- function(){
  fluidPage(
    
    tags$h3("Examples of interactive tables for retaining user-defined scenarios"),
    tags$br(),
    
    # This would be the button that retains selected options in the current Size Limit Builder - for this demo, we're going to use the button to randomly draw an entry from a CSV with fake data. 
    fluidRow(
      column(
        8,
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
      )
    ),
    # Interactive Table of retained size limits - Option #1
    fluidRow(
      tabBox(
        width = 12,
        height = "500px",
        type = "tabs",
        collapsible = FALSE,
        tabPanel(
          title = h6(strong("Example #1")),
          collapsible = FALSE, 
          width = 12,
          solidHeader = FALSE,
          status = NULL,
          boxPad(
            color = "white",
            
            # Table
            DT::dataTableOutput("exampleTable1"),
            
            # Delete selected row button
            actionBttn(
              "deleteRow1",
              strong("Delete Selected Row:"),
              size = "sm",
              block = TRUE,
              color = "warning",
              style = "gradient"
            )
          )
        )
      )
    )
  )
}