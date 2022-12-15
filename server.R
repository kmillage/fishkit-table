

server <- function(input, output, session) {
  
  # Container storing retained user-defined options
  # Note: we need to think about how the retained options are stored for all species (to print in the report or export to another tab) versus what needs to be displayed   
  retainedData <- reactiveValues(all_species = NULL)
  
  # Add a randomly drawn entry to the retained data object when the "Retain option:" button is pressed
  observeEvent(input$addRow, {
    
    new_option <- fake_data[sample(nrow(fake_data %>% filter(species == "Example fish")), 1), ]
    
    retainedData$all_species <- isolate(retainedData$all_species) %>%
      bind_rows(new_option)
    
  })
  
  # Example table #1
  output$exampleTable1 <- renderDT({
    
    req(!is.null(retainedData$all_species))
    
    plot_data
    
    datatable(data = retainedData$all_species, 
              extensions = 'Buttons',
              options = list( 
                 dom = "Blfrtip", 
                 # buttons = 
                 #   list("copy", list(
                 #     extend = "collection"
                 #     , buttons = c("csv", "excel", "pdf")
                 #     , text = "Download"
                 #   )) # end of buttons customization
                 # 
                 # customize the length menu
                 lengthMenu = list(c(10, 20, -1), # declare values
                                  c(10, 20, "All") # declare titles
                 ),
                 pageLength = 10
               ) # end of options
    ) # end of datatables
    
  })

}