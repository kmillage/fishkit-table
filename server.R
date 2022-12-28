
### ----------------------------------------------------------------------------
### Server --------
### ----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Container storing retained user-defined options
  # Note: Bill - You can probably streamline many aspects of this process. I wanted to create a single object from which all of the different example tables could draw and thus I did some formatting up front. You might be better doing any necessary formatting within the reactive call to the table since you'll only need to do it once.
  retainedData <- reactiveValues(all = NULL,
                                 selected_row = NULL)
  
  ### Add a randomly drawn entry to the retained data object 
  observeEvent(input$addRow, {
    
      # Randomly draw a row from our fake dataset to simulate user selected options
      new_option <- fake_data[sample(nrow(fake_data), 1), ]
      
      # Format retained options for display and make charts
      dat_keep <- new_option %>%
        mutate(Option = case_when(slot_limit == "Y" ~ paste0("slot ", min_size, " to ", max_size, " ", units, " | ", "F/M = ", fishing_pressure),
                                  slot_limit == "N" ~ paste0(min_size, " ", units, " | ", "F/M = ", fishing_pressure))) %>%
        dplyr::select(Option, slot_limit, min_size, max_size, species, sustainability_score, catch_score) %>%
        mutate(Notes = NA,
               Ranking = 0)
      
      # Update list of all user-defined options
      retainedData$all <- isolate(retainedData$all) %>%
        bind_rows(dat_keep)
    
  }, ignoreInit = T)
  
  ##### EXAMPLE 1 --------------------------------------------------------------
  
  ### Open modal when user has clicked on a row in the table
  observeEvent(input$table_click, {
    
    # Identify selected row
    retainedData$selected_row <- as.numeric(input$table_click) + 1 #JS row ids start from 0
    req(retainedData$selected_row)
    
    # Update widget starting values to reflect what's currently in the table
    updateTextInput(session, inputId = "Notes",
                    value = retainedData$all[retainedData$selected_row, ]$Notes)

    updatePickerInput(session, inputId = "Ranking",
                      selected = retainedData$all[retainedData$selected_row, ]$Ranking)
    
    # Open modal
    toggleModal(session, "input_modal", toggle = "open")
    
  })
  
  ### Save notes and ranking value upon saving and close modal
  observeEvent(input$save, {
    
    # Get selected row
    selected_row <- retainedData$selected_row
    req(selected_row)
    
    # Add custom input notes
    retainedData$all[selected_row, ]$Notes <- input$Notes
    
    # Add rating
    retainedData$all[selected_row, ]$Ranking <- as.numeric(input$Ranking)
    
    # Close modal
    toggleModal(session, "input_modal", toggle = "close")

    # Reset selected row counter 
    retainedData$selected_row <- NULL
    
  })
  
  ### Delete selected row from retained options and close modal
  observeEvent(input$delete_row, {
      
      # Get selected row
      selected_row <- retainedData$selected_row
      req(selected_row)
      
      # Remove row from species data 
      retainedData$all <- isolate(retainedData$all)[-selected_row,]

      # Close modal
      toggleModal(session, "input_modal", toggle = "close")
      
      # Reset selected row value
      retainedData$selected_row <- NULL

  })
  
  ### UI output for the modal windown telling the user which option is being edited
  output$modal_title <- renderUI({
    
    selected_row <- retainedData$selected_row
    req(selected_row)
    
    tags$h3(paste0("Edit Option: ", retainedData$all[selected_row, "Option"]))
    
  })
    
  ### Example table #1 ---------
  output$exampleTable1 <- renderReactable({
    
    req(!is.null(retainedData$all))
    
    # Make donut charts
    plot_data <- retainedData$all %>%
      mutate(s_plot = map(sustainability_score, donut_chart),
             c_plot = map(catch_score, donut_chart, fill = c_color))
    
    # Create dummy df with placeholders for the table
    table_data <- plot_data %>%
      mutate(Edit = NA,
             s_plot = NA,
             c_plot = NA) %>%
      dplyr::select(Option, s_plot, c_plot, Notes, Ranking, Edit)
    
    req(nrow(table_data) > 0)
    
    # Make table
    reactable(
      table_data,
      
      # Options
      # This JS function converts click actions into a reactive variable that shiny recognizes
      onClick = JS("function(rowInfo, colInfo) {
    // Only handle click events on the 'Edit' column
    if (colInfo.id !== 'Edit') {
      return
    }
    // Send the click event to Shiny
    if (window.Shiny) {
      Shiny.setInputValue('table_click', rowInfo.id, {priority: 'event'})
    }
  }"),
      highlight = F, compact = T, style = list(color = "#000000"),
      pagination = T, showPageSizeOptions = TRUE, paginationType = "simple", # pagination
      sortable = FALSE, # Columns are not sortable by default
      showSortable = T, # Allow specified columns (Ranking) to be sortable

      # Cell contents
      columns = list(
        Option = colDef(align = "left", cell = function(value, index) {
          size_range_chart(label = value, 
                           slot_limit = plot_data$slot_limit[[index]], 
                           min_size = plot_data$min_size[[index]], 
                           max_size = plot_data$max_size[[index]])
        }),
        s_plot = colDef(name = "Sustainability Score", align = "center", cell = function(value, index) {
          p <- htmltools::plotTag(plot_data$s_plot[[index]], alt="plots", width = 70, height = 70)
          return(p)
        }),
        c_plot = colDef(name = "Catch Score", align = "center", cell = function(value, index) {
          p <- htmltools::plotTag(plot_data$c_plot[[index]], alt="plots", width = 70, height = 70)
          return(p)
        }),
        Ranking = colDef(align = "center", sortable = T, cell = function(value) rating_stars(value)), # Bill - If you decide to do away with the sorting, delete the 'sortable = T' call here. 
        Edit = colDef(align = "center", cell = function(){
          tags$div(id = "edit_button", icon("pen-to-square")) # Bill - CSS for this "button" is in the main.css file
        })
      ),
      
      # Vertically center align cell contents
      defaultColDef = colDef(vAlign = "center", headerVAlign = "center")
    )
    
  })
  
  ##### Example table #2 -------------------------------------------------------
  
  output$exampleTable2 <- renderReactable({
    
    req(!is.null(retainedData$all))
    
    # Get retained options for selected species
    table_data <- retainedData$all %>%
      dplyr::select(-slot_limit, -min_size, -max_size, -species)
    
    req(nrow(table_data) > 0)

    # Make table 
    reactable(
      table_data,
      
      # Options
      selection = "multiple", onClick = "select", highlight = T, style = list(color = "#000000"),
      pagination = T, showPageSizeOptions = TRUE, paginationType = "simple", 
      
      # Define custom cell contents by column
      columns = list(
        sustainability_score = colDef(name = "Sustainability Score", align = "left", cell = function(value) {
          width <- paste0(value, "%")
          bar_chart(value, width = width)
        }),
        catch_score = colDef(name = "Catch Score", align = "left", cell = function(value) {
          width <- paste0(value, "%")
          bar_chart(value, width = width, fill = c_color)
        })
      ),
      
      # Vertically center align cell contents
      defaultColDef = colDef(vAlign = "center", headerVAlign = "center")
    )
    
  })
  
  ##### Example table #3 -------------------------------------------------------
  
  output$exampleTable3 <- renderReactable({

    req(!is.null(retainedData$all))
    
    # Get retained options for selected species
    table_data <- retainedData$all %>%
      dplyr::select(-slot_limit, -min_size, -max_size, -species)
      
    
    req(nrow(table_data) > 0)
    
    # Color scales
    s_pal <- function(x) rgb(colorRamp(c("#ffffff", s_color))(x), maxColorValue = 255)
    c_pal <- function(x) rgb(colorRamp(c("#ffffff", c_color))(x), maxColorValue = 255)

    # Make table
    reactable(
      table_data,
      
      # Options
      selection = "multiple", onClick = "select", highlight = T, style = list(color = "#000000"),
      pagination = T, showPageSizeOptions = TRUE, paginationType = "simple", 
      
      # Define custom cell contents by column
      columns = list(
        sustainability_score = colDef(name = "Sustainability Score", align = "center", style = function(value) {
          normalized <- (value) / 100
          color <- s_pal(normalized)
          list(background = color)
        }),
        catch_score = colDef(name = "Catch Score", align = "center", style = function(value) {
          normalized <- (value) / 100
          color <- c_pal(normalized)
          list(background = color)
        })
      ),
      
      # Vertically center align cell contents
      defaultColDef = colDef(vAlign = "center", headerVAlign = "center")
    )

  })
  
  ##### Example table #4 -------------------------------------------------------
  
  output$exampleTable4 <- renderReactable({
    
    req(!is.null(retainedData$all))
    
    # Get retained options for selected species
    table_data <- retainedData$all %>%
      dplyr::select(-slot_limit, -min_size, -max_size, -species)
    
    req(nrow(table_data) > 0)
    
    # Color scales
    s_pal <- function(x) rgb(colorRamp(c("#ffffff", s_color))(x), maxColorValue = 255)
    c_pal <- function(x) rgb(colorRamp(c("#ffffff", c_color))(x), maxColorValue = 255)
    
    # Make table
    reactable(
      table_data,
      
      # Options
      selection = "multiple", onClick = "select", highlight = T, style = list(color = "#000000"),
      pagination = T, showPageSizeOptions = TRUE, paginationType = "simple", 
      
      # Define custom cell contents by column
      columns = list(
        sustainability_score = colDef(name = "Sustainability Score", align = "center", cell = function(value) {
          b <- status_badge(value, color = s_color, background = s_pal(value/100), border_color = s_color)
          tagList(b)
        }),
        catch_score = colDef(name = "Catch Score", align = "center", cell = function(value) {
          b <- status_badge(value, color = "#fff", background = c_pal(value/100), border_color = c_color)
          tagList(b)
        })
      ),
      
      # Vertically center align cell contents
      defaultColDef = colDef(vAlign = "center", headerVAlign = "center")
    )
    
  })
  
  ##### Example table #5 -------------------------------------------------------
  
  output$exampleTable5 <- renderReactable({
    
    req(!is.null(retainedData$all))
    
    # Get retained options for selected species
    table_data <- retainedData$all %>%
      dplyr::select(-slot_limit, -min_size, -max_size, -species) %>%
      mutate(sustainability_score = map(sustainability_score, function(x){c(x, 100-x)}),
             catch_score = map(catch_score, function(x){c(x, 100-x)})) %>%
      relocate(Notes, .after = last_col()) %>%
      relocate(Ranking, .after = last_col())
    
    req(nrow(table_data) > 0)
    
    # Make table
    reactable(
      table_data,
      
      # Options
      selection = "multiple", onClick = "select", highlight = T, style = list(color = "#000000"),
      pagination = T, showPageSizeOptions = TRUE, paginationType = "simple", 
      
      # Define custom cell contents by column
      columns = list(
        sustainability_score = colDef(name = "Sustainability Score", align = "center", cell = function(value, index) {
          sparkline(value, chart_type = "pie", height = 50)
        }),
        catch_score = colDef(name = "Catch Score", align = "center", cell = function(value, index) {
          sparkline(value, chart_type = "pie", height = 50)
        })
      ),
      
      # Vertically center align cell contents
      defaultColDef = colDef(vAlign = "center", headerVAlign = "center")
    )
    
  })
  
  ##### Test report ------------------------------------------------------------
  
  # ### NOTE - Bill, this isn't currently working - The table isn't visable for some reason when doing rmarkdown::render(), but it works if you manually knit the file. 
  # 
  # #Download report
  # output$testReport <- downloadHandler(
  # 
  #   filename = "test_report.pdf",
  #   content = function(file) {
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     tempReport <- file.path(tempdir(), "test_report.Rmd")
  #     file.copy("markdown/test_report.Rmd", tempReport, overwrite = TRUE)
  #     
  #     retained_options <- retainedData$all
  #     tempFile <- file.path(tempdir(), "retained_options.Rdata")
  #     save(retained_options, file = tempFile)
  #     
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     
  #     rmarkdown::render(tempReport, 
  #                       output_file = file,
  #                       params = list(
  #                         dat_path = tempFile
  #                       ),
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )

}