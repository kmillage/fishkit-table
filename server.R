
donut_theme <- theme_void()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )


server <- function(input, output, session) {
  
  # Container storing retained user-defined options
  retainedData <- reactiveValues(all_species = NULL)
  
  # Add a randomly drawn entry to the retained data object when the "Retain option:" button is pressed
  observeEvent(input$addRow, {
    
    new_option <- fake_data[sample(nrow(fake_data %>% filter(species == "Example fish")), 1), ]
    
    # Format retained options for display and make charts
    dat_keep <- new_option %>%
      mutate(Option = case_when(slot_limit == "Y" ~ paste0("slot ", min_size, " to ", max_size, " ", units, " | ", "F/M = ", fishing_pressure),
                                slot_limit == "N" ~ paste0(min_size, " ", units, " | ", "F/M = ", fishing_pressure))) %>%
      dplyr::select(Option, species, sustainability_score, catch_score)
    
    # Add to retained data container
    retainedData$all_species <- isolate(retainedData$all_species) %>%
      bind_rows(dat_keep)
    
  })
  
  ### Plotting functions for table --------
  
  # Render a bar chart with a label on the left
  bar_chart <- function(label, width = "100%", height = "1rem", fill = "#00bfc4", background = "#EEEEEE") {
    bar <- div(style = list(background = fill, width = width, height = height))
    chart <- div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
    div(style = list(display = "flex", alignItems = "center", color = fill), 
        paste0(label, "%"), # text 
        chart) # chart
  }
  
  ### Example table #1 ---------
  output$exampleTable1 <- renderReactable({
    
    req(!is.null(retainedData$all_species))
    
    # Get retained options for selected species
    
    species_data <- retainedData$all_species %>%
      dplyr::filter(species == "Example fish") %>%
      dplyr::select(-species)
    
    req(nrow(species_data) > 0)

    reactable(
      species_data, # Data
      selection = "single", onClick = "select", highlight = TRUE, showSortable = TRUE, pagination = T, showPageSizeOptions = TRUE, paginationType = "simple", # options
      columns = list(
        sustainability_score = colDef(name = "Sustainability Score", align = "left", cell = function(value) {
          width <- paste0(value, "%")
          bar_chart(value, width = width)
        }),
        catch_score = colDef(name = "Catch Score", align = "left", cell = function(value) {
          width <- paste0(value, "%")
          bar_chart(value, width = width, fill = "#0096D6")
        })
      ),
      style = list(color = "#000000")
    )
    
  })
  
  ### Example table #2 ---------
  output$exampleTable2 <- renderReactable({

    req(!is.null(retainedData$all_species))

    # Get retained options for selected species

    species_data <- retainedData$all_species %>%
      dplyr::filter(species == "Example fish") %>%
      dplyr::select(-species) %>%
      group_by(Option) %>%
      summarize(sustainability_score = list(c(unique(sustainability_score), 100-unique(sustainability_score))),
                catch_score = list(c(unique(catch_score), 100-unique(catch_score))))

    req(nrow(species_data) > 0)

    # Make table
    reactable(
      species_data,
      columns = list(
        sustainability_score = colDef(name = "Sustainability Score", align = "left", cell = function(value) {
          sparkline(value, chart_type = "pie", height = 50)
        }),
        catch_score = colDef(name = "Catch Score", align = "left", cell = function(value) {
          sparkline(value, chart_type = "pie", height = 50)
        }),
      ),
      style = list(color = "#000000")
    )

  })
  
  ### Example table #3 ---------
  output$exampleTable3 <- renderReactable({

    req(!is.null(retainedData$all_species))

    # Get retained options for selected species

    species_data <- retainedData$all_species %>%
      dplyr::filter(species == "Example fish") %>%
      dplyr::select(-species)

    req(nrow(species_data) > 0)
    
    # Color scale
    blue_pal <- function(x) rgb(colorRamp(c("#ffffff", "#00bfc4"))(x), maxColorValue = 255)

    # Make table
    reactable(
      species_data,
      selection = "single", onClick = "select", highlight = TRUE, compact = T, # Selection options
      columns = list(
        sustainability_score = colDef(style = function(value) {
          normalized <- (value) / 100
          color <- blue_pal(normalized)
          list(background = color)
        })
      ),
      style = list(color = "#000000")
    )

  })
  
  ### Example table #4 ---------
  output$exampleTable4 <- renderReactable({
    
    req(!is.null(retainedData$all_species))
    
    # Get retained options for selected species
    
    # species_data <- retainedData$all_species %>%
    #   dplyr::filter(species == "Example fish") %>%
    #   dplyr::select(-species) %>%
    #   mutate()
    
    df = iris %>%
      as_tibble() %>%
      nest_by(Species) %>%
      mutate(s_plot = list(
        ggplot(data, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point() + ggtitle(Species)
      ))
    
    df2 <- df %>% dplyr::select(Species) %>%
      mutate(s_plot = unique(Species))
    
    req(nrow(df) > 0)
    
    # Make table
    reactable(
      df2,
      selection = "single", onClick = "select", highlight = TRUE, compact = T, style = list(color = "#000000"), # Options
      columns = list(
        s_plot = colDef(cell = function(index) {
          p <- htmltools::plotTag(df$s_plot[[index]], alt="plots")
          return(p)
        })
      )
    )
    
  })
  

    # Table
    # 
    # datatable(data = display_data, 
    #           options = list(paging = TRUE,    ## paginate the output
    #                          pageLength = 10,  ## number of rows to output for each page
    #                          #scrollX = TRUE,   ## enable scrolling on X axis
    #                          scrollY = TRUE,   ## enable scrolling on Y axis
    #                          autoWidth = TRUE, ## use smart column width handling
    #                          server = FALSE,   ## use client-side processing
    #                          dom = "ftip",
    #                          columnDefs = list(list(targets = '_all', className = 'dt-center'))
    #           ),
    #           #extensions = 'Buttons',
    #           selection = 'single', ## enable selection of a single row
    #           #filter = 'bottom', ## include column filters at the bottom
    #           rownames = FALSE ## don't show row numbers/names
    # ) # end of datatables
}