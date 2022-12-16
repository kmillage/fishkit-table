
### Plotting functions and helpers --------

# Colors
s_color <- "#005387"
c_color <- "#0096D6"
grey_color <- "#EEEEEE"

# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "1rem", fill = s_color, background = grey_color) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
  div(style = list(display = "flex", alignItems = "center", color = fill), 
      paste0(label, "%"), # text 
      chart) # chart
}

# Render a donut chart with a label in the middle
donut_theme <- theme_void()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(color = NA, fill = NA),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

donut_chart <- function(value, fill = s_color, background = grey_color) {
  
  d <- tibble(category = c("value", "extra"),
              values = c(value, 100 - value)) %>%
    mutate(ymax = cumsum(values),
           ymin = c(0, head(ymax, n=-1)))
  
  p <- ggplot(d)+
    aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category) +
    geom_rect() +
    coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
    donut_theme +
    scale_fill_manual(values = c(background, fill))+
    xlim(.5, 4) +
    annotate(geom = 'text', x = 0.5, y = 0, label = paste0(value, "%"))
  return(p)
}


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
  
  ### Example table #1 ---------
  output$exampleTable1 <- renderReactable({
    
    req(!is.null(retainedData$all_species))
    
    # Get retained options for selected species
    species_data <- retainedData$all_species %>%
      dplyr::filter(species == "Example fish") %>%
      dplyr::select(-species) %>%
      #distinct(Option, sustainability_score, catch_score) %>%
      mutate(s_plot = map(sustainability_score, donut_chart),
             c_plot = map(catch_score, donut_chart, fill = c_color))
    
    # Create dummy df with placeholders for the table
    table_data <- species_data %>%
      dplyr::select(Option) %>%
      mutate(s_plot = NA,
             c_plot = NA,
             Notes = NA)
    
    req(nrow(table_data) > 0)
    
    # Make table
    reactable(
      table_data,
      # Options
      selection = "single", onClick = "select", highlight = TRUE, compact = T, style = list(color = "#000000"),
      # Cell contents
      columns = list(
        s_plot = colDef(name = "Sustainability Score", align = "center", cell = function(value, index) {
          p <- htmltools::plotTag(species_data$s_plot[[index]], alt="plots", width = 70, height = 70)
          return(p)
        }),
        c_plot = colDef(name = "Catch Score", align = "center", cell = function(value, index) {
          p <- htmltools::plotTag(species_data$c_plot[[index]], alt="plots", width = 70, height = 70)
          return(p)
        })
      )
    )
    
  })
  
  ### Example table #2 ---------
  output$exampleTable2 <- renderReactable({
    
    req(!is.null(retainedData$all_species))
    
    # Get retained options for selected species
    species_data <- retainedData$all_species %>%
      dplyr::filter(species == "Example fish") %>%
      dplyr::select(-species)
    
    req(nrow(species_data) > 0)

    # Make table 
    reactable(
      species_data,
      # Options
      selection = "single", onClick = "select", highlight = TRUE, showSortable = TRUE, pagination = T, showPageSizeOptions = TRUE, paginationType = "simple", style = list(color = "#000000"),
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
      )
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
    
    # Color scales
    s_pal <- function(x) rgb(colorRamp(c("#ffffff", s_color))(x), maxColorValue = 255)
    c_pal <- function(x) rgb(colorRamp(c("#ffffff", c_color))(x), maxColorValue = 255)

    # Make table
    reactable(
      species_data,
      # Options
      selection = "single", onClick = "select", highlight = TRUE, compact = T, style = list(color = "#000000"),
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
      )
    )

  })
  
  ### Example table #4 ---------
  output$exampleTable4 <- renderReactable({
    
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
      # Options
      style = list(color = "#000000"),
      # Define custom cell contents by column
      columns = list(
        sustainability_score = colDef(name = "Sustainability Score", align = "center", cell = function(value) {
          sparkline(value, chart_type = "pie", height = 50)
        }),
        catch_score = colDef(name = "Catch Score", align = "center", cell = function(value) {
          sparkline(value, chart_type = "pie", height = 50)
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