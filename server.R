
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
    annotate(geom = 'text', x = 0.5, y = 0, label = paste0(value, "%"), color = fill)
  return(p)
}

# Star making function
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

# Little colored badges
status_badge <- function(value, color = "#aaa", background = "#fff", border_color = "#aaa", width = "80px", height = "25px") {
  tags$div(style = paste0("display: inline-block; padding: 0.125rem 0.75rem; font-weight: 600; font-size: 0.75rem; border: 2px solid ", border_color, "; border-radius: 15px; color:", color, "; background:", background, "; width:", width, "; height: ", height, ";"),
           paste0(value, "%"))
}

server <- function(input, output, session) {
  
  # Container storing retained user-defined options
  retainedData <- reactiveValues(all = NULL)
  
  # Add a randomly drawn entry to the retained data object when the "Retain option:" button is pressed
  observeEvent(input$addRow, {
    
      # Randomly draw a row from our fake dataset to simulate user selected options
      new_option <- fake_data[sample(nrow(fake_data), 1), ]
      
      # Format retained options for display and make charts
      dat_keep <- new_option %>%
        mutate(Option = case_when(slot_limit == "Y" ~ paste0("slot ", min_size, " to ", max_size, " ", units, " | ", "F/M = ", fishing_pressure),
                                  slot_limit == "N" ~ paste0(min_size, " ", units, " | ", "F/M = ", fishing_pressure))) %>%
        dplyr::select(Option, species, sustainability_score, catch_score) %>%
        mutate(Notes = NA,
               Ranking = 0)
      
      # Update list of all user-defined options
      retainedData$all <- isolate(retainedData$all) %>%
        bind_rows(dat_keep)
    
  }, ignoreInit = T)
  
  # Delete selected row from retained options
  observeEvent(input$deleteRow, {
      
      # Get selected row
      selected_row <- getReactableState("exampleTable1", "selected")
      
      # Remove row from species data 
      retainedData$all <- isolate(retainedData$all)[-selected_row,]

  }, ignoreInit = T)
  
  # Add notes to the selected row
  observeEvent(input$addNotes, {
    
    # Get selected row
    selected_row <- getReactableState("exampleTable1", "selected")
    
    # Add custom input notes
    retainedData$all[selected_row, "Notes"] <- input$Notes
    
  }, ignoreInit = T)
  
  # Add ranking to the selected row
  observeEvent(input$addRanking, {
    
    # Get selected row
    selected_row <- getReactableState("exampleTable1", "selected")
    
    # Add custom input notes
    retainedData$all[selected_row, "Ranking"] <- input$Ranking
    
  }, ignoreInit = T)
  
  ### Example table #1 ---------
  output$exampleTable1 <- renderReactable({
    
    req(!is.null(retainedData$all))
    
    # Make donut charts
    plot_data <- retainedData$all %>%
      mutate(s_plot = map(sustainability_score, donut_chart),
             c_plot = map(catch_score, donut_chart, fill = c_color))
    
    # Create dummy df with placeholders for the table
    table_data <- plot_data %>%
      dplyr::select(Option, Notes, Ranking) %>%
      mutate(s_plot = NA,
             c_plot = NA) %>%
      relocate(Notes, .after = last_col()) %>%
      relocate(Ranking, .after = last_col())
    
    req(nrow(table_data) > 0)
    
    # Make table
    reactable(
      table_data,
      
      # Options
      selection = "single", onClick = "select", highlight = F, compact = T, style = list(color = "#000000"),
      pagination = T, showPageSizeOptions = TRUE, paginationType = "simple", # pagination
      sortable = FALSE, # So not all columns are sortable
      showSortable = TRUE, # Allow some columns to be sortable
      
      # Cell contents
      columns = list(
        s_plot = colDef(name = "Sustainability Score", align = "center", cell = function(value, index) {
          p <- htmltools::plotTag(plot_data$s_plot[[index]], alt="plots", width = 70, height = 70)
          return(p)
        }),
        c_plot = colDef(name = "Catch Score", align = "center", cell = function(value, index) {
          p <- htmltools::plotTag(plot_data$c_plot[[index]], alt="plots", width = 70, height = 70)
          return(p)
        }),
        Ranking = colDef(sortable = T, align = "center", cell = function(value) rating_stars(value))
      ),
      
      # Vertically center align cell contents
      defaultColDef = colDef(vAlign = "center", headerVAlign = "center")
    )
    
  })
  
  ### Example table #2 ---------
  output$exampleTable2 <- renderReactable({
    
    req(!is.null(retainedData$all))
    
    # Get retained options for selected species
    table_data <- retainedData$all
    
    req(nrow(table_data) > 0)

    # Make table 
    reactable(
      table_data %>% dplyr::select(-species),
      
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
  
  ### Example table #3 ---------
  output$exampleTable3 <- renderReactable({

    req(!is.null(retainedData$all))
    
    # Get retained options for selected species
    table_data <- retainedData$all
    
    req(nrow(table_data) > 0)
    
    # Color scales
    s_pal <- function(x) rgb(colorRamp(c("#ffffff", s_color))(x), maxColorValue = 255)
    c_pal <- function(x) rgb(colorRamp(c("#ffffff", c_color))(x), maxColorValue = 255)

    # Make table
    reactable(
      table_data %>% dplyr::select(-species),
      
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
  
  ### Example table #4 ---------
  output$exampleTable4 <- renderReactable({
    
    req(!is.null(retainedData$all))
    
    # Get retained options for selected species
    table_data <- retainedData$all
    
    req(nrow(table_data) > 0)
    
    # Color scales
    s_pal <- function(x) rgb(colorRamp(c("#ffffff", s_color))(x), maxColorValue = 255)
    c_pal <- function(x) rgb(colorRamp(c("#ffffff", c_color))(x), maxColorValue = 255)
    
    # Make table
    reactable(
      table_data %>% dplyr::select(-species),
      
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
  
  ### Example table #5 ---------
  output$exampleTable5 <- renderReactable({
    
    req(!is.null(retainedData$all))
    
    # Get retained options for selected species
    table_data <- retainedData$all %>%
      mutate(sustainability_score = map(sustainability_score, function(x){c(x, 100-x)}),
             catch_score = map(catch_score, function(x){c(x, 100-x)})) %>%
      relocate(Notes, .after = last_col()) %>%
      relocate(Ranking, .after = last_col())
    
    req(nrow(table_data) > 0)
    
    # Make table
    reactable(
      table_data %>% dplyr::select(-species),
      
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

}