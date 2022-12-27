
### Plotting functions and helpers --------

# Colors
s_color <- "#005387"
c_color <- "#0096D6"
grey_color <- "#EEEEEE"
orange_color <- "#E88727"

# Render a bar chart with a label on the left - this is the simple version used in Example Table 2
bar_chart <- function(label, width = "100%", height = "1rem", fill = s_color, background = grey_color) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
  div(style = list(display = "flex", alignItems = "center", color = fill), 
      paste0(label, "%"), # text 
      chart) # chart
}

# Render a bar chart showing the size range (as a function of the species' Lmax) that can be harvested 
size_range_chart <- function(label, height = "0.5rem", fill = orange_color, background = grey_color, slot_limit = "N", min_size = 50, max_size = 100, Lmin = 0, Lmax = 100) {
  
  if(slot_limit == "N"){
    
    percent_out <- ((min_size)/(Lmax-Lmin))*100
    out_bar <- div(style = list(background = background, width = paste0(round(percent_out, 2), "%"), height = height))
    bar <- div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = fill), out_bar)
    tagList(tags$p(label),
            bar)
    
  }else if(slot_limit == "Y"){
    
    percent_low <- ((min_size)/(Lmax-Lmin))*100
    percent_high <- ((Lmax-max_size)/(Lmax-Lmin))*100
    percent_in <- 100-(percent_low+percent_high)
    out_bar_low <- div(style = list(background = background, width = paste0(round(percent_low, 2), "%"), height = height, float = "left"))
    in_bar <- div(style = list(background = fill, width = paste0(round(percent_in, 2), "%"), height = height, float = "left"))
    out_bar_high <- div(style = list(background = background, width = paste0(round(percent_high, 2), "%"), height = height, float = "left"))
    bar <- div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = fill), out_bar_low, in_bar, out_bar_high)
    tagList(tags$p(label),
            bar)
    
  }
}

# Donut chart with a label in the middle
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

# Theme for donut chart
donut_theme <- theme_void()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(color = NA_character_, fill = "transparent"),
    plot.background = element_rect(color = NA_character_, fill = "transparent"),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

# Stars filled based on the rating
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

# Little colored circular badges
status_badge <- function(value, color = "#aaa", background = "#fff", border_color = "#aaa", width = "80px", height = "25px") {
  tags$div(style = paste0("display: inline-block; 
                          padding: 0.125rem 0.75rem; 
                          font-weight: 600; 
                          font-size: 0.75rem; 
                          border: 2px solid ", border_color, "; 
                          border-radius: 15px; 
                          color:", color, "; 
                          background:", background, "; 
                          width:", width, "; 
                          height: ", height, ";"),
           paste0(value, "%"))
}

### UI --------

interactiveTable <- function(){
  fluidPage(
    
    # Modal for facilitator inputs
    bsModal("input_modal", title = "", trigger = "table_click", size = "large",
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
            ),
            # Add custom footer with different close button - This is perhaps uncessary, but I couldn't find another way to register that the user had closed the modal, which allows me to reset the selected row input. Without this, you can't click on the same row twice to edit it without first clicking on another row
            tags$head(tags$style("#input_modal .modal-footer{ display:none}")), # hide standard footer
            
            tags$hr(),
            
            # Now add our own button with an id that we can call on the server side
            column(2, offset = 10, align = "right",
                   actionBttn("close_modal", "Done")
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