
#-----------------
#UI Libraries
#---------------

library(shiny)
library(bs4Dash)
library(waiter)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(bsplus)
library(shinyjs)
library(shinybusy)
library(shinycssloaders)
library(fresh)
library(shinyalert)

#-------------------------
#App specific libraries
#-------------------------

library(tidyverse)
library(DT)
library(reactable)
#devtools::install_github("Bart6114/sparklines")
library(htmlwidgets)
library(sparklines)

# The content for each tab is stored in a separate file. Source all .R files in the current directory that start with "ui_":  
sapply(list.files(
  pattern = "^ui_.*\\.R$",
  path = ".",
  full.names = TRUE
),
source)

fake_data <- read_csv("www/fake_data.csv")

#------------------------------------------
# Setup theme (fresh library)
# Built on AdminLTE3
# Does not take bootstrap built-in themes
# NULL will retain default value
#------------------------------------------

inputTheme <- fresh::create_theme(

  #Custom variables
  bs4dash_vars(
    navbar_light_color = NULL,
    navbar_light_active_color = NULL,
    navbar_light_hover_color = NULL
  ),
  
  #Color contrast
  bs4dash_yiq(
    contrasted_threshold = 10,
    text_dark = "#FFF", #allows switch to white if not enough contrast
    text_light = "#272c30" #allow swicth to dark grey if not enough contrast
  ),
  
  #Layout options
  bs4dash_layout(
    font_size_root = NULL,
    sidebar_width = NULL,
    sidebar_padding_x = NULL,
    sidebar_padding_y = NULL,
    sidebar_mini_width = NULL,
    control_sidebar_width = NULL,
    boxed_layout_max_width = NULL,
    screen_header_collapse = NULL,
    main_bg = NULL,
    content_padding_x = NULL,
    content_padding_y = NULL
  ),
  
  #Sidebar skin
  bs4dash_sidebar_light(
    bg = NULL,
    hover_bg = NULL,
    color = NULL,
    hover_color = NULL,
    active_color = NULL,
    submenu_bg = NULL,
    submenu_color = NULL,
    submenu_hover_color = NULL,
    submenu_hover_bg = NULL,
    submenu_active_color = NULL,
    submenu_active_bg = NULL,
    header_color = NULL
  ),
  
  #Status custom colors
  bs4dash_status(
    primary = NULL,
    secondary = NULL,
    success = NULL,
    info = NULL,
    warning = NULL,
    danger = NULL,
    light = NULL,
    dark = NULL
  ),
  
  #Main custom colors
  bs4dash_color(
    blue = NULL,
    lightblue = NULL,
    navy = NULL,
    cyan = NULL,
    teal = NULL,
    olive = NULL,
    green = NULL,
    lime = NULL,
    orange = NULL,
    yellow = NULL,
    fuchsia = NULL,
    purple = NULL,
    maroon = NULL,
    red = NULL,
    black = NULL,
    gray_x_light = NULL,
    gray_600 = NULL,
    gray_800 = NULL,
    gray_900 = NULL,
    white = NULL
  ),
  
  #Button options
  bs4dash_button(
    default_background_color = NULL,
    default_color = NULL,
    default_border_color = NULL,
    padding_y_xs = NULL,
    padding_x_xs = NULL,
    line_height_xs = NULL,
    font_size_xs = NULL,
    border_radius_xs = NULL
  ),
  
  #Font options
  bs4dash_font(
    size_base = NULL,
    size_lg = NULL,
    size_sm = NULL,
    size_xs = NULL,
    size_xl = NULL,
    weight_light = NULL,
    weight_normal = NULL,
    weight_bold = NULL,
    family_sans_serif = NULL,
    family_monospace = NULL,
    family_base = NULL
  )
)

#------------------------------------
#Sliders
#Options:
#"Shiny", "Flat", "Big", "Modern", "Sharp", "Round", "Square", "Nice", "Simple", "HTML5"
#------------------------------------
inputSliderSkin<-chooseSliderSkin(
  skin = "Modern",
  color = "red" #Colors work with: 'Shiny', "Flat', 'Modern', 'HTML5'
)

#-----------------------------------------------------
#Footer - For client use (turned off in UI by default)
#-----------------------------------------------------
footer = dashboardFooter(
  left = div(
    fluidRow(
      h4()
  )),

  right = 
    tags$a(href="https://natureanalytics.ca", 
                tags$image(src="NA.png", 
                           height=60, 
                           width=60
                ),  
                target="_blank")
)
