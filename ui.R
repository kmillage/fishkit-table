

#------------------------
#Header
#------------------------

header <- dashboardHeader(
  
  #Branding for the app - client logo, link to website, etc.
  title = dashboardBrand(
    title = "Your brand",
    color = "primary",
    href = "https://www.google.com",
    image = "https://natureanalytics.ca/wp-content/uploads/2021/09/Your-logo-example-e1632748674383.png",
  ),
  
  #Icon to open/close sidebar
  sidebarIcon = icon("compress"),
  
  #Icon for right control bar
  controlbarIcon = shiny::icon("th")
)


#------------------------
#Sidebar
#------------------------

sidebar <- dashboardSidebar(
  
  #----------------
  #Settings
  #---------------
  minified = TRUE,
  collapsed = FALSE,

  #----------------
  #NA footer
  #----------------
  shiny::tags$div(
    style = "position: absolute; bottom: 4px;", 
    tags$a(href="https://natureanalytics.ca", 
           tags$img(src="NA.png",
                    height=60,
                    width=60,
           ),
           target="_blank")
  ),
  
  #-------------
  #Items
  #-------------
  sidebarMenu(
    menuItem(
      tabName = "home",
      text = "Dashboard",
      icon = icon("home")
    )
  )
)


#----------------------------------
#Body
#----------------------------------

body<-dashboardBody(
  
  #---------------
  #Call to
  #---------------
  useShinyjs(),
  
  
  #----------------------
  #Read styling items
  #----------------------
  use_theme(inputTheme),
  inputSliderSkin,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
  ),
  
  
  tabItems(
    tabItem(
      tabName = "home",
      interactiveTable()
    )
  )
)

#-----------------
#Dashboard Page
#-----------------

bs4Dash::dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  #footer = footer,
  dark = FALSE
)



