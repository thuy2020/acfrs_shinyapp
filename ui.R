library("DT")
library(shiny)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(htmltools)
library(fresh)
navbarPage(
 title = "Government Finance Data Transparency", 
 header = use_theme(
   create_theme(
     theme = "united", 
     bs_vars_navbar(
       padding_horizontal = "20px",
       padding_vertical = "50px",
       default_bg = "#153D77",
       default_color = "#FFFFFF",
       default_link_color = "#FFFFFF",
       default_link_active_color = "#FFFFFF"
     
   )
 )),
 
  tabPanel("Home",
          
           fluidPage(
             wellPanel("Debt bubbles in 50 states and DC, top 100 counties, and top 100 cities", 
                       style = "background: white"), 
             plotlyOutput("all_entities_liab_rev_ratio"),
             plotOutput("top4_cities"),
             plotlyOutput("revevue_per_cap_50cities_chart"))),
  
  
  navbarMenu("Analysis",
   tabPanel("States",
      fluidPage(
        plotOutput("states_ratio_chart", width = "80%"), 
        plotOutput("states_chart"), 
      
    )),
  
   tabPanel("Cities",
      fluidPage(
        plotOutput("cities_chart"),
        DTOutput("cities")
    )),
   
   tabPanel("Counties",
            fluidPage(
              plotOutput("counties_chart"),
              DTOutput("counties")
            )),
   
   tabPanel("Districts",
            fluidPage(
              plotOutput("")
            ))
  ),
  tabPanel(
    "Data",
    fluidPage(
      wellPanel(
        "The table below shows data of states",
        p("Click the download button below to download the data to your machine")
      ),
      selectInput("select_entity",
                  label = "Choose an entity:",
                  choices = NULL),
      
      plotOutput("entity_components"),
      
      downloadButton(
        "download_data",
        "Download data"
      ),
      hr(),
      DTOutput("states")
      
    )
    
  ),
  tabPanel(
    "Blogs",
    fluidPage()
),

tabPanel(
  "About",
  fluidPage(
    wellPanel("This is an awesome project!")
  )
),
collapsible = TRUE

)


# States
# counties
# Cities
# Special Districts
# School Districts
# Community College Districts

