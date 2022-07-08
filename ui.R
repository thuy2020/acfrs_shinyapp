library("DT")
library(shiny)
library(shinydashboard)
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
   ))),
 ########## Begin - Home
  tabPanel("Home",
           fluidPage(
             wellPanel("Debt bubbles in 50 states and DC, top 100 counties, and top 100 cities"), 
             plotlyOutput("all_entities_liab_rev_ratio"),
             plotOutput("top4_cities"),
             plotlyOutput("revevue_per_cap_50cities_chart"))),
  
  ########## Begin - Analysis
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
            ))),
 #########End - Analysis
 ####### Begin - Data
  navbarMenu("Data",
    ##1
    tabPanel("Data Exploration",
              # Chart part----------------         
            fluidPage(
              titlePanel("Data Exploration"),
              fluidRow(column(selectInput("select_entity_type", "Select a Type of Government",
                                          choices = NULL), width = 4),
              ##
              sidebarLayout(
                sidebarPanel(
                  textInput("name", "Enter Name", "State of Arizona")
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Plot", plotOutput("state_gov_vs_mean_chart")),
                    tabPanel("Table", DTOutput(""))
                  )))
              )),
      
            # Data part----------------
          fluidPage(
            # fluidRow(column(selectInput("select_entity_type", "Select a Type of Government",
            #                    choices = NULL), width = 4),
            #         column(selectInput("select_indicator","Select an Indicator",
            #                     choices = NULL), width = 4)),
            wellPanel("Search by name of state, entity or category"),
            downloadButton(
              "download_data", "Download data"
            ),
            hr(),
            DTOutput("acfrs")
          )),
  
  ##2
  tabPanel("Codebook",
           fluidPage(
             tags$h3("Description of each field in the dataset"),
             p("Category: Type of Government:  General Purpose (e.g.  a state, county, city, town, village or borough), School District, Special District, Community College District or Public Higher Education"))
           ),
  ##3
  tabPanel("Methodology",
           fluidPage(
             tags$h3("How we collected the data"),
           )
           )),
  
  ########## End - Data
 ########Begin - About
 navbarMenu("About",
    tabPanel("Government Finance Data Transparency Project",
  
  fluidPage(
    tags$h3("What is Government Finance Data Transparency Project?"),
    p("A very important project!"),
    
  )),

  tabPanel("Q&A",
           
           fluidPage(
             tags$h3("Answers to many of your questions"),
             p("A legit answer."),
           )  
           )),
  
 ########End - About
  
collapsible = TRUE

)


# States
# counties
# Cities
# Special Districts
# School Districts
# Community College Districts

