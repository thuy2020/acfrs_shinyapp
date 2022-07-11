library("DT")
library(shiny)
library(shinydashboard)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(htmltools)
library(fresh)
library(markdown)
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
             
             plotOutput("states_ratio_chart", width = "80%"),
             wellPanel("Debt bubbles in 50 states and DC, top 100 counties, and top 100 cities"),
             plotlyOutput("all_entities_liab_rev_ratio"),
             wellPanel("Something important about these charts."),
             fluidRow(
               column(6, plotOutput("liabilities_by_gov_cat_pie")),
               column(6, plotOutput("liabilities_by_type_pie"))
             )
             )),
  
  ########## Begin - Analysis
  navbarMenu("Analysis",
   tabPanel("States",
      fluidPage(
        plotOutput("top10_bottom10_states"), 
    )),
   tabPanel("Counties",
            fluidPage(
              plotOutput("counties_chart")
              #DTOutput("counties")
            )),
   tabPanel("Cities",
      fluidPage(
        plotOutput("top100_cities_ratio"),
        wellPanel("Why do we see what we see above? Some insightful explanation here."),
        plotlyOutput("revevue_per_cap_50cities_chart"),
        wellPanel("Description of the chart. 
                  Why do we see what we see above? Some insightful explanation here."),
        plotOutput("top4_cities"),
        plotlyOutput("fisc_cities_less2Mpop_chart"),
        wellPanel("Description of the chart. 
                  Why do we see what we see above? Some insightful explanation here.")
        
    )),

   tabPanel("Districts",
            fluidPage(
              plotOutput("")
            )),
   tabPanel("Special Districts",
            fluidPage(
              plotOutput("")
            )),
   tabPanel("School Districts",
            fluidPage(
              plotOutput("")
            )),
   tabPanel("Community College Districts",
            fluidPage(
              plotOutput("")
            ))
   ),
 
 #########End - Analysis
 ####### Begin - Data
  navbarMenu("Data",
    ##1
    tabPanel("Data Exploration",
              # Chart part----------------         
            fluidPage(
              titlePanel("Data Exploration"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("select_type_government", "Select a Type of Government", choices = NULL),
                  textInput("name", "Enter Name", "State of Arizona")
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Overall Plot"),
                    tabPanel("Entity Plot", plotOutput("state_gov_vs_mean_chart")),
                    tabPanel("Data Table", DTOutput(""))
                  )))
              ),
      
            # Data part----------------
          fluidPage(
            wellPanel("Entity Plot: Absolute value of an entity compared to its group type of government."),
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
             p("Category: Type of Government:  General Purpose (e.g.  a state, county, city, town, village or borough), 
               School District, Special District, Community College District or Public Higher Education"))
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
    includeMarkdown("test.md")
  )),

  tabPanel("Q&A",
           
           fluidPage(
             tags$h3("Answers to many of your questions"),
              tags$h4("How was the data collected?"),
              p("A legit answer.")
             
           )  
           )),
  
 ########End - About
  
collapsible = TRUE

)




