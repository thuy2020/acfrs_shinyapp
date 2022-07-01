library("DT")
library("tidyverse")
library(ggrepel)
library(ggplot2)
library(dplyr)
library(readr)
library(viridis)
library(forcats)
library(broom)
library(geojsonio)
library(RColorBrewer)
library(rgeos)
library(plotly)
library(shiny)

options(scipen = 999)
acfrs <- readRDS("data/data_from_dbsite.RDS") %>% select(-c(census_id, id, year))
states <- read_csv("data/states.csv")
counties <- read_csv("data/counties_ranking.csv")
cities <- read_csv("data/cities_ranking.csv")
fisc_50cities <- rio::import("data/FiSC 50 Cities Final.xlsx", sheet = "Chart Data")

topstates_liabilities <- states %>% arrange(desc(lib_rev_ratio)) %>% slice(1:8)
bottomstate_liabilities <- states %>% arrange(lib_rev_ratio) %>% slice(1:8)
counties %>% 
  arrange(desc(lib_rev_ratio)) %>% slice(1:5) -> d 

function(input, output, session) {
  
  output$acfrs <- renderDT({
    acfrs %>%
      datatable(rownames = input$show_rownames,
                extensions = c("Buttons", "Responsive"),
                option = list(button = c("excel", "pdf"),dom = "Bftip"))
  })
  
  output$download_data <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(), "_acfrs.csv")},
    content = function(file) {
      acfrs %>% 
        write_csv(file)
    })
  
  output$counties <- renderDT({
    counties %>%
      select(-c(FIPS, state_abv)) %>% 
      datatable(rownames = input$show_rownames,
                extensions = "Responsive")
  })
  
  
  output$states_chart <- renderPlot({
    topstates_liabilities %>% rbind(bottomstate_liabilities) %>% 
      mutate(state = fct_reorder(state, lib_rev_ratio)) %>% 
      
      #layer 1
      ggplot(aes(state, lib_rev_ratio)) +
      geom_segment(aes(x = state, xend = state, y = 0, yend = lib_rev_ratio), color = "grey") +
      geom_point(aes(state, lib_rev_ratio), color = "orange", size = 10) +
      
      #layer2 
      geom_text(aes(label = round(lib_rev_ratio)), color = "white", size = 4) +
      
      
      # base
      geom_hline(yintercept = round(mean(states$lib_rev_ratio)),
                 linetype = "dashed", color = "#a63e37", size = .3) +
      annotate(geom = "text", x = 1, y = 100, label = paste("Mean"), color = "#a63e37") +
      
      geom_hline(yintercept = round(median(states$lib_rev_ratio)),
                 linetype = "dashed", color = "#377da6", size = .3) +
      annotate(geom = "text", x = 1, y = 60, label = paste("Median"), color = "#377da6") +
      
      #theme
      theme_minimal() +
      labs(
        x = "",
        y = "Ratio",
        title = "State governments with lowest and highest ratios of liabilities to revenue",
        subtitle = "",
        caption = "Note: Mean and median are values of all 50 states and DC")  +
      theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) 
    
    
  })
  
  
  output$cities <- renderDT({
    cities %>%
      arrange(desc(population)) %>%
      datatable(rownames = FALSE,
                extensions = "Responsive")
  })
  
  
  output$counties_chart <- renderPlot({
    counties %>% 
      filter(lib_rev_ratio != Inf & state != "Puerto Rico") %>% 
      ggplot(aes(population, lib_rev_ratio)) +
      geom_point(aes(size = population), color = "cornflowerblue", alpha = .3) +
      
      # add lines 
      geom_hline(yintercept = round(mean(counties$lib_rev_ratio)),
                 linetype = "dashed", color = "#a63e37", size = .5) +
      annotate(geom = "text", x = 5000000, y = 60, label = paste("Mean = ", round(mean(counties$lib_rev_ratio)))) +
      
      # top ones
      geom_point(aes(population, lib_rev_ratio), 
                 data = d,
                 color = "#a63e37") +
      
      geom_text_repel(aes(label = name), data = d,
                      nudge_y = 0.1, nudge_x = 0.1, segment.curvature = -0.2, color = "#a63e37", size = 6) +
      
      
      scale_x_log10() + 
      scale_fill_viridis(discrete=TRUE, guide="none", option="A") +
      #scale_color_viridis(option = "H") +
      labs(
        x = "Population (log scale)",
        y = "Ratio",
        title = "Ratios of Liabilities to Revenue",
        subtitle = "Counties", 
        caption = "Note: Excluding Puerto Rico") +
      guides(size = "none") +
      theme_minimal()
    
    
  })
  
  
  output$cities_chart <- renderPlot({
    region <- data.frame(state.name, state.region) %>% rename(state = state.name)
    top_liab_rev_ratio <- cities %>% arrange(desc(lib_rev_ratio)) %>% slice(1:4)
    
    bottom_liab_ratio <- cities %>% arrange(lib_rev_ratio) %>% slice(1:4)
    
    cities %>% left_join(region) %>% 
      ggplot(aes(population, lib_rev_ratio)) +
      geom_point(aes(size = population, color = state.region), alpha = .5) +
      
      # add lines 
      geom_hline(yintercept = round(mean(cities$lib_rev_ratio)),
                 linetype = "dashed", color = "gray", size = .5) +
      annotate(geom = "text", x = 6000000, y = 200, label = paste("Mean = ", round(mean(cities$lib_rev_ratio)))) +
      
      # top 4
      geom_text_repel(aes(label = city),
                      data = top_liab_rev_ratio, 
                      nudge_y = 0.1, nudge_x = 0.05, segment.curvature = -0.2, size = 5) +
      
      # bottom 4
      geom_text_repel(aes(label = city), 
                      data = bottom_liab_ratio,
                      nudge_y = 0.1, nudge_x = 0.05, segment.curvature = -0.1, size = 4) +
      
      
      scale_x_log10() +
      scale_fill_viridis(discrete=TRUE, guide= "none", option="A") +
      theme_minimal() +
      labs(
        x = "Population (log scale)",
        y = "Ratio",
        title = "Ratios of Liabilities to Revenue in Top 100 Cities",
        subtitle = "") +
      guides(size = "none", color = guide_legend("Region")) +
      theme(legend.position = "bottom") 
  })
  
  
  output$states_ratio_chart <- renderPlot({
    spdf <- geojson_read("data/us_states_hexgrid.geojson",  what = "sp")
    spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
    
    data <- states %>% select(state, lib_rev_ratio)
    
    spdf_fortified <- tidy(spdf, region = "google_name") %>% 
      left_join(., data, by = c("id" = "state"))
    
    # binning
    spdf_fortified$bin <- cut(spdf_fortified$lib_rev_ratio, 
                              breaks=5, include.lowest = TRUE, labels=c())
    
    # Pcolor scale coming from the viridis color palette
    my_palette <- rev(magma(12))[c(-1,-8)]
    
    
    # Calculate the centroid of each hexagon to add the label:
    centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))
    
    ggplot() +
      geom_polygon(data = spdf_fortified, aes(fill = bin, x = long, y = lat, group = group) , size=0, alpha=0.9) +
      geom_text(data=centers, aes(x=x, y=y, label=id), color="white", size=3, alpha=0.6) +
      theme_void() +
      scale_fill_manual(
        values=my_palette, guide = "none"
        #name="Scale",
        #guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"),
        #                     label.position = "bottom", title.position = 'bottom', nrow=1)
      ) +
      ggtitle( "Liabilities-to-revenue Ratio" ) +
      theme(
        legend.position = c(0.5, 1.1),
        text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA), 
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        plot.title = element_text(size= 20, hjust=0.5, color = "#a61c00", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
      )
  })
  

  
  output$revevue_per_cap_50cities_chart <- renderPlotly({

    ggplotly(fisc_50cities %>% 
               filter(Population <=2000000) %>% 
               ggplot(aes(Population, `Total General Revenue (with exclusions) 2020`, text = City)) +
               geom_point(color = "lightblue") +
               
               geom_point(aes(size = `General Revenue to Per Capita Income Ratio`), color = "#A6D753", alpha = .4) +
               
               
               scale_x_continuous(breaks = c(600000, 900000, 1200000, 1500000), 
                                  labels = c("600K", "900K", "1.2M", "1.5M")) +
               scale_y_continuous(breaks = c(3000000000, 6000000000, 9000000000),
                                  labels = c( "$3B", "$6B", "$9B"))+
               guides(size = "none") +
               
               labs(title = "Total General Revenue by Population (excluding the 4 Most Populated Cities)", 
                    caption = "Note: Outer cirles represent General Revenue to Per Capita Income Ratio") +
               
               theme_minimal() +
               theme(legend.title = element_blank(),
                     legend.position = "bottom")) 
    
  })
  
  
  output$top4_cities <- renderPlot({
    
    fisc_50cities %>% 
    filter(Population > 2000000) %>% 
    pivot_longer(cols = c(`Per Capita General Revenue excluding Grants`, `General Revenue Adjusted for Cost of Living Index`),
                 names_to = "Per Capita Revenue", 
                 values_to = "value") %>% 
    ggplot(aes(value, City, fill = `Per Capita Revenue`)) +
    
    geom_col(width = c(0.8, 0.5)) +
    scale_fill_manual(values = c("#E17D85", "#8EA0CB")) +
    
    geom_vline(xintercept = 6482.832, color = "#8EA0CB") +
    annotate("text", label = "Mean of 50 cities", x= 7100, y = 1.5, color = "#8EA0CB") +
    
    geom_vline(xintercept = 5949.024, color = "#E17D85") +
    annotate("text", label = "Mean of 50 cities adjusted", x = 6900, y = 2, color = "#E17D85")+
    
    labs(title = "Per Capital General Revenue in the 4 Most Populated Cities",
         y = "", 
         x = "USD") +
    
    
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom")
  })
  
 
output$all_entities_liab_rev_ratio <- renderPlotly({

  
  state <- read_csv("data/states.csv") %>% select(state, lib_rev_ratio) %>% 
    mutate(lib_rev_ratio = round(lib_rev_ratio)) %>% 
    rename(State = state, `Liability-Renenue Ratio` = lib_rev_ratio) 
  
  city <- read_csv("data/cities.csv") %>% select(state, city, lib_rev_ratio) %>% 
    mutate(lib_rev_ratio = round(lib_rev_ratio)) %>% 
    rename(City = city, `Liability-Renenue Ratio` = lib_rev_ratio)
  
  county <- read_csv("data/counties.csv") %>% arrange(desc(population)) %>% slice(1:100) %>% 
    select(state, name, lib_rev_ratio) %>% 
    mutate(lib_rev_ratio = round(lib_rev_ratio)) %>% 
    rename(County = name, `Liability-Renenue Ratio` = lib_rev_ratio) 
  liab_rev_ratio_allentities <- state %>% 
    ggplot(aes(State, `Liability-Renenue Ratio`)) +
    geom_point(color = "purple", 
               size = 10, alpha = .2) +
    
    #2
    geom_point(aes(City, `Liability-Renenue Ratio`),
               data = city,
               color = "#E7B800", 
               size = 10, alpha = .2) +
    
    geom_point(aes(County, `Liability-Renenue Ratio`),
               data = county,
               color = "#00AFBB", 
               size = 10, alpha = .2) +
    labs(
      x = "",
      y = "") +
    scale_y_continuous(labels = scales::percent_format(suffix = "%", scale = 1)) +
    
    theme(axis.text.x  = element_text(color="white"),
          axis.ticks = element_blank(),
          panel.background = element_blank()
    ) 
  
  ggplotly(liab_rev_ratio_allentities)
  
})

# Data tab
updateSelectInput(session,
                  "select_entity_type",
                  choices = unique(acfrs$category))

# updateSelectInput(session,
#                   "select_entity",
#                   choices = NULL)
# 
updateSelectInput(session,
                  "select_indicator",
                  choices = colnames(acfrs)[7:20])
  
}