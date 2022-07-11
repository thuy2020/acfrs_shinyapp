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


function(input, output, session) {
  
  ########Begin - Home Tab---------------
  
  output$all_entities_liab_rev_ratio <- renderPlotly({
    all_entities_liab_rev_ratio <- readRDS("all_entities_liab_rev_ratio.RDS")
    
  })
  output$liabilities_by_gov_cat_pie <- renderPlot({
    pie(pie_df2$value, labels = paste0(pie_df2$label_state_local, "\n ",pie_df2$prop,"%"), border = "white",
        col = brewer.pal(6, "Pastel1"),
        main = "Liabilities by Government Category \n(Fiscal Year 2020)")
    
  })
  
  output$liabilities_by_type_pie <- renderPlot({
    pie(pie_df$value2, labels = paste0(pie_df$label_liabilities_type, "\n ",pie_df$prop,"%"), border = "white",
        col = brewer.pal(6, "Set2"),
        main = "Liabilities by Type \n(Fiscal Year 2020)")
  })
  
  output$states_ratio_chart <- renderPlot({
    spdf <- geojson_read("data/us_states_hexgrid.geojson",  what = "sp")
    spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))

    data <- states %>% select(state, lib_rev_ratio)

    spdf_fortified <- tidy(spdf, region = "google_name") %>%
      left_join(., data, by = c("id" = "state"))

    # binning
    spdf_fortified$bin <- cut(spdf_fortified$lib_rev_ratio,
                              breaks=12, include.lowest = TRUE, labels=c())

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
      ggtitle( "Liabilities-to-revenue Ratio in State Governments") +
      theme(
        legend.position = c(0.5, 1.1),
        text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        plot.title = element_text(size= 20, hjust=0.5, color = "#a61c00", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
      )
  })
  
  ####### End - Home Tab ---------------
  ########Begin - Analysis Tab--------------- 
   ## States
  output$top10_bottom10_states <- renderPlot({
    top10_bottom10_states
    
  })
  
  ## Counties
  output$counties <- renderDT({
    counties %>%
      select(-c(FIPS, state_abv)) %>% 
      datatable(rownames = input$show_rownames,
                extensions = "Responsive")
  })
  
  output$counties_chart <- renderPlot({
    counties_chart 
    
  })
  
  ## Cities
  output$cities <- renderDT({
    cities %>%
      arrange(desc(population)) %>%
      datatable(rownames = FALSE,
                extensions = "Responsive")
  })
  
  output$top100_cities_ratio <- renderPlot({
    top100_cities_ratio
    
  })
  
  output$revevue_per_cap_50cities_chart <- renderPlotly({
    fisc_50cities_chart
  })
  
  
  output$top4_cities <- renderPlot({
    
    top4_cities
  })
  
  output$fisc_cities_less2Mpop_chart <- renderPlotly({
    
    fisc_cities_less2Mpop_chart
  })

  ####### Begin - Data Tab ----------------------------------------
  ##entity explorer 
updateSelectInput(session,
                  "select_type_government",
                  choices = c("State", "County", "City", "Special District", "School District",
                              "Charter School", "Community College District","Public Higher Education"
                              ))

updateSelectInput(session,
                  "select_indicator",
                  choices = colnames(acfrs)[7:20])

     
comparison_state_mean_state_gov <- readRDS("comparison_state_mean_state_gov.RDS")

output$state_gov_vs_mean_chart <- renderPlot({
  comparison_state_mean_state_gov
})

## download data
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

}