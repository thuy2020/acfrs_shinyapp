library(tidyverse)
library(forcats)

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




