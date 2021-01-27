#### Init ####
source("code/init.R")
source("code/fct_theme.R")
source("code/fct_palette.R")
library("rnaturalearth")
library("rnaturalearthdata")
world <- ne_countries(returnclass = 'sf')

#### Get the Data ####
plastics <- tidytuesdayR::tt_load(2021, week = 5)$plastics

tidyplastics <- plastics %>%
  select(-parent_company) %>% 
  group_by(country, year, num_events, volunteers) %>% 
  summarise(across(empty:grand_total, sum, na.rm = TRUE), 
            .groups = "drop") 


#### plots ####

plasticstarrynight <- tidyplastics %>% 
  ggplot() + 
  aes(x = grand_total , y = country, 
      color = as.factor(year)) +
  geom_text(label="★", size = 5, family = "HiraKakuPro-W3") +
  # geom_point() +
  xlim(0, 10000) +
  scale_color_manual(values = c("#440154FF","#FDE725FF")) +
  labs(title = "Plastic Quantity Per Country",
       x = "Plastic Quantity",
       y = NULL,
       subtitle = "Starry Night 😇",
       caption = "Source : Break Free from Plastic
       \nTidyTuesday Plastic Pollution
       \nby: Lewis Hounkpevi") +
  theme_lewis(legend.direction = "vertical", 
              legend.position =  "right",
              base_size = 15) +
  theme(axis.text.y = element_text(size = 5),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "#5555AA", 
                                        colour = NA))

#### Volunteers map ####

volunteers_map <- map(c(2019, 2020), function(x){world %>% 
    select(geounit, region_wb, geometry)  %>% 
    left_join(tidyplastics %>% 
                filter(year == x) %>%
                mutate(tranche = case_when(volunteers < 10 ~ "1- Less than 10",
                                           volunteers < 20 ~ "2- between 10 and 20",
                                           volunteers < 100 ~ "3- between 20 and 100",
                                           volunteers < 400 ~ "4- between 100 and 400",
                                           volunteers >= 400 ~ "5- 400 and more")),
              by = c("geounit" = "country")) %>% 
    st_as_sf(crs = 4326)}) %>% 
  map(function(x){
    ggplot(data = x) +
      geom_sf(aes(fill = tranche)) +
      scale_fill_viridis_d() +
      
      geom_sf_text(size = 2.5, aes(label = volunteers)) +
      # facet_grid( ~ year, shrink = FALSE, drop = FALSE) +
      labs( y = NULL,  x = NULL) +
      theme_lewis() +
      theme(axis.text = element_blank(),
            legend.text=element_text(size=rel(0.5)),
            axis.ticks = element_blank(),
            legend.position = "bottom",
            legend.direction = "horizontal")})


volunteers_map_patch <- (volunteers_map[[1]] +
    volunteers_map[[2]]+
  plot_layout(ncol = 2)) +
  plot_annotation(title = "Volunteers map per year", 
                  caption = "Source : Break Free from Plastic
       \nTidyTuesday Plastic Pollution
       \nby: Lewis Hounkpevi",
       theme = theme_lewis())




#### Save ####

ggsave(here::here("graphes" , "Y2021-M1-W5 Plastic Pollution",
                  "plasticstarrynight.png"), 
       plot = plasticstarrynight, 
       width = 30, height = 20, units = "cm")

ggsave(here::here("graphes" , "Y2021-M1-W5 Plastic Pollution",
                  "volunteers_map_patch.png"), 
       plot = volunteers_map_patch, 
       width = 30, height = 20, units = "cm")

