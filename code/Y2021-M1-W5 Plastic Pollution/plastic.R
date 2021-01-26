#### Init ####
source("code/init.R")
source("code/fct_theme.R")
source("code/fct_palette.R")

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


#### Save ####

ggsave(here::here("graphes" , "Y2021-M1-W5 Plastic Pollution",
                  "plasticstarrynight.png"), 
       plot = plasticstarrynight, 
       width = 30, height = 20, units = "cm")

