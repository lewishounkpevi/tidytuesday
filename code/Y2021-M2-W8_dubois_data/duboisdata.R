#### Init ####

source("code/init.R")
source("code/fct_theme.R")
source("code/fct_palette.R")

library(extrafont)

#### Get the Data ####

tuesdata <- tidytuesdayR::tt_load(2021, week = 8)

conjugal <- tuesdata$conjugal


conjugal_condition <- conjugal %>% 
  pivot_longer(cols = Single:`Divorced and Widowed`,
               names_to = "Marital") %>% 
  mutate(hole = 2,
         label = scales::percent(value/100, accuracy = 1),
         Population = if_else(Population == "Germany", 
                              "White", "Colored")) %>% 
  ggplot() +
  aes(x = hole, fill = Marital, y = value) +
  geom_col(width = 2) +
  scale_fill_manual(values = c("#C9146C", "#DA9112", "#129188")) +
  geom_text(aes(label=label), 
            position = position_stack(vjust = 0.5),
            colour = "white",
            fontface = "bold",
            size=2, 
            family="PCentury",
            vjust=-1) +
  geom_text(aes(x = 1, y = 0, 
                label = glue::glue("Population: {Population}\nAge: {Age}")),
            colour = "#DA9112",
            fontface = "bold",
            family="Century", 
            size=3,
            vjust = 2,
            hjust = 0.2
            
            ) +

  coord_polar(theta = "y", start=-pi/2) +
  ylim(0, 200) +
  xlim(0, 3) +
  labs(title = "Conjugal Condition", 
       # subtitle = "tests gsgsgsgs",
       caption = "Source : Du Bois Data Challenge
       \nTidyTuesday: Y2021-W8
       \nby: Lewis Hounkpevi") +
  facet_grid(Age ~ Population) +
  add2ggplot::theme_du_bois() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.margin = margin(10, 0, 0, 0),
    legend.background = element_blank(),
    legend.text = element_text(size = 8, hjust = 0.5, 
                               face = "bold",
                               family = "Century",
                               color = "#DA9112"),
    plot.background = element_rect(fill = NA, color = NA),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    strip.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(20, 0, 20, 0),
    plot.title = element_text(size = 15, face = "bold", 
                              colour = "#C9146C",
                              family = "Century",
                              vjust = 0,
                              hjust = 0,
                              margin = margin(b = 0.5, unit = "cm")), 
    
    plot.caption = element_text(size = rel(0.8), face = "plain", 
                                family = "Century",
                                margin = margin(t = 0.5, unit = "cm"),
                                color = "#129188",
                                vjust = 0.5, 
                                hjust = 0)
  )

# library(cowplot)
# bg <- here::here("code", "old", "calm.jpg")
# 
# 
#   
#   ggdraw() + 
#   # draw_label("Draft", colour = "#80404080", size = 120, angle = 45) +
#   draw_image(bg, scale = 1, width = 0.5) +
#   draw_plot(p, scale = 1, width = 1)


#### Save ####

ggsave( plot = conjugal_condition, 
        filename = here::here("graphes", "Y2021-M2-W8_dubois_data",
                              "conjugal_condition.jpeg"),
        width = 15, height = 20, units = "cm", 
        pointsize = 5, quality = 100, 
        bg = "white", dpi = 200)


