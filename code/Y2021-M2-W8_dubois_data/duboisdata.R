#### Init ####

source("code/init.R")
source("code/fct_theme.R")
source("code/fct_palette.R")

library(extrafont)

#### Get the Data ####

tuesdata <- tidytuesdayR::tt_load(2021, week = 8)

#### Conjugal Condition ####

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


#### Georgia Pop #####


georgia_pop <- tuesdata$georgia_pop

comparative <- georgia_pop %>% 
  pivot_longer(cols = -Year,
               names_to = "color") %>%
  ggplot() +
  aes(y = value, x = Year) +
    geom_line(aes(linetype = color), size = 1.5) +
  scale_y_reverse(breaks = seq(0, 100, 5), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1790, 1890, 10), expand = c(0, 0)) +
  coord_flip() +
  add2ggplot::theme_du_bois() +
  labs(title = "COMPARATIVE INCREASE OF WHITE AND COLORED\n POPULATION OF GEORGIA.", 
       y = "Percents",
       caption = "Source : Du Bois Data Challenge
       \nTidyTuesday: Y2021-W8
       \nby: Lewis Hounkpevi") +
  guides(linetype = guide_legend(
    keywidth = 5,
    keyheight = 2,
    default.unit = "cm",
    label.position = "left",
    label.theme = NULL,
    label.hjust = 0.5,
    label.vjust = 0.5)
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.margin = margin(10, 0, 0, 0),
    legend.key = element_rect(fill = NA, color = NA),
    legend.spacing.y = unit(3, 'cm'),
    legend.background = element_blank(),
    legend.text = element_text(size = 10,
                               hjust = 0.5, 
                               face = "bold",
                               family = "mono",
                               color = "black",
                               margin = margin(r = 2,
                                               l = 2,
                                               unit = "cm")),
    plot.background = element_rect(fill = NA, color = NA),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(hjust = 0.5, 
                                vjust = 0.5,
                                family = "mono",
                                color = "black",
                                size = 15
                                ,
                                margin = margin(b = 0.5,
                                                t = 2,
                                                unit = "cm")
                                ),
    axis.text = element_text(family = "mono", size = 15),
    strip.text = element_blank(),
    panel.grid.major.x = element_line(linetype = "solid", 
                                    colour = "orange", size = 0.5),
    panel.grid.major.y =  element_line(linetype = "solid", 
                                         colour = "orange", size = 1),
    panel.border = element_rect(fill = NA,  colour = "#E7E9EA"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 0, 20, 0),
    plot.title = element_text(size = 18, face = "bold", 
                              colour = "black",
                              family = "mono",
                              vjust = 0.5,
                              hjust = 0.5,
                              margin = margin(b = 0.5, unit = "cm")), 
    
    plot.caption = element_text(size = 10, face = "plain", 
                                family = "Century",
                                margin = margin(t = 0.5, unit = "cm"),
                                color = "#129188",
                                vjust = 0.5, 
                                hjust = 0)
  )

bg <- "https://w7.pngwing.com/pngs/982/58/png-transparent-torn-brown-paper-illustration-paper-parchment-scape-house-lukkoye-paper-sheet-miscellaneous-material-file-folders.png"

comparative <- ggbackground(comparative, bg)



#### Save ####

ggsave( plot = conjugal_condition, 
        filename = here::here("graphes", "Y2021-M2-W8_dubois_data",
                              "conjugal_condition.jpeg"),
        width = 15, height = 20, units = "cm", 
        pointsize = 5, quality = 100, 
        bg = "white", dpi = 200)


ggsave( plot = comparative, 
        filename = here::here("graphes", "Y2021-M2-W8_dubois_data",
                              "comparative.jpeg"),
        width = 30, height = 40, units = "cm", 
        pointsize = 5, quality = 100, 
        bg = "white", dpi = 200)