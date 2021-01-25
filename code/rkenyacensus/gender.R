#### Init ####

# remotes::install_github("Shelmith-Kariuki/rKenyaCensus")

library(tidytuesdayR)
library(rKenyaCensus)
library(tidyverse)

#### Get the Data ####

gender <- tidytuesdayR::tt_load(2021, week = 4)$gender

#### Gender ####

gender_df <- gender %>%
  pivot_longer(cols = Male:Intersex,
               names_to = "genre",
               values_to = "nbre") %>% 
  drop_na() %>%
  select(County,  genre,  nbre) %>%
  arrange(County, nbre) %>% 
  mutate(nbre2 = if_else(genre == "Female", -1*nbre, nbre ),
         County = fct_reorder(County, desc(nbre))) %>%
  filter(County != "Total")  %>% 
  mutate(County = fct_reorder(County, desc(nbre)))


gender_pyramid <- gender_df %>% 
  ggplot() +
  aes(y = County, x = nbre2, fill = genre) +
  geom_col() +
  scale_y_discrete () +
  scale_fill_viridis_d() +
  scale_x_continuous(breaks = seq(-round(max(gender_df$nbre2), -5),
                                  round(max(gender_df$nbre2), -5),
                                  500000),
                     labels = paste0(as.character(
                       c(seq(round(max(gender_df$nbre2)/1000, -2),
                             0, -500),
                         seq(500,
                             round(max(gender_df$nbre2)/1000, -2),
                             500))),
                       "k")) +
  labs(title = "Kenya County Gender Pyramid",
       x = " ",
       y = " ",
       caption = "Source : Kenya National Bureau of Statistics (February 2020)
       \nTidyTuesday rKenyacensus
       \nby: Lewis Hounkpevi") +
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text = element_text(face = "bold",
                                 size = rel(0.5)), 
        axis.title.x = element_text(face = "bold", 
                                    margin = margin(b = 0.5, t = 0.5, unit = "cm")), 
        axis.title.y = element_text(face = "bold", 
                                    size = rel(2/3), hjust = 0.5, vjust = 0.5, 
                                    colour = "#440154FF", 
                                    margin = margin(r = 0.5, unit = "cm")), 
        title = element_text(face = "plain", hjust = 0.5), 
        plot.title = element_text(size = 12, face = "bold", 
                                  colour = "#440154FF", vjust = 1), 
        plot.subtitle = element_text(size = rel(2/3), face = "bold", 
                                     colour = "#440154FF", vjust = 1, hjust = 0.5, 
                                     margin = margin(b = 0.5, unit = "cm")),
        plot.caption = element_text(size = rel(1), vjust = 1, hjust = 0), 
        legend.text = element_text(face = "bold", rel(3/5), colour = "#440154FF"), 
        legend.direction = "horizontal", 
        legend.position = "bottom", legend.title = element_blank(), 
        panel.grid.major = element_line(linetype = "solid", 
                                        colour = "#E7E9EA", size = 0.5), 
        panel.border = element_rect(fill = NA, 
                                    colour = "#E7E9EA"), 
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = NA, colour = NA, 
                                        size = 0.5))

#### Save ####

ggsave( plot = gender_pyramid, 
        filename = here::here("graphes", "gender_pyramid.jpeg"),
        width = 30, height = 15, units = "cm", 
        pointsize = 5, quality = 100, 
        bg = "white", dpi = 200)

#### Session information ####

sessionInfo()
