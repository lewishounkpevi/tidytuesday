#### Init ####

source("code/init.R")
source("code/fct_theme.R")
source("code/fct_palette.R")

#### Get the Data ####

tuesdata <- tidytuesdayR::tt_load(2021, week = 6)

hbcu_all <- tuesdata$hbcu_all

hbcu_black <- tuesdata$hbcu_black

hs_students <- tuesdata$hs_students





a <- hs_students %>% 
  mutate(Total = if_else(Total > 10000, str_sub(Total, 1, 4) %>% as.double(), Total)) %>% 
  rename(year = Total) %>% 
  select(!contains(c("Standard", "Total"))) %>% 
  mutate(across(White1:last_col(), as.double)) %>% 
  pivot_longer(cols = 2:last_col(), names_to = "group",
               values_to = "percentage") %>% 
  filter(year >= 1980) %>% 
  ggplot(aes(x = year, y = percentage, color = group)) +
  geom_line()



a <- hbcu_all %>% 
  select(year, males, females) %>% 
  pivot_longer(cols = c( "males", "females"),
               names_to = "gender",
               values_to = "number"
               ) %>% 
  mutate(label = if_else(gender == "males",
                         fontawesome("fa-male"),
                         fontawesome("fa-female"))) %>% 
  ggplot()+
  aes(y = number, x= gender, fill = gender) +
  geom_col() +
  geom_text(family='fontawesome-webfont', size = 20, aes(label = label)) +
  facet_wrap(~year, nrow = 10,
             ncol = 4)



