#### Init ####

# remotes::install_github("Shelmith-Kariuki/rKenyaCensus")

# library(tidytuesdayR)
# library(rKenyaCensus)
# library(tidyverse)
# library(sf)
# library(cartogram)

source("code/init.R")
source("code/fct_theme.R")
source("code/fct_palette.R")

#### Get the Data ####

crops <- tidytuesdayR::tt_load(2021, week = 4)$crops

#### CROPS ####

# tidycrops <- crops %>% 
#   pivot_longer(cols = -SubCounty,
#                names_to = "crops",
#                values_to = "population") %>% 
#   mutate(population = replace_na(population, 0))


tidycrops <- crops %>%
  filter(SubCounty != "KENYA") %>% 
  mutate_all(~replace_na(., 0))


#### Segmentation ####

#### Scaled dataframe ####

df_scale <- bind_cols(tidycrops %>%  select(SubCounty),
                      scale(tidycrops %>% select(-SubCounty))
                      %>% as_tibble()) 

row.names(df_scale) <- tidycrops$SubCounty

distance_df_scale <- dist(df_scale %>% select(-SubCounty))


#### ACP ####


tic()
acp <- PCA(df_scale, quali.sup = 1)

graphesacp_indi <- fviz_pca_ind(acp,
             repel = FALSE,
             col.ind = im[6],
             pointsize = .5,
             geom = "point",
             # labelsize = 2
             
             ) +
  labs(title = "PCA Individuals Projection",
       subtitle = "Which counties are close to each other according to their crops ?",
       caption = "Source : Kenya National Bureau of Statistics (February 2020)
       \nTidyTuesday rKenyacensus (Crops data)
       \nby: Lewis Hounkpevi") +
  theme_lewis() +
  ggrepel::geom_label_repel(aes(label = df_scale$SubCounty), 
                            color = ca[1],
                            size = 2,
                            max.overlaps = 1000,
                            box.padding   = 0.0010, 
                            point.padding = 0.5,
                            segment.color = 'grey50')


graphesvar <- fviz_pca_var(acp, 
                           repel = TRUE,
                           col.var = ca[7],
                           col.circle = im[3]) +
  theme_lewis() +
  labs(title = "Projection variables ACP",
       subtitle = "Which crops are correlated to each other their growing households?",
       caption = "Source : Kenya National Bureau of Statistics (February 2020)
       \nTidyTuesday rKenyacensus (Crops data)
       \nby: Lewis Hounkpevi")

toc()
#### ACP SAVE ####

ggsave(here::here("graphes" , "graphesacp_indi.png"), 
       plot = graphesacp_indi, 
       width = 30, height = 20, units = "cm")

ggsave(here::here("graphes" , "graphesvar.png"), 
       plot = graphesvar, 
       width = 30, height = 20, units = "cm")

#### KMEANS ####

# Detect optimal K number
tic()
km_sihl_viz <- fviz_nbclust(df_scale[-1], kmeans, 
                            method = "silhouette",
                            k.max = 40)
toc()

# Kmeans with k = 6 (optimal number found)

tic()
km6_model <- df_scale[-1] %>%  
  kmeans(centers = 6, nstart = 50,
         iter.max = 500, algorithm = "MacQueen") 
toc()


# Indicateur de silhouette

tic()
sil6 <- silhouette(km6_model$cluster, distance_df_scale) 

graphe_silhouette_km <- sil6 %>%
  fviz_silhouette(
    palette = viridis_pal()(6),
    main = "Silhouette Kmeans Graphs",
    ggtheme = theme_lewis()
  )


silclust_km <- sil6[, c(1,3)] %>% 
  as_tibble() %>% 
  group_by(cluster) %>% 
  summarise(moyenne_sil = round(mean(sil_width), 2))

silkm_moy <- round(mean(sil6[, 3]), 2)

toc()

# Visualisation des clusters

tic()

graphe_cluster_km <- fviz_cluster(km6_model, data = df_scale[-1],
                                  ellipse.type = "convex",
                                  labelsize = 8,
                                  outlier.labelsize = 30,
                                  palette =  viridis_pal()(6),
                                  main = "Kmeans Clusters Graphs",
                                  ggtheme = theme_lewis(base_size = 12)) +
  ggrepel::geom_label_repel(aes(label = df_scale$SubCounty), 
                            color = ca[1],
                            size = 2,
                            max.overlaps = 1000,
                            box.padding   = 0.0010, 
                            point.padding = 0.5,
                            segment.color = 'grey50')  +
  labs(title = "Counties Clustering according to their crops (Kmeans)",
       subtitle = "Which counties are close to each other according to their crops ?",
       caption = "Source : Kenya National Bureau of Statistics (February 2020)
       \nTidyTuesday rKenyacensus (Crops data)
       \nby: Lewis Hounkpevi")
toc()

#### KMEANS SAVE ####

ggsave(here::here("graphes" , "graphe_silhouette_km.png"), 
       plot = graphe_silhouette_km, 
       width = 30, height = 20, units = "cm")

ggsave(here::here("graphes" , "graphe_cluster_km.png"), 
       plot = graphe_cluster_km, 
       width = 30, height = 20, units = "cm")



### HCLUST ####

cah_opti <- NbClust(data = df_scale, diss = NULL, 
                    distance = "euclidean",
                    min.nc = 2, max.nc = 10,
                    method = "ward.D2", index = "all")

# cah_opti$All.index
# cah_opti$Best.nc
# cah_opti$All.CriticalValues
# cah_opti$Best.partition


# Modélisation

tic()
cah <- df_scale[-1] %>%
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  fastcluster::hclust(method = "ward.D2")
toc()
# 133.596 sec elapsed

# Meilleur nombre de classe
JLutils::best.cutree(cah)

## Coupure du dendogramme
tic()
cah6 <- cutree(cah, k = 6)
toc()

# Visualisation des clusters

tic()
dend_cah <- as.dendrogram(cah)

dend_cah <- color_branches(dend_cah, k = 6, groupLabels = TRUE) %>% 
  color_labels(k = 6) %>%
  set("branches_k_color", k = 6) %>% set("branches_lwd", 1) %>%
  set("labels_cex", 0.4) %>% set("labels_colors", k = 6) %>%
  set("leaves_pch", 19) %>% set("leaves_cex", 0.5) %>% 
  set_labels(df_scale$SubCounty)

graphe_dendo_cah <- ggplot(as.ggdend(dend_cah)) + 
  theme_lewis() +
  theme(axis.line.x = element_blank(),
        axis.text.x =  element_blank()) +
  labs(title = "Counties Dendogram (Hclust)",
       y = NULL,
       x = NULL,
       subtitle = "Which counties are close to each other according to their crops ?",
       caption = "Source : Kenya National Bureau of Statistics (February 2020)
       \nTidyTuesday rKenyacensus (Crops data)
       \nby: Lewis Hounkpevi")


silcah6 <- silhouette(cah6, distance_df_scale) 

graphe_silhouette_cah6 <- silcah6 %>% 
  fviz_silhouette(palette = viridis_pal()(6),
                  main = "Silhouette Hclust Graphs",
                  ggtheme = theme_lewis())


silclust_cah6 <- silcah6[, c(1,3)] %>% 
  as_tibble() %>% 
  group_by(cluster) %>% 
  summarise(moyenne_sil = round(mean(sil_width), 2))

silcah6_moy <- round(mean(silcah6[, 3]), 2)

toc()

tic()
graphe_cluster_cah6 <- fviz_cluster(
  list(data = df_scale[-1], 
       cluster = cah6),
  # repel = TRUE,            
  show.clust.cent = TRUE, 
  palette = viridis_pal()(6),         
  ggtheme = theme_lewis(base_size = 12),
  main = "Graphe des clusters CAH"
) +
ggrepel::geom_label_repel(aes(label = df_scale$SubCounty), 
                            color = ca[1],
                            size = 2,
                            max.overlaps = 1000,
                            box.padding   = 0.0010, 
                            point.padding = 0.5,
                            segment.color = 'grey50')  +
  labs(title = "Counties Clustering according to their crops (HClust)",
       subtitle = "Which counties are close to each other according to their crops ?",
       caption = "Source : Kenya National Bureau of Statistics (February 2020)
       \nTidyTuesday rKenyacensus (Crops data)
       \nby: Lewis Hounkpevi")

toc()


#### Hclust SAVE ####

ggsave(here::here("graphes" , "graphe_dendo_cah.png"), 
       plot = graphe_dendo_cah, 
       width = 30, height = 20, units = "cm")

ggsave(here::here("graphes" , "graphe_cluster_cah6.png"), 
       plot = graphe_cluster_cah6, 
       width = 30, height = 20, units = "cm")

ggsave(here::here("graphes" , "graphe_silhouette_cah6.png"), 
       plot = graphe_silhouette_cah6, 
       width = 30, height = 20, units = "cm")


#### Clustering Compar ####

silpatch <- (graphe_silhouette_cah6 + 
               graphe_silhouette_km +
               plot_layout(ncol = 2)) +
  plot_annotation(title = "Silhouette Graphs", 
                  subtitle = "1 Hclust = 4 Kmeans, 2 Hclust = 1 Kmeans, 3 Hclust = 6 Kmeans, 4 Hclust = 2 Kmeans, 5 Hclust = 5 Kmeans, 6 Hclust = 3 Kmeans",
                  caption = "Source : Kenya National Bureau of Statistics (February 2020)
       \nTidyTuesday rKenyacensus (Crops data)
       \nby: Lewis Hounkpevi",
theme = theme_lewis())


cluspatch <- (graphe_cluster_cah6 + 
                graphe_cluster_km +
               plot_layout(ncol = 2)) +
  plot_annotation(title = "Clusters Graphs",
                  subtitle = "1 Hclust = 4 Kmeans, 2 Hclust = 1 Kmeans, 3 Hclust = 6 Kmeans, 4 Hclust = 2 Kmeans, 5 Hclust = 5 Kmeans, 6 Hclust = 3 Kmeans",
       theme = theme_lewis())


#### Compare SAVE ####

ggsave(here::here("graphes" , "silpatch.png"), 
       plot = silpatch, 
       width = 30, height = 20, units = "cm")

ggsave(here::here("graphes" , "cluspatch.png"), 
       plot = cluspatch, 
       width = 40, height = 20, units = "cm")

#### Session information ####

sessionInfo()
