library(tidyverse)

df_map <- dat_pro %>% 
    select(iso, region = country_x, clus)

world <- map_data("world")

world <- left_join(world, df_map)

g <- ggplot(world, aes(x = long, y = lat)) +
    geom_polygon(aes(group = group, fill = as.factor(clus)), 
              size = 0.1, color = "grey50") +
    scale_fill_viridis_d("cluster") +
    theme_void(base_size = 8) +
    theme(legend.position = c(0.1, 0.3), 
          legend.key.size = unit(0.25, "cm"))

g


ggsave("figures/map_clusters.png", plot = g, device = "png", 
       width = 4, height = 3, dpi = 400)
