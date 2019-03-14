library(magrittr)
load("tmp_data/population.RData")

#### generate relations ####
amount_friends <- 10
distance_matrix_equal <- matrix(1, 4, 4) %>% `diag<-`(0)
cross_unit_proportion_child_of <- 0.002
cross_unit_proportion_friend <- 0.01
weight_child_of <- 50
weight_friend <- 10

settings <- popgenerator::init_relations_settings(
  population, amount_friends, distance_matrix_equal, 
  cross_unit_proportion_child_of, cross_unit_proportion_friend,
  weight_child_of, weight_friend
) 

relations <- popgenerator::generate_relations(settings)

#### transform relations to igraph ####
graph <- igraph::graph_from_data_frame(
  relations[c("from", "to", "weight")],
  directed = FALSE,
  vertices = population[,c("id", "birth_time", "unit")]
)

#### plot population graph nodes ####
library(ggraph)
layout <- ggraph::create_layout(graph, layout = 'igraph', algorithm = 'fr', start.temp = 10)
p <- ggraph(layout) +
  geom_node_point(
    mapping = aes(color = birth_time, shape = unit),
    size = 1.2
  ) +
  ggplot2::scale_color_continuous(low = "darkgreen", high = "red") +
  theme_graph(
    background = "#FAFAFA"
  ) +
  ggplot2::guides(
    color = guide_colorbar(title = "Time of birth"),
    shape = guide_legend(title = "Group")
  )

ggsave(
  filename = "figures/code_sample_II_relations_generation.jpg",
  plot = p,
  device = "jpg",
  width = 180, 
  height = 150, 
  units = "mm", 
  dpi = 300
)
