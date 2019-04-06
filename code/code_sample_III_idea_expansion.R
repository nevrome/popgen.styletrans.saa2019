library(magrittr)
load("tmp_data/population.RData")
load("tmp_data/relations.RData")
load("tmp_data/relations_graph.RData")

# store network in pajek file format
rel_small <- relations %>% dplyr::select(from, to, weight)
graph_file <- "tmp_data/relations_network.paj"
utils::write.table(rel_small, file = graph_file, sep = " ", row.names = F, col.names = F)
incomplete_pajek <- readLines(graph_file)
incomplete_pajek <- rlang::prepend(
  incomplete_pajek, 
  #paste(c(paste("*Vertices", nrow(population), collapse = " "), "*Edges"), collapse = "\n")    
  paste(c(paste("*Vertices", nrow(population), collapse = " "), population$id, "*Edges"), collapse = "\n") 
)
writeLines(incomplete_pajek, graph_file)

# prepare initial ideas distribution configuration
ideas_file <- "tmp_data/idea_config_file.txt"
moment_zero_population <- population %>% dplyr::filter(
  birth_time < 0 & death_time > 0
) %$% id
innovators <- sample(moment_zero_population, 9)
start_idea_1 <- paste0("idea_1", ";", paste(innovators[1:3], collapse = " "), ";", 1)
start_idea_2 <- paste0("idea_2", ";", paste(innovators[4:6], collapse = " "), ";", 1)
start_idea_3 <- paste0("idea_3", ";", paste(innovators[7:9], collapse = " "), ";", 1)  

writeLines(
  c(start_idea_1, start_idea_2, start_idea_3),
  ideas_file
)

# define output file 
output_file <- "tmp_data/gluesless_result.txt"

# run gluesless simulation
system2("../gluesless/build/gluesless", args = c("-pi", graph_file, "-ii", ideas_file, "-o", output_file, "-q"))

 
# # calculate idea proportions proxy
# idea_proportions <- calculate_all_idea_proportions_over_time(
#   y$model_id,
#   populations, 
#   y$timeframe, 
#   y$model_group, 
#   simulation_results,
#   by_unit = TRUE
# )
# 
# # write idea proportions
# write_idea_proportions(
#   y$model_id, 
#   idea_proportions, 
#   dir_path
# )
# 
