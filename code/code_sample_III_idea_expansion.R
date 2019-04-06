library(magrittr)
load("tmp_data/population.RData")
load("tmp_data/relations.RData")
load("tmp_data/relations_graph.RData")

#### store network in pajek file format ###
rel_small <- relations %>% dplyr::select(from, to, weight)
graph_file <- "tmp_data/relations_network.paj"
utils::write.table(rel_small, file = graph_file, sep = " ", row.names = F, col.names = F)
incomplete_pajek <- readLines(graph_file)
incomplete_pajek <- rlang::prepend(
  incomplete_pajek,
  paste(c(paste("*Vertices", nrow(population), collapse = " "), population$id, "*Edges"), collapse = "\n") 
)
writeLines(incomplete_pajek, graph_file)

#### prepare initial ideas distribution configuration ####
ideas_file <- "tmp_data/idea_config_file.txt"
moment_zero_population <- population %>% dplyr::filter(
  birth_time < 0 & death_time > 0
) %$% id
innovators <- sample(moment_zero_population, 9)
start_idea_1 <- paste0("idea_1", ";", paste(innovators[1:3], collapse = " "), ";", 1)
start_idea_2 <- paste0("idea_2", ";", paste(innovators[4:6], collapse = " "), ";", 1)

writeLines(
  c(start_idea_1, start_idea_2),
  ideas_file
)

#### define output file ####
output_file <- "tmp_data/gluesless_result.txt"

#### run gluesless simulation ####
system2("../gluesless/build/gluesless", args = c("-pi", graph_file, "-ii", ideas_file, "-o", output_file, "-q"))

#### read results ####
result <- readLines("tmp_data/gluesless_result.txt")
idea_1_nodes <- as.integer(unlist(strsplit(result[which(result == "idea_1") + 1], split = " ")))
idea_development <- population %>%
  dplyr::mutate(
    idea = ifelse(id %in% idea_1_nodes, "idea_1", "idea_2"),
    from = birth_time - abs(birth_time - death_time)/2,
    to = death_time
  ) %>%
  aoristAAR::aorist(
    split_vars = c("unit", "idea"),
    method = "number"
  ) %>%
  dplyr::group_by(
    date, unit
  ) %>%
  dplyr::mutate(
    freq = sum / sum(sum)
  ) %>%
  dplyr::ungroup()

#### plot development ####
library(ggplot2)
idea_development %>%
  ggplot() +
  geom_area(aes(x = date, y = freq, fill = idea, group = idea)) +
  geom_line(aes(x = date, y = freq, group = idea), position = "stack") +
  theme_bw() +
  facet_grid(rows = dplyr::vars(unit)) +
  xlab(expression(paste("t"))) +
  ylab("variant occurrence [%]") +
  xlim(0, 200)
