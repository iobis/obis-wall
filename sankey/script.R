library(dplyr)
library(networkD3)

res <- read.csv("result.csv", stringsAsFactors = FALSE)
records <- res %>% group_by(ronname, major) %>% summarize(records = sum(records)) %>% filter(major != "") %>% select(ron = ronname, area = major, records)

rons <- data.frame(
  name = unique(records$ron),
  id = seq(0, length(unique(records$ron)) - 1)
)
areas <- data.frame(
  name = unique(records$area),
  id = seq(length(unique(records$ron)), length(unique(records$ron)) + length(unique(records$area)) - 1)
)
nodes <- rbind(rons, areas)

links <- records %>%
  left_join(rons, by = c("ron" = "name")) %>%
  left_join(areas, by = c("area" = "name")) %>%
  select(ronid = id.x, areaid = id.y, records, ron, area)

sankeyNetwork(Links = links, Nodes = nodes, Source = "areaid", Target = "ronid", Value = "records", NodeID = "name", fontSize = 12, nodeWidth = 50)
