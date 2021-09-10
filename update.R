
# refresh data from google sheet if token is present
if(gs4_has_token()){
  
  dag <- googledrive::drive_get("redistricting vars") %>%
    gs4_get() %>% 
    read_sheet("DAG") 
  1
  1
  
  write_csv(dag, "dag.csv")
  
  node_attributes <-  googledrive::drive_get("redistricting vars") %>%
    gs4_get() %>% 
    read_sheet("DAG_node_attributes") 
  
  write_csv(node_attributes, "node_attributes.csv")
  
} else { warning("Google sheets is not authorized, run lines above to get auth tokens if you want to update the data.")}

# load data
dag <- read.csv("dag.csv") %>% filter(cite_weight > 0)
node_attributes <- read.csv("node_attributes.csv")


library(literature)

# now with node and edge attributes 
lit <- review(dag, 
              edge_attributes = c("edge",
                                  "core",
                                  "mechanism", 
                                  "cites", 
                                  "cites_empirical", 
                                  "cite_weight", 
                                  "cite_weight_empirical"), 
              node_attributes = node_attributes)



# all edges
edges <- lit$edgelist %>% 
  mutate(
    detail = paste(edge, mechanism, cites, sep = "<br>") %>% str_remove_all("NA"),
    type = edge,
    title = paste0("<p>", detail, "</p>"),
    #label = type,
    color = ifelse(str_detect(type, "^increase"), "#81a275", "#617d9f"),
    color = ifelse(str_detect(type, "^decrease"), "#b14552", color) ) %>%
  distinct()

core <- edges %>% filter(core, !is.na(cites))

cited <- edges %>% filter(cite_weight>0)



# node attributes
nodes <- lit$nodelist %>% mutate(label = node, 
                                 id = node,
                                 # scale nodes by degree
                                 icon.size = degree + 40,
                                 title = paste0("<p>", type, ": ", label,"</p>") %>% str_remove("NA:"),
                                 # levels in case we want Hierarchical Layout
                                 level = ifelse(type == "goal", 1:2, 3:4),
                                 # FontAwesome.com shapes for fun
                                 shape = "icon",
                                 icon.color = case_when(node %in% c(cited$to, cited$from) ~ "black",
                                                        !node %in% c(cited$to, cited$from) ~ "grey"),
                                 icon.code = case_when(type == "condition" ~ "f205", # chess board
                                                       type == "goal" ~ "f24e", # scale  "f05b", # crosshairs
                                                       type == "policy" ~ "f0e3", # gavel
                                                       type == "value" ~ "f004", # "f4be", # hand with heart
                                                       type == "effect" ~ "f080", # "f681", # data 
                                                       type == "metric" ~ "f1de",# "f548", # ruler 
                                                       TRUE ~ "f0c8"), #square
                                 icon.face =  "'FontAwesome'",
                                 icon.weight = "bold")


# save datasets to call in Shiny
save(nodes, file = "nodes.RData")
save(edges, file = "edges.RData")

