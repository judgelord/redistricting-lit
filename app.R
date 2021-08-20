library(shiny)
library(shinyWidgets)
library(visNetwork)
library(tidyverse)
library(metathis)
library(igraph)
library(googlesheets4)
library(fontawesome)

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
    
    

# List of choices for selectInput
node_choices <- as.list(1:length(nodes$id))
names(node_choices) <- paste(nodes$type, nodes$id, sep = ": ") %>% str_remove("NA:")

selected <- slice_head(nodes, n = 8)

nodes_selected <- as.list(1:length(selected$id))
names(nodes_selected) <- paste(selected$type, selected$id, sep = ": ")

edge_choices <- as.list(1:length(unique(edges$type)))
names(edge_choices) <- unique(edges$type)

server <- function(input, output) {
    output$network <- renderVisNetwork({
        load("nodes.RData")
        load("edges.RData")
    
        # make directed graph
        visNetwork(nodes=nodes[input$selected_nodes,], 
                   edges=edges, width = "100%") %>% 
            visEdges(width=5, color= edges$color, arrows = "to", arrowStrikethrough = F, smooth = T) %>%
            visNodes(scaling=list(min=30)) %>%
            visOptions(highlightNearest = list(enabled = T, degree = 0, hover = T) ) %>%
            visInteraction(hover=TRUE, zoomView = TRUE, 
                           navigationButtons = TRUE,
                           tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;
                font-family: sans-serif;font-size:14px;font-color:#000000;background-color: #e3fafa;
                -moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;
                 border: 0px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
                 max-width:200px;overflow-wrap: normal') %>%
            visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -50)) %>%
            addFontAwesome() %>%
            visLayout(randomSeed = 12)
    })
}

ui <- fluidPage(
    # metadata for social sharing
    meta() %>%
        meta_social(
            title = "Redistricting DAG",
            description = "",
            url = "https://judgelord.shinyapps.io/redistricting-lit/",
            #image = "https://www.sophie-e-hill.com/img/crony_preview.png",
            #image_alt = "An image for social media cards",
            #twitter_creator = "@judgelord",
            #twitter_card_type = "summary",
            #twitter_site = "@judgelord"
        ),
    
    titlePanel("Redistricting DAG"),
    verticalLayout(p("This “exploratory DAG” pulls nodes and edges from",
                     a(href="https://docs.google.com/spreadsheets/d/13S_p2AY05PzPvUkp4t8xNW6xbefntktaGo9JAtfXw1Q/edit#gid=578039899", 
                       "this google sheet"),
                     style = "font-size:20px;")),
    # pickerInput(
    #     inputId = "selected_edges",
    #     label = "Select edges to show",
    #     choices = edge_choices,
    #     selected = edge_choices,
    #     multiple = T,
    #     options = list(`actions-box` = TRUE)
    #     ),
    pickerInput(
        inputId = "selected_nodes",
        label = "Select nodes to show",
        choices = node_choices,
        selected = nodes_selected,
        multiple = T,
        options = list(`actions-box` = TRUE)
    ),
    sidebarLayout(
        sidebarPanel(
        h4("Guide"),
        tags$ul(
            tags$li(em("Scroll")," to zoom"), 
            tags$li(em("Drag")," to move around"),
            tags$li(em("Hover"),"or ", em("tap"), "icons and connections for more info"), 
            style = "font-size:15px;"), 
        #p("You can also use the drop-down menu to locate a particular individual or organization in the network.", style="font-size:15px"), br(),
        #p("The lines represent:", style="font-size:15px"),
        p(HTML("&horbar;"), "positive relationship", style="color:#81a275;font-size:15px"),
        p(HTML("&horbar;"), "negative relationship", style="color:#b14552;font-size:15px"),
        p(HTML("&horbar;"), "unclear or heterogeneous relationship", style="color:#617d9f;font-size:15px"),
        # br(),
        # p("Thicker lines indicate ...", style = "font-size:15px;"),
        # hr(),
        # p("Created by", a(href="https://judgelord.github.io/", "Devin Judge-Lord"),
        #   HTML("&bull;"),
          "Code on", a(href="https://github.com/judgelord/redistricting-lit/", "Github"),
        #  HTML("&bull;"), 
        #   "Follow me on",
        #   a(href="https://twitter.com/judgelord", "Twitter"),
        #   style="font-size:15px;"), 
        width=3),
        mainPanel(
        visNetworkOutput("network", height="80vh", width="100%"), width=9
    )
    )
)

shinyApp(ui = ui, server = server)


