## =================
# Rémi Toupin
# Projet : Network thesis
# created: 29 Mar 2020
# updated: 16 June 2021
# Encoding : UTF-8

library(igraph)
library(tidygraph)
library(tidyr)
library(tidyverse)
library(dplyr)

# RUN 20210617_PrepPublicsPropre.R avant de rouler ce code
# RUN 20210422_NetworkPreparation.R et nettoyer les fichiers de noeuds et edges avant de rouler ce code

## =================
# 1) Prepare info for papers
## =================

papers_10 <- c(53869748, 54363041, 54641347, 55016289, 55701738, 56833212, 56998671, 57098278, 57313486, 59541576)

network_H1 <- papers %>%
  filter(id_art %in% papers_10) %>%
  select(id_art, titre, Revue) %>%
  rename(ID_Art = id_art)

nonscitest <- tibble(
  nbrts = as.numeric(),
  nbmts = as.numeric(),
  ncomp = as.numeric(),
  nodescp = as.numeric(),
  edgescp = as.numeric(),
  nusersrts = as.numeric(),
  nusersmts = as.numeric(),
  number_isol = as.numeric(),
  totalnonsci = as.numeric(),
  largenonsci = as.numeric(),
  secnonsci = as.numeric(),
  isononsci = as.numeric()
)


## =================
# 1) Load network informations (nodes and edges)
## =================

#set working directory
setwd("Données_Articles/Network/AllNetworks")

node_files <- dir(pattern = "*year.csv")
edge_files <- dir(pattern = "*RTMentions.csv")

group <- c("Academia", "Communication", "Personal", "Political", "Professional", "Unassigned")
components_type <- c("total", "largest", "secondary", "isolates")
Tweet <- c("RT", "MT")

interactions_metrics <- tibble(groupsource = rep(group, each = 12), grouptarget = rep(group, each = 2, 6), Tweet = rep(Tweet, 36))

proportions2 <- tibble(groupcat = rep(group, 4), component = rep(components_type, each = 6))

papers_network <- c(1:10)
for (i in papers_network) {
  
  nodes <- read.csv(node_files[i], header = T, encoding = "UTF-8", sep=";")
  edges <- read.csv(edge_files[i], header = T, encoding = "UTF-8", sep=";")
  nodes$groupcat <- gsub("Pro", "Professional", nodes$groupcat)
  
  network <- graph_from_data_frame(edges, vertices = nodes) %>% as_tbl_graph()
  nodes$ndegrees <- degree(network, v = V(network), mode = c("all"),loops = TRUE, normalized = FALSE)
  network <- graph_from_data_frame(edges , vertices = nodes) %>% as_tbl_graph()
  
  lp <- nodes %>% 
    count(comp) %>%
    slice(which.max(n)) %>%
    pull(as.numeric(comp))
  maincomp <- induced_subgraph(network, V(network)$comp==lp)
  noisol <- induced_subgraph(network, v = V(network)$ndegrees<1)
  nodescp <- round(gorder(maincomp)/gorder(network)*100, digits = 1)
  edgescp <- round(gsize(maincomp)/gsize(network)*100, digits = 1)
  number_isol <- round(gorder(noisol)/gorder(network)*100, digits = 1)
  meannode <- round(gorder(network)/max(nodes$comp), digits = 0)
  
  nrts <- nrow(edges[edges$Tweet == "RT", ])
  nmts <- nrow(edges[edges$Tweet == "MT", ])
  ncom <- length(unique(nodes$comp))
  
  degrees <- degree(network, v = V(network), mode = c("out"),loops = TRUE, normalized = FALSE)
  degrees <- sort.int(degrees, decreasing = T)
  topdeg <- degrees [1:5]
  topdeg
  meandegree <- gsize(network)*2/gorder(noisol)
  meandegree
  
  nodesmaincomp <- nodes %>%
    filter(comp == lp)
  nodesseccomp <- nodes %>%
    group_by(comp) %>%
    filter(n() > 1) %>%
    filter(comp != lp)
  nodesisolates <- nodes %>%
    group_by(comp) %>%
    filter(n() == 1)
  
  nusersrts <- edges %>%
    filter(Tweet == "RT") %>%
    count(Target) %>%
    nrow(.)
  
  nusersmts <- edges %>%
    filter(Tweet == "MT") %>%
    count(Target) %>%
    nrow(.)
  
  totalnonsci <- round(nrow(nodes[nodes$groupcat == "Political",])/nrow(nodes)*100, digits = 1)
  largenonsci <- round(nrow(nodesmaincomp[nodesmaincomp$groupcat != "Academia",])/nrow(nodes[nodes$groupcat != "Academia",])*100, digits = 1)
  secnonsci <- round(nrow(nodesseccomp[nodesseccomp$groupcat != "Academia",])/nrow(nodes[nodes$groupcat != "Academia",])*100, digits = 1)
  isononsci <- round(nrow(nodesisolates[nodesisolates$groupcat != "Academia",])/nrow(nodes[nodes$groupcat != "Academia",])*100, digits = 1)
  nonsci <- c(nrts, nmts, ncom, nodescp, edgescp, nusersrts, nusersmts, number_isol, totalnonsci, largenonsci, secnonsci, isononsci)
  nonscitest <- rbind(nonscitest, nonsci)
  
  proportions <- nodes %>%
    group_by(comp) %>%
    mutate(lcomp = as.integer(comp==lp), secomp = as.integer(comp!=lp & n() > 1), isolates = as.integer(n() == 1)) %>%
    ungroup %>%
    group_by(groupcat) %>%
    summarise(total = round(n()/nrow(nodes)*100, digits = 1),
              largest = round(sum(lcomp == 1)/nrow(nodesmaincomp)*100, digits = 1),
              secondary = round(sum(secomp == 1)/nrow(nodesseccomp)*100, digits = 1),
              isolates = round(sum(isolates == 1)/nrow(nodesisolates)*100, digits = 1)
              )
  
  proportions <- gather(proportions, key = "component", value = "share of users", total:isolates)
  
  proportions2 <- left_join(proportions2, proportions, by = c("groupcat", "component"))
  
  proportions2$`share of users`[is.na(proportions2$`share of users`)] <- 0
  
  colnames(proportions2)[ncol(proportions2)] <- paste0("paper", i)
  
  rtin <- edges %>%
    group_by(ID = Target) %>%
    count(Source, Tweet) %>%
    rename(Target = ID) %>%
    rename(ID = Source) %>%
    left_join(nodes %>% select(ID, groupcat), by = "ID")  %>%
    distinct() %>%
    rename(Source = ID) %>%
    rename(ID = Target)
  
  interactions <- nodes %>%
    left_join(rtin, by = "ID") %>%
    rename(interactions = n) %>%
    rename(grouptarget = groupcat.x) %>%
    rename(groupsource = groupcat.y)
  
  interactions$interactions[is.na(interactions$interactions)] <- 0
  interactions <- interactions[complete.cases(interactions$Source),]
  interactions$groupsource[is.na(interactions$groupsource)] <- "Unassigned"
  
  interactions_summary <- interactions %>%
    group_by(groupsource, grouptarget, Tweet) %>%
    summarise(interactions = sum(interactions)) %>%
    ungroup() %>%
    group_by(groupsource) %>%
    mutate(perc = round(interactions/sum(interactions)*100, digits = 1)) %>% 
    as.data.frame()
    
  interactions_metrics <- left_join(interactions_metrics, interactions_summary %>% select(groupsource, grouptarget, Tweet, perc), by = c("groupsource", "grouptarget", "Tweet"))
  
  interactions_metrics$perc[is.na(interactions_metrics$perc)] <- 0
  
  colnames(interactions_metrics)[ncol(interactions_metrics)] <- paste0("paper", i)
} 

colnames(nonscitest) <- c("nbrts", "nbmts", "ncomp", "nodescp", "edgescp", "nusersrts", "nusersmts", "number_isol", "totalnonsci", "largenonsci", "secnonsci", "isononsci")
network_H1 <- cbind(network_H1, nonscitest)
network_H1 <- network_H1 %>%
  select(-ID_Art)

colnames(proportions2) <- c("groupcat", "component", "Fossil fuels", "Syrian drought", "Extinction risk", "Bumblebees", "Public health", "Dietary change", "Food production", "Harappa", "EU Nature Legislation", "Ecological networks")

proportions2 <- gather(proportions2, key = "article", value = "proportions", `Fossil fuels`:`Ecological networks`)

proportions2$component <- gsub("largest", "Largest component", proportions2$component)
proportions2$component <- gsub("secondary", "Secondary components", proportions2$component)
proportions2$component <- gsub("isolates", "Isolated nodes", proportions2$component)

proportions_sorted <- proportions2 %>%
  mutate(component = factor(component, levels=c("Largest component", "Secondary components", "Isolated nodes", "total")), groupcat = factor(groupcat, levels=c("Unassigned", "Personal", "Professional", "Political", "Communication", "Academia"))) %>%
  group_by(groupcat, component) %>%
  mutate(propmean = mean(proportions))
         

colnames(interactions_metrics) <- c("groupsource", "grouptarget", "Tweet", "Fossil fuels", "Syrian drought", "Extinction risk", "Bumblebees", "Public health", "Dietary change", "Food production", "Harappa", "EU Nature Legislation", "Ecological networks")

interactions_metrics$type_interaction <- paste0(interactions_metrics$groupsource, sep = " from ", interactions_metrics$grouptarget)
interactions_metrics <- interactions_metrics %>%
  relocate(type_interaction, .before= `Fossil fuels`)

interactions_metrics2 <- gather(interactions_metrics, key = "article", value = "interactions", `Fossil fuels`:`Ecological networks`)

interactions_sorted <- interactions_metrics2 %>%
  mutate(grouptarget = fct_reorder(grouptarget, -interactions))

#write.csv(network_H1, "Proportions_Nonscientists.csv")
#write.csv(interactions_metrics, "Interactions_Metrics.csv")