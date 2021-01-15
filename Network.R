library(bibliometrix)
library(tidyverse)
library(visNetwork)
library(geomnet)
library(igraph)


#Import BibTex file downloaded from Scopus
file <- "Sabatini_Keywords_2021.bib"
M <- convert2df(file, dbsource = "scopus", format = "bibtex")


# Create dataframe with all keywords, separated by paper
keywords <- strsplit(M$DE, "; *")
names(keywords) <- rownames(M)
keywords <- lapply(keywords, function(x){data.frame(key=x)})
(keywords_wide <- bind_rows(keywords, .id = "id" ) %>% 
  mutate(id=str_squish(id) ) %>% 
    mutate(key=str_replace(key, "\\(|\\)", "")) %>% 
    filter(!is.na(key)) %>% 
    filter(key != "") %>% 
    ### reclassify some keywords
    mutate(key=factor(key)) %>% 
    mutate(key=fct_collapse(key, 
                            CONSERVATION = c("BIODIVERSITY CONSERVATION", "CONSERVATION", "CONSERVATION PRIORITIES"), 
                            `MIXED-EFFECT MODELS`=c("MIXED-EFFECT MODELS", "MIXED EFFECT MODELS"), 
                            `MULTI-TAXONOMIC DIVERSITY`=c("MULTI-TAXONOMIC DIVERSITY", "MULTI-TAXON"), 
                            `MULTIVARIATE DISPERSION` = c("MULTIVARIATE DISPERSION", "MULTIVARIATE DISPERSION FROM GROUP CENTROID"),
                            `FOREST INVENTORY` = c("NATIONAL FOREST INVENTORIES", "FOREST INVENTORY"),
                            `OLD-GROWTH FOREST` = c("OLD-GROWTH FOREST", "OLD-GROWTH FORESTS", "OLD-GROWTH"), 
                            `RESTORATION` = c("RESTORATION OPPORTUNITIES", "RESTORATION"), 
                            `STRUCTURAL HETEROGENEITY` = c("STRUCTURAL HETEROGENEITY", "STRUCTURAL HETEROGENEITY INDEX"), 
                            `VASCULAR PLANTS` = c("VASCULAR PLANTS", "VASCULAR PLANT"), 
                            `VEGETATION DATABASES` = c("VEGETATION DATABASES", "PLOT DATABASE"), 
                            `REGRESSION TREES` = c("BOOSTED REGRESSION TREES", "REGRESSION TREE"), 
                            `SUSTAINABLE FOREST MANAGEMENT` = c("SUSTAINABLE FOREST MANAGEMENT", "FOREST MANAGEMENT"), 
                            `INDICATOR SPECIES` = c("INDICATOR SPECIES ANALYSIS", "INDICATORS"),
                            `SPECIES TRAITS` = c("SPECIES TRAITS", "LIFE-HISTORY TRAITS")
                            ))
    )
# Identify co-occurrences among keywords across papers
co.key <- udpipe::cooccurrence(keywords_wide, group="id", term="key")


# Create an edges dataframe from the existing pairwise co-occurrences.
edges <- data.frame(from = co.key$term1, 
                    to = co.key$term2, 
                    value=co.key$cooc)

nodes <- data.frame(id = unique(c(edges$from, edges$to)),
                    label = unique(c(edges$from, edges$to)),
                    #color = "#606482",
                    shadow = TRUE) %>% 
  left_join(keywords_wide %>% 
              group_by(key) %>% 
              summarize(value=n()), 
            by=c("label"="key")) %>% 
  mutate(font.size = 10*value) #%>% 
  #mutate(font.size=ifelse(font.size<17, NA, font.size))

#Use louvan to create a graph 
graph <- graph_from_data_frame(edges, directed = FALSE)
louvain <- cluster_louvain(graph)

louvain_df <- data.frame(as.list(membership(louvain)))
louvain_df <- as.data.frame(t(louvain_df))
louvain_df$label <- nodes$label

nodes <- left_join(nodes, louvain_df, by = "label") %>% 
  rename(group=V1)

# Visualize network
(mynetwork <- visNetwork(nodes, edges, height="100%", width="1000px")) %>% 
  visOptions(highlightNearest = TRUE)
# Export network to html
htmlwidgets::saveWidget(mynetwork, "network.html")
