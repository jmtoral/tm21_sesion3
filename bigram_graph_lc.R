# Visualización de bigramas en un grafo
# 4 de septiembre de 2021


# Bibliotecas ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse, tidytext, rvest, 
               igraph, # Análisis de redes
               ggraph
               )

# 

# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

url <- "https://lopezobrador.org.mx/2021/09/01/discurso-del-presidente-andres-manuel-lopez-obrador-durante-el-tercer-informe-de-gobierno/"

informe <- url %>% 
  read_html() %>% 
  html_nodes(".entry-content p") %>% 
  html_text() %>% 
  as_tibble()


# calcular bigramas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

bigramas <- informe %>% 
  unnest_tokens(bigramas, value, token = "ngrams", n = 2) %>% 
  separate(bigramas, c("palabra1", "palabra2"), sep = " ") %>% 
  filter(!palabra1 %in% c(tm::stopwords("es"))) %>% 
  filter(!palabra2 %in% c(tm::stopwords("es"))) %>% 
  count(palabra1, palabra2, sort = T)


# crear la red ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

bigram_graph <- bigramas %>% 
  filter(!is.na(palabra1)) %>% 
  filter(n > 1) %>% 
  graph_from_data_frame()

bigram_graph


# grafo -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

set.seed(123)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = F,
                 arrow = a
                 ) + #peso
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()







