pacman::p_load(tidyverse, tidytext, rvest, tm, hrbrthemes, extrafont)
#loadfonts(device = "win")
library(igraph)
library(ggraph)

url <- "https://lopezobrador.org.mx/2021/09/01/discurso-del-presidente-andres-manuel-lopez-obrador-durante-el-tercer-informe-de-gobierno/"

informe <- url %>% 
  read_html() %>% 
  html_nodes(".entry-content p") %>% 
  html_text() %>% 
  as_tibble()




# bigrams -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


tk_cnt <- informe %>% 
  unnest_tokens(palabras, value, token ="ngrams", n=2) %>% 
  separate(palabras, c("palabras1", "palabras2"), sep =" ") %>% 
  filter(!palabras1 %in% c(stopwords("es"), "decir")) %>% 
  filter(!palabras2 %in% c(stopwords("es"), "decir")) %>% 
  count(palabras1, palabras2,sort =T)


bigram_graph <- tk_cnt %>%
  filter(n > 1) %>%
  graph_from_data_frame()

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
