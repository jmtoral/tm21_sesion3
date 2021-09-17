library(udpipe)
library(tidyverse)

es <- udpipe_download_model(language = "spanish-ancora")
udmodel_es <- udpipe_load_model(file = es$file_model)

txt <- "fuimos Juan Manuel, Juan Ricardo, José María y yo al parque"

texto <- iconv(txt, from = "Latin1", to = "UTF-8")

x <- udpipe_annotate(udmodel_es, x = texto)
x <- as.data.frame(x)
str(x)


txt <- readtext::readtext("literatura/pedro_páramo.pdf")

txt <- txt$text %>% 
str_replace_all("[»—]", "")
#txt <- iconv(txt$text, from = "Latin1", to = "UTF-8")

x <- udpipe_annotate(udmodel_es, x = txt)
x <- as.data.frame(x)
str(x)


x %>% 
  count(upos, sort=T)

x %>% 
  filter(upos == "NOUN") %>% 
  count(token, sort = T)


x %>% 
  filter(upos == "VERB") %>% 
  count(token, sort = T)
