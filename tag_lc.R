# Entity Recognition - Tagging


## Open NLP

### SpacyR, UDpipe


# bibliotecas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pacman::p_load(udpipe, tidyverse)


# modelo UD ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

es <- udpipe_download_model(language = "spanish-ancora")
udmodel <- udpipe_load_model(file = es$file_model)

txt <- "El día está soleado."

txt <- iconv(txt, from = "Latin1", to = "UTF-8") #windows

#txt <- iconv(txt, from = "Latin1", to = "ASCII//TRANSLIT")

tag <- udpipe_annotate(udmodel, x = txt)
tag <- as_tibble(tag)



# Análisis novela ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# análisis estilométrico

cronopios <- readtext::readtext("literatura/cronopios_famas.pdf")
cronopios.txt <- cronopios$text

#cronopios.txt <- iconv(cronopios.txt, from = "Latin1", to = "UTF-8") #windows

tag <- udpipe_annotate(udmodel, x = cronopios.txt)
tag <- as_tibble(tag)


tag %>% 
  count(upos, sort = T)

tag %>% 
  filter(upos == "NOUN") %>% 
  count(token, sort = T)

tag %>% 
  filter(upos == "NOUN") %>% 
  count(lemma, sort = T)

tag %>% 
  filter(upos == "VERB") %>% 
  count(lemma, sort = T)




# Pedro Páramo

pp <- readtext::readtext("literatura/pedro_páramo.pdf")

pp.txt <- pp$text %>% 
  str_replace_all("[»—«]", "")

tag <- udpipe_annotate(udmodel, x = pp.txt)

tag <- tag %>% 
  as_tibble()



tag %>% 
  filter(upos == "NOUN") %>% 
  count(lemma, sort = T)

tag %>% 
  filter(upos == "VERB") %>% 
  count(lemma, sort = T)














