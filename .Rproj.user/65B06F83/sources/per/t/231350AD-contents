# Modelo de clasificación con LASSO
# 4 de septiembre



# Bibliotecas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


pacman::p_load(tidyverse,
               tidytext,
               rsample,
               yardstick, # Evaluación de modelos 
               broom, # modelos tidy
               glmnet # General lineal models
               )

#tidymodels


# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

amlo <- readtext::readtext("politica/Hacia una economía moral.pdf") %>% 
  as_tibble() %>% 
  unnest_tokens(oracion, text, token = "sentences") %>% 
  mutate(titulo = "Hacia una economía moral") %>% 
  select(-doc_id) %>% 
  select( titulo, oracion)

set.seed(123)

felcal <- epubr::epub("politica/Decisiones difíciles.epub") %>% 
  unnest(data) %>% 
  as_tibble() %>% 
  filter(nword > 200) %>% 
  select(titulo = title, text) %>% 
  unnest_tokens(oracion, text, token = "sentences") %>% 
  sample_frac(0.5)

libros <- amlo %>% 
  bind_rows(felcal) %>% 
  mutate(document = row_number()) 


# Pre procesamiento ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tidy_libros <- libros %>% 
  unnest_tokens(palabra, oracion) %>% # Palabras por oración features
  group_by(palabra) %>% 
  filter(n() > 10) %>% ### 
  ungroup() %>% 
  filter(!palabra %in% tm::stopwords("es")) %>% 
  filter(!str_detect(palabra, "[0-9]+"))

tidy_libros %>% 
  count(titulo, palabra, sort = T) %>% 
  group_by(titulo) %>% 
  top_n(20) %>% 
  ungroup %>% 
  ggplot(aes(
    x = reorder_within(palabra, n, titulo), 
             y = n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~titulo, scales = "free")



# Estimar el modelo -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Hacer muestras

libros_muestra <- libros %>% 
  select(document) %>% 
  initial_split()

train_data <- training(libros_muestra)
test_data <- testing(libros_muestra)

## Matriz sparse
## Entrenar el modelo

sparse_palabras <- tidy_libros %>% 
  count(document, palabra) %>% 
  inner_join(train_data) %>% 
  cast_sparse(document, palabra, n)

class(sparse_palabras)
dim(sparse_palabras)

### Identificador de documento

pal_rownames <- as.integer(rownames(sparse_palabras))


union_libros <- data.frame(document = pal_rownames) %>% 
  left_join(libros %>% 
              select(document, titulo))


# Estimación del modelo lineal --------------------------------------------------------------------------------------------------------------------------------------------------------------------

es_amlo <- union_libros$titulo == "Hacia una economía moral"

modelo <- cv.glmnet(sparse_palabras,
                    es_amlo,
                    family = "binomial", # Logística
                    keep = T)

plot(modelo)

plot(modelo$glmnet.fit)


# Evaluación --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

coefs <- modelo$glmnet.fit %>% 
  tidy() %>% 
  filter(lambda == modelo$lambda.1se)

coefs %>% 
  group_by(estimate > 0) %>%  # True o FALSE
  top_n(10, abs(estimate)) %>% 
  ungroup %>% 
  ggplot(aes(
    x = reorder(term, estimate),
    y = estimate,
    fill = estimate > 0
  )) +
  geom_col() +
  coord_flip()

## ROC

intercept <- coefs %>% 
  filter(term == "(Intercept)") %>% 
  pull(estimate)

clasificaciones <- tidy_libros %>% 
  inner_join(test_data) %>% 
  inner_join(coefs, by = c("palabra" = "term")) %>% 
  group_by(document) %>% 
  summarise(score = sum(estimate)) %>% 
  mutate(prob = plogis(intercept + score))


classifications

clases_comentadas <- clasificaciones %>% 
  left_join(libros %>% 
              select(titulo, document), by = "document") %>% 
  mutate(titulo = as.factor(titulo))

clases_comentadas %>% 
  roc_curve(titulo, prob) %>% 
  ggplot(aes(x = specificity,
             y = 1-sensitivity)) +
  geom_line(
    size = 1.5
  ) +
  geom_abline()


clases_comentadas %>% 
  roc_auc(titulo, prob)

clases_comentadas %>% 
  mutate(
    prediccion = case_when(
      prob > 0.5  ~ "Hacia una economía moral",
      TRUE ~ "Decisiones difíciles"
    )
  ) %>% 
  mutate(prediccion = as.factor(prediccion)) %>% 
  conf_mat(titulo, prediccion)


clases_comentadas %>% 
  filter(prob > 0.8,
         titulo == "Hacia una economía moral") %>% 
  sample_n(10) %>% 
  inner_join(libros %>% 
               select(document, oracion)) %>% 
  select(prob, oracion) -> pred


clases_comentadas %>% 
  filter(prob < 0.2,
         titulo == "Hacia una economía moral") %>% 
  sample_n(10) %>% 
  inner_join(libros %>% 
               select(document, oracion)) %>% 
  select(prob, oracion) -> errores


clases_comentadas %>% 
  filter(prob > 0.8,
         titulo == "Decisiones difíciles") %>% 
  #sample_n(10) %>% 
  inner_join(libros %>% 
               select(document, oracion)) %>% 
  select(prob, oracion) -> errores
