install.packages("spacyr")

library(spacyr)

spacy_install(lang_models = c("en_core_web_sm", "es_core_news_sm"), prompt = FALSE)

spacy_initialize(model = "es_core_news_sm")

txt <- c(d1 = " Vine a Comala porque me dijeron que acá vivía mi padre, un tal Pedro Páramo. Mi madre me lo dijo. Y yo le prometí que vendría a verlo en cuanto ella muriera. Le apreté sus manos en señal de que lo haría, pues ella estaba por morirse y yo en un plan de prometerlo todo.",
         d2 = "Era ese tiempo de la canícula, cuando el aire de agosto sopla caliente, envenenado por el olor podrido de la saponarias.")
parsedtxt <- spacy_parse(txt)

spacy_finalize()
