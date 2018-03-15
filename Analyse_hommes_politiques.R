rm(list=ls())

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

set_wd <- function() {
  usePackage("rstudioapi" ) # make sure you have it installed
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}

if(.Platform$OS.type == "unix")
  set_wd()
if(.Platform$OS.type == "windows") {
  set_wd()
  usePackage("installr")
  updater()
  Sys.getenv("R_ZIPCMD", "zip")
}

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

usePackage("rtweet")
#usePackage("httpuv")
usePackage("tidyverse")
usePackage("tidytext")
usePackage("devtools")
devtools::install_github("ThinkRstat/stopwords")
usePackage("stopwords")
usePackage("scales")
usePackage("readxl")
usePackage("wordcloud")
usePackage("SnowballC")

theme_set(theme_minimal())

Clean_String <- function(string){
  # Lowercase
  temp <- tolower(string)
  #' Remove everything that is not a number or letter (may want to keep more 
  #' stuff in your actual analyses). 
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  # Split it
  temp <- stringr::str_split(temp, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  return(temp)
}

medias_extreme_gauche = c("Politis_fr", "MarianneleMag")
medias_gauche = c("lemondefr", "libe", "lobs")
medias_general = c("RMCInfo", "Europe1", "franceinter", "France24_fr", "LCP", "France2tv", "France3tv")
medias_droite = c("Le_Figaro", "LePoint", "le_Parisien", "LaCroix")
medias_extreme_droite = c("RTenfrancais", "Valeurs")
medias = c(medias_extreme_gauche, medias_gauche, medias_general, medias_droite, medias_extreme_droite)

radios = c("RMCInfo", "Europe1", "RTLPetitMatin", "franceinter")
journaux = c("lemondefr", "libe", "Le_Figaro", "Valeurs", "LePoint", "le_Parisien", "LaCroix")
autres_infos = c("RTenfrancais", "France24_fr", "LCP", "France2tv", "France3tv")
journaux_radios = c(radios, journaux, autres_infos)

h_p_extreme_gauche = c("JLMelenchon", "Francois_Ruffin", "PhilippePoutou")
h_p_gauche = c("benoithamon", "manuelvalls", "Anne_Hidalgo", "fhollande")
h_p_gouvernement = c("EmmanuelMacron", "EPhilippePM", "gerardcollomb", "BrunoLeMaire", "JY_LeDrian", "BGriveaux", "CCastaner", "N_Hulot")
h_p_droite = c("laurentwauquiez", "vpecresse", "cestrosi", "alainjuppe", "jf_cope")
h_p_extreme_droite = c("MLP_officiel", "GilbertCollard", "f_philippot")
h_p = c(h_p_extreme_gauche, h_p_gauche, h_p_gouvernement, h_p_droite, h_p_extreme_droite)

p_p = c("FranceInsoumise", "enmarchefr", "lesRepublicains", "FN_officiel", "partisocialiste")

texte_selec = c('migrant', 'migratoire', 'calais', 'migration')

tweets_hommes_politiques <- read_excel("Fichiers/tweets_hommes_politiques.xlsx")

#thp <- filter(tweets_hommes_politiques, grepl(paste(texte_selec, collapse = "|"), text) & is_retweet != 1) %>%
#  select(c("screen_name", "text")) %>%
#  mutate(texte_traite = lapply(tolower(text), Clean_String))

thp <- filter(tweets_hommes_politiques, grepl(paste(texte_selec, collapse = "|"), text) & is_retweet != 1) %>%
  filter(created_at >= as.Date("2017-01-01")) %>%
  mutate(created_at_month = as.Date(cut(created_at, breaks="month"))) %>%
  mutate(rt_fav = (retweet_count + favorite_count))

thp_sauvegarde <- thp

summary(thp)
names(thp)

str(head(thp$created_at))
head(thp$rt_fav)
plot(thp$created_at)

# Idée : superposer les deux graphiques
# nombre de favoris en fonction du temps
ggplot(aes(x = created_at, y = favorite_count), data = thp) + geom_point()
# nombre de retweet en fonction du temps
ggplot(aes(x = created_at, y = retweet_count), data = thp) + geom_point(aes(colour = bord_politique, size = favorite_count))

# somme rt et fav sur par mois
ggplot(data = thp, aes(x = created_at_month, y = rt_fav)) + 
  geom_bar(aes(fill = bord_politique), stat = "identity")

# problème
# nombre de tweets par mois
ggplot(thp, aes(created_at_month)) +
  geom_bar(aes(fill = bord_politique))

# nombre de tweets par bord politique
ggplot(thp, aes(bord_politique)) +
  geom_bar()

# pourcentage par mois des tweets par bord politique
ggplot(thp, aes(factor(created_at_month), fill = factor(bord_politique))) +
  geom_bar(position = "fill")

# nombre de tweets par bord politique
ggplot(thp, aes(x = created_at, fill = bord_politique)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~bord_politique, ncol = 1)

### Analyse du texte ###

thp$text2 <- Unaccent(thp$text)

thp_texte <- data_frame(line = 1:nrow(thp), text2 = thp$text2, bord_politique = thp$bord_politique) 

stopwords_twitter <- c("t.co", "https", "c'est", "migratoire", "migrant", "france", "l'immigration", "immigration", "l`immigration", "pays", "migrants", "l\`immigration", "migratoires", "limmigration", "faut", "faire", "devons")

tidytext <- thp_texte %>%
  unnest_tokens(word, text2) %>%
  filter(!word %in% c(stopwords_twitter, stopwords_iso$fr)) %>%
  count(word, sort = TRUE)

colnames(tidytext)

summary(tidytext)

# 25 mots les plus récurrents
ggplot(tidytext[1:25,], aes(reorder(word, n), n)) + 
  geom_bar(stat = "identity", fill = "#E3693E") +
  geom_text(aes(label= as.character(n)), check_overlap = TRUE, size = 4) + 
  coord_flip() + 
  xlab(" ") + 
  ylab("Volume") + 
  labs(title = "25 mots les plus récurrents",
       caption = "dbeley")

# tidytextmining.com
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

tidy_tweets <- thp %>% 
  #filter(!str_detect(text2, "^RT")) %>%
  filter(!str_detect(text2, paste(c("^RT", "^@", "^#"),collapse = '|'))) %>%
  mutate(text2 = str_replace_all(text2, replace_reg, "")) %>%
  unnest_tokens(word, text2, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% c(stopwords_twitter, stopwords_iso$fr),
         str_detect(word, "[a-z]"))

colnames(thp)
colnames(tidy_tweets)

frequency <- tidy_tweets %>% 
  filter(!str_detect(word, paste(c("^@", "^#"), collapse = "|"))) %>%
  group_by(bord_politique) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% 
              group_by(bord_politique) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency <- frequency %>% 
  select(bord_politique, word, freq) %>% 
  spread(bord_politique, freq)

### NUAGE DE MOTS ###

## GENERAL ##

frequency_2 <- tidy_tweets %>% 
  filter(!str_detect(word, paste(c("^@", "^#"), collapse = "|"))) %>%
  count(word, sort = TRUE) 

wordcloud(words = frequency_2$word, freq = frequency_2$n, colors=brewer.pal(8, "Dark2"), max.words = 50)

## EXTREME GAUCHE ##

frequency_extreme_gauche <- frequency %>%
  select(word, Extrême_gauche) %>%
  na.omit() %>%
  arrange(desc(Extrême_gauche))

wordcloud(words = frequency_extreme_gauche$word, freq = frequency_extreme_gauche$Extrême_gauche, colors=brewer.pal(8, "Dark2"), max.words = 50)

## GAUCHE ##

frequency_gauche <- frequency %>%
  select(word, Gauche) %>%
  na.omit() %>%
  arrange(desc(Gauche))

wordcloud(words = frequency_gauche$word, freq = frequency_gauche$Gauche, colors=brewer.pal(8, "Dark2"), max.words = 50)

## GVT ##

frequency_gvt <- frequency %>%
  select(word, Gouvernement) %>%
  na.omit() %>%
  arrange(desc(Gouvernement))

wordcloud(words = frequency_gvt$word, freq = frequency_gvt$Gouvernement, colors=brewer.pal(8, "Dark2"), max.words = 50)

## DROITE ##

frequency_droite <- frequency %>%
  select(word, Droite) %>%
  na.omit() %>%
  arrange(desc(Droite))

wordcloud(words = frequency_droite$word, freq = frequency_droite$Droite, colors=brewer.pal(8, "Dark2"), max.words = 50)

## EXTREME DROITE ##

frequency_extreme_droite <- frequency %>%
  select(word, Extrême_droite) %>%
  na.omit() %>%
  arrange(desc(Extrême_droite))

wordcloud(words = frequency_extreme_droite$word, freq = frequency_extreme_droite$Extrême_droite, colors=brewer.pal(8, "Dark2"), max.words = 50)

### COMPARAISON BORDS POLITIQUE ###

ggplot(frequency, aes(Gouvernement, Extrême_droite)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

ggplot(frequency, aes(Extrême_gauche, Extrême_droite)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

ggplot(frequency, aes(Extrême_gauche, Gouvernement)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

word_ratios <- tidy_tweets %>%
  filter(!str_detect(word, paste(c("^RT", "^@", "^#"),collapse = '|'))) %>%
  filter(bord_politique %in% c("Gauche", "Extrême_gauche", "Droite", "Extrême_droite")) %>%
  count(word, bord_politique) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(bord_politique, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log(Gauche + Extrême_gauche / Droite + Extrême_droite)) %>%
  arrange(desc(logratio))

word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (Gauche/Droite)") +
  scale_fill_discrete(name = "", labels = c("Gauche", "Droite"))
