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
  installr::install.rtools()
}

usePackage("rtweet")
usePackage("openxlsx")
usePackage("httpuv")
usePackage("tidyverse")

medias_extreme_gauche = c("Politis_fr", "MarianneleMag")
medias_gauche = c("lemondefr", "libe", "lobs")
medias_general = c("RMCInfo", "Europe1", "franceinter", "France24_fr", "LCP", "France2tv", "France3tv")
medias_droite = c("Le_Figaro", "LePoint", "le_Parisien", "LaCroix")
medias_extreme_droite = c("RTenfrancais", "Valeurs")
medias = c(medias_extreme_gauche, medias_gauche, medias_general, medias_droite, medias_extreme_droite)

hommes_politiques_extreme_gauche = c("JLMelenchon", "Francois_Ruffin", "PhilippePoutou")
hommes_politiques_gauche = c("benoithamon", "manuelvalls", "Anne_Hidalgo", "fhollande")
hommes_politiques_gouvernement = c("EmmanuelMacron", "EPhilippePM", "gerardcollomb", "BrunoLeMaire", "JY_LeDrian", "BGriveaux", "CCastaner", "N_Hulot")
hommes_politiques_droite = c("laurentwauquiez", "vpecresse", "cestrosi", "alainjuppe", "jf_cope")
hommes_politiques_extreme_droite = c("MLP_officiel", "GilbertCollard", "f_philippot")
hommes_politiques = c(hommes_politiques_extreme_gauche, hommes_politiques_gauche, hommes_politiques_gouvernement, hommes_politiques_droite, hommes_politiques_extreme_droite)

partis_politiques = c("FranceInsoumise", "enmarchefr", "lesRepublicains", "FN_officiel", "partisocialiste")

texte_selec = c('migrant', 'migratoire', 'calais', 'migration')


### Médias ###

paste(texte_selec, collapse = " ")

# 6 minutes
start.time <- Sys.time()
#tweets_medias <- get_timelines(medias, n = 15000)
tweets <- search_tweets(
  "migratoire OR calais OR migration OR migrant&lang:fr", n = 18000, include_rts = FALSE
)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

## plot time series of tweets
ts_plot(tweets, "3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #rstats Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

nrow(tweets)
length(unique(tweets$screen_name))


write.xlsx(tweets, "Fichiers/tweets_utilisateurs.xlsx")
