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

# 6 minutes
start.time <- Sys.time()
tweets_medias <- get_timelines(medias, n = 15000)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

tweets_medias$bord_politique[tweets_medias$screen_name %in% medias_extreme_gauche] <- "Extrême_gauche"
tweets_medias$bord_politique[tweets_medias$screen_name %in% medias_gauche] <- "Gauche"
tweets_medias$bord_politique[tweets_medias$screen_name %in% medias_general] <- "Généraliste"
tweets_medias$bord_politique[tweets_medias$screen_name %in% medias_droite] <- "Droite"
tweets_medias$bord_politique[tweets_medias$screen_name %in% medias_extreme_droite] <- "Extrême_droite"

write.xlsx(tweets_medias, "Fichiers/tweets_medias.xlsx")
#write.csv(tweets_medias, "Fichiers/tweets_medias.csv")
#fwrite(tweets_medias, "Fichiers/tweets_medias.csv")


### Hommes politique ###

# 7 minutes
start.time <- Sys.time()
tweets_hommes_politiques <- get_timelines(hommes_politiques, n = 15000)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

tweets_hommes_politiques$bord_politique[tweets_hommes_politiques$screen_name %in% hommes_politiques_extreme_gauche] = "Extrême_gauche"
tweets_hommes_politiques$bord_politique[tweets_hommes_politiques$screen_name %in% hommes_politiques_gauche] = "Gauche"
tweets_hommes_politiques$bord_politique[tweets_hommes_politiques$screen_name %in% hommes_politiques_gouvernement] = "Gouvernement"
tweets_hommes_politiques$bord_politique[tweets_hommes_politiques$screen_name %in% hommes_politiques_droite] = "Droite"
tweets_hommes_politiques$bord_politique[tweets_hommes_politiques$screen_name %in% hommes_politiques_extreme_droite] = "Extrême_droite"

write.xlsx(tweets_hommes_politiques, "Fichiers/tweets_hommes_politiques.xlsx")
#write.csv(tweets_hommes_politiques, "Fichiers/tweets_hommes_politiques.csv")


### Partis politique ###

# 2 minutes
start.time <- Sys.time()
tweets_partis_politiques <- get_timelines(partis_politiques, n = 15000)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

write.xlsx(tweets_partis_politiques, "Fichiers/tweets_partis_politiques.xlsx")
#write.csv(tweets_partis_politiques, "Fichiers/tweets_partis_politiques.csv")
