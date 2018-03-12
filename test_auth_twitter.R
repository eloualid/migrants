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

usePackage('httr')
usePackage('httpuv')

# 1. Find OAuth settings for twitter:
#    https://dev.twitter.com/docs/auth/oauth
oauth_endpoints("twitter")

# 2. Register an application at https://apps.twitter.com/
#    Make sure to set callback url to "http://127.0.0.1:1410/"
#
#    Replace key and secret below
myapp <- oauth_app("twitter",
                   key = "5MCG6Lwl3JZPHJAMS0ktEHY17",
                   secret = "tvTpe8h7mh1F74I4THXNkJlp2VzTAjdMPo7k4xhIsgvB5YOqWa"
)

# 3. Get OAuth credentials
twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp)

# 4. Use API
req <- GET("https://api.twitter.com/1.1/statuses/home_timeline.json",
           config(token = twitter_token))
stop_for_status(req)
content(req)
