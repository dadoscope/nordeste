library(stringr)
library(ggplot2)
library(tidyr)
library(devtools)
library(lubridate)
library(dplyr)
library(rtweet)

api_key = "3I8JehozX8N4Bojg0qSdmDFLX"
api_secret = "QBg2E2TcqtvlK0vrRIwIWZGSXjlPoYaLXwjrePRDLztepAU6cg"
access_token = "2433072234-seryr8c6OCyNU5veonMKf5hvX8JKCUOiA20TcoC"
access_token_secret = "rHOyUzdhL5GN6lqWh3LOxmuKD2hb0IvpeJm4hrs24O4nx"
   
args = commandArgs(trailingOnly=TRUE)
cat(length(args))
if(length(args) > 2)
{
   dataini <- args[1]
   datafim <- args[2]
   terms <- args[-c(1,2)]
}else{
   terms <-c("Bolsonaro")
   dataini <-c("2019-07-18")
   datafim <-c("2019-07-22")
}

df_tweets <-data.frame()
for(i in 1:length(terms)){
  term <- terms[i]
  tweets <-search_tweets(term, n=20000, since = dataini, until = datafim, lang="pt-br",geocode="-9.92198,-45.47674,500mi")
}

save(tweets,file=paste0("df_tweets_bolsonaro_nordeste_",dataini,".Rdat"))

