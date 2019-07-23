library(stringr)
library(ggplot2)
library(tidyr)
library(devtools)
library(lubridate)
library(dplyr)
library(rtweet)

args = commandArgs(trailingOnly=TRUE)
dataini <- args[1]
datafim <- args[2]
terms <-c("Bolsonaro")

df_tweets <-data.frame()
for(i in 1:length(terms)){
  term <- terms[i]
  tweets <-search_tweets(term, n=30000, since = dataini, until = datafim, lang="pt-br",geocode="-9.92198,-45.47674,500mi")
}

saveRDS(tweets,file=paste0("tweets_bolsonaro_nordeste_",dataini,".rds"))

