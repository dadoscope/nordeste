library(abjutils)
library(abjData)
library(tidyverse)
library(brazilmaps)
library(colorRamps)
library(ggrepel)
library(JLutils)#devtools::install_github("larmarange/JLutils")

setwd("/home/charles/GitRepos/dadoscope/nordeste")
votos <- readRDS("tweets_nordeste_20190721.rds")
votos <- rbind(votos, readRDS("tweets_bolsonaro_nordeste_2019-07-22.rds"))
votos <- rbind(votos, readRDS("tweets_bolsonaro_nordeste_2019-07-20.rds"))

ceara <- c("FORTALEZA, BRASIL", "FORTALEZA - CEARÁ - BRASIL", "FORTALEZA","FORTALEZA CEARÁ","CEARÁ, BRASIL","FORTALEZA-CE","FORTALEZA, CEARÁ","MOSSORÓ, BRASIL") 
bahia <- c("SALVADOR","ILHÉUS -BAHIA, BRASIL","BAHIA, BRASIL","SALVADOR, BRASIL","ILHÉUS","SALVADOR-BA","BAHIA","SALVADOR, BAHIA","SALVADOR - BAHIA") 
pernambuco <- c("OLINDA, BRASIL","CARUARU PERNAMBUCO BRASIL","PERNAMBUCO, BRASIL","RECIFE, BRASIL","RECIFE","RECIFE, PE","PERNAMBUCO-BRASIL","GARANHUNS, BRASIL","PERNAMBUCO","JANGA PAULISTA PERNAMBUCO ") 
paraiba <- c("JOÃO PESSOA","CAMPINA GRANDE, BRASIL","JOÃO PESSOA, BRASIL" )    
riograndedonorte <- c("NATAL, BRASIL","NATAL/RN","RN BRASIL","NATAL RN") 
piaui <- c("TERESINA, BRASIL") 
maranhao <- c("SÃO LUÍS, BRASIL") 
alagoas <- c("MACEIÓ, BRASIL","ALAGOAS, BRASIL") 
sergipe <- c("ARACAJU, BRASIL")  
nordeste <- c(ceara, 
              bahia, 
              pernambuco, 
              paraiba, 
              riograndedonorte, 
              piaui, 
              maranhao, 
              sergipe, 
              alagoas)

votos <- votos %>% mutate(location = toupper(location))
votos <- votos %>% filter(location %in% nordeste)
votos$UF <- rep(NA, nrow(votos))
votos$UF[which(votos$location %in% bahia)] <- "BA"  
votos$UF[which(votos$location %in% ceara)] <- "CE"  
votos$UF[which(votos$location %in% pernambuco)] <- "PE"  
votos$UF[which(votos$location %in% paraiba)] <- "PB"  
votos$UF[which(votos$location %in% riograndedonorte)] <- "RN"  
votos$UF[which(votos$location %in% piaui)] <- "PI"  
votos$UF[which(votos$location %in% maranhao)] <- "MA"  
votos$UF[which(votos$location %in% alagoas)] <- "AL"  
votos$UF[which(votos$location %in% sergipe)] <- "SE"  

pnud_muni <- abjData::pnud_muni
br_uf_map <- abjData::br_uf_map
pnud_uf <- abjData::pnud_uf

votos_por_uf <- votos %>% 
  ungroup() %>%
	group_by(UF) %>% 
  dplyr::summarise(total = n())

votos %>% 
  filter(!is.na(hashtags)) %>%
  unnest(hashtags) %>%
  mutate(hashtags = toupper(hashtags))%>%
  group_by(UF, hashtags) %>% 
  dplyr::summarise(hashs = n()) %>% 
  arrange(hashs, hashtags) %>% 
  tail(20)

uf_code <- data.frame(UF = c("MA",
                                     "PI",
                                     "CE",
                                     "RN",
                                     "PB",
                                     "PE",
                                     "AL",
                                     "SE",
                                     "BA"), 
                              hashtag = c("200DIASDEVERGONHA",
                                          "BOLSONAROXENÓFOBO",
                                          "VAZAJATO",
                                          "NORDESTECOMBOLSONARO",
                                          "ORGULHODESERNORDESTINO",
                                          "ORGULHODONORDESTE",
                                          "ALAGOASALERTA",
                                          "BOLSONAROENVERGONHAOBRASIL",
                                          "SANATÓRIOGERAL"),
                              code = c(21,22,23,24,25,26,27,28,29))

votos_por_uf <- uf_code %>% left_join(votos_por_uf)

uf_map <- get_brmap("State") %>% filter(Region == "2")
uf_map <- uf_map %>% inner_join(votos_por_uf, c("State" = "code"))

palette <- colorRampPalette(c("red", "orange", "cyan"))(100)

p1 <- ggplot(uf_map) +
	geom_sf(data = uf_map, aes(fill = total)) +
        geom_sf_label(data = uf_map, aes(label = total))+	
	scale_fill_gradientn(colors=palette,breaks=c(0,200,400,600,800,1000,1200),limits=c(0,1400))+
	theme_bw() +
	labs(fill = "# de Tuítes", title = "Número de tuítes contendo o termo 'Bolsonaro'")

png("mapa_tuites_bolsonaro_nordeste.png",width=4800,height=2700,res=300)
print(p1)
dev.off()


p2 <- ggplot(uf_map) +
  geom_sf(data = uf_map, aes(fill = total)) +
  geom_sf_label(data = uf_map, aes(label = hashtag))+	
  scale_fill_gradientn(colors=palette,breaks=c(0,200,400,600,800,1000,1200),limits=c(0,1400))+
  theme_bw() +
  labs(fill = "# de Tuítes", title = "Principais hashtags associadas ao termo 'Bolsonaro'")

png("mapa_hashtags_bolsonaro_nordeste.png",width=4800,height=2700,res=300)
print(p2)
dev.off()


p3 <- votos %>% ungroup() %>%
  filter(!is.na(hashtags)) %>%
  unnest(hashtags) %>%
  mutate(hashtags = toupper(hashtags)) %>%
  group_by(hashtags)%>%
  dplyr::summarise(total = n()) %>%
  arrange(total)%>%
  tail(20) %>%
  ggplot(aes(x=reorder(hashtags,total), y = total, fill = "red")) +
  geom_bar(stat="identity", show.legend = FALSE)+	
  theme_bw() +
  coord_flip()+
  labs(title = "Principais hashtags associadas ao termo 'Bolsonaro' no Nordeste", x = "Hashtags", y = "Número de tuítes")

png("principais_hashtags_bolsonaro_nordeste.png",width=4800,height=2700,res=300)
print(p3)
dev.off()
