library(tidyverse)
library(readr)
library(ggthemes)
library(sqldf)
library(sqldf)
library(tidytext)
library(tm)
library(quanteda)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(wordcloud)

kickstarter <- read.csv("kickstarter_projects.csv", stringsAsFactors = FALSE)

kickstarter <- kickstarter %>% filter (! duplicated(kickstarter$id))

kickstarter$achievement_ratio <- ((kickstarter$pledged/kickstarter$goal)*100)

kick1 <- sqldf("select top_category as Categories, avg(pledged) as Average from kickstarter where state = 'successful' or state = 'live' group by top_category")

Ans1a <- ggplot(data = kick1, aes(x = reorder(Categories, -Average), y = Average, fill =Average))+
  geom_bar(stat= "identity",show.legend = FALSE)+
  xlab("")+
  ylab("")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle=90, size=rel(1), hjust=1))+ ggtitle("Most successful categories")

Ans1a

uscities <- read.csv("uscitiesv1.3.csv")

kicklocation <- sqldf("Select city,state_name, lat, lng, state, pledged, achievement_ratio from kickstarter k join uscities u on k.location_town = u.city where k.location_state = u.state_id")

kicksuccess <- sqldf("Select state_name, count(state) as Number from kicklocation where state = 'successful' or state = 'live' group by state_name order by Number DESC Limit 10")

Bonus1 <- ggplot(kicksuccess, aes(x = reorder(state_name, -Number), y=Number)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=state_name, xend=state_name, y=0, yend=Number)) +
  xlab("")+
  ylab("")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle=90, size=rel(1), hjust=1))+ ggtitle("Top 10 States by successful Kickstarter projects")

Bonus1

kicklocation1 <- sqldf("Select city,state_name, lat, lng, avg(pledged) as Number from kicklocation where state = 'successful' or state = 'live' group by city order by Number DESC Limit 50")

kicklocation2 <- sqldf(("Select city,state_name, lat, lng, avg(pledged) as Number from kicklocation where state = 'successful' or state = 'live' group by city"))

successIcons <- icons(
  iconUrl = "success.png",
  iconWidth = 15, iconHeight = 15,
  iconAnchorX = 7.5, iconAnchorY = 7.5)

Bonus2 <- leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik",options = providerTileOptions(attribution = ""))%>%
  addCircleMarkers(data=kicklocation2,lng = ~lng, lat = ~lat, radius = 5, stroke = 2, opacity = 0.7, 
                   popup=~paste("City Name:",kicklocation2$city,"<br>",
                                "State:",kicklocation2$state_name,"<br/>",                                                "Avg Pledged Amount:",kicklocation2$Number,"<br/>"),clusterOptions = markerClusterOptions())%>%
  addMarkers(data=kicklocation1,lng = ~lng, lat = ~lat,
             icon = successIcons,
             popup=~paste("City Name:",kicklocation1$city,"<br>",
                          "State:",kicklocation1$state_name,"<br/>",
                          "Avg Pledged Amount:",kicklocation1$Number,"<br/>"))%>%
  setView(lng = -95.7129, lat = 37.0902, zoom = 3)%>%
  addEasyButton(easyButton(
    icon="fa-globe", 
    title="New York City",
    onClick=JS("function(btn, map){ map.setZoom(3); }")))

Bonus2

#Ques2

top1000 <- kickstarter%>%top_n(1000, achievement_ratio)
bottom1000 <- kickstarter%>%top_n(-1000, achievement_ratio)
bottom1000<- head(bottom1000,1000)

top1000_corpus <- Corpus(VectorSource(top1000$blurb))
bottom1000_corpus <- Corpus(VectorSource(bottom1000$blurb))


clean <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))  
  corpus <- tm_map(corpus, removeNumbers)
  
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

top1000_corpus_clean <- clean(top1000_corpus)
bottom1000_corpus_clean <- clean(bottom1000_corpus)

#top1000_stemmed <- tm_map(top1000_corpus_clean, stemDocument)
#bottom1000_stemmed <- tm_map(bottom1000_corpus_clean, stemDocument)

stemCompletion <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

#top1000_comp <- lapply(top1000_stemmed, stemCompletion, 
                     dictionary=top1000_corpus_clean)

#DTM
top1000_dtm <- DocumentTermMatrix(top1000_corpus_clean, control = list(weighting = function(x) weightTfIdf(x, normalize = T)))
top1000_m <- as.matrix(top1000_dtm)

bottom1000_dtm <- DocumentTermMatrix(bottom1000_corpus_clean, control = list(weighting = function(x) weightTfIdf(x, normalize = T)))


#TDM
top1000_tdm <- TermDocumentMatrix(top1000_corpus_clean)
top1000_tdm1 <- as.matrix(top1000_tdm)


top1000_td<- tidy(top1000_dtm)
head(top1000_td)

bottom1000_td<- tidy(bottom1000_dtm)
head(bottom1000_td)


top15_words <- top1000_td %>% group_by(term) %>%
  summarise(n = sum(count)) %>%
  top_n(n = 15, wt = n)  %>%
  ungroup() %>%
  mutate(term = reorder(term, n))

top1000_tf_idf <-  top1000_td %>%
  bind_tf_idf(term, document, count) %>%  
  arrange(desc(tf_idf)) 

bottom1000_tf_idf <-  bottom1000_td %>%
  bind_tf_idf(term, document, count) %>%  
  arrange(desc(tf_idf))

bottom1000_tf_idf

#word cloud
set.seed(3)
wordcloud(top1000_tf_idf$term, top1000_tf_idf$tf, max.words = 50, colors = c("#a1d99b", "#74c476", "#41ab5d", "#238b45", "#006d2c", "#00441b"), scale =c(2,.5))

top1000_tf_idf$state<- "successful"
bottom1000_tf_idf$state <- "unsuccessful"

top10_tf_idf<- head(top1000_tf_idf,10)
bottom10_tf_idf<- head(bottom1000_tf_idf,10)

all_tf_idf<- rbind(top10_tf_idf,bottom10_tf_idf)

combined <- merge(top1000_tf_idf,bottom1000_tf_idf,by="term")
combined_final<- combined[!duplicated(combined$term), ]
combined_final<-arrange(combined_final,desc(count.x))
combined_final<- head(combined_final,20)

library(plotrix)

Ans2b <- pyramid.plot(combined_final$count.x, combined_final$count.y,labels = combined_final$term,top.labels = c("successful", " ", "unsuccessful"),main = "Common Words",lxcol=c("#2ca25f"),rxcol=c("#de2d26"),gap=2,unit = NULL, labelcex=0.7)

Ans2b

#Ans2c

library(readability)
library(quanteda)

FRE_corpus <- corpus(top1000_corpus)

FRE_top1000 <- textstat_readability(FRE_corpus,measure =c('Flesch.Kincaid'))

FRE_top1000

FRE_top1000$achievement_ratio <- top1000$achievement_ratio 

Ans2c<- ggplot(data=FRE_top1000, aes(y=achievement_ratio /1000 , x= Flesch.Kincaid, color = Flesch.Kincaid ))+
  geom_point(alpha=0.7, size=3,show.legend = FALSE)+ geom_smooth(method="loess", se=F,show.legend = FALSE) + 
  scale_color_gradient(low = "#0091ff", high = "#f0650e")+
  theme_bw() +
  xlab("FRE")+
  ylab("Achievement Ratio")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

Ans2c

#Ques3
pos <- read.table("positive-words.txt", as.is=T)
neg <- read.table("negative-words.txt", as.is=T)

sentiment <- function(words=c("really great good stuff bad")){
  require(quanteda)
  tok <- quanteda::tokens(words)
  pos.count <- sum(tok[[1]]%in%pos[,1])
  neg.count <- sum(tok[[1]]%in%neg[,1])
  out <- (pos.count - neg.count)/(pos.count+neg.count)
  return(out)
}

for(i in 1:1000)
{top1000$tone[i] <- sentiment(top1000_corpus[[i]]$content)}

Ans3a<- ggplot(data=top1000[is.finite(top1000$tone), ], aes(x=achievement_ratio/1000 , y= tone, color = tone, na.rm = FALSE))+
  geom_point(alpha=0.7, size=3,show.legend = FALSE,na.rm = FALSE)+ geom_smooth(method="gam", se=F,show.legend = FALSE,na.rm = FALSE) + 
  scale_color_gradient(low = "#ffeda0", high = "#2ca25f")+
  theme_bw() +
  ylab("Tone")+
  xlab("Achievement Ratio")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

Ans3a

#Ques 3b

for(i in 1:1000)
{bottom1000$tone[i] <- sentiment(bottom1000_corpus[[i]]$content)}

topnbottom <- rbind(top1000, bottom1000)

topnbottom$PosNeg<- ifelse(((as.numeric(as.character(topnbottom$tone))) < 0), "Negative", "Positive")

topnbottom1 <- na.omit(topnbottom)

topnbottom1_pos <- paste(topnbottom1$blurb[topnbottom1$PosNeg =="Positive"],collapse = " ")
topnbottom1_neg <- paste(topnbottom1$blurb[topnbottom1$PosNeg =="Negative"],collapse = " ")
comb <- c(topnbottom1_pos, topnbottom1_neg)

## Referred: https://www.kaggle.com/elvinouyang/identifying-the-buzzwords-in-kickstarter-part-ii/code

create_clean_corpus <- function(text_vector){
  text_corpus_clean <- VCorpus(VectorSource(text_vector))
  text_corpus_clean <- tm_map(text_corpus_clean, removePunctuation)
  text_corpus_clean <- tm_map(text_corpus_clean, content_transformer(tolower))
  text_corpus_clean <- tm_map(text_corpus_clean, removeNumbers)
  text_corpus_clean <- tm_map(text_corpus_clean, removeWords,c(stopwords()))
  text_corpus_clean <- tm_map(text_corpus_clean, removePunctuation)
  text_corpus_clean <- tm_map(text_corpus_clean, stripWhitespace)
  return(text_corpus_clean)
}

comb_clean <- create_clean_corpus(comb)
combined_tdm <- TermDocumentMatrix(comb_clean, control=list(weighting = weightTfIdf))
colnames(combined_tdm) <- c("Positive", "Negative")
combined_tdm <- as.matrix(combined_tdm)

set.seed(25)
comparison.cloud(combined_tdm, colors = c("green", "red"), max.words = 100,scale=c(1,1.8))

#Ques 3c

myvars <- c("blurb", "pledged", "achievement_ratio")
top1000new <- top1000[myvars]

top1000new <- top1000new %>% 
  mutate (number = row_number())


top1000new_td <- top1000new %>% 
  unnest_tokens(word, blurb)

top1000new_td <- top1000new_td %>% 
  anti_join(stop_words)

sentiment_nrc <- top1000new_td %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(number, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  setNames(c(names(.)[1],paste0('nrc_', names(.)[-1]))) %>%
  mutate(score_nrc = nrc_positive - nrc_negative) %>%
  ungroup()

top1000new_full <- full_join(top1000new, sentiment_nrc)  %>% 
  mutate_each(funs(replace(., which(is.na(.)), 0)), starts_with("nrc"))

Ans3c <- top1000new_full %>%
  gather(emotion, intensity,starts_with("nrc_")) %>%
  mutate(emotion = substring(emotion,5)) %>%
  filter(!emotion %in% c("positive", "negative")) %>%
  ggplot(aes(x=achievement_ratio/1000, y=intensity)) +
  geom_smooth(se = FALSE,method = 'gam') + facet_wrap(~emotion, nrow = 2)+
  scale_color_brewer(palette="Dark2")+
    xlab("Achievement Ratio")+
    ylab("Intensity")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle=90, size=rel(1), hjust=1))+ ggtitle("Different Emotions with respect to Achievement Ratio")

Ans3c