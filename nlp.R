library(tm)
library(dplyr)
library(SnowballC) #Stemming
library(ggplot2)
library(stringr)
library(tidytext)
library(wordcloud)
library(reshape2)
library(tidyr)
library(widyr)
library(igraph)
library(ggraph)

# Insert data
ari <- read.csv("csv/ArianaGrande.csv", encoding = "UTF-8", na.strings = "nan")
bey <- read.csv("csv/Beyonce.csv", encoding = "UTF-8", na.strings = "nan")
eil <- read.csv("csv/BillieEilish.csv", encoding = "UTF-8", na.strings = "nan")
bts <- read.csv("csv/BTS.csv", encoding = "UTF-8", na.strings = "nan")
cardi <- read.csv("csv/CardiB.csv", encoding = "UTF-8", na.strings = "nan")
puth <- read.csv("csv/CharliePuth.csv", encoding = "UTF-8", na.strings = "nan")
cold <- read.csv("csv/ColdPlay.csv", encoding = "UTF-8", na.strings = "nan")
drake <- read.csv("csv/Drake.csv", encoding = "UTF-8", na.strings = "nan")
dua <- read.csv("csv/DuaLipa.csv", encoding = "UTF-8", na.strings = "nan")
ed <- read.csv("csv/EdSheeran.csv", encoding = "UTF-8", na.strings = "nan")
emi <- read.csv("csv/Eminem.csv", encoding = "UTF-8", na.strings = "nan")
bieb <- read.csv("csv/JustinBieber.csv", encoding = "UTF-8", na.strings = "nan")
katy <- read.csv("csv/KatyPerry.csv", encoding = "UTF-8", na.strings = "nan")
khal <- read.csv("csv/Khalid.csv", encoding = "UTF-8", na.strings = "nan")
gaga <- read.csv("csv/LadyGaga.csv", encoding = "UTF-8", na.strings = "nan")
mar <- read.csv("csv/Maroon5.csv", encoding = "UTF-8", na.strings = "nan")
minaj <- read.csv("csv/NickiMinaj.csv", encoding = "UTF-8", na.strings = "nan")
post <- read.csv("csv/PostMalone.csv", encoding = "UTF-8", na.strings = "nan")
riri <- read.csv("csv/Rihanna.csv", encoding = "UTF-8", na.strings = "nan")
sel <- read.csv("csv/SelenaGomez.csv", encoding = "UTF-8", na.strings = "nan")
swift <- read.csv("csv/TaylorSwift.csv", encoding = "UTF-8", na.strings = "nan")

df = rbind.data.frame(bey, eil, bts, cardi, puth, cold, drake, dua, ed, emi, 
                      bieb, katy, khal, gaga, mar, minaj, post, riri, sel, swift)
df = df[,2:7]
df = rbind.data.frame(df, ari)
df$Year = as.numeric(df$Year)
df$Decade = ifelse(df$Year<1990, "80s", ifelse(df$Year<2000, "90s",
                   ifelse(df$Year<2010, "00s", ifelse(df$Year<2020, "10s","20s"))))

#Let's inspect the dataset
summary(df)
#Keep songs with 10 or more characters
#Keep songs whose year appears higher than 1980
df <- 
  df %>% 
  distinct(Lyric, .keep_all = TRUE) %>%
  distinct(Title, .keep_all = TRUE) %>%
  filter(str_length(Lyric) > 30) %>% 
  filter(Year>1980)

#Remove duplicates
df = df[-c(grep("Remix", df$Title)),]
df = df[-c(grep("Edit", df$Title)),]
df = df[-c(grep("Acoustic", df$Title)),]
df = df[-c(grep("Live", df$Title)),c(1,2,3,4,6)]


#Create corpus
doc_id = seq(1,3182, by=1)

songs_data <- df %>% 
  mutate(doc_id = doc_id) %>%
  select(doc_id,text=Lyric,Artist,Title,Album, Year, Decade)

songs_source <- DataframeSource(songs_data)
songs_corpus <- VCorpus(songs_source)

#Text normalization
#Convert to lower case
songs_corpus <- songs_corpus %>% tm_map(content_transformer(tolower))
#Remove punctuation
songs_corpus <- songs_corpus %>% tm_map(removePunctuation)
#Remove numbers
songs_corpus <- songs_corpus %>% tm_map(removeNumbers)
#Remove multiple whitespace
songs_corpus <- songs_corpus %>% tm_map(stripWhitespace)
#Remove stop words
stopw = c(stopwords("en"), "oh", "uhhuh", "mhmmm", "yeah", "ayo", "ey", "yo",
         "ow", "owww", "ooh", "ah", "aah", "na", "la")
songs_corpus <- songs_corpus %>% tm_map(removeWords, stopw)
#Stemming
clean_corpus <- songs_corpus %>% tm_map(stemDocument)
#Term Document Matrix, to inspect by 
tdm = TermDocumentMatrix(clean_corpus)
#TDM to matrix for analysis
mat = as.matrix(tdm)
#Frequency of words analysis
term_frequency <- rowSums(mat)
term_frequency <- sort(term_frequency, decreasing = TRUE)
most_freq = data.frame(word=names(term_frequency), freq=term_frequency, row.names=NULL)
top_most_freq = most_freq[1:20,]

top_most_freq %>%
  ggplot(aes(x=word, y=freq)) +
  geom_segment(aes(x= reorder(word,desc(freq)), xend=word, y=0, yend=freq),color="skyblue", size=1) +
  geom_point(color="blue", size=4, alpha=0.6) +
  theme_minimal()+
  theme(text = element_text(size = 10),
        plot.title = element_text(size = 16,color = "#ff5a5f", face = "bold",margin                                   = margin(b = 7)),
        plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 7))) +
  xlab("Word") +
  ylab("Frequency")

################################################################

#Add new stop words
stopw = c(stopwords("en"), "oh", "uhhuh", "mhmmm", "yeah", "ayo", "ey", "yo",
          "ow", "owww", "ooh", "ah", "aah", "na", "la","can", "like",
          "get", "got", "dont", "babi", "baby", "make")
clean_corpus2 <- songs_corpus %>% tm_map(removeWords, stopw)

#Term Document Matrix, to inspect by 
tdm2 = TermDocumentMatrix(clean_corpus2)
#TDM to matrix for analysis
mat2 = as.matrix(tdm2)
#Frequency of words analysis
#frequency_words <- freq_terms(
#  songs_data$text, top = 15, at.least = 3, stopwords = stopw)
term_frequency2 <- rowSums(mat2)
term_frequency2 <- sort(term_frequency2, decreasing = TRUE)
most_freq2 = data.frame(word=names(term_frequency2), freq=term_frequency2, row.names=NULL)
most_freq2 = most_freq2[1:20,]

most_freq2 %>%
  ggplot(aes(x=word, y=freq)) +
  geom_segment(aes(x= reorder(word,desc(freq)), xend=word, y=0, yend=freq),color="skyblue", size=1) +
  geom_point(color="blue", size=4, alpha=0.6) +
  theme_minimal()+
  theme(text = element_text(size = 10),
        plot.title = element_text(size = 16,color = "#ff5a5f", face = "bold",margin                                   = margin(b = 7)),
        plot.subtitle = element_text(size = 10, color = "darkslategrey", margin =                                       margin(b = 7))) +
  xlab("Word") +
  ylab("Frequency")


#Sentyment analysis
bing = get_sentiments("bing")
most_freq%>% 
          inner_join(bing)%>%
          acast(word ~ sentiment,value.var="freq", fill = 0) %>%
          comparison.cloud(colors = c("red", "blue"),
          max.words = 300)

df$Lyric = df$Lyric%>%
            tolower() %>%
            removePunctuation() %>%
            stripWhitespace()

df_tidy <- df %>%
  unnest_tokens(word, Lyric) %>%
  distinct() %>%
  filter(!word %in% stopw) %>%
  filter(nchar(word) > 2) %>% 
  select(Artist,Title,Album,Year,Decade, word)

ratio_song <- df_tidy %>% 
  inner_join(bing) %>%
  group_by(Title,Artist, sentiment)%>%
  summarize(score = n()) %>%
  spread(sentiment, score) %>% 
  ungroup() %>%
  mutate(ratio = positive / (positive + negative), 
         Title = reorder(Title, ratio))

ratio_song %>%
  top_n(20) %>%
  ggplot(aes(x = Title, y = ratio)) +
  geom_point(aes(colour = factor(Artist)), size = 4) +
  coord_flip() +
  labs(title = "Top 20 Most Positive Songs",
       x = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        panel.grid = element_line(linetype = "dashed", color = "darkgrey", size = .5))

ratio_song %>%
  mutate(ratio = 1 - ratio, 
         Title = reorder(Title, ratio)) %>%
  top_n(20) %>%
  ggplot(aes(x = Title, y = ratio)) +
  geom_point(aes(colour = factor(Artist)), size = 4) +
  coord_flip() +
  labs(title = "Top 20 Most Negative Songs",
       x = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        panel.grid = element_line(linetype = "dashed", color = "darkgrey", size = .5))


#Rank artists pos vs neg
music_by_artist <- df %>% 
  group_by(Artist) %>% 
  summarise(n = n())
music_by_artist %>%
  ggplot(aes(x=Artist, y=n)) + 
  geom_bar(stat = 'identity') +
  labs(title = "Number of songs per artist", y="")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

artist_sent <- df_tidy %>% 
               inner_join(bing) %>%
               group_by(Artist, sentiment)%>%
               summarize(score = n()) %>%
               spread(sentiment, score) %>% 
               ungroup() %>%
               gather(positive, negative, key="Polarity", value="Words")
               #mutate(ratio = positive / (positive + negative), 
                #      Title = reorder(Title, ratio))
artist_sent %>%
  ggplot(aes(x=Artist, y=Words, fill=Polarity)) + 
  geom_bar(stat = 'identity', position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#Radar chart by decades
nrc = get_sentiments("nrc")

decade_sent <- df_tidy %>% 
  inner_join(nrc) %>%
  group_by(Decade, sentiment)%>%
  summarize(score = n()) %>%
  spread(sentiment, score) %>% 
  ungroup() 

ggradar(decade_sent[decade_sent$Decade=="80s",], grid.max = 80,
        plot.title = "80s sentiment")
ggradar(decade_sent[decade_sent$Decade=="90s",], grid.max = 2000,
        plot.title = "90s sentiment")
ggradar(decade_sent[decade_sent$Decade=="00s",], grid.max = 8000,
        plot.title = "00s sentiment")
ggradar(decade_sent[decade_sent$Decade=="10s",], grid.max = 18000,
        plot.title = "10s sentiment")
ggradar(decade_sent[decade_sent$Decade=="20s",], grid.max = 2600,
        plot.title = "20s sentiment")
