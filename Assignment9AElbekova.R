# Aidai Elbekova
# Assignment 9

mytext <- schrute::theoffice
library(rvest)
library(tidyverse)
library("ggplot2")
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(textdata)
library(dplyr)

# 1. In a scatter plot diagram, show how the sentiment score and IMDB rating of 
# each episode are related. Do you detect any correlation?

tidy_schrute <- mytext %>%
  unnest_tokens(word, text) %>% anti_join(stop_words)

schrute_sentiment <- tidy_schrute %>%
  inner_join(get_sentiments("bing")) %>%
  count(season, episode, sentiment) %>%
  pivot_wider(names_from=sentiment, values_from=n, values_fill=0) %>% 
  mutate(sentiment=(positive-negative)/(positive+negative))  

schrute_imdb <- tidy_schrute %>% 
  group_by(season, episode) %>% 
  summarize(imdb_ep = mean(imdb_rating)) %>%
  unite(seas_ep, season, episode, sep=".")

schrute_sentiment_new <- cbind(schrute_sentiment, imdb_ep=schrute_imdb$imdb_ep,
                               seas_ep=schrute_imdb$seas_ep)

ggplot(schrute_sentiment_new, aes(x=sentiment, y=imdb_ep)) + 
  geom_point(color="blue")

# There is no clear correlation

# 2. Plot a heatmap that shows the average sentiment among different seasons 
# and episodes.
schrute_m <- schrute_sentiment_new %>%
  select(seas_ep, imdb_ep, sentiment)
schrute_m$seas_ep <- as.numeric(schrute_m$seas_ep)
schrute_matrix <-as.matrix(schrute_m)

heatmap(schrute_matrix)

# 3. In a bar graph, show the most positive 10 characters and the average 
#sentiment score of their lines.
schrute_sentiment_character <- tidy_schrute %>%
  inner_join(get_sentiments("bing")) %>%
  count(character, sentiment) %>%
  pivot_wider(names_from=sentiment, values_from=n, values_fill=0) %>% 
  mutate(sentiment=(positive-negative)/(positive+negative)) %>%
  arrange(desc(positive))%>%
  head(10) %>%
  arrange(desc(sentiment))

schrute_sentiment_character %>%
  ggplot(., aes(x=sentiment, y=character))+
  geom_bar(stat='identity')

# 4. In four separate bar plots, show the average intensity of emotions, 
# belonging to the characters Michael, Dwight, Pam and Jim.
schrute_group <- tidy_schrute%>%
  filter(character=="Michael"|
           character=="Dwight"|
           character=="Pam"|
           character=="Jim") %>%
  inner_join(get_sentiments("bing")) %>%
  count(season, episode, character, sentiment) %>%
  pivot_wider(names_from=sentiment, values_from=n, values_fill=0) %>% 
  mutate(sentiment=(positive-negative)/(positive+negative))

schrute_group %>%
  ggplot(aes(x=character,y=sentiment, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  theme_bw()

# 5. Visualize the common words contributing to each emotion in a comparison 
# cloud (There should be 8 different word groups in your cloud.)

schrute_emotion <- tidy_schrute %>%
  inner_join(get_sentiments("nrc")) %>%
  count(character, episode, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from=n, values_fill=0) %>%
  select(-negative, -positive) 
