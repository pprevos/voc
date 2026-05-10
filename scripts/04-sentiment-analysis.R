# Sentiment Analysis examples

library(tidytext)
library(dplyr)

# Generate test corpus
sentences <- c("Tap water tastes awful",
               "The water pressure is not too bad",
               "I can't afford to pay the bill this month",
               "How much water did I use last month?",
               "The water tastes like chlorine",
               "Fluoride in tap water calcifies the pineal gland")

test_corpus <- data.frame(id = 1:length(sentences),
                          text = sentences)

rename(test_corpus, No = id, Text = text)

# Compare lexicon methods
data.frame(text = "Water restrictions are bad for wellbeing") |>
  unnest_tokens(word, text) |>
  left_join(get_sentiments("afinn"), by = "word") |>
    rename(AFINN = value) |>
    left_join(get_sentiments("bing"), by = "word") |>
    rename(Bing = sentiment) |>
    left_join(get_sentiments("loughran"), by = "word") |>
    rename(Loughran = sentiment, Lexicon = word) |>
    mutate(across(everything(), ~ replace(., is.na(.), ""))) |>
    t()

# Use NRC lexicon
nrc <- test_corpus |>
  unnest_tokens(word, text) |>
  left_join(get_sentiments("nrc")) |>
  filter(!is.na(sentiment)) |>
  count(sentiment) |>
  mutate(sentiment = fct_reorder(str_to_title(sentiment), n))

# Set standard theme
library(ggplot2)
set_theme(theme_bw(base_size = 16))

ggplot(nrc) +
  aes(sentiment, n) +
  geom_col(fill = "grey", col = "darkgrey") +
  coord_flip() + 
  labs(x = "Sentiment", y = "Occurence")

data.frame(s = "The utility provides disgusting water and can be frustrating.") |>
  unnest_tokens(word, s) |>
  left_join(get_sentiments("afinn"))

# AFINN method

# Tokenise in to single words and apply AFINN lexicon
library(dplyr)
library(tidytext)

sentiments_afinn <- test_corpus |>
  unnest_tokens(word, text) |>
  left_join(get_sentiments("afinn"), by = "word") |>
  group_by(id) |>
  summarise(word_count = n(), 
            pos_sum = sum(value[value > 0], na.rm = TRUE),
            neg_sum = abs(sum(value[value < 0], na.rm = TRUE)),
            net_sum = sum(value, na.rm = TRUE)) |>
  right_join(test_corpus, by = "id") |>
  select(-text) |> 
  mutate(valence = if_else((pos_sum + neg_sum) != 0, 
  (pos_sum - neg_sum) / (pos_sum + neg_sum), 0),
  sentiment = round(net_sum / word_count, 1),
  subjectivity = round((pos_sum + neg_sum) / word_count, 1))

# Present the results
test_corpus |>
  left_join(sentiments_afinn) |>
  select(Text = text, Total = net_sum, Valence = valence,
         Sentiment = sentiment, Subjectivity = subjectivity)

# Sentimentr package
# https://cran.r-project.org/web/packages/sentimentr/
library(sentimentr)

# Caluclate sentiment scores
sentiments <- sentiment(sentences)

# Display
test_corpus |>
  mutate(Sentiment = round(sentiments$sentiment, 2)) |>
  select(Text = text, Sentiment)

# Ollamar library to interact with local LLMs
library(ollamar)
library(httr2)
library(purrr)

# Function to score a vector of sentences
score_sentiments <- function(sentences, model = "llama3.1:latest", temp = 0) {
  system_prompt <- "You are an analyst that performs sentiment analysis on text towards tap water, you must score each text with a floating-point number representing the sentiment of the text in a scale from -1.0 (negative) to 1.0 (positive), where 0.0 represents neutral sentiment. Provide ONLY the number as your response."
  # Define the list of prompts
  requests <- lapply(sentences, function(text) {
    msgs <- create_messages(create_message(system_prompt, role = "system"),
                            create_message(text, role = "user"))
    chat(model = model,
         messages = msgs,
         output = "req",
         temperature = temp)})
  
  message(paste("Processing", length(sentences), "sentences ..."))
  
  # Run the API
  responses <- req_perform_parallel(requests)
  
  # Process the responses
  scores_raw <- sapply(responses, resp_process, "text")
  scores_numeric <- round(as.numeric(gsub("[^0-9.-]", "", scores_raw)), 1)
  return(data.frame(sentences, scores_raw, scores_numeric))
}

score_sentiments(test_corpus$Text) |>
  select(Text = sentences, Sentiment = scores_numeric)

# Test variance of LLM responses (change temperature or model name)
score_sentiments(rep("I can't afford to pay the bill this month", 100), 
                 temp = 1) |> 
  dplyr::pull(scores_numeric) |>
  summary()

# Tap water tweets case study
library(stringr)
library(stringi)
library(readr)
library(ggplot2)

# ETL
tapwater_tweets <- read_csv("data/tapwater_tweets.csv") |>
  mutate(text = stri_enc_toutf8(text),
         text = str_replace_all(text, "https?://\\S+|www\\.\\S+", ""),
         text = str_replace_all(text, "@\\w+", ""),
         text = stri_replace_all_regex(text, "[^\\p{L}\\p{N}\\p{P}\\p{Z}]", " "),
         text = str_squish(text),
         id = as.factor(id)) |>
  filter(!is.na(text), text != "")

## Tokenise and clean the tweets
tidy_tweets <- tapwater_tweets |>
  unnest_tokens(word, text) |>
  anti_join(stop_words) |>
  filter(!word %in% c("tap", "water", "rt", "gt", "amp", as.character(0:9)))

## Most common words
tidy_tweets_count <- tidy_tweets |>
  count(word, sort = TRUE) |>
  top_n(10) |>
  mutate(word = reorder(word, n))

ggplot(tidy_tweets_count) + 
  aes(word, n) +
  geom_col(fill = "lightgrey", col = "darkgrey") +
  labs(x = NULL, y = "Occurence") + 
  coord_flip()

# Sentiment analysis multiple methods
library(magrittr)
library(tidyr)
library(forcats)
library(sentimentr)

# AFINN lexicon
tweets_afinn <- tidy_tweets |>
  inner_join(get_sentiments("afinn")) |>
  group_by(id) |>
  summarise(AFINN = mean(value)) |>
  mutate(AFINN = AFINN / 5)

# sentimentr
tweets_sentimentr <- tapwater_tweets %$%
  sentiment_by(get_sentences(text),
               list(id)) |>
  select(id, sentimentr = ave_sentiment)

llama_results <- "data/tapwater_tweets_sentiments__llma.csv"

# file.remove(llama_results)

if (!file.exists(llama_results)) {
  sentiments_llama <- score_sentiments(tapwater_tweets$text)
  
  tweets_llama <- tapwater_tweets |>
    bind_cols(sentiments_llama, by = c("text" = "sentences")) |>
    select(id, Llama = scores_numeric)

  write_csv(tweets_llama, llama_results)
} else {
  tweets_llama <- read_csv(llama_results) |>
    mutate(id = factor(id))}

# Combine results and visualise / analyse
tapwater_sentiments <- tapwater_tweets |> 
  left_join(tweets_afinn) |>
  mutate(AFINN = if_else(is.na(AFINN), 0, AFINN)) |>
  left_join(tweets_sentimentr) |>
  left_join(tweets_llama) |>
  select(-text) |>
  pivot_longer(-id, names_to = "Method", values_to = "Score") |>
  mutate(Method = fct_relevel(Method, c("AFINN", "sentimentr", "Llama")))

ggplot(tapwater_sentiments) +
  aes(Method, Score) +
  geom_boxplot(fill = "lightgrey") +
  coord_flip() + 
  theme_minimal(base_size = 24) +
  labs(x = NULL)

# Compare sentiment distributions 
tapwater_sentiments_test <- tapwater_sentiments |>
  filter(!id %in% tapwater_sentiments$id[is.na(tapwater_sentiments$Score)]) |>
  mutate(id = factor(id))

friedman.test(Score ~ Method | id, data = tapwater_sentiments_test)
