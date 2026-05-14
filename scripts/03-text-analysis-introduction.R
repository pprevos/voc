# Exploratory analysis of simulated interviews
library(tidyverse)
library(tidytext)

# ETL
interview_files <- list.files("data/synthetic-interviews", full.names = TRUE)

parse_transcript <- function(file_path) {
  raw_text <- read_file(file_path)
  # Extract the interview number
  interview_id <- str_extract(basename(file_path), "\\d+")
  # Split the content by the "INTERVIEWER:" tag
  sections <- str_split(raw_text, "INTERVIEWER:")[[1]]
  # Remove the first empty element if it exists (text before the first question)
  sections <- sections[sections != ""]
  map_df(sections, function(sec) {
    # Split the section into Question and Agent response
    parts <- str_split(sec, "AGENT:")[[1]]
    if (length(parts) >= 2) {
      tibble(
        interview_nr = as.integer(interview_id),
        question = str_trim(parts[1]),
        response = str_trim(parts[2])
      )
    } else {
      NULL # Handle cases where there is no response (e.g., final comments)
    }
  })
}

# Read and combine all transcripts into one data frame
interview_data <- map_df(interview_files, parse_transcript)

# Tokenise by word and count their frequency
interview_tokens <- interview_data |>
  unnest_tokens(word, response) |>
  anti_join(stop_words, by = "word") |>
  filter(!str_detect(word, "^[0-9]+$"))

# Generate the Wordcloud (top 100 words)
library(ggwordcloud)

word_counts_cloud <- interview_tokens |>
  count(word, sort = TRUE) |>
  top_n(50) |>
  mutate(rotation = sample(c(0, 90), n(), replace = TRUE))

ggplot(word_counts_cloud) +
  aes(label = word, size = n, angle = rotation) +
  geom_text_wordcloud(shape = "square", rm_outside = TRUE) +
  scale_size_area(max_size = 15) +
  coord_fixed() +
  theme_void()

# Remove filler words and generate barchart
word_counts_bar <- interview_tokens |>
  filter(word %notin% c("water", "um", "ah", "ya", "yeah", "em", "eh",
                        "uh", "mate", "bit", "bloody", "time", "righto")) |>
  count(word, sort = TRUE) |>
  top_n(10) |>
  mutate(word = fct_reorder(word, n))

ggplot(word_counts_bar) +
  aes(word, n) +
  geom_segment(aes(xend = word, yend = 0)) +
  geom_point(size = 8, col = "darkgrey") +
  coord_flip() +
  theme_minimal(base_size = 28) +
  labs(x = "Word", y = "Count")

# Visualise TF-IDF
interview_tfidf <- interview_tokens |>
  count(interview_nr, word, sort = TRUE) |>
  bind_tf_idf(word, interview_nr, n) |>
  group_by(interview_nr) |>
  slice_max(tf_idf, n = 10, with_ties = FALSE) |>
  ungroup() |>
  mutate(interview_nr = paste("Interviewee", interview_nr))

  ggplot(interview_tfidf) +
    aes(tf_idf,
        reorder_within(word, tf_idf, interview_nr)) +
    geom_col(fill = "grey") +
    scale_y_reordered() +
    facet_wrap(~interview_nr, scales = "free") +
    labs(x = "TF-IDF Score",
         y = NULL) +
    theme_minimal(base_size = 20)
