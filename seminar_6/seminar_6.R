# Seminar 6

# Load required libraries
library(quanteda)
library(readtext)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(tm)
library(wordcloud)
library(quanteda.textstats)

# Part 1: Sentiment Analysis

# Read the tone dictionary CSV file as a dataframe
tone_dict_df <- read.csv2("Ballads_sent_dict.csv", header = TRUE)

# Filter positive and negative words based on the Score column
positive_words <- tone_dict_df$Word[tone_dict_df$Score > 0]
negative_words <- tone_dict_df$Word[tone_dict_df$Score < 0]

# Create a dictionary object from positive and negative words
tone_dict <- dictionary(list(positive = positive_words, negative = negative_words))

# Read in the Ukrainian ballad corpus as a text file
balladsent <- readtext("Ukr_ballads")

# Create a corpus object from the text file
balladcorpus <- corpus(balladsent)

# Tokenize the corpus, removing punctuation, symbols, numbers, and separators
balladcorpustok <- tokens(balladcorpus, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_separators = TRUE)
balladcorpustok <- tokens_tolower(balladcorpustok)

# Create a document-feature matrix from the tokenized corpus
balladcorpussent <- dfm(balladcorpustok)

# Apply the tone dictionary to the document-feature matrix
balladcorpussent <- dfm_lookup(balladcorpussent, dictionary = tone_dict)

# Convert the document-feature matrix to a data frame
sentiment_df <- convert(balladcorpussent, to = "data.frame")

# Calculate the relative sentiment score for each document (the ratio of positive words to negative words)
sentiment_df <- sentiment_df %>%
  mutate(relsent = ifelse(negative == 0, NA, positive / negative)) %>%
  arrange(relsent)

# Remove the '.txt' extension from the document IDs in the data frame
sentiment_df$doc_id <- gsub(".txt", "", as.character(sentiment_df$doc_id))

# Create a scatter plot of relative sentiment scores versus document IDs
ggplot(data = subset(sentiment_df, !is.na(relsent)), mapping = aes(x = relsent, y = reorder(doc_id, relsent))) +
  geom_point() +
  ggtitle("Relative Sentiment Scores of Ukrainian Ballads")

# Reshape the data frame so that the positive and negative word counts are in a single column
sent_track <- sentiment_df %>%
  pivot_longer(cols = c(positive, negative), names_to = "posneg", values_to = "value")

# Create a stacked bar chart showing the proportion of positive and negative words in each document as a percentage
ggplot(sent_track, aes(fill = posneg, y = reorder(doc_id, -relsent), x = value * 100)) +
  geom_bar(position = "fill", stat = "identity") +
  ggtitle("Ukrainian Ballads: Sentiment Words Proportion") +
  scale_fill_manual(values = c("lightblue", "lightpink")) +
  scale_x_continuous(labels = percent_format()) +
  xlab(label = NULL) +
  ylab(label = NULL) +
  labs(fill = NULL)

# Calculate the frequency of each word in the document-feature matrix
word_freq <- textstat_frequency(balladcorpussent)
print(head(word_freq, 20))

# Part 2: Detailed Sentiment Analysis

# Read the tone dictionary CSV file as a dataframe
tone_dict_df <- read.csv2("Ballads_sent_dict.csv", header = TRUE)

# Filter positive and negative words based on the Score column
positive_words <- tone_dict_df$Word[tone_dict_df$Score > 0]
negative_words <- tone_dict_df$Word[tone_dict_df$Score < 0]

# Create a dictionary object from positive and negative words
tone_dict <- dictionary(list(positive = positive_words, negative = negative_words))

# Clean the corpus
clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Read in the text files
love <- read_file("Ballads about love and premarital relationships.csv")
family <- read_file("Ballads about family relationships and conflicts.csv")
history <- read_file("Ballads about relations and conflicts of social and hist.circumstances.csv")

# Combine the text files into a single corpus
balladsent <- c(love, family, history)

# Create a VectorSource object from the corpus
ukr_songs_source <- VectorSource(balladsent)

# Create a VCorpus object from the VectorSource object
ukr_songs_corpus <- VCorpus(ukr_songs_source)

# Clean the corpus using the defined clean_corpus() function
ukr_songs_text <- clean_corpus(ukr_songs_corpus)

# Create a DocumentTermMatrix object from the cleaned corpus
ukr_songs_dtm <- DocumentTermMatrix(ukr_songs_text)

# Create a tidy dataframe from the DocumentTermMatrix object
ukr_songs <- tidy(ukr_songs_dtm)

# Inner join the tidy dataframe with the tone dictionary dataframe
ukr_songs_lex_words <- inner_join(ukr_songs, tone_dict_df, by = c("term" = "Word"))

# Add a column to identify the file each term came from
ukr_songs_lex_words <- ukr_songs_lex_words %>%
  mutate(file = case_when(
    document == 1 ~ "love",
    document == 2 ~ "family",
    document == 3 ~ "history"
  ))

# Count the number of occurrences of each term by sentiment and file
ukr_songs_count <- ukr_songs_lex_words %>%
  count(file, Score)

# Plot the count of sentiments for each file
ggplot(ukr_songs_count, aes(x = Score, y = n)) +
  geom_col() +
  facet_wrap(~ file, ncol = 1) +
  ggtitle("Count of Sentiments in Ukrainian Ballads") +
  theme_gdocs()

# Create new columns for positive and negative sentiment based on the score cutoff
ukr_songs_lex_words <- ukr_songs_lex_words %>%
  mutate(pos = Score > 0, neg = Score < 0) %>%
  mutate(polarity = sign(Score))

# Plot polarity vs. term count for each file
ggplot(ukr_songs_lex_words, aes(x = count, y = polarity, color = file, fill = file, group = file)) +
  geom_smooth() +
  ggtitle("Polarity vs. Term Count in Ukrainian Ballads") +
  theme_minimal()

# Analyze word impact and frequency
ukr_songs_tidy_pol <- ukr_songs_lex_words %>%
  filter(count >= 5) %>%
  mutate(pos_or_neg = ifelse(polarity > 0, "pos", "neg"))

# Plot sentiment word polarity
ggplot(ukr_songs_tidy_pol, aes(reorder(term, polarity), polarity, fill = pos_or_neg)) +
  geom_col() +
  ggtitle("Sentiment Word Polarity in Ukrainian Ballads") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_gdocs() +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1)) +
  facet_wrap(~ file, ncol = 1)

# Plot sentiment word frequency
ggplot(ukr_songs_tidy_pol, aes(reorder_within(term, count, polarity), count, fill = pos_or_neg)) +
  geom_col() +
  ggtitle("Sentiment Word Frequency in Ukrainian Ballads") +
  scale_fill_manual(values = c("blue", "red")) +
  xlab("Words") +
  theme_gdocs() +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1)) +
  facet_wrap(~ file, ncol = 1)


#Homework Task: Word Cloud Visualization
# 1. Create a word cloud that displays the most frequent words in the Ukrainian ballads dataset. The word cloud should show both positive and negative words with different colors.
