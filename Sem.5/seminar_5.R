# Seminar 5: Sentiment Analysis of Ukrainian Ballads

# Part 1: Set Up and Sentiment Analysis Using Quanteda ----

# Install and load required packages
install.packages('quanteda')
install.packages('readtext')
install.packages('dplyr')
install.packages('tidyverse')
install.packages('scales')

library(quanteda)
library(readtext)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)

# Load the Tone Dictionary ----
tone_dict <- read.table(file = 'tone-dict-uk.tsv', sep = '\t', header = TRUE)

# Create a Sentiment Dictionary ----
sent_dict <- dictionary(list(
  pos = tone_dict[tone_dict$V2 >= 1, "V1"],
  neg = tone_dict[tone_dict$V2 <= -1, "V1"]
))

# Load and Prepare the Ukrainian Ballads Corpus ----
balladsent <- readtext("Ukr_ballads")
balladcorpus <- corpus(balladsent)

# Tokenize and Preprocess the Corpus ----
balladcorpustok <- tokens(balladcorpus, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_separators = TRUE) %>%
  tokens_tolower()

# Create a Document-Feature Matrix Using the Sentiment Dictionary ----
balladcorpussent <- dfm(balladcorpustok, dictionary = sent_dict)
head(balladcorpussent)

# Convert the Document-Feature Matrix to a Data Frame ----
sentiment_df <- convert(balladcorpussent, to = "data.frame")

# Calculate Relative Sentiment Scores ----
sentiment_df <- sentiment_df %>%
  mutate(relsent = pos / neg) %>%
  arrange(relsent)

# Clean Document IDs ----
sentiment_df$doc_id <- gsub(".txt", "", as.character(sentiment_df$doc_id))

# Visualize Relative Sentiment Scores ----
ggplot(data = subset(sentiment_df, doc_id != "NULL"), 
       mapping = aes(x = relsent, y = reorder(doc_id, relsent))) +
  geom_point() +
  labs(title = "Relative Sentiment Scores by Document", x = "Relative Sentiment", y = "Document ID")

# Export and Import Document IDs ----
write.csv(sentiment_df$doc_id, "categories.csv")
tlg <- read.csv2("categories.csv")

# Merge Document IDs Back into the Sentiment Data Frame ----
sentiment_df <- merge(sentiment_df, tlg, by = "doc_id")

# Reshape the Data for Visualization ----
sent_track <- gather(data = subset(sentiment_df, doc_id != "NULL"), "posneg", "value", 2:3)

# Visualize the Proportion of Positive and Negative Words ----
ggplot(sent_track, aes(fill = posneg, y = reorder(doc_id, -relsent), x = value * 100)) + 
  geom_bar(position = "fill", stat = "identity") +
  labs(title = "Ukrainian Ballads: Sentiment Words Proportion", 
       fill = NULL) +
  scale_fill_manual(values = c("lightblue", "lightpink")) +
  scale_x_continuous(labels = percent_format()) +
  theme_bw()

# Analyze Word Frequencies ----
library(quanteda.textstats)
textstat_frequency(balladcorpussent)

# Part 2: Advanced Sentiment Analysis Using Tidytext ----

# Install and Load Additional Required Packages
install.packages('tidyr')
install.packages('magrittr')
install.packages('qdap')
install.packages('tm')
install.packages('syuzhet')
install.packages('tidytext')

library(tidyr)
library(magrittr)
library(qdap)
library(tm)
library(syuzhet)
library(tidytext)

# Reload and Clean the Corpus ----
balladsent <- c(
  read_file("Ballads about love and premarital relationships.csv"),
  read_file("Ballads about family relationships and conflicts.csv"),
  read_file("Ballads about relations and conflicts of social and hist.circumstances.csv")
)

ukr_songs_source <- VectorSource(balladsent)
ukr_songs_corpus <- VCorpus(ukr_songs_source)
clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}
ukr_songs_text <- clean_corpus(ukr_songs_corpus)

# Create a Document-Term Matrix and Tidy Data ----
ukr_songs_dtm <- DocumentTermMatrix(ukr_songs_text)
ukr_songs <- tidy(ukr_songs_dtm)

# Join with Tone Dictionary and Annotate Data ----
ukr_songs_lex_words <- inner_join(ukr_songs, tone_dict, by = c("term" = "V1")) %>%
  mutate(file = case_when(
    document == 1 ~ "love",
    document == 2 ~ "family",
    document == 3 ~ "history"
  )) %>%
  mutate(index = as.numeric(document))

# Count Sentiment Words by Category ----
ukr_songs_count <- ukr_songs_lex_words %>%
  count(V2, index, file)

# Visualize Sentiment Counts ----
library(ggthemes)
ggplot(ukr_songs_count, aes(x = V2, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5) +
  facet_wrap(~ file, ncol = 1) +
  theme_gdocs() +
  labs(title = "Sentiment Counts in Ukrainian Ballads", x = "Sentiment Score", y = "Count")

# Polarity Analysis and Visualization ----
ukr_songs_lex_words <- ukr_songs_lex_words %>%
  mutate(polarity = sign(V2))

ggplot(ukr_songs_lex_words, aes(x = count, y = polarity, color = file, fill = file, group = file)) + 
  geom_smooth() +
  theme_minimal() +
  labs(title = "Polarity Analysis by File", x = "Count", y = "Polarity")

# Word Impact and Frequency Analysis ----
ukr_songs_tidy_pol <- ukr_songs_lex_words %>%
  filter(abs(count) >= 5) %>%
  mutate(pos_or_neg = ifelse(polarity > 0, "pos", "neg"))

# Visualize Word Polarity ----
ggplot(ukr_songs_tidy_pol, aes(reorder(term, polarity), polarity, fill = pos_or_neg)) +
  geom_col() + 
  ggtitle("Ukrainian Ballads: Sentiment Word Polarity") + 
  scale_fill_manual(values = c("blue", "red")) +
  theme_gdocs() +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1)) +
  facet_wrap(~ file, ncol = 1)

# Visualize Word Frequency ----
ggplot(ukr_songs_tidy_pol, aes(reorder_within(term, count, polarity), count, fill = pos_or_neg)) +
  geom_col() + 
  ggtitle("Ukrainian Ballads: Sentiment Word Frequency") + 
  scale_fill_manual(values = c("blue", "red")) +
  xlab("Words") + 
  theme_gdocs() +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1)) +
  facet_wrap(~ file, ncol = 1)

# Homework Task ----
# Task: Conduct sentiment analysis on a different Ukrainian folk song corpus.
# 1. Find or compile a corpus of Ukrainian folk songs on a different genre (e.g., calendar and ritual songs, historical songs, etc.).
# 2. Use the tone dictionary and the methods covered in this seminar to analyze the sentiment of the new corpus.
# 3. Visualize the sentiment distribution and compare it with the results from the ballads corpus.
# 4. Submit a short report discussing any significant findings or patterns observed in the sentiment of the new corpus.
