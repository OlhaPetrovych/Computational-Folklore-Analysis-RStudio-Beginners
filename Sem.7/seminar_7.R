# Seminar 7

# Load required libraries
library(readtext)
library(tidytext)
library(quanteda)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(quanteda.textplots)
library(jsonlite)
library(usethis)

# Read the Ukrainian ballads text file
ukr <- readtext("Ukr_ballads")
print(head(ukr))
print(encoding(ukr))

# Clean document IDs by removing '.txt'
ukr$doc_id <- gsub(".txt", "", ukr$doc_id)
print(head(ukr))

# Create a corpus object
qukr <- corpus(ukr)
print(summary(qukr))

# Add document variables
docvars(qukr, "text") <- str_sub(ukr$doc_id, end = -6) %>% str_to_title()
print(summary(qukr))

# Visualize number of tokens and type-token ratio
ggplot(summary(qukr), aes(x = Text, y = Tokens, group = 1)) +
  geom_line() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ggtitle("Number of Tokens per Ballad") +
  xlab("Ballad") +
  ylab("Number of Tokens")

ggplot(summary(qukr), aes(x = Tokens, y = Types, group = 1, label = Text)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(check_overlap = TRUE) +
  ggtitle("Type-Token Ratio (TTR) per Ballad") +
  xlab("Number of Tokens") +
  ylab("Number of Types")

# Tokenize the corpus and remove punctuation and numbers
qukrtokens <- tokens(qukr, remove_punct = TRUE, remove_numbers = TRUE)

# Keywords in context (KWIC) for the pattern "син"
kwic_results <- kwic(qukrtokens, pattern = "син")
print(kwic_results)

# Load stopwords from URL and add additional stopwords
sw_url <- "https://raw.githubusercontent.com/stopwords-iso/stopwords-iso/master/stopwords-iso.json"
data_stopwords <- fromJSON(readLines(sw_url, encoding = "UTF-8"))
data_stopwords <- lapply(data_stopwords, unique)
usethis::use_data(data_stopwords, overwrite = TRUE)

# Extract Ukrainian stopwords and add custom ones
stopwords <- c(data_stopwords[["uk"]], "не", "а", "на", "ой", "хоч", "в", "ні", "нехай", "тепер", "у", "ж", "то", "по", "бо", "ще", "і", "щоб", "там", "же", "за", "гей", "вже", "тут", "аж")
stopwords_df <- data.frame(stopwords = stopwords)
stops <- as.character(stopwords_df$stopwords)

# Create Document-Feature Matrix (DFM), remove stopwords, and trim rare terms
qukrdfm <- dfm(qukrtokens, tolower = TRUE) %>%
  dfm_remove(pattern = stops) %>%
  dfm_trim(min_termfreq = 10)

# Number of different words
print(nfeat(qukrdfm))

# Generate and plot word clouds
textplot_wordcloud(qukrdfm, min_size = 2, max_size = 5, max_words = 70, color = brewer.pal(8, "Dark2"))
textplot_wordcloud(dfm_group(qukrdfm, groups = text), color = brewer.pal(5, "Set1"), min_size = 1, max_size = 3.5, max_words = 80, comparison = TRUE)


# Homework Task:
# 1. Investigate the usage and context of the keyword "пан" in the Ukrainian ballads and visualize its occurrences.
