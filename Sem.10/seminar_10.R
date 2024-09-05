#Seminar 10

# Load required libraries
library(tidyverse)
library(tidytext)
library(writexl)
library(udpipe)
library(ggplot2)

# Part 1: Finals and Rhyming Analysis
# -------------------------------------

# Read in ballads corpus
ballads <- read_csv2("ballads_corpus_Khmelnytsky region.csv")

# Extract the final 2 characters and the full word from each verse line after removing punctuation
ballads_fin <- ballads %>%
  mutate(
    finals = str_extract(str_replace_all(str_to_lower(Text), "[[:punct:]]", ""), ".{0,2}$"),
    last_word = str_extract(str_replace_all(str_to_lower(Text), "[[:punct:]]", ""), "(\\w+)[[:punct:]]*$")
  ) %>%
  filter(!is.na(finals), nchar(finals) > 1)  # Remove lines with missing finals or one-character finals

# Save cleaned data
write_xlsx(ballads_fin, "ballads_fin.xlsx")

# Create a rhyming dictionary
rhyme_dict <- ballads_fin %>%
  group_by(last_word) %>%
  summarise(rhymes = paste0(unique(finals), collapse = "|"))

# Count the frequency of finals
ballads_finals_count <- ballads_fin %>%
  count(finals, sort = TRUE)

# Display top 10 most frequent finals
head(ballads_finals_count, 10)

# Create a bar plot of the top 30 frequency distribution of finals
ggplot(ballads_finals_count %>% top_n(30, n), aes(x = n, y = fct_reorder(finals, n))) +
  geom_col(fill = "#4c72b0") +
  labs(x = "Frequency", y = "Finals in each line") +
  ggtitle("Top 30 Frequency Distribution of Finals") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Generate a table of finals, last words, and line numbers
finals_table <- ballads_fin %>%
  mutate(line_number = row_number()) %>%
  group_by(finals) %>%
  summarize(
    last_words = paste(last_word, collapse = ", "),
    line_numbers = paste(line_number, collapse = ", ")
  ) %>%
  left_join(ballads_finals_count, by = "finals")

# Remove unnecessary rows (e.g., for a cleaner presentation)
finals_table <- finals_table[-c(1:6), ]

# Save the finals table to an Excel file
write_xlsx(finals_table, "figure_3.xlsx")


# Part 2: Parts of Speech (POS) Analysis
# ---------------------------------------

# Load the Ukrainian UDPipe model
ud_model <- udpipe_download_model(language = "ukrainian-iu")
ud_model <- udpipe_load_model(ud_model$file_model)

# Use UDPipe to tokenize and tag each line of text in the corpus
corpus_tags <- udpipe(ud_model, x = ballads_fin$last_word)

# Extract sentence and UPOS columns, excluding punctuation
sentence_upos <- corpus_tags %>%
  filter(upos != "PUNCT") %>%
  select(sentence, upos)

# Count occurrences of each part of speech
upos_counts <- sentence_upos %>%
  group_by(upos) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Create a bar plot visualization of POS frequency
ggplot(upos_counts, aes(x = reorder(upos, -count), y = count)) +
  geom_bar(stat = "identity", fill = "#4c72b0") +
  ggtitle("Part-of-Speech Frequency in Ukrainian Ballads") +
  xlab("Part of Speech") +
  ylab("Count")

# Save the POS counts to an Excel file
write_xlsx(upos_counts, "figure_9b.xlsx")

#Homework Task:
# 1. Using the upos_counts data generated from the POS analysis, create a word cloud to visualize the most frequent parts of speech in the Ukrainian ballads corpus.
# 2***. Experiment with different color palettes and settings in the wordcloud function to create visually distinct word clouds for each POS category.