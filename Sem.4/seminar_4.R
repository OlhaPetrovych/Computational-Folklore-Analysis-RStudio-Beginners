# Statistical Data Science
# Seminar 4

# Part 2: Counting Syllables in a Line

# Install and load the required package ----
# (Uncomment the next line if the package is not already installed)
# install.packages("stringr")
library(stringr)
library(writexl)
library(ggplot2)

# Load the Data ----
data <- read.csv2("ballads_corpus_Khmelnytsky region.csv", stringsAsFactors = FALSE)

# Function to Count Vowels (as a proxy for syllables) ----
count_vowels <- function(word) {
  vowels <- c("А", "Е", "Є", "И", "І", "Ї", "О", "У", "Ю", "Я", "а", "е", "є", "и", "і", "ї", "о", "у", "ю", "я")
  pattern <- paste(vowels, collapse = "|")
  vowels_count <- str_count(word, pattern)
  return(vowels_count)
}

# Count Vowels in Each Line of Text ----
data <- data %>%
  mutate(Vowel_Count = sapply(Text, function(row) {
    words <- str_split(row, "\\s+")[[1]]
    vowels_count <- sapply(words, count_vowels)
    sum(vowels_count)
  }))

# Save the Updated Data to an Excel File ----
write_xlsx(data, "figure_2a.xlsx")

# Display the Updated Data (Optional) ----
print(data)

# Analysis of Vowel Count ----
# Create a Frequency Table for Vowel Counts
count_table <- table(data$Vowel_Count)

# Convert the Frequency Table to a Data Frame
count_data <- as.data.frame(count_table)
names(count_data) <- c("Vowel_Count", "Count")
count_data$Vowel_Count <- as.integer(as.character(count_data$Vowel_Count))

# Filter the Data for Vowel Counts Between 4 and 12 ----
filtered_data <- count_data %>%
  filter(Vowel_Count >= 4 & Vowel_Count <= 12)

# Save the Filtered Data to an Excel File ----
write_xlsx(filtered_data, "figure_2b.xlsx")

# Visualization: Frequency of Syllable Counts in Each Line ----
ggplot(filtered_data, aes(x = as.factor(Vowel_Count), y = Count)) +
  geom_bar(stat = "identity", fill = "#4c72b0") +
  labs(x = "Vowel Count", y = "Frequency", title = "Frequency of Syllable Count in Each Line") +
  scale_x_discrete(labels = as.character(4:12)) +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# New Homework Task ----
# Task: Compare the distribution of syllable counts in a different ballads corpus.
# 1. Load your own dataset of folk songs "your_own_corpus.csv" into R.
# 2. Use the provided function to count vowels (syllables) in each line of the new corpus.
# 3. Visualize the distribution of syllable counts, focusing on the range from 4 to 12.
# 4. Compare this distribution to the Khmelnytsky region corpus and write a short report discussing any notable differences or patterns.