# Statistical Data Science
# Seminar 2-3

# Install necessary packages ----
# (Uncomment the next two lines if the packages are not already installed)
# install.packages('tidyverse')
# install.packages('writexl')

# Load necessary libraries ----
library(tidyverse)
library(writexl)

# Load the data ----
ballads_corpus <- read_csv2("ballads_corpus_Khmelnytsky region.csv")

# Data Processing ----
# Identify non-empty lines as TRUE and empty lines as FALSE
ballads_corpus <- ballads_corpus %>%
  mutate(is_empty = is.na(Text) | Text == "")

# Identify stanzas by detecting two consecutive non-empty lines
ballads_corpus <- ballads_corpus %>%
  mutate(stanza_num = cumsum(is_empty & !lag(is_empty, default = FALSE) & !lead(is_empty, default = FALSE)))

# Export the processed data to Excel ----
write_xlsx(ballads_corpus, "figure_1a.xlsx")

# Group rows by stanza number and count the number of lines in each stanza ----
stanza_lengths <- ballads_corpus %>%
  filter(!is_empty) %>% # Remove empty rows
  group_by(stanza_num) %>%
  summarise(num_lines = n()) %>%  # Count the number of lines per stanza
  ungroup()

# Export stanza lengths to Excel ----
write_xlsx(stanza_lengths, "figure_1b.xlsx")

# Visualization ----
# Visualize the distribution of stanza lengths
ggplot(stanza_lengths, aes(x = num_lines)) +
  geom_histogram(binwidth = 1, color = "#4c72b0", fill = "#dd8452") +
  labs(title = "Distribution of Stanza Lengths", x = "Number of Lines", y = "Count") +
  scale_x_continuous(breaks = 1:max(stanza_lengths$num_lines)) +
  stat_bin(aes(label = ..count..), binwidth = 1, geom = "text", vjust = -0.5) +
  theme_minimal()  # Apply a minimal theme for cleaner visualization

# New Homework Task ----
# Task: Analyze the stanza structure of a different corpus from a neighboring region.
# 1. Create and load your own dataset of folk songs "your_own_corpus.csv" into R.
# 2. Apply the same steps as in this seminar to identify stanza numbers and calculate stanza lengths.
# 3. Visualize the stanza length distribution and compare it to the Khmelnytsky region corpus.
# 4. Write a short report discussing any similarities or differences you observe between the two datasets.
