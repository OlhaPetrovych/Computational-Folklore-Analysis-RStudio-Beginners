# Seminar 8

# Install and load necessary libraries
required_packages <- c("tidyverse", "stringr", "writexl", "udpipe")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(stringr)
library(writexl)
library(udpipe)

# Read the corpus file
corpus <- read.csv2("corpus_vocatives.csv")

# Step 1: Count the number of vocatives in refrains and couplets for each "poem_id"
corpus_summary <- corpus %>%
  summarise(
    total_vocative_refrains = sum(str_detect(refrain, "\\b[[:alpha:]]+\\b"), na.rm = TRUE),
    total_vocative_couplets = sum(str_detect(vocatives, "\\b[[:alpha:]]+\\b") & !str_detect(refrain, "\\b[[:alpha:]]+\\b"), na.rm = TRUE)
  )

# Step 2: Calculate the overall percentage ratio between vocatives in refrains and couplets
corpus_summary <- corpus_summary %>%
  mutate(
    total_vocatives = total_vocative_refrains + total_vocative_couplets,
    perc_vocatives_refrains = (total_vocative_refrains / total_vocatives) * 100,
    perc_vocatives_couplets = (total_vocative_couplets / total_vocatives) * 100
  )

# Display summary statistics for vocatives in refrains vs couplets
print(corpus_summary)

# Step 3: Visualize the overall percentage ratio using ggplot2
corpus_percentage <- corpus_summary %>%
  gather(key = "type", value = "percentage", perc_vocatives_refrains, perc_vocatives_couplets) %>%
  mutate(type = recode(type, perc_vocatives_refrains = "Refrains", perc_vocatives_couplets = "Verse"))

# Using a bar chart
ggplot(corpus_percentage, aes(x = type, y = percentage, fill = type)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 5) +
  labs(title = "Overall Percentage of Vocatives in Refrains vs Verse",
       x = "Type",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

ggsave("voc.in_refrains_vs_verse.png", plot = last_plot(), width = 8, height = 6)

# Step 4: Compare the song's length to the number of vocatives
corpus_summary <- corpus %>%
  group_by(poem_id) %>%
  summarise(
    total_lines = n(),
    total_vocatives = sum(str_detect(vocatives, "\\b[[:alpha:]]+\\b"))
  )

# Display summary statistics for song's length vs number of vocatives
print(corpus_summary)

write_xlsx(corpus_summary, "length_vs_vocatives.xlsx")

# Step 5: Visualize the comparison using ggplot2
ggplot(corpus_summary, aes(x = total_lines, y = total_vocatives)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Song's Length vs Number of Vocatives",
       x = "Total Number of Lines in a Song",
       y = "Total Number of Vocatives") +
  theme_minimal()

ggsave("Length_vs_Vocatives.png", plot = last_plot(), width = 8, height = 6)

# Visualize the ratio of vocatives to the number of songs with the same number of vocatives
summary_dimensions <- corpus_summary %>%
  group_by(total_vocatives) %>%
  summarise(song_count = n())

write_xlsx(summary_dimensions, "Vocatives_vs_Songs.xlsx")

ggplot(summary_dimensions, aes(x = factor(total_vocatives), y = song_count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = song_count), vjust = -0.5, size = 3) +
  labs(title = "Distribution of Number of Vocatives in Folk Songs",
       x = "Total Number of Vocatives in a Folk Song",
       y = "Number of Songs") +
  theme_minimal()

ggsave("Distribution_of_Number_of_Voc_in_Songs.png", plot = last_plot(), width = 8, height = 6)

# Part 2: Tokenize and differentiate lines in the "vocatives" column
corpus <- corpus %>%
  mutate(vocative_tokens = str_split(vocatives, "\\s+")) %>%
  mutate(
    vocative_group = case_when(
      vocatives == "" ~ 0,
      lengths(vocative_tokens) == 1 ~ 1,
      TRUE ~ 2
    ),
    vocative_category = ifelse(vocative_group == 1, "non-extended vocatives", ifelse(vocative_group == 2, "extended vocatives", NA))
  )

# Calculate the total number in both groups
summary_vocatives <- corpus %>%
  filter(!is.na(vocative_category)) %>%
  group_by(vocative_category) %>%
  summarise(total_lines = n())

# Display summary statistics for both groups
print(summary_vocatives)

# Visualize the results using ggplot2
ggplot(summary_vocatives, aes(x = vocative_category, y = total_lines, fill = vocative_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = total_lines), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Distribution of Extended and Non-Extended Vocatives in Ukrainian Folk Songs",
       x = "Vocative Categories",
       y = "Number of Lines") +
  theme_minimal()

ggsave("extended_non-extended.png", plot = last_plot(), width = 8, height = 6)

# Part 3: POS-tag analysis
# Load the pre-trained Ukrainian model for tokenization and POS-tagging
ud_model <- udpipe_download_model(language = "ukrainian-iu")
ud_model <- udpipe_load_model(ud_model$file_model)

# Tokenize the "vocatives" column
corpus_tokens <- udpipe_annotate(ud_model, x = corpus$vocatives)
corpus_tokens <- as.data.frame(corpus_tokens) %>%
  filter(upos != "PUNCT", !is.na(token_id)) %>%
  select(poem_id = doc_id, pos = token_id, sentence, lemma, upos)

# Save tokens to Excel
write_xlsx(corpus_tokens, "corpus_POS_tokens.xlsx")

# Read the corpus file
corpus_tokens <- read.csv2("corpus_POS_tokens.csv")

# Create a mapping of UPOS tags to their full names
upos_mapping <- c(
  "ADJ" = "adjective",
  "ADP" = "adposition",
  "ADV" = "adverb",
  "CCONJ" = "coordinating conjunction",
  "DET" = "determiner",
  "INTJ" = "interjection",
  "NOUN" = "noun",
  "NUM" = "numeral",
  "PART" = "particle",
  "PRON" = "pronoun",
  "SCONJ" = "subordinating conjunction"
)

# Apply the mapping to the upos column to get full names
corpus_tokens <- corpus_tokens %>%
  mutate(upos_full = upos_mapping[upos])

# Calculate the percentage distribution of each POS tag
pos_counts <- corpus_tokens %>%
  group_by(upos) %>%
  summarize(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(desc(percentage))

# Visualize the results using ggplot2 (percentage)
ggplot(pos_counts, aes(x = reorder(upos, percentage), y = percentage, fill = upos)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f%%", percentage)), vjust = -0.5, size = 3) +
  scale_fill_manual(
    values = scales::hue_pal()(length(upos_mapping)),
    labels = upos_mapping
  ) +
  labs(title = "Distribution of Part-of-Speech Tags in Ukrainian Folk Song Vocatives",
       x = "Part of Speech",
       y = "Percentage",
       fill = "Part of Speech (Full Name)") +
  theme_minimal()

# Save the plot
ggsave("POS_Tags_Percentage.png", plot = last_plot(), width = 8, height = 6)

# Structural Types
# Add a flag for appositives (vocatives with a hyphen "-")
corpus_tokens <- corpus_tokens %>%
  group_by(poem_id) %>%
  mutate(
    hyphen_present = any(str_detect(sentence, "-"))
  ) %>%
  mutate(
    pos_sequence = paste(upos, collapse = "+"),
    pos_sequence = if_else(hyphen_present, "appositive", pos_sequence)
  ) %>%
  ungroup()

# Adjust POS combination counting to include appositives
pos_combinations <- corpus_tokens %>%
  group_by(poem_id, pos_sequence) %>%
  summarize(pos_sequence = first(pos_sequence)) %>%
  ungroup() %>%
  group_by(pos_sequence) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Calculate the percentage for each POS combination
total_combinations <- sum(pos_combinations$count)
pos_combinations <- pos_combinations %>%
  mutate(percentage = (count / total_combinations) * 100)

# Select the top 15-20 most frequent POS combinations
top_pos_combinations <- pos_combinations %>%
  top_n(15, wt = count) %>%
  arrange(desc(count))

# Save top POS combinations to Excel
write_xlsx(top_pos_combinations, "Top_POS_Combinations.xlsx")

# Visualize top POS combinations with percentages
ggplot(top_pos_combinations, aes(x = reorder(pos_sequence, percentage), y = percentage, fill = pos_sequence)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f%%", percentage)), vjust = -0.5, size = 3) +
  labs(title = "Top POS Combinations in Ukrainian Folk Song Vocatives",
       x = "POS Combination",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save the top POS combinations plot
ggsave("Top_POS_Combinations.png", plot = last_plot(), width = 10, height = 6)


# Part 4: Semantic groups
# Map numerical values to meaningful labels
corpus$semantic_group <- factor(corpus$group, levels = 1:9,
                                labels = c("Proper Names", "Kinship", "By Age", "Social Position",
                                           "Religious", "Occupation", "Residency", "Birds and Animals", "Inanimate Objects"))

# Filter out NA values in the "group" column
corpus <- corpus %>%
  filter(!is.na(group))

# Calculate the total number of occurrences
total_occurrences <- nrow(corpus)

# Determine the number of each group and calculate the percentage
group_counts <- corpus %>%
  group_by(semantic_group) %>%
  summarise(count = n(), percentage = (count / total_occurrences) * 100) %>%
  arrange(count)

# Visualize the results using ggplot2
ggplot(group_counts, aes(x = reorder(semantic_group, count), y = count, fill = semantic_group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Semantic Groups of Vocatives in Ukrainian Folk Songs",
       x = "Semantic Group",
       y = "Number of Instances") +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", "#b2df8a", "#fdbf6f", "#cab2d6")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("semantic_groups_histogram.png", plot = last_plot(), width = 8, height = 6)

# Additional plot for percentage ratio
ggplot(group_counts, aes(x = reorder(semantic_group, count), y = percentage, fill = semantic_group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Percentage Ratio of Semantic Groups",
       x = "Semantic Group",
       y = "Percentage") +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", "#b2df8a", "#fdbf6f", "#cab2d6")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("semantic_groups_percentage.png", plot = last_plot(), width = 8, height = 6)


# Part 5: Genre statistics
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming your corpus is stored in a dataframe named 'corpus'

# 1. Modify the table to keep only the relevant columns
corpus_modified <- corpus %>% 
  select(poem_id, pos, vocatives, genre)

corpus_modified <- corpus_modified %>%
  mutate(genre = recode(genre,
                        "родинно-обрядові, весільні" = "Family-ritual, Wedding Songs",
                        "родинно-побутові, пісні про кохання" = "Family-life, Love Songs",
                        "родинно-побутові, пісні родинного життя" = "Family-life, Songs about Family Life",
                        "жартівливі та сатиричні" = "Humorous and Satirical Songs",
                        "календарно-обрядова, русальні, петрівчані, купальські" = "Calendar-ritual, Rusalka, Petrivka, and Kupala Songs",
                        "календарно-обрядова, веснянка" = "Calendar-ritual, Vesnianka (Spring) Songs",
                        "календарно-обрядова, колядки та щедрівки" = "Calendar-ritual, Carols and Shchedrivkas",
                        "танцювальні" = "Dance Songs",
                        "балади" = "Ballads",
                        "історичні" = "Historical Songs",
                        "родинно-побутові, колискові та дитячі" = "Family-life, Lullabies and Children's Songs",
                        "соціально-побутові, бурлацькі, наймитські, пісні про панщину" = "Social-life, Burlak, Serf, and Songs about Serfdom",
                        "соціально-побутові, рекрутські, солдатські" = "Social-life, Recruits' and Soldiers' Songs",
                        "соціально-побутові, чумацькі" = "Social-life, Chumak Songs",
                        "календарно-обрядова, жнивні" = "Calendar-ritual, Harvest Songs"
  ))


# 2. Write the same genre in each line for the same poem_id
corpus_modified <- corpus_modified %>%
  group_by(poem_id) %>%
  mutate(genre = first(genre)) %>%
  ungroup()

# 3. Filter lines with vocatives
vocative_songs <- corpus_modified %>% 
  filter(!is.na(vocatives) & vocatives != "")

# 4. Calculate the number of songs with vocatives in the same genre
# Calculate the total number of vocative songs and the percentage for each genre
genre_stats <- vocative_songs %>%
  distinct(poem_id, genre) %>%  # Ensure each song is counted only once
  group_by(genre) %>%
  summarise(num_vocative_songs = n()) %>%
  ungroup() %>%
  mutate(
    total_songs_in_genre = sum(num_vocative_songs),  # Total songs with vocatives across all genres
    percentage_with_vocatives = (num_vocative_songs / total_songs_in_genre) * 100
  ) %>%
  arrange(desc(percentage_with_vocatives))

# Print the genre statistics
print(genre_stats)

# 5. Create a chart to visualize the percentage of songs with vocatives for each genre, sorted in descending order by percentage, with percentage labels
ggplot(genre_stats, aes(x = reorder(genre, -percentage_with_vocatives), y = percentage_with_vocatives)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", percentage_with_vocatives)),  # Format the labels as percentages
            hjust = -0.1,  # Adjust the position of the labels (slightly outside the bar)
            color = "black") +
  labs(title = "Percentage of Songs with Vocatives by Genre", x = "Genre", y = "Percentage of Songs with Vocatives") +
  theme_minimal() +
  coord_flip()  # Flip coordinates to make the bars horizontal

ggsave("vocatives_by_genre.png", plot = last_plot(), width = 12, height = 6)

# Homework Task:
# 1. Data Exploration: Load the provided CSV file 'corpus_vocatives.csv' and perform a basic exploratory analysis using the summary() and str() functions. Report on the number of rows, columns, and the types of data in each column.