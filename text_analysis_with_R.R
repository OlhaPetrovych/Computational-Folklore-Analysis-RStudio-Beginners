# Install packages (only necessary once)
install.packages(c(
  "tidyverse",
  "quanteda",
  "quanteda.textstats",
  "quanteda.textplots",
  "tidytext",
  "stopwords",
  "ggplot2",
  "topicmodels",
  "wordcloud",
  "utf8",
  "timechange",
  "lubridate",
  "reshape2",
  "writexl"
))

# Load packages (necessary each time we start R)
library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(tidytext)
library(stopwords)
library(ggplot2)
library(topicmodels)
library(wordcloud)
library(utf8)
library(reshape2)
library(writexl)

# Import the CSV file
# A file-selection window will open.

songs <- read_csv("Tolgitud_2852_valitud_Järva_laulu.csv")

# Display the first six rows
head(songs)

# Examine the structure of the dataset
str(songs)

# Get the number of rows and columns
dim(songs)

# Display all column names
names(songs)

# Get a summary of the variables
summary(songs)

# Examine the distribution of song types
songs %>%
  count(type_names, sort = TRUE)

#Step 2. Preparing and cleaning the English translations
# Create a working dataset
songs_en <- songs %>%
  mutate(
    song_id = row_number(),
    text = as.character(english_translation)
  ) %>%
  filter(
    !is.na(text),
    trimws(text) != ""
  )

# How many songs are available for analysis?
nrow(songs_en)

# Look at one example
songs_en$text[1]

# Create a quanteda corpus
corp_en <- corpus(
  songs_en,
  text_field = "text"
)

# Examine the corpus
summary(corp_en)

# Display the first two texts in the corpus
as.character(corp_en)[1:2]

# Tokenisation
# Convert the corpus into tokens
tokens_en <- tokens(
  corp_en,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
)

# Convert all words to lowercase
tokens_en <- tokens_tolower(tokens_en)

# Remove English stopwords
tokens_en <- tokens_remove(
  tokens_en,
  pattern = stopwords("en")
)

# Display the first 30 tokens
head(as.character(tokens_en[[1]]), 30)

# Step 3. Word frequency analysis
# Create a document-feature matrix
dfm_en <- dfm(tokens_en)

# Examine the matrix
dfm_en

# Calculate word frequencies across the corpus
word_freq <- textstat_frequency(dfm_en)

# Display the 20 most frequent words
head(word_freq, 20)

# Visualise the most frequent words
# Select the 20 most frequent words
top_words <- word_freq %>%
  slice_max(frequency, n = 20) %>%
  arrange(frequency)

# Create a bar chart
ggplot(
  top_words,
  aes(
    x = reorder(feature, frequency),
    y = frequency,
    fill = feature
  )
) +
  geom_col(width = 0.75) +
  
# Add frequency numbers at the end of each bar
  geom_text(
    aes(label = frequency),
    hjust = -0.3,
    size = 3.5,
    color = "black"
  ) +
  
  coord_flip() +
  
  labs(
    title = "Most Frequent Words in Järvamaa Runosongs",
    subtitle = "English translations",
    x = "Word",
    y = "Frequency",
    fill = "Words"
  ) +
  
  # Allow space for frequency labels
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "grey40"
    ),
    axis.text = element_text(size = 10)
  )

# Create a word cloud
set.seed(123)

wordcloud(
  words = word_freq$feature,
  freq = word_freq$frequency,
  max.words = 80,
  random.order = FALSE,
  scale = c(3, 0.5),
  colors = RColorBrewer::brewer.pal(8, "Set1")
)

# Step 4. Finding keywords and examining their contexts
# Search for the word "horse"
kwic_horse <- kwic(
  tokens_en,
  pattern = "horse",
  window = 8
)

# Display the results
head(kwic_horse, 20)

# Search for several animal-related terms
kwic_wolf <- kwic(
  tokens_en,
  pattern = "wolf",
  window = 8
)

kwic_dog <- kwic(
  tokens_en,
  pattern = "dog",
  window = 8
)

head(kwic_wolf, 10)
head(kwic_dog, 10)

kwic_animals <- kwic(
  tokens_en,
  pattern = phrase(c(
    "horse",
    "bird",
    "wolf",
    "dog",
    "cow"
  )),
  window = 8
)

head(kwic_animals, 30)

#4.2. Comparing vocabulary across song types
# Identify the two most frequent song types
top_types <- songs_en %>%
  filter(!is.na(type_names)) %>%
  count(type_names, sort = TRUE) %>%
  slice_head(n = 2) %>%
  pull(type_names)

# Select songs belonging to these categories
songs_compare <- songs_en %>%
  filter(type_names %in% top_types) %>%
  mutate(group = as.character(type_names))

# Create a corpus
corp_compare <- corpus(
  songs_compare,
  text_field = "text"
)

# Explicitly assign group information to the corpus
docvars(corp_compare, "group") <- songs_compare$group

# Tokenise texts
tokens_compare <- tokens(
  corp_compare,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en"))

# Create document-feature matrix
dfm_compare <- dfm(tokens_compare)

# Check the group information
head(docvars(dfm_compare))

# Calculate keywords distinguishing the first category
keywords <- textstat_keyness(
  dfm_compare,
  target = docvars(dfm_compare, "group") == top_types[1]
)

# Display the 20 most distinctive words
head(keywords, 20)

#Step 5. The bag-of-words model
# Examine the document-feature matrix
dfm_en

# Display the first five songs and first ten words
dfm_en[1:5, 1:10]

#Step 6. Introduction to topic modelling (LDA)
# Remove features appearing in fewer than five songs
dfm_lda <- dfm_trim(
  dfm_en,
  min_docfreq = 5,
  docfreq_type = "count"
)

# Convert the matrix to the format required by topicmodels
dtm_lda <- convert(
  dfm_lda,
  to = "topicmodels"
)

# Examine the dimensions
dim(dtm_lda)

# Make the results reproducible
set.seed(123)

# Run LDA
lda_model <- LDA(
  dtm_lda,
  k = 25,
  method = "Gibbs",
  control = list(
    seed = 123,
    burnin = 500,
    iter = 1000,
    thin = 100
  )
)

# Examine the model
lda_model

# Extract the topic-word probabilities
topic_terms <- tidytext::tidy(
  lda_model,
  matrix = "beta"
)

# Display the ten most characteristic words per topic
top_topic_words <- topic_terms %>%
  group_by(topic) %>%
  slice_max(
    beta,
    n = 10,
    with_ties = FALSE
  ) %>%
  ungroup()

top_topic_words

# Save the table to Excel
write_xlsx(
  top_topic_words,
  "top_topic_words.xlsx"
)

#Visualise the topics
top_topic_words %>%
  mutate(
    term = reorder_within(
      term,
      beta,
      topic
    )
  ) %>%
  ggplot(
    aes(
      x = term,
      y = beta,
      fill = factor(topic)
    )
  ) +
  geom_col(
    width = 0.75
  ) +
  coord_flip() +
  facet_wrap(
    ~ topic,
    scales = "free",
    ncol = 5
  ) +
  scale_x_reordered() +
  scale_fill_viridis_d(
    option = "turbo",
    guide = "none"
  ) +
  labs(
    title = "25 Topics in Järvamaa Runosongs",
    subtitle = "Topic-word probabilities based on English translations",
    x = NULL,
    y = "Word probability"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    panel.spacing = unit(1, "lines")
  )

ggsave("LDA_topic_modelling_results.png", width = 12, height = 10)







# ==================================================
# HOMEWORK HINT: Removing Estonian stopwords
# ==================================================

# Stopwords are common words (e.g. conjunctions,
# prepositions and pronouns) that may be removed
# to focus on more content-related vocabulary.
# Consider whether removing them is appropriate
# for your research question.

# Use the Estonian list from the stopwords-iso source
et_stop <- stopwords::stopwords(
  language = "et",
  source = "stopwords-iso"
)

# Inspect the list before applying it
length(et_stop)
head(et_stop, 20)

# Create a version of the tokens without stopwords
tokens_et_clean <- tokens_remove(tokens_et, et_stop)

# You can also create your own list of words to remove.
# First, inspect the most frequent words and decide
# which ones are not relevant to your research question.
# my_stop <- c("...", "...")
# tokens_et_clean <- tokens_remove(tokens_et_clean, my_stop)

# Use tokens_et_clean instead of tokens_et for further
# frequency or topic analysis, if you choose to remove stopwords.
