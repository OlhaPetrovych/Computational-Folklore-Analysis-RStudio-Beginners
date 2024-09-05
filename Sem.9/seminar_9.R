# Seminar 9

# Load necessary libraries
library(stringr)
library(writexl)
library(dplyr)
library(ggplot2)

# Read the CSV file
data <- read.csv2("ballads_fin_str.csv")

# Function to convert a word to a formula with stressed (1) and unstressed (0) vowels
convert_to_formula <- function(word) {
  # Find stressed vowels (indicated by a caret '^') and all vowels
  stressed_vowels <- str_extract_all(word, "[АЕЄИІЇОУЮЯаеєиіїоуюя]\\^")[[1]]
  vowels <- str_extract_all(word, "[АЕЄИІЇОУЮЯаеєиіїоуюя]")[[1]]
  
  # Create a formula where stressed vowels are '1' and unstressed are '0'
  formula <- paste0(ifelse(paste0(vowels, "^") %in% stressed_vowels, "1", "0"), collapse = "")
  
  return(formula)
}

# Function to determine the rhyme type from the formula
determine_rhyme <- function(formula) {
  # Check the last and penultimate characters in the formula to determine rhyme type
  last_character <- substr(formula, nchar(formula), nchar(formula))
  penultimate_character <- substr(formula, nchar(formula) - 1, nchar(formula) - 1)
  
  # Determine rhyme type based on the stressed (1) and unstressed (0) pattern
  if (penultimate_character == "1") {
    return("Feminine")
  } else if (last_character == "1") {
    return("Masculine")
  } else {
    return("Dactylic")
  }
}

# Apply the formula and rhyme type functions to the dataset
data <- data %>%
  mutate(
    formula = sapply(last_word, convert_to_formula),
    rhyme_type = sapply(formula, determine_rhyme)
  )

# Save the resulting data to an Excel file
write_xlsx(data, "figure 5.xlsx")

# Count the total number of each rhyme type and calculate percentages
rhyme_counts <- data %>%
  group_by(rhyme_type) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Visualize the rhyme type distribution using ggplot2
ggplot(rhyme_counts, aes(x = reorder(rhyme_type, -percentage), y = percentage, fill = rhyme_type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Rhyme Type Percentages",
    x = "Rhyme Type",
    y = "Percentage"
  ) +
  scale_fill_manual(values = c("#4c72b0", "#dd8452", "#55a868")) +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), vjust = -0.5, color = "black", size = 3.5) +
  theme_minimal()

# Save the plot as an image file
ggsave("rhyme_type_percentages.png", width = 8, height = 6)

#Homework Task
#1. Visualize the distribution of rhyme types in the ballads using a treemap.

#1.1.Treemap Visualization:
# - Use the data on rhyme type frequencies calculated in the seminar.
# - Create a treemap where each rectangle represents a rhyme type. The size of each rectangle should correspond to the frequency (or percentage) of that rhyme type.
# - Color each rectangle by rhyme type to distinguish them visually.
# - Add labels inside the rectangles showing the rhyme type and its percentage.
# 1.2. Customization:
# -  Customize the treemap by choosing a color palette that is easy on the eyes and provides good contrast between different rhyme types.
# - Add a title to the treemap, such as "Distribution of Rhyme Types in Ballads".
# 1.3. Export:
# -  Save the treemap as rhyme_treemap.png.