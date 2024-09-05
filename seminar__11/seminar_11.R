#Seminar 11

# Load required libraries
library(tidyverse)
library(writexl)
library(ggplot2)

# Read the CSV file
data <- read.csv2("dumas_2024.csv")

# Roots of colors
color_roots <- c("чорн", "черн", "ворон", "темн", "сив", "сиз", "зелен", 
                 "жовт", "біл", "бѣл", "сір", "син", "червон", "чист", 
                 "ясн", "кар", "красн", "срѣб", "среб", "сриб", "сріб", 
                 "золот", "злат", "злот", "позлочист")

# Words to exclude from detected colors
exclude_words <- c("білозерець", "білозорець", "білше", "білшого", "більш", 
                   "більше", "більшь", "більший", "більшого", "більшу", 
                   "більшъ", "біля", "бѣлозерець", "бѣлозорець", "білозо̒рець", 
                   "бѣля", "бѣляки", "ворон", "воронъ", "ворона", "злата", 
                   "злато", "златоглавахъ", "златом", "златомъ", "злоти", 
                   "злота", "злотихъ", "злотыхъ", "злотныкивъ", "злото", 
                   "злотоглавахъ", "злотом", "злотомъ", "золота", "золотаренко", 
                   "золото", "золотом", "золотомъ", "кара", "карае", "кара̒е", 
                   "кара̒є", "кара́ти", "кара́ты", "карайе", "карали", "карасевъ", 
                   "карасивъ", "карати", "караты", "карають", "карбачем", 
                   "карбаче̍мъ", "карбова́ли", "карбачемъ", "карбовали", 
                   "карбовалы", "кардаша", "карма", "кармазинах", "карма́зинахъ", 
                   "кармазинахъ", "кармазини", "кармазиномъ", "карман", "кармана", 
                   "карманах", "кармани", "карманивъ", "карманъ", "карманы", 
                   "картузиках", "карты", "сивъ", "сизокрыльци", "син", 
                   "сина", "синам", "синами", "синах", "сини", "синив", 
                   "синивъ", "синило", "синка", "синку", "синов", "синове", 
                   "синовець", "синови", "синовъ", "синок", "сином", "синомь", 
                   "синомъ", "синонько", "синонька", "синоньку", "синочками", 
                   "синочку", "сину", "синуов", "синчика", "синъ", "сины", 
                   "синятини", "сірахомъ", "сірку", "сребла", "злота", 
                   "сребраныка", "сребло", "среблом", "сребломъ", "сребра", 
                   "сребраникам", "сребраныкам", "сребраныкив", "сребраныкы", 
                   "сребреники", "сребро", "сребром", "сребромъ", "сребряник", 
                   "сребряники", "срибла", "срибло", "сриблом", "срібла", 
                   "срібло", "сріблом", "срібломъ", "срібляникамъ", 
                   "срібляники", "срібляниківъ", "срібникам", "срібники", 
                   "срібняка", "срѣбла", "темнак", "темница", "темници", 
                   "темни̒ці", "темни̒цю", "темни̒ця", "темницу", "темницы", 
                   "темницю", "темниця", "темницях", "темноти", "темныци", 
                   "темныцы", "темныць", "темныцю", "темныця", "темньць", 
                   "темньщи", "червонцами", "червонцю", "червонця", 
                   "черниговськый", "чернигський", "ясности", "яснылы", "ясняють")

# Function to detect colors in text
detect_colors <- function(tokens, color_roots, exclude_words) {
  detected_colors <- character(0)
  for (root in color_roots) {
    regex_pattern <- paste0("\\b", root, "[а-я]*\\b")
    matches <- grep(regex_pattern, tokens, ignore.case = TRUE, value = TRUE)
    matches <- setdiff(matches, exclude_words)
    detected_colors <- c(detected_colors, matches)
  }
  unique(detected_colors)
}

# Tokenize and convert text to lowercase
data <- data %>%
  mutate(tokens = str_split(tolower(Text), "\\W+"))

# Detect colors in the tokenized "Text" column
data <- data %>%
  mutate(detected_colors = map(tokens, detect_colors, color_roots, exclude_words)) %>%
  mutate(detected_colors = map_chr(detected_colors, ~ ifelse(length(.x) > 0, paste(.x, collapse = ", "), NA)))

# Count the number of lines with detected colors
num_lines_with_colors <- sum(!is.na(data$detected_colors))
print(paste("Number of lines with detected colors:", num_lines_with_colors))

# Initialize a vector to store the counts of each color root
root_counts <- numeric(length(color_roots))

# Count occurrences of each color root
for (i in seq_along(color_roots)) {
  root <- color_roots[i]
  root_counts[i] <- sum(grepl(root, data$detected_colors, ignore.case = TRUE))
}

# Combine counts of multiple roots
combine_root_counts <- function(root_counts, color_roots, root_to_combine, roots_to_join) {
  root_index <- which(color_roots == root_to_combine)
  join_indices <- which(color_roots %in% roots_to_join)
  root_counts[root_index] <- root_counts[root_index] + sum(root_counts[join_indices])
  color_roots <- color_roots[-join_indices]
  root_counts <- root_counts[-join_indices]
  list(root_counts = root_counts, color_roots = color_roots)
}

# Combine counts of multiple roots
root_info <- combine_root_counts(root_counts, color_roots, "біл", c("бѣл"))
root_counts <- root_info$root_counts
color_roots <- root_info$color_roots

root_info <- combine_root_counts(root_counts, color_roots, "чорн", c("черн"))
root_counts <- root_info$root_counts
color_roots <- root_info$color_roots

root_info <- combine_root_counts(root_counts, color_roots, "сріб", c("среб", "сриб", "срѣб"))
root_counts <- root_info$root_counts
color_roots <- root_info$color_roots

root_info <- combine_root_counts(root_counts, color_roots, "золот", c("злат", "злот", "позлочист"))
root_counts <- root_info$root_counts
color_roots <- root_info$color_roots

# Create a data frame with color roots and their counts
root_counts_df <- data.frame(Color_Root = color_roots, Count = root_counts)
root_counts_df <- root_counts_df[order(-root_counts_df$Count), ]

# Define a color palette based on the meanings of color roots
color_palette <- c("чорн" = "black", "ворон" = "#301934", "темн" = "#242526",
                   "сив" = "#D9DDDC", "сиз" = "#BEBDB8", "зелен" = "green",
                   "жовт" = "yellow", "біл" = "white", "сір" = "dimgray",
                   "син" = "blue", "червон" = "red", "чист" = "#ddffdd",
                   "ясн" = "#C6FCFF", "кар" = "#622A0F", "красн" = "#E3242B", 
                   "сріб" = "#777B7E", "золот" = "gold")

# Map color roots to colors in the data frame
root_counts_df$Color <- color_palette[root_counts_df$Color_Root]

# Create a bar plot using ggplot2 with outlined bars
ggplot(root_counts_df, aes(x = reorder(Color_Root, -Count), y = Count, fill = Color_Root)) +
  geom_bar(stat = "identity", colour = "black") + # Add outline to bars
  labs(title = "Occurrences of Colors",
       x = "Colors",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = Count), vjust = -0.3, color = "black", size = 3) +
  scale_fill_manual(values = color_palette) +
  scale_x_discrete(labels = c("чорн" = "чорний", "ворон" = "вороний", 
                              "темн" = "темний", "сив" = "сивий", 
                              "сиз" = "сизий", "зелен" = "зелений", 
                              "жовт" = "жовтий", "біл" = "білий", 
                              "сір" = "сірий", "син" = "синій", 
                              "червон" = "червоний", "чист" = "чистий", 
                              "ясн" = "ясний", "кар" = "карий", 
                              "красн" = "красний", "сріб" = "срібний", 
                              "золот" = "золотий"))

ggsave("Occurrences_of_Colors.png", plot = last_plot(), width = 8, height = 6)

# Define color palettes for achromatic and chromatic colors
achromatic_colors <- c("чорн" = "black", "ворон" = "#301934", "темн" = "#242526", 
                       "сив" = "#D9DDDC", "сиз" = "#BEBDB8", "біл" = "white", 
                       "сір" = "dimgray", "сріб" = "#777B7E")

chromatic_colors <- c("жовт" = "yellow", "син" = "blue", "червон" = "red", 
                      "кар" = "#622A0F", "красн" = "#E3242B", "золот" = "gold", 
                      "зелен" = "green", "чист" = "#ddffdd", "ясн" = "#C6FCFF")

# Map color roots to colors in the data frame
root_counts_df$Color <- ifelse(root_counts_df$Color_Root %in% names(achromatic_colors), 
                               achromatic_colors[root_counts_df$Color_Root], 
                               chromatic_colors[root_counts_df$Color_Root])

# Create a bar plot for achromatic colors
achromatic_df <- root_counts_df %>% filter(Color_Root %in% names(achromatic_colors))
ggplot(achromatic_df, aes(x = reorder(Color_Root, -Count), y = Count, fill = Color_Root)) +
  geom_bar(stat = "identity", colour = "black") + # Add outline to bars
  labs(title = "Occurrences of Achromatic Colors",
       x = "Colors",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = Count), vjust = -0.3, color = "black", size = 3) +
  scale_fill_manual(values = achromatic_colors) +
  scale_x_discrete(labels = c("чорн" = "чорний", "ворон" = "вороний", 
                              "темн" = "темний", "сив" = "сивий", 
                              "сиз" = "сизий", "біл" = "білий", 
                              "сір" = "сірий", "сріб" = "срібний"))

ggsave("Occurrences_of_Achromatic_Colors.png", plot = last_plot(), width = 8, height = 6)

# Create a bar plot for chromatic colors
chromatic_df <- root_counts_df %>% filter(Color_Root %in% names(chromatic_colors))
ggplot(chromatic_df, aes(x = reorder(Color_Root, -Count), y = Count, fill = Color_Root)) +
  geom_bar(stat = "identity", colour = "black") + # Add outline to bars
  labs(title = "Occurrences of Chromatic Colors",
       x = "Colors",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = Count), vjust = -0.3, color = "black", size = 3) +
  scale_fill_manual(values = chromatic_colors) +
  scale_x_discrete(labels = c("жовт" = "жовтий", "син" = "синій", 
                              "червон" = "червоний", "кар" = "карий", 
                              "красн" = "красний", "золот" = "золотий", 
                              "зелен" = "зелений", "чист" = "чистий", 
                              "ясн" = "ясний"))

ggsave("Occurrences_of_Chromatic_Colors.png", plot = last_plot(), width = 8, height = 6)

# Calculate total counts for achromatic and chromatic colors
achromatic_count <- sum(root_counts_df$Count[root_counts_df$Color_Root %in% names(achromatic_colors)])
chromatic_count <- sum(root_counts_df$Count[root_counts_df$Color_Root %in% names(chromatic_colors)])
total_count <- achromatic_count + chromatic_count

# Create a data frame for the pie chart with percentages
pie_df <- data.frame(
  Type = c("Achromatic", "Chromatic"),
  Count = c(achromatic_count, chromatic_count)
)
pie_df$Percentage <- pie_df$Count / total_count * 100
pie_df$Label <- paste0(pie_df$Type, "\n", round(pie_df$Percentage, 1), "%")

# Define the colors for the pie chart
pie_colors <- c("Achromatic" = "grey", "Chromatic" = "#FF5733")

# Create the pie chart with percentage labels
ggplot(pie_df, aes(x = "", y = Count, fill = Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = pie_colors) +
  labs(title = "Distribution of Achromatic and Chromatic Colors",
       x = NULL,
       y = NULL) +
  theme_void() +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5))

ggsave("Distribution_of_Achromatic_and_Chromatic_Colors.png", plot = last_plot(), width = 8, height = 6)

#
# 1. Analyze the frequency of part-of-speech (POS) tag combinations in the context of color words within a given text dataset. This task involves extracting word combinations around color words, performing POS tagging, and visualizing the results.
# 1.1. Context Extraction:
# - Extract the words before and after the detected color words. 
# 1.2. POS Tagging:
# - Utilize UDPipe model for Ukrainian to tag the parts of speech (POS) for each word combination (i.e., the color word with the words before and after it).
# - Implement the get_pos_tags function to generate POS tags for each word combination.
# 1.3. Frequency Analysis:
# - Calculate the frequency of each POS tag combination across the dataset. 
# - Filter out combinations with a frequency of less than 10 if necessary.
# 1.4. Visualization:
# - Create a bar plot showing the frequency of each POS tag combination. Include counts as labels on the bars.
# - Save the bar plot as a PNG file.