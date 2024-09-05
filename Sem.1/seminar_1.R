# Statistical Data Science
# 1st Seminar Session

# Basic Calculations ----
# Perform basic arithmetic operations
2 * (5 + 4)        # Multiplication and addition
3^3                # Exponentiation
sqrt(25)           # Square root calculation

# Variables ----
# Assign values to variables and perform operations
x <- 4 + 3         # Use <- for assignment in R
5 * x              # Multiply x by 5
x + 2              # Add 2 to x

# Boolean Variables ----
x <- 1             # Reassign x to 1
x                  # Print the value of x
x == 2             # Check if x equals 2
x == 1             # Check if x equals 1

# Extracting Data with Booleans ----
# Use boolean vectors to extract specific elements
z <- 1:4           # Create a vector z with values 1 through 4
z                  # Print z
z[c(FALSE, TRUE, FALSE, TRUE)]  # Extract elements where TRUE

# Data and Working with Data ----
# Load and explore the dataset
df <- read.csv("folklore_archive.csv", stringsAsFactors = FALSE)

# Initial Information About the Data Set
# Explore the distribution of a categorical variable
sort(table(df[,"Уснословесний.жанр"]))  # Sort and display the frequency of each genre
sort(table(df[,"Місце.побутування"]))   # Sort and display the frequency by location

# Inspecting Specific Columns
head(df[,"Респондент"])  # Display the first few entries of the "Respondent" column
tail(df[,"Респондент"])  # Display the last few entries of the "Respondent" column
summary(df[,"Респондент"])  # Summary statistics of the "Respondent" column

# New Homework Task ----
# Task: Calculate the average age of respondents and determine which genre is most common among the oldest respondents.
# 1. Find out which 'Уснословесний.жанр' is most common among these respondents.
# 2. Write your observations in a brief report.
