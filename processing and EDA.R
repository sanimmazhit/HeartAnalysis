
install.packages("dplyr")

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(scales)
library(gridExtra)
library(corrplot)

# Load the dataset
file_path <- "/Users/skmazhit/Desktop/ind project /Sanim Mazhit - 33176A/heart.csv"
data <- read.csv("/Users/skmazhit/Desktop/ind project /Sanim Mazhit - 33176A/heart.csv")

#to display the structure of my dataset
str(data) 

#to summary the dataset
summary(data)


# 1. Cleaning the Data

# Check for missing values
missing_summary <- sapply(data, function(x) sum(is.na(x)))
print("Missing values summary:")
print(missing_summary)


# Handle missing values (impute with median for numerical and mode for categorical)
data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))


# Handle outliers using IQR method
data <- data %>%
  mutate(across(where(is.numeric), ~ {
    Q1 <- quantile(., 0.25, na.rm = TRUE)
    Q3 <- quantile(., 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    ifelse(. < lower_bound, lower_bound, ifelse(. > upper_bound, upper_bound, .))
  }))

# Ensure consistency of categorical variables
data <- data %>% mutate(across(where(is.character), as.factor))

# 2. Feature Engineering

# Scale numerical features
scaled_data <- data %>% mutate(across(where(is.numeric), ~ scale(.) %>% as.numeric()))

# Encode categorical variables (one-hot encoding)
data <- scaled_data %>% mutate(across(where(is.factor), as.factor))
data <- data %>% mutate(across(where(is.factor), ~ as.numeric(as.factor(.))))

# 3. Exploratory Data Analysis (EDA)

# Load necessary libraries

# Load the dataset
file_path <- "/Users/skmazhit/Desktop/ind project /Sanim Mazhit - 33176A/heart.csv"
data <- read.csv(file_path)

# Ensure categorical columns are factors
data <- data %>% mutate(across(where(is.character), as.factor))

# 1. Histograms for Numeric Columns
print("Generating histograms for numeric columns...")
numeric_columns <- data %>% select(where(is.numeric))

histograms <- lapply(names(numeric_columns), function(col) {
  ggplot(data, aes_string(x = col)) +
    geom_histogram(fill = "blue", color = "black", bins = 30) +
    theme_minimal() +
    labs(title = paste("Histogram of", col), x = col, y = "Frequency")
})

# Display histograms in a grid
grid.arrange(grobs = histograms, ncol = 2)

# 2. Boxplots Grouped by Target (if available)
if ("target" %in% colnames(data)) {
  print("Generating boxplots grouped by target...")
  boxplots <- lapply(names(numeric_columns), function(col) {
    ggplot(data, aes_string(x = "factor(target)", y = col)) +
      geom_boxplot(fill = "cyan", color = "black") +
      theme_minimal() +
      labs(title = paste("Boxplot of", col, "by Target"), x = "Target", y = col)
  })
  
  # Display boxplots in a grid
  grid.arrange(grobs = boxplots, ncol = 2)
}

# 3. Correlation Heatmap for Numeric Columns
if (ncol(numeric_columns) > 1) {
  print("Generating correlation heatmap...")
  correlation_matrix <- cor(numeric_columns, use = "complete.obs")
  corrplot(correlation_matrix, method = "color", type = "upper", 
           title = "Correlation Heatmap", tl.cex = 0.8, cl.cex = 0.8)
}

# 4. Distribution of Categorical Variables
categorical_columns <- data %>% select(where(is.factor))

if (ncol(categorical_columns) > 0) {
  print("Generating bar plots for categorical columns...")
  barplots <- lapply(names(categorical_columns), function(col) {
    ggplot(data, aes_string(x = col)) +
      geom_bar(fill = "orange", color = "black") +
      theme_minimal() +
      labs(title = paste("Bar Plot of", col), x = col, y = "Count")
  })
  
  # Display bar plots in a grid
  grid.arrange(grobs = barplots, ncol = 2)
}

# 5. Pairwise Scatterplots (Optional for fewer numeric columns)
if (ncol(numeric_columns) <= 5) {
  print("Generating pairwise scatterplots...")
  pairs(numeric_columns, main = "Pairwise Scatterplots", pch = 19, col = "blue")
}

