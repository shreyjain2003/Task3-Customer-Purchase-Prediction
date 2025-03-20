# Bank Marketing Dataset: Decision Tree Classifier

# Load Required Libraries
library(dplyr)        # Data manipulation
library(ggplot2)      # Data visualization
library(caret)        # Machine learning
library(rpart)        # Decision tree
library(rpart.plot)   # Decision tree visualization

# Load the Dataset
#url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.csv"

# Load the Dataset from Local File
bank_data <- read.csv("C:/Users/DELL/OneDrive/Desktop/PROJECTS/ProdgyInternship/bank/bank.csv", sep=";")


# Display Initial Data Overview
cat("\nInitial Data Overview:\n")
print(str(bank_data))
print(summary(bank_data))
print(dim(bank_data))
print(head(bank_data))

# Handle Missing Values
cat("\nHandling Missing Values:\n")
print(colSums(is.na(bank_data)))  # Check missing values

# Convert Categorical Variables
bank_data <- bank_data %>% mutate(
  y = as.factor(y),
  job = as.factor(job),
  marital = as.factor(marital),
  education = as.factor(education),
  default = as.factor(default),
  housing = as.factor(housing),
  loan = as.factor(loan),
  contact = as.factor(contact),
  month = as.factor(month),
  poutcome = as.factor(poutcome)
)

# Split Data into Training and Testing Sets
set.seed(123)
train_index <- createDataPartition(bank_data$y, p = 0.8, list = FALSE)
train_data <- bank_data[train_index, ]
test_data <- bank_data[-train_index, ]

# Build Decision Tree Model
cat("\nBuilding Decision Tree Model:\n")
dt_model <- rpart(y ~ ., data = train_data, method = "class")

# Visualize the Decision Tree
rpart.plot(dt_model, type = 2, extra = 104, tweak = 1.2, box.palette = "Blues")

# Make Predictions on Test Data
cat("\nMaking Predictions:\n")
predictions <- predict(dt_model, test_data, type = "class")

# Evaluate Model Performance
conf_matrix <- confusionMatrix(predictions, test_data$y)
cat("\nConfusion Matrix:\n")
print(conf_matrix)

# Model Accuracy
cat("\nModel Accuracy:\n")
print(conf_matrix$overall['Accuracy'])
