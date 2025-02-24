# Load the dataset
data <- read.csv("Report.csv")
# Load the required library
library(class)
# Check for missing values and print the total count
print(sum(is.na(data)))
# Convert the target variable to a factor type
data$Target <- as.factor(data$Target)
# Remove rows with missing values
data <- na.omit(data)
# Set a seed for reproducibility and split the dataset into training and testing sets
set.seed(123) 
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))  # 70% training data
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]
# Select predictor variables for training and testing
train_X <- train_data[, c("Air.temperature..K.", "Process.temperature..K.", "Rotational.speed..rpm.", "Torque..Nm.", "Tool.wear..min.")]
train_y <- train_data$Target
test_X <- test_data[, c("Air.temperature..K.", "Process.temperature..K.", "Rotational.speed..rpm.", "Torque..Nm.", "Tool.wear..min.")]
test_y <- test_data$Target
# Set the number of neighbors (k) for the KNN algorithm
k <- 3
# Apply the KNN algorithm to classify the test data
predictions <- knn(train = train_X, test = test_X, cl = train_y, k = k)
# Calculate and print the accuracy of the model
Kaccuracy <- sum(predictions == test_y) / length(test_y)
print(paste("Accuracy:", round(Kaccuracy * 100, 2), "%"))


#Naive Bayes
# Load the required library
library(e1071)
# Set a seed for reproducibility and split the dataset into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))  # 70% training data
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]
# Select predictor variables for training and testing
train_X <- train_data[, c("Air.temperature..K.", "Process.temperature..K.", "Rotational.speed..rpm.", "Torque..Nm.", "Tool.wear..min.")]
train_y <- train_data$Target
test_X <- test_data[, c("Air.temperature..K.", "Process.temperature..K.", "Rotational.speed..rpm.", "Torque..Nm.", "Tool.wear..min.")]
test_y <- test_data$Target
# Train a Naive Bayes model using the training data
nb_model <- naiveBayes(train_X, train_y)
# Predict the target variable for the test dataset
predictions <- predict(nb_model, test_X)
# Calculate and print the accuracy of the model
Naccuracy <- sum(predictions == test_y) / length(test_y)
print(paste("Accuracy:", round(Naccuracy * 100, 2), "%"))


#Support Vector Machines (SVM)
library(e1071)
View(data)
# Convert the target variable to a factor type
data$Target <- as.factor(data$Target)
# Set a seed for reproducibility and split the dataset into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))  # 70% training data
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]
# Select predictor variables for training and testing
train_X <- train_data[, c("Air.temperature..K.", "Process.temperature..K.", "Rotational.speed..rpm.", "Torque..Nm.", "Tool.wear..min.")]
train_y <- train_data$Target
test_X <- test_data[, c("Air.temperature..K.", "Process.temperature..K.", "Rotational.speed..rpm.", "Torque..Nm.", "Tool.wear..min.")]
test_y <- test_data$Target
# Train an SVM model with a radial kernel
svm_model <- svm(Target ~ ., data = train_data[, c(names(train_X), "Target")], kernel = "radial")
# Predict the target variable for the test dataset
predictions <- predict(svm_model, test_X)
# Calculate and print the accuracy of the SVM model
Saccuracy <- sum(predictions == test_y) / length(test_y)
print(paste("Accuracy:", round(Saccuracy * 100, 2), "%"))


#Decision Trees (DT)
# Load required libraries
library(rpart)
library(rpart.plot)
# Convert the target variable to a factor type
data$Target <- as.factor(data$Target)
# Set a seed for reproducibility and split the dataset into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))  # 70% training data
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]
# Select predictor variables for training and testing
train_X <- train_data[, c("Air.temperature..K.", "Process.temperature..K.", "Rotational.speed..rpm.", "Torque..Nm.", "Tool.wear..min.")]
train_y <- train_data$Target
test_X <- test_data[, c("Air.temperature..K.", "Process.temperature..K.", "Rotational.speed..rpm.", "Torque..Nm.", "Tool.wear..min.")]
test_y <- test_data$Target
# Train a decision tree model
dt_model <- rpart(Target ~ ., data = train_data[, c(names(train_X), "Target")], method = "class")
# Visualize the decision tree
rpart.plot(dt_model)
# Predict the target variable for the test dataset
predictions <- predict(dt_model, test_X, type = "class")
# Calculate and print the accuracy of the decision tree model
Daccuracy <- sum(predictions == test_y) / length(test_y)
print(paste("Accuracy:", round(Daccuracy * 100, 2), "%"))


#Model Accuracy Comparison
# Load the required library for visualization
library(ggplot2)
# Calculate model accuracies in percentage
naive_bayes_accuracy <- Naccuracy * 100       # Accuracy of Naive Bayes
knn_accuracy <- Kaccuracy * 100               # Accuracy of KNN
decision_tree_accuracy <- Daccuracy * 100     # Accuracy of Decision Tree
SVM_accuracy <- Saccuracy * 100               # Accuracy of SVM

# Create a data frame to hold the accuracy values for each model
accuracy_df <- data.frame(
  Model = c("Naive Bayes", "KNN", "Decision Tree", "SVM"),
  Accuracy = c(naive_bayes_accuracy, knn_accuracy, decision_tree_accuracy, SVM_accuracy)
)

# Plot the accuracy comparison using ggplot2
ggplot(accuracy_df, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +  # Create bar plot
  labs(title = "Model Accuracy Comparison", y = "Accuracy (%)", x = "Model") +  # Add labels
  theme_minimal() +  # Use a minimal theme
  scale_fill_brewer(palette = "Set2") +  # Use a color palette
  geom_text(aes(label = round(Accuracy, 1)), vjust = -0.5)  # Add accuracy values on bars

