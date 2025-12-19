# =========================
# STEP 1: Load libraries
# =========================
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)

# =========================
# STEP 2: Read dataset
# =========================
df <- read.csv("heart_disease_uci.csv", na.strings = c("", "NA", "?", "null"))

# Look at the data
head(df)
str(df)
dim(df)

# =========================
# STEP 3: Remove duplicate rows
# =========================
df <- df[!duplicated(df), ]

# Remove ID column (not a useful feature)
if ("id" %in% names(df)) {
    df$id <- NULL
}

# OPTIONAL: remove ca (too many missing in this dataset)
if ("ca" %in% names(df)) {
    df$ca <- NULL
}

# =========================
# STEP 4: Fix "wrong missing" values
# =========================
# In many UCI heart files, chol = 0 means missing
if ("chol" %in% names(df)) {
    df$chol[df$chol == 0] <- NA
}

# =========================
# STEP 5: Convert categorical columns to factor
# =========================
df$sex <- as.factor(df$sex)
df$dataset <- as.factor(df$dataset)
df$cp <- as.factor(df$cp)
df$fbs <- as.factor(df$fbs)
df$restecg <- as.factor(df$restecg)
df$exang <- as.factor(df$exang)
df$slope <- as.factor(df$slope)
df$thal <- as.factor(df$thal)
df$num <- as.factor(df$num)

# =========================
# STEP 6: Handle missing values
# =========================

# 6.1 Numeric columns: fill NA with median
num_cols <- c("age", "trestbps", "chol", "thalch", "oldpeak")

for (col in num_cols) {
    df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
}

# 6.2 Categorical columns: fill NA with mode (most common value)
get_mode <- function(x) {
    t <- table(x)
    names(t)[which.max(t)]
}

cat_cols <- c("sex", "dataset", "cp", "fbs", "restecg", "exang", "slope", "thal", "num")

for (col in cat_cols) {
    df[[col]][is.na(df[[col]])] <- get_mode(df[[col]])
}

# Check missing after cleaning
colSums(is.na(df))

# =========================
# STEP 7: Create binary target (No/Yes)
# =========================
df$heart_disease <- ifelse(df$num == "0", "No", "Yes")
df$heart_disease <- as.factor(df$heart_disease)

# =========================
# STEP 8: Simple statistics
# =========================
summary(df)

# =========================
# STEP 9: Required Plots (EDA)
# =========================

# 9.1 Target distribution
ggplot(df, aes(x = heart_disease)) +
    geom_bar() +
    labs(title = "Heart Disease Target Distribution")

# 9.2 Histogram (example numeric)
ggplot(df, aes(x = age)) +
    geom_histogram(bins = 25) +
    labs(title = "Age Distribution")

# 9.3 Bar plot (categorical example)
ggplot(df, aes(x = sex, fill = heart_disease)) +
    geom_bar(position = "dodge") +
    labs(title = "Heart Disease by Sex")

# 9.4 Boxplot (target vs numeric example)
ggplot(df, aes(x = heart_disease, y = chol)) +
    geom_boxplot() +
    labs(title = "Cholesterol vs Heart Disease")

# 9.5 Correlation matrix (numeric only)
num_data <- df[, num_cols]
cor_mat <- cor(num_data)
corrplot(cor_mat, method = "color")

# 9.6 Scatter plots (numeric vs numeric)
plot(df$age, df$chol,
    xlab = "Age", ylab = "Cholesterol",
    main = "Scatter Plot: Age vs Cholesterol"
)

# =========================
# STEP 10: Train/Test split (70/30)
# =========================
set.seed(123)
n <- nrow(df)
train_index <- sample(1:n, size = 0.7 * n)

train <- df[train_index, ]
test <- df[-train_index, ]

# =========================
# STEP 11: Logistic Regression model
# =========================
model <- glm(heart_disease ~ age + sex + trestbps + chol + thalch + exang,
    data = train, family = "binomial"
)

summary(model)

# =========================
# STEP 12: Prediction + Accuracy
# =========================
prob <- predict(model, newdata = test, type = "response")

pred <- ifelse(prob > 0.5, "Yes", "No")
pred <- factor(pred, levels = levels(test$heart_disease))

cm <- table(Predicted = pred, Actual = test$heart_disease)
cm

accuracy <- sum(diag(cm)) / sum(cm)
accuracy
