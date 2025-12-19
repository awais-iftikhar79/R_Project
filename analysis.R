# Step 1: Load the dataset
data <- read.csv("heart_disease_uci.csv")
head(data)

# Step 2.1: View structure of dataset
str(data)

# Step 2.2: Check dimensions (rows, columns)
dim(data)

# Step 2.3: View column names
colnames(data)

# Step 3: Check missing values in each column
colSums(is.na(data))

# Check duplicate rows
sum(duplicated(data))

# Remove duplicates
data <- data[!duplicated(data), ]


# Remove unnecessary columns
data$ca <- NULL
data$id <- NULL

# List of numerical columns
num_cols <- c("trestbps", "chol", "thalch", "oldpeak")

# Replace NA with median
for (col in num_cols) {
    data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
}

head(data)
colSums(is.na(data))

# Function to calculate mode
get_mode <- function(x) {
    ux <- unique(x[!is.na(x)])
    ux[which.max(tabulate(match(x, ux)))]
}

# Categorical columns
cat_cols <- c("fbs", "exang")

# Replace NA with mode
for (col in cat_cols) {
    data[[col]][is.na(data[[col]])] <- get_mode(data[[col]])
}

# Convert categorical variables to factors
data$sex <- as.factor(data$sex)
data$dataset <- as.factor(data$dataset)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$thal <- as.factor(data$thal)

# Convert target variable to factor (binary later)
data$num <- as.factor(data$num)


summary(data)
