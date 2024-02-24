# Load the datasets
data_june_2022 <- read.csv('Born_in_Wales_Data_June_2022.csv')
follow_up_june_2022 <- read.csv('Born_in_Wales_Follow-up_Data_June_2022.csv')

# Display the first few rows of each dataset to understand its structure
print("June 2022 Data:")
print(head(data_june_2022))

print("Follow-up June 2022 Data:")
print(head(follow_up_june_2022))

# Examine the structure of the data, check for missing values, and handle them appropriately
# Display information about the datasets
print(str(data_june_2022))

print(str(follow_up_june_2022))

# Check for missing values
print("Missing values in June 2022 Data:")
print(colSums(is.na(data_june_2022)))

print("Missing values in Follow-up June 2022 Data:")
print(colSums(is.na(follow_up_june_2022)))

# Merging of the two datasets
merged_data <- merge(data_june_2022, follow_up_june_2022, by.x='SYSTEM_ID', by.y='STUDY_ID', all=FALSE)

## For some reason when loading the dataset some rows are empty
# Replace empty spaces with NA
merged_data[merged_data == ""] <- NA

# Display the first few rows of the dataset
head(merged_data)

# Display the last few rows of the dataset
tail(merged_data)

# Display the structure of the dataset
str(merged_data)

# Summarize the dataset
summary(merged_data)

# Impute missing values with the mean for numeric columns
numeric_columns <- sapply(merged_data, is.numeric)  # Identify numeric columns  
numeric_columns_names <- names(numeric_columns)[numeric_columns]  # This get the names of numeric columns  
for (col in numeric_columns_names) {
  merged_data[[col]][is.na(merged_data[[col]])] <- mean(merged_data[[col]], na.rm = TRUE)  
}

# View the dataframe after imputation
print(merged_data)

#this will check if they are any missing value for the numeric columns
colSums(is.na(merged_data))


# Identify character columns
character_columns <- sapply(merged_data, is.character)
character_columns_names <- names(character_columns)[character_columns]

print(character_columns)

# This define a function to calculate mode with handling for NA values
calculate_mode <- function(x) {
  x <- x[!is.na(x)]
  unique_values <- unique(x)
  mode_value <- unique_values[which.max(tabulate(match(x, unique_values)))]
  return(mode_value)
}

# Impute missing values with mode for character columns
for (col in character_columns_names) {
  merged_data[[col]][is.na(merged_data[[col]])] <- calculate_mode(merged_data[[col]])  
}

# Print summary statistics of the dataset
summary(merged_data)

# Check for missing values in the entire dataset
any(is.na(merged_data))


# Selecting demographic columns
demographic_columns <- c(
  "What.is.the.main.language.spoken.in.your.home.",
  "What.is.your.ethnic.group.",
  "Nationality",
  "What.is.your.current.relationship.status..",
  "Would.you.consider.yourself.to.be."
)

# Extracting demographic data
demographic_data <- merged_data[, demographic_columns]

# Renaming columns for better readability 
colnames(demographic_data) <- c(
  "Main_Language",
  "Ethnic_Group",
  "Nationality",
  "Relationship_Status",
  "Self_Perception"
)

# Printing the first few rows of the demographic data
head(demographic_data)


# Define the numerical columns
numerical_columns <- c(
  "My.weight..before.pregnancy..in.Kg.is.",
  "How.old.are.your.other.children..please.separate.each.child.s.age.by.a.comma...E.g...2..4.and.8",
  "How.many.people.live.in.your.home..not.including.you..",
  "How.many.people..who.are.not.part.of.your.household..did.you.talk.to.in.person.yesterday..e.g..were.within.1.metre.and.exchanged.words.but.did.not.touch...",
  "How.many.people..who.are.not.part.of.your.household..did.you.have.direct.physical.contact.with.yesterday..hugged..touched..",
  "WIMD.2019.Rank",
  "WIMD.2019.Decile",
  "WIMD.2019.Quintile",
  "WIMD.2019.Quartile"
)

# Summary statistics for numerical variables
summary_stats <- summary(merged_data[, numerical_columns])
print(summary_stats)


# Calculate the first and third quartiles
Q1 <- quantile(merged_data$numerical_columns, 0.25)
Q3 <- quantile(merged_data$numerical_columns, 0.75)

# Calculate the interquartile range (IQR)
IQR <- Q3 - Q1

# Define the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- merged_data$numerical_columns[merged_data$numerical_columns < lower_bound | merged_data$numerical_columns > upper_bound]

# Print or visualize the outliers
print(outliers)


# Load necessary libraries for plotting
install.packages("ggplot2")
library(ggplot2)
library(dplyr)

# Histograms for numerical variables
num_cols <- sapply(merged_data, is.numeric)
numeric_data <- merged_data[, num_cols]

histograms <- lapply(names(numeric_data), function(column_name) {
  ggplot(data = data.frame(x = numeric_data[[column_name]]), aes(x = x)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(title = paste("Histogram of", column_name))
})

print(histograms)


# list of categorical variables
categorical_variables <- c(
  "Start.time.x", "Expected.date.of.delivery.of.your.baby", 
  "What.is.the.main.language.spoken.in.your.home.", "What.is.your.ethnic.group.", 
  "Nationality", "What.is.your.current.relationship.status..", 
  "Would.you.consider.yourself.to.be.", "Have.you.had.symptoms.that.are.associated.with.COVID19..fever..dry.cough..loss.of.taste.or.smell..fatigue..muscle.pain..", 
  "When.did.your.symptoms.start..approximately..", "What.symptoms.did.you.have.", 
  "What.treatment.did.you.have..", "Have.you.had.a.test.", 
  "Have.you.experienced.low.mood.during.your.pregnancy.", "Little.interest.or.pleasure.in.doing.things.", 
  "Feeling.down..depressed..or.hopeless.", "Trouble.falling.or.staying.asleep..or.sleeping.too.much", 
  "Feeling.tired.or.having.little.energy.", "Poor.appetite.or.overeating.", 
  "Feeling.bad.about.yourself.or.that.you.are.a.failure.of.have.let.yourself.or.family.down.", "Trouble.concentrating.on.things..such.as.reading.the.newspaper.or.watching.television.", 
  "Moving.or.speaking.so.slowly.that.other.people.could.have.noticed..Or.the.opposite.being.so.fidgety.or.restless.that.you.have.been.moving.around.a.lot.more.than.usual.", "Thoughts.that.you.would.be.better.off.dead..or.of.hurting.yourself.in.some.way.", 
  "Do.you.smoke.", "Do.you.drink.alcohol.", 
  "Feeling.nervous..anxious.or.on.edge.", "Not.being.able.to.stop.or.control.worrying.", 
  "Worrying.too.much.about.different.things.", "Trouble.relaxing.", 
  "Being.so.restless.that.it.is.hard.to.sit.still.", "Becoming.easily.annoyed.or.irritable.", 
  "Feeling.afraid.as.if.something.awful.might.happen.", "Please.tell.us.the.type.and.amount.of.physical.activity.involved.in.your.work", 
  "Physical.exercise.such.as.swimming..jogging..aerobics..football..tennis..gym.workout.etc.", "Cycling..including.cycling.to.work.and.during.leisure.time", 
  "Walking..including.walking.to.work..shopping..for.pleasure.etc.", "Housework.Childcare", 
  "Gardening.DIY", "How.would.you.describe.your.usual.walking.pace...Please.mark.one.box.only.", 
  "Have.you.had.any.periods.of.bad.stress.in.your.pregnancy..", "any.serious.relationship.difficulties.with.your.husband.or.partner.or.separated.or.divorced.", 
  "any.serious.legal.or.financial.problems.", "were.you.or.someone.close.to.you.a.victim.of.violence.or.crime.", 
  "someone.close.with.a.serious.illness", "the.death.of.someone.close.to.you..", 
  "was.this.stressful.event.related.to.coronavirus.", "During.this.time.did.you.have.someone.who.could.support.you.emotional.or.financially", 
  "Do.you.feel.you.were.able.to.answer.the.last.two.questions.above.accurately.", "What.is.your.view.on.having.the.COVID.vaccination.in.pregnancy..have.you.or.would.you.have.the.COVID.vaccination.when.pregnant.and.why.", 
  "Start.time.y", "Delivery.date", "Started.breastfeeding", "Stopped.breastfeeding"
)

# Create bar plots for each categorical variable
barplots <- lapply(categorical_variables, function(variable) {
  ggplot(merged_data, aes(x = !!as.name(variable))) +
    geom_bar() +
    labs(title = paste("Bar plot of", variable))
})

# Print or plot the bar plots
print(barplots)


# Scatter plots for pairs of numerical variables
scatterplots <- combn(names(numeric_data), 2, function(pair) {
  ggplot(data = numeric_data, aes_string(x = pair[1], y = pair[2])) +
    geom_point() +
    labs(title = paste("Scatter plot of", pair[1], "vs", pair[2]), x = pair[1], y = pair[2])
}, simplify = FALSE)

# Print or plot the scatterplots
print(scatterplots)

# Correlation heatmap
correlation_matrix <- cor(numeric_data)
heatmap <- ggplot(data = as.data.frame(as.table(correlation_matrix)), aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  labs(title = "Correlation Heatmap")

print(heatmap)


#feature engineering
# Search for columns related to symptoms
symptom_cols <- grep("symptoms", names(merged_data), ignore.case = TRUE, value = TRUE)
print(symptom_cols)

# Extracting the relevant column
symptoms_column <- merged_data$`Have.you.had.symptoms.that.are.associated.with.COVID19..fever..dry.cough..loss.of.taste.or.smell..fatigue..muscle.pain..`

# Split the column to obtain individual symptom scores
symptoms <- strsplit(symptoms_column, ",")

# Convert each symptom score to numeric
symptoms_numeric <- lapply(symptoms, function(x) sum(as.numeric(x), na.rm = TRUE))

# Define a function to convert categorical values to numeric no = 0 and yes 1
convert_to_numeric <- function(x) {
  ifelse(tolower(x) == "no", 0, 1)
}

# Apply the function to each element of symptoms
symptoms_numeric <- lapply(symptoms, convert_to_numeric)

head(symptoms_numeric)

# Adding the total score of symptoms as a new variable to the dataset
merged_data$total_symptoms <- unlist(symptoms_numeric)

# Checking if the new variable is added successfully
print(head(merged_data$total_symptoms))

# Extracting month and day of week from date variables
merged_data$delivery_month <- as.factor(format(as.Date(merged_data$Delivery.date), "%m"))
merged_data$delivery_day_of_week <- as.factor(weekdays(as.Date(merged_data$Delivery.date)))

#convert values to dates
dates <- as.Date(merged_data$Delivery.date, format = "%Y-%m-%d", errors = "coerce")

# Check for missing or incorrect values in Delivery.date
incorrect_dates <- sum(is.na(merged_data$Delivery.date) | merged_data$Delivery.date == "")
if (incorrect_dates > 0) {
  print(paste("There are", incorrect_dates, "incorrect or missing dates in the Delivery_date column."))
} else {
  print("All values in the Delivery.date column are correct dates.")
}

# Define a list of categorical columns that need to be encoded
categorical_columns <- c('What.is.the.main.language.spoken.in.your.home?',
                         'What.is.your.ethnic.group?',
                         'Nationality',
                         'What.is.your.current.relationship.status.?',
                         'Would.you.consider.yourself.to.be:'
)

# Iterate over each categorical column and encode its values
for (col in categorical_columns) {
  merged_data[[col]] <- as.numeric(factor(merged_data[[col]]))
}

print(merged_data)


### Regression Analysis
# Load necessary libraries
install.packages("glmnet")
library(dplyr)
library(tidyr)
library(glmnet)

# Select relevant features
features <- merged_data %>%
  select(What.is.the.highest.level.of.education.you.have.reached., 
        My.weight..before.pregnancy..in.Kg.is., 
        My.height.in.centimetres.is..,
        Number.of.children,
        How.are.you.planning.to.feed.your.baby.)
      
# Encode categorical variables
features <- features %>%
  mutate(What.is.the.highest.level.of.education.you.have.reached. = as.factor(What.is.the.highest.level.of.education.you.have.reached.))

print(features)

# Split data into train and test sets
set.seed(123)  # for reproducibility
train_index <- sample(1:nrow(features), 0.7 * nrow(features))
train_data <- features[train_index, ]
test_data <- features[-train_index, ]

# Convert the variable (How.are.you.planning.to.feed.your.baby.)to binary Breast milk only is 1 and Breast and bottle is 0
train_data$binary_outcome <- ifelse(train_data$How.are.you.planning.to.feed.your.baby. == "Breast milk only", 1, 0)

# Perform logistic regression
logit_model <- glm(binary_outcome ~ ., data = train_data, family = "binomial", maxit = 100)

# Summary of the model
summary(logit_model)

# Convert the outcome variable to a factor with appropriate levels
test_data$How.are.you.planning.to.feed.your.baby. <- factor(test_data$How.are.you.planning.to.feed.your.baby., levels = c("Breast milk only", "Breast and bottle", "Don't know yet"))

# Replace NA values with "Don't know yet"
test_data$How.are.you.planning.to.feed.your.baby.[is.na(test_data$How.are.you.planning.to.feed.your.baby.)] <- "Don't know yet"

# Check the structure of the test data again
str(test_data)

# Predictions on test data
predictions <- predict(logit_model, newdata = test_data, type = "response")


# Summary statistics of the test data
summary(test_data)


## Decision Tree or Clustering Analysis
# Load required libraries
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Train a decision tree model
decision_tree <- rpart(How.are.you.planning.to.feed.your.baby. ~ ., data = train_data, method = "class")

# Print text representation of the decision tree
printcp(decision_tree)

# Visualize the decision tree
rpart.plot(decision_tree)

## Survival Analysis and Cox Regression
install.packages("survival")
library(survival)

# Convert character variables to Date objects
merged_data$Stopped.breastfeeding <- as.Date(merged_data$Stopped.breastfeeding)
merged_data$Started.breastfeeding <- as.Date(merged_data$Started.breastfeeding)

# Calculate the duration of breastfeeding
merged_data$duration_breastfeeding <- as.numeric(difftime(merged_data$Stopped.breastfeeding, merged_data$Started.breastfeeding, units = "days"))

# Fit a Kaplan-Meier survival curve
survfit_object <- survfit(Surv(duration_breastfeeding) ~ 1, data = merged_data)

# Plot the Kaplan-Meier curve
plot(survfit_object, main = "Kaplan-Meier Survival Curve", xlab = "Time", ylab = "Survival Probability")

summary(survfit_object)


# Fitting of Cox regression model
cox_model <- coxph(Surv(duration_breastfeeding) ~ What.is.your.current.relationship.status.. + Do.you.work, data = merged_data)

# Summary of the Cox regression model
summary(cox_model)

