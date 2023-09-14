
              #2022 US College Ranking Analysis using R

                       #Students Name
                 #Institution of Affiliation
                       #Professor’s Name
                  #Course Number and code
                      #Date of Submission


# Read the dataset into a data frame named college_data
college_data <- read.csv("C:\\Users\\n\\Downloads\\2022 US College Rankings 2.csv")

#Gives an observation of the dataset
college_data

#Displays the first five elements
head(college_data)

#Gives the Last five elements
tail(college_data)

#Preliminary Descriptive Statistical Analysis:
# Summary statistics for numeric variables
summary(college_data$Adjusted.Rank)
summary(college_data$Tuition)
summary(college_data$Enrollment.Numbers)

# Levels of categorical variables
levels(college_data$College_Name)

#Exploratory Data Analysis:
# Box plot of tuition by adjusted rank
boxplot(college_data$Tuition ~ college_data$Adjusted.Rank)

# Histogram of tuition
hist(college_data$Tuition)

# Load necessary libraries
library(ggplot2)


# Box plot of tuition by adjusted rank
boxplot(college_data$Tuition ~ college_data$Adjusted.Rank,
        main = "Tuition by Adjusted Rank",
        xlab = "Adjusted Rank",
        ylab = "Tuition")

# Histogram of tuition
hist(college_data$Tuition,
     main = "Distribution of Tuition",
     xlab = "Tuition",
     ylab = "Frequency")


# Frequency chart of adjusted rank
rank_freq <- table(college_data$Adjusted.Rank)
barplot(rank_freq,
        main = "Frequency of Adjusted Rank",
        xlab = "Adjusted Rank",
        ylab = "Frequency")


# Frequency table of enrollment numbers
table(college_data$Enrollment_Numbers)

#Detailed Analysis with Sub-groups:
# Create a faceted bar plot of tuition by adjusted rank for the top 10 colleges
library(ggplot2)
top_10_colleges <- college_data[1:10, ]
ggplot(top_10_colleges, aes(x = College.Name, y = Tuition)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Adjusted.Rank)

#Correlation and Scatterplot Analysis:
# Correlation plot
correlation_matrix <- cor(college_data[, c("Adjusted.Rank", "Tuition", "Enrollment.Numbers")])
correlation_matrix

# Scatterplot matrix with faceting
ggplot(college_data, aes(x = Adjusted.Rank, y = Tuition)) +
  geom_point() +
  facet_grid(Enrollment.Numbers ~ .)

#Multiple Regression Model:
# Formulating the multiple regression model
model <- lm(Tuition ~ Adjusted.Rank + Enrollment.Numbers, data = college_data)
summary(model)

#Regression Model Testing and Prediction
# Prediction using the regression model
new_data <- data.frame(Adjusted.Rank = 15, Enrollment.Numbers = 5000)
predicted_tuition <- predict(model,newdata = new_data)
predicted_tuition

