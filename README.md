# Analysis-of-Fast-Food-s-Nutritional-Content
library(dplyr)
library(ggplot2)

data <- read.csv("MultipleFiles/FastFoodNutritionMenuV2.csv", stringsAsFactors = FALSE)

highest_calories_item <- data[which.max(data$Calories), ]
print("Item with the highest calories:")
print(highest_calories_item)

average_calories <- data %>%
  group_by(Company) %>%
  summarise(Average_Calories = mean(Calories, na.rm = TRUE))

print("Average calories per item of all companies:")
print(average_calories)

comparison_table <- data %>%
  summarise(
    Average_Sodium = mean(Sodium, na.rm = TRUE),
    Average_Fat = mean(`Total Fat (g)`, na.rm = TRUE),
    Average_Cholesterol = mean(Cholesterol, na.rm = TRUE),
    Average_Carbs = mean(Carbs, na.rm = TRUE)
  )

print("Average Sodium, Fat, Cholesterol, and Carbs:")
print(comparison_table)

ggplot(data, aes(x = Calories)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black") +
  labs(title = "Histogram of Calories", x = "Calories", y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = Sodium)) +
  geom_histogram(binwidth = 100, fill = "green", color = "black") +
  labs(title = "Histogram of Sodium", x = "Sodium (mg)", y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = `Total Fat (g)`)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  labs(title = "Histogram of Total Fat", x = "Total Fat (g)", y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = Cholesterol)) +
  geom_histogram(binwidth = 20, fill = "purple", color = "black") +
  labs(title = "Histogram of Cholesterol", x = "Cholesterol (mg)", y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = Carbs)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") +
  labs(title = "Histogram of Carbs", x = "Carbs (g)", y = "Frequency") +
  theme_minimal()

dev.off()
