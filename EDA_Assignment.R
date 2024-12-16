#Part 1
library(ggplot2)
library(dplyr)

path <- "C:/Users/uppur/Downloads/School/ames.csv"
ames <- read.csv(path)

head(ames)
summary(ames)
str(ames)

features <- c("SalePrice", "TotRmsAbvGrd", "OverallCond", "YrSold", "YearBuilt", "LandSlope")
ames_selected <- ames %>% select(all_of(features))

#Part 2
plot_histogram <- function(data, column, title, xlabel, ylabel) {
  ggplot(data, aes_string(x = column)) +
    geom_histogram(binwidth = 10, fill = "blue", alpha = 0.7) +
    geom_vline(aes_string(xintercept = paste0("mean(", column, ", na.rm = TRUE)")), 
               color = "black", linetype = "dashed") +
    labs(title = title, x = xlabel, y = ylabel)
}

plot_histogram(ames, "SalePrice", "Distribution of Sale Prices", "Sale Price", "Number of Houses")

plot_histogram(ames, "TotRmsAbvGrd", "Distribution of Total Rooms Above Grade", "Total Rooms", "Number of Houses")

plot_histogram(ames, "OverallCond", "Distribution of Overall Condition", "Condition", "Number of Houses")

# Summary stats
summary_stats <- function(data, column) {
  list(
    Mean = mean(data[[column]], na.rm = TRUE),
    Median = median(data[[column]], na.rm = TRUE),
    SD = sd(data[[column]], na.rm = TRUE)
  )
}

summary_stats(ames, "SalePrice")
summary_stats(ames, "TotRmsAbvGrd")
summary_stats(ames, "OverallCond")

#Part 3
# Subsets based on OverallCond
below_avg <- ames %>% filter(OverallCond < 5)
average <- ames %>% filter(OverallCond == 5)
above_avg <- ames %>% filter(OverallCond > 5)

# Plot distributions
ggplot() +
  geom_histogram(data = below_avg, aes(x = SalePrice, fill = "Below Average"), alpha = 0.5, binwidth = 10000) +
  geom_histogram(data = average, aes(x = SalePrice, fill = "Average"), alpha = 0.5, binwidth = 10000) +
  geom_histogram(data = above_avg, aes(x = SalePrice, fill = "Above Average"), alpha = 0.5, binwidth = 10000) +
  scale_fill_manual(values = c("Below Average" = "yellow", "Average" = "gray", "Above Average" = "cyan")) +
  labs(title = "Sale Price Distribution by Overall Condition", x = "Sale Price", y = "Number of Houses") +
  theme_minimal()

#Part 4
numeric_columns <- ames %>% select(where(is.numeric))
correlations <- cor(numeric_columns, use = "complete.obs")
saleprice_corr <- correlations[,"SalePrice"]

# Most positively and negatively correlated features
most_positive <- names(sort(saleprice_corr, decreasing = TRUE))[2]
most_negative <- names(sort(saleprice_corr))[1]

cat("Most Positively Correlated:", most_positive, "\n")
cat("Most Negatively Correlated:", most_negative, "\n")

# Scatter plot
ggplot(ames, aes_string(x = most_positive, y = "SalePrice")) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = paste("Scatter Plot of", most_positive, "vs SalePrice"),
       x = most_positive, y = "Sale Price")

ggplot(ames, aes_string(x = most_negative, y = "SalePrice")) +
  geom_point(alpha = 0.5, color = "red") +
  labs(title = paste("Scatter Plot of", most_negative, "vs SalePrice"),
       x = most_negative, y = "Sale Price")

#Part 5
ames <- ames %>% mutate(Age = YrSold - YearBuilt)

# Scatter plot of Age vs SalePrice
ggplot(ames, aes(x = Age, y = SalePrice)) +
  geom_point(alpha = 0.3, color = "green") +
  labs(title = "Scatter Plot of Home Age vs Sale Price", 
       x = "Age of Home (Years)", y = "Sale Price")
