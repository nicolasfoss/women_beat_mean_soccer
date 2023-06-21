# Start your code here!
library(tidyverse, quietly = TRUE)

#__________________________
# Exploratory data analysis
#__________________________

# Load files

men <- read_csv("men_results.csv")
women <- read_csv("women_results.csv")

str(men)
head(men)
summary(men)
paste("---------------------------------------------")
str(women)
head(women)
summary(women)

# Look at unique values in the columns naming teams and types of tournaments.
# We might find some problems.
# Men first:

for (col in names(men[,c(3,4,7)])) {
  unique_values <- unique(men[[col]])
  print(paste("Unique values in column", col, ":"))
  print(unique_values)
  print("-----------------------------")
}

# Women next:

for (col in names(women[,c(3,4,7)])) {
  unique_values <- unique(men[[col]])
  print(paste("Unique values in column", col, ":"))
  print(unique_values)
  print("-----------------------------")
}

# There do not seem to be any immediate problems.
# There are 311 teams in home, 306 teams in each dataset for away.
# There are 141 tournament names.
# the men dataset has more than 10x the rows of women due to starting in the 1800's.

#__________________________
# Filter
#__________________________

men_filtered <- men %>% 
  filter(tournament == "FIFA World Cup",
         date > "2002-01-01")

women_filtered <- women %>% 
  filter(tournament == "FIFA World Cup",
         date > "2002-01-01")

str(men_filtered)
paste("-----------------------------")
str(women_filtered)

#__________________________
# Create the new variable
# for hypothesis testing
#__________________________

men_calc <- men_filtered %>% 
  mutate(goals_scored = home_score + away_score)

summary(men_calc)

paste("--------------------------------------------------")

women_calc <- women_filtered %>% 
  mutate(goals_scored = home_score + away_score)

summary(women_calc)

#__________________________
# Choose the correct hypothesis
# test.
#__________________________

# Plot the distribution of the outcome variable we are looking at.
# The goals scored variable in each dataset has strong positive skewness.

paste("The mean of men's goals scored is ", mean(men_calc$goals_scored), ", and the standard deviation is ", sd(men_calc$goals_scored)
)

paste("--------------------------------------------------")

paste("The mean of women's goals scored is ", mean(women_calc$goals_scored), ", and the standard deviation is ", sd(women_calc$goals_scored)
)

ggplot(men_calc, aes(goals_scored)) + 
  geom_boxplot() + 
  coord_flip() + 
  labs(title = "Men's Soccer",
       subtitle = "Distribution of the Goals Scored Variable",
       x = "Goals Scored"
  ) + 
  theme_linedraw()

ggplot(men_calc, aes(goals_scored)) + 
  geom_density(color = "red",
               size = 2
  ) +  
  labs(title = "Men's Soccer",
       subtitle = "Distribution of the Goals Scored Variable",
       x = "Goals Scored"
  ) + 
  theme_linedraw()

ggplot(women_calc, aes(goals_scored)) + 
  geom_boxplot() + 
  coord_flip() + 
  labs(title = "Women's Soccer",
       subtitle = "Distribution of the Goals Scored Variable",
       x = "Goals Scored"
  ) + 
  theme_linedraw()

ggplot(women_calc, aes(goals_scored)) + 
  geom_density(color = "red",
               size = 2
  ) +  
  labs(title = "Women's Soccer",
       subtitle = "Distribution of the Goals Scored Variable",
       x = "Goals Scored"
  ) + 
  theme_linedraw()

# The appropriate test could be the Wilcoxon rank-sum test given that the data are not normally distributed.

test_result <- wilcox.test(women_calc$goals_scored,
                           men_calc$goals_scored, 
                           alternative = "greater"
)

p_val <- test_result$p.value
result <- if_else(p_val <= 0.01, "reject", "fail to reject")

result_df <- data.frame(p_val, result)

result_df