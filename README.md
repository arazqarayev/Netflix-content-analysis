# ======================================================
# 📊 Netflix Basic Content Analysis in R
# Description: Simple data analysis of Netflix dataset
# ======================================================

# 1️⃣ Libraries
library(tidyverse)

# 2️⃣ Read Data
# Make sure netflix.csv is in the same folder as this script
df <- read.csv("netflix.csv")

# Quick preview
head(df)
summary(df)

# 3️⃣ Number of Titles by Year
# Count how many titles were released each year
titles_by_year <- df %>%
  group_by(year) %>%
  summarise(total_titles = n()) %>%
  arrange(desc(total_titles))

print(titles_by_year)

# 4️⃣ Visualization
# Plot the number of Netflix titles by year
ggplot(titles_by_year, aes(x = year, y = total_titles)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Number of Netflix Titles by Year",
       x = "Year",
       y = "Total Titles") +
  theme_minimal()

# 5️⃣ IMDb Average by Genre
