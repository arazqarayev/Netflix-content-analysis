# ==
# 📊 Netflix Basic Content Analysis in R
# Description: Simple data analysis of Netflix dataset
# ======================================================
<img width="870" height="602" alt="image" src="https://github.com/user-attachments/assets/48ea19f9-56d4-4d1b-b0fc-4f8b90ea995b" />

# 1️⃣ Libraries
library(tidyverse)

# 2️⃣ Read Data

df <- read.csv("netflix.csv")

# Quick preview
head(df)
summary(df)

# 3️⃣ Number of Titles by Year

titles_by_year <- df %>%
  group_by(year) %>%
  summarise(total_titles = n()) %>%
  arrange(desc(total_titles))

print(titles_by_year)

# 4️⃣ Visualization

ggplot(titles_by_year, aes(x = year, y = total_titles)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Number of Netflix Titles by Year",
       x = "Year",
       y = "Total Titles") +
  theme_minimal()

# 5️⃣ IMDb Average by Genre (Optional)
# Average IMDb score per genre (if column exists)
if("imdb_score" %in% names(df) & "genre" %in% names(df)) {
  avg_imdb <- df %>%
    group_by(genre) %>%
    summarise(average_imdb = mean(imdb_score, na.rm = TRUE)) %>%
    arrange(desc(average_imdb))
  print(avg_imdb)
}

# 6️⃣ Save results (optional)
# write.csv(titles_by_year, "titles_by_year.csv", row.names = FALSE)
