# Netflix-content-analysis
title: "Netflix Content Analysis (R)"
output:
  html_document:
    toc: true
    toc_float: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, fig.width=8, fig.height=4.5)
library(tidyverse)
library(janitor)
library(lubridate)
library(plotly)
library(GGally)
```

## 1) Amaç & Yol Haritası

Bu çalışma, `netflix.csv` verisini kullanarak **yıllara**, **türlere** ve **dillere** göre içerik dağılımlarını analiz eder.  
Adımlar:
1. Veri yükleme ve hızlı kalite kontrol
2. Temel temizlik (sütun isimleri, tip dönüşümleri)
3. Keşif görselleştirmeleri
4. Trend analizi (yıllara göre)
5. Tür & dil kırılımlarında IMDb dağılımı
6. Opsiyonel: interaktif grafik (plotly)

> Dosya yolu varsayılan olarak `data/netflix.csv` kabul edilmiştir.

## 2) Veri Yükleme & Ön Kontrol

```{r load}
data_path <- file.path("data", "netflix.csv")
df <- readr::read_csv(data_path, show_col_types = FALSE) %>% clean_names()

glimpse(df)
summary(df)
```

```{r type-clean}
# Tarih & sayısal alanları güvenli dönüştürme
if ("premiere" %in% names(df)) {
  df <- df %>% mutate(premiere = ymd(premiere, quiet = TRUE))
}

if ("year" %in% names(df)) {
  df <- df %>% mutate(year = suppressWarnings(as.integer(year)))
}

if ("runtime" %in% names(df)) {
  df <- df %>% mutate(runtime = suppressWarnings(as.numeric(runtime)))
}

# Yardımcı alanlar
df <- df %>%
  mutate(
    premiere_year = case_when(
      "premiere" %in% names(df) ~ year(premiere),
      TRUE ~ year
    ),
    content = paste0(coalesce(title, ""), " ", coalesce(genre, ""), " ", coalesce(language, ""))
  )
```

## 3) Veri Kalitesi: Eksik Değerler & Benzersizler

```{r quality}
missing_by_col <- df %>% summarise(across(everything(), ~ sum(is.na(.))))
missing_by_col

unique_counts <- df %>% summarise(across(everything(), ~ dplyr::n_distinct(.)))
unique_counts
```

## 4) En Çok İçerik Üretilen Yıllar

```{r releases-per-year}
releases_per_year <- df %>%
  filter(!is.na(premiere_year)) %>%
  count(premiere_year, sort = FALSE)

gg_year <- ggplot(releases_per_year, aes(x = premiere_year, y = n)) +
  geom_line() + geom_point() +
  labs(title = "Yıllara Göre İçerik Sayısı", x = "Yıl", y = "Adet")
gg_year
# Interaktif versiyon (opsiyonel)
ggplotly(gg_year)
```

## 5) IMDb Puanlarının Türlere Göre Ortalaması (Top Türler)

```{r imdb-genre}
# En sık görülen 12 türü al, uzun listeyi kısalt
top_genres <- df %>% count(genre, sort = TRUE) %>% slice_head(n = 12) %>% pull(genre)

avg_imdb_by_genre <- df %>%
  filter(genre %in% top_genres, !is.na(imdb_score)) %>%
  group_by(genre) %>%
  summarise(avg_imdb = mean(imdb_score, na.rm = TRUE), n = n(), .groups = "drop") %>%
  arrange(desc(avg_imdb))

gg_imdb_genre <- ggplot(avg_imdb_by_genre, aes(x = avg_imdb, y = fct_reorder(genre, avg_imdb))) +
  geom_col() +
  labs(title = "IMDb Ortalaması (Top Türler)", x = "Ortalama IMDb", y = "Tür")
gg_imdb_genre
ggplotly(gg_imdb_genre)
```

## 6) Hangi Dillerde Daha Fazla İçerik Var? (Top Diller)

```{r languages}
top_langs <- df %>% count(language, sort = TRUE) %>% slice_head(n = 12)

gg_lang <- ggplot(top_langs, aes(x = n, y = fct_reorder(language, n))) +
  geom_col() +
  labs(title = "En Çok İçerik Üretilen Diller", x = "Adet", y = "Dil")
gg_lang
ggplotly(gg_lang)
```

## 7) Ortalama Süre (runtime) Yıllara Göre Nasıl Değişmiş?

```{r runtime-trend}
if (all(c("runtime","premiere_year") %in% names(df))) {
  runtime_year <- df %>%
    filter(!is.na(runtime), !is.na(premiere_year)) %>%
    group_by(premiere_year) %>%
    summarise(mean_runtime = mean(runtime, na.rm = TRUE), .groups = "drop")

  gg_rt <- ggplot(runtime_year, aes(x = premiere_year, y = mean_runtime)) +
    geom_line() + geom_point() +
    labs(title = "Ortalama Runtime (Dakika) — Yıllara Göre", x = "Yıl", y = "Ortalama Runtime")
  gg_rt
  ggplotly(gg_rt)
}
```

## 8) IMDb Dağılımı (Boxplot) — Türlere Göre

```{r imdb-box}
if ("imdb_score" %in% names(df)) {
  df %>%
    filter(genre %in% top_genres, !is.na(imdb_score)) %>%
    ggplot(aes(x = fct_reorder(genre, imdb_score, .fun = median, .desc = TRUE), y = imdb_score)) +
    geom_boxplot(outlier.alpha = 0.25) +
    labs(title = "IMDb Puan Dağılımı — Tür Bazında", x = "Tür (Top)", y = "IMDb")
}
```

## 9) IMDb vs Runtime Korelasyonu (Scatter & Korelasyon)

```{r corr}
if (all(c("runtime","imdb_score") %in% names(df))) {
  cor_val <- cor(df$runtime, df$imdb_score, use = "complete.obs")
  cor_val

  gg_corr <- ggplot(df, aes(x = runtime, y = imdb_score)) +
    geom_point(alpha = 0.25) +
    geom_smooth(method = "loess", se = FALSE) +
    labs(title = paste0("IMDb vs Runtime — Korelasyon: ", round(cor_val, 3)),
         x = "Runtime (dk)", y = "IMDb")
  gg_corr
}
```

## 10) Ek: Çok Değişkenli Hızlı Bakış (GGally)

```{r ggpairs, eval=TRUE}
num_cols <- df %>% select(where(is.numeric)) %>% drop_na() %>% names()
if (length(num_cols) >= 2) {
  GGally::ggpairs(df %>% select(all_of(num_cols)) %>% drop_na() %>% slice_head(n = 500))
}
```

## 11) Özet & Notlar

- **Yıllar:** Hangi yıllarda içerik hacmi artmış → içerik stratejisi için ipucu.  
- **Türler:** Hangi türler yüksek IMDb alıyor → kalite algısı.  
- **Diller:** Çok dillilik eğilimleri → pazar genişlemesi sinyali.  
- **Süre:** Ortalama runtime değişimi → prodüksiyon standartları.  
