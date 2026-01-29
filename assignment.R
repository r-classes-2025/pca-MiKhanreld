# установите и загрузите пакеты
library(friends)
library(tidyverse)
library(tidytext)
library(factoextra) 


# 1. отберите 6 главных персонажей (по количеству реплик)
# сохраните как символьный вектор
top_speakers <- friends |> 
  count(speaker, sort = TRUE) |>
  slice_head(n = 6) |>
  pull(speaker)
  

# 2. отфильтруйте топ-спикеров, 
# токенизируйте их реплики, удалите из них цифры
# столбец с токенами должен называться word
# оставьте только столбцы speaker, word
friends_tokens <- friends |> 
  filter(speaker %in% top_speakers) |>
  unnest_tokens(word, text) |>
  filter(!str_detect(word, "\\d")) |>
  select(speaker, word)


# 3. отберите по 500 самых частотных слов для каждого персонажа
# посчитайте относительные частотности для слов (от ВСЕХ слов персонажа!)
friends_tf <- friends_tokens |>
  count(speaker, word) |>
  group_by(speaker) |>
  arrange(desc(n)) |>
  mutate(tf = n / sum(n)) |>
  slice_head(n = 500) |>
  ungroup() |>
  select(speaker, word, tf)


# 4. преобразуйте в широкий формат; 
# столбец c именем спикера превратите в имя ряда
friends_tf_wide <- friends_tf |> 
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) |>
  tibble::column_to_rownames("speaker")


# 5. установите зерно 123
# проведите кластеризацию k-means (k = 3) на относительных значениях частотности (nstart = 20)
# используйте scale()
set.seed(123)
km.out <- kmeans(scale(friends_tf_wide), centers = 3, nstart = 20)


# 6. примените к матрице метод главных компонент (prcomp)
# центрируйте и стандартизируйте, использовав аргументы функции
pca_fit <- prcomp(friends_tf_wide, center = TRUE, scale. = TRUE)


# 7. биплот: текст, цвет = кластер, 20 переменных по cos2
q <- fviz_pca_biplot(
  pca_fit,
  geom = "text",
  habillage = as.factor(km.out$cluster),
  select.var = list(cos2 = 20),
  repel = FALSE
) +
  theme_minimal()