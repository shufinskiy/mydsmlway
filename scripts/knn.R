library(FNN)
library(dplyr)
set.seed(42)

## Загрузка таблицы iris
data <- iris

## Функция Z-score(стандартная оценка)
z_score <- function(x){
  x <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  x
}

## Стандартизация данных
data <- data %>% 
  mutate_if(is.numeric, z_score) %>% 
  mutate(Species = ifelse(Species == 'setosa', 1, 
                          ifelse(Species == 'versicolor', 2, 3)))

## Разбиение данных на тренировочную и тестовую выборки
train <- sample(seq(1:150), 120, replace = FALSE)
test <- setdiff(seq(1:150), train)

train_df <- data[train, ]
test_df <- data[test, -5]

## Функция расчёта ближайщих соседей по евклидову расстоянию (не стоит использовать кроме как пример)
knn_euclidean <- function(train, test, cl, k){
  cl <- substitute(cl)
  
  ## Для каждого тестового наблюдения рассчитаем евклидово расстояние до всех
  ## наблюдений в тренировочной выборке, отбираем K самых ближайщих,
  ## из значений зависимой переменной этих K наблюдений, выбираем наиболее часто
  ## встречающееся и присваиваем его зависимой переменной тестового наблюдения
  sapply(seq(1, nrow(test)), function(x){
    euc <- sapply(seq(1, nrow(train)), function(n){
      ## Функция расчёта евклидова расстояния
      euc_dist <- function(x, y){
        sqrt(sum((x - y)^2))
      }
      euc_dist(test[x,], subset(train[n, ], select = -eval(cl)))
    })
    ## Выбираем K наиболее близких значений и отбираем 
    ## наиболее часто встречающееся значение зависимой переменной
    k_nearest <- which(euc %in% sort.int(euc, partial = k, decreasing = FALSE)[1:k])
    tbl <- table(subset(train[k_nearest, ], select = eval(cl)))
    q <- as.numeric(names(tbl[which(tbl == max(tbl))]))
  })
}

knn_custom_cl <- knn_euclidean(train_df, test_df, Species, 5)

cl <- data[train, 5]

## KNN-классификация пакета FNN
knn_fnn_cl <- FNN::knn(train = subset(train_df, select = -Species),
              test = test_df,
              cl = cl,
              k = 5)

## KNN-классификация пакета class
knn_class_cl <- class::knn(train = subset(train_df, select = -Species),
                           test = test_df,
                           cl = cl,
                           k = 5)

print(table(data[test, 5] == knn_custom_cl))
print(table(data[test, 5] == knn_fnn_cl))
print(table(data[test, 5] == knn_class_cl))