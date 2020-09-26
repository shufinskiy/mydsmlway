### **Метод ближайших соседей (K-nearset neighbor)**

Непараметрический необобщающий метод машинного обучения

1. Для каждого наблюдения найти К наиболее похожих наблюдений из обучающей выборки
2. Для классификации:
	- Получить мажоритарный класс среди выбранных записей и назначить его наблюдению
3. Для регрессии (KNN-регрессия):
	- Найти среди выбранных записей среднее значение зависимой переменной и назначить его для наблюдения

**Метрические показатели расстояния** - метрические показатели, которые обобщают в одном числе, насколько далеко одна запись находится от другой.

Показатель К, обычно, подбирается путём кросс-валидации на тренировочных данных. Помимо заданной константы К может вариароваться от локальной плотности точек (radius-based neighbor learning). 

Для нахождения расстояния между объектами обычно используется либо евклидово расстояние (корень квадратный из суммы разностей квадратов признаков) или манхэттенское расстояние (модуль разности признаков).

Обобщённая формула расстояния Минковского:

<p align="center">
	<img width="300" height="150" src = "https://render.githubusercontent.com/render/math?math=\rho(x,%20y)%20=%20(\sum^n_{i=1}%20\left|x_i%20-%20y_i\right|^p)^{1/p}">
</p>

Метод KNN часто используется как первый в пайплайне обучения модели. На основе модели KNN создаётся признак, который выдаёт какие-то вероятности независимой переменной и этот признак уже далее используется в других моделях (не KNN)

KNN не работает на больших объёмах данных из-за двух причин:
  - На больший данных у нас получится огромная матрица расстояний, которая может не влезть в память, плюс это очень долго.
  - Проклятие размерности. При большом кол-ве признаков мы приходим к тому, что точки получаются примерно равноудаленны от центра и KNN не может правильно выбрать нужные нам точки.

#### Scikit-learn
Классификация:
  - [KNeighborsClassifier](https://scikit-learn.org/stable/modules/generated/sklearn.neighbors.KNeighborsClassifier.html#sklearn.neighbors.KNeighborsClassifier) - обучение на основе числа соседей, указанного пользователем
  - [RadiusNeighborsClassifier](https://scikit-learn.org/stable/modules/generated/sklearn.neighbors.RadiusNeighborsClassifier.html#sklearn.neighbors.RadiusNeighborsClassifier) - обучение на основе числа соседй в пределах радиуса, указанного пользователем


```r
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

## TRUE
##	30
```
