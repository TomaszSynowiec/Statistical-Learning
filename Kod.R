# Statistical learning w praktyce
 

# Cały kod liczy się około 15min

# Ładujemy wszystkie potrzebne biblioteki
library(ggplot2)
library(reshape2)
library(boot)
library(tidyr)
library(leaps)
library(glmnet)
library(pls)
library(tree)
library(randomForest)
library(gbm)
library(dbarts)
library(xgboost)
library(knitr)
library(kableExtra)
library(caret)

set.seed(123)

# Wczytujemy dane
data <- read.csv("C:/Users/Tomasz/Desktop/projekt statistical/dane.csv")


head(data)
str(data)
summary(data)


# Zmieniamy zmienne z typu TRUE/FALSE na 1/0 i zmienne jakościowe na factor
data$grade <- as.factor(data$grade)

data$quartile_zone <- as.factor(data$quartile_zone)

data$has_basement <- as.integer(as.logical(data$has_basement))

data$renovated <- as.integer(as.logical(data$renovated))

data$nice_view <- as.integer(as.logical(data$nice_view))

data$perfect_condition <- as.integer(as.logical(data$perfect_condition))

data$has_lavatory <- as.integer(as.logical(data$has_lavatory))

data$single_floor <- as.integer(as.logical(data$single_floor))

str(data)
summary(data)


# Wyrzucamy kolumne date i month bo są one zbędne w naszej analizie.
data <- subset(data, select = -c(date, month))

# Sprawdzamy czy w naszym zbiorze nie ma wartości pustych
colSums(is.na(data))


# Tworzymy macierz korelacji i mape ciepła
numeric_columns = sapply(data, is.numeric)  
correlation_matrix = cor(data[, numeric_columns], use = "complete.obs")

melted_corr = melt(correlation_matrix)

ggplot(data = melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4)


correlation_matrix

# Widzimy, że zmiennymi najbardziej skorelowanymi ze zmienną price są: living_in_m2, real_bathrooms, bedrooms


# Teraz trochę analizy danych

# Cena vs powierzchnia mieszkania
ggplot(data, aes(x = living_in_m2, y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, col = 'red')+
  labs(title = "Cena vs Powierzchnia domu", x = "Powierzchnia domu (m2)", y = "Cena") +                                                     
  theme_minimal() 

# Rozkład cen domów
ggplot(data, aes(x = price)) +
  geom_histogram(binwidth = 50000, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Rozkład cen domów", 
       x = "Cena (price)", 
       y = "Liczba domów") +
  theme_minimal()

# Rozkład powierzchni domów
ggplot(data, aes(x = living_in_m2)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  labs(title = "Rozkład powierzchni mieszkalnej domów", x = "Powierzchnia mieszkalna (m2)", y = "Liczba domów") +
  theme_minimal()

# Rozkład cen w różnych strefach
ggplot(data, aes(x = price, fill = quartile_zone)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7,color = "darkgrey") +
  labs(title = "Rozkład cen domów w różnych strefach", x = "Cena", y = "Liczba domów") +
  theme_minimal()

# Cena vs liczba sypialni
ggplot(data, aes(x = factor(bedrooms), y = price, fill = factor(bedrooms))) +
  geom_boxplot() +
  labs(title = "Cena vs Liczba sypialni", x = "Liczba sypialni", y = "Cena", fill = "Liczba sypialni") +
  theme_minimal()

# Cena vs strefa zamieszkania
ggplot(data, aes(x = factor(quartile_zone), y = price, fill = factor(quartile_zone))) +
  geom_boxplot() +
  labs(title = "Cena vs strefa zamieszkania", x = "", y = "Cena", fill="strefa")+
  theme_minimal()





#  ----------------------- GLM -----------------------



# Stworzymy kilka modeli liniowych

# Tworzenie modelu regresji liniowej z wybranymi zmiennymi
model_1 <- glm(price ~ grade + living_in_m2 + quartile_zone, data = data)

summary(model_1)


# Drugi model to będzie Model_1 + zmienne z wyższą korelacją
model_2 <- glm(price ~ grade + living_in_m2 + quartile_zone + bedrooms + real_bathrooms + single_floor, data = data)

summary(model_2)

# Trzeci model to będzie model pełny
model_3 <- glm(price ~ . , data = data)

summary(model_3)

# Czwarty model to będzie model pełny bez zmiennej bedrooms
model_4 <- glm(price ~ .-bedrooms , data = data)

summary(model_4)





#  ----------------------- WALIDACJA KRZYŻOWA -----------------------





# Robimy walidację krzyżową żeby sprawdzić jakie błędy zwracają nasze modele

models <- list(
  model_1,
  model_2, 
  model_3, 
  model_4
)


cv.error.10 <- numeric(length(models))

for (i in seq_along(models)) {
  glm.fit <- glm(models[[i]], data = data)  
  cv.error.10[i] <- cv.glm(data, glm.fit, K = 10)$delta[1]  
}

# Liczymy błędy RMSE
rmse.errors <- sqrt(cv.error.10)
Model_1=rmse.errors[1]
Model_2=rmse.errors[2]
Model_3=rmse.errors[3]
Model_4=rmse.errors[4]
# Przekształcenie wyników do ramki danych
cv_data <- data.frame(
  Model = paste0("Model_", seq_along(models)),
  CV_Error = cv.error.10,
  RMSE = rmse.errors
)

# Wizualizacja błędów z wyświetlaniem wartości
ggplot(cv_data, aes(x = Model, y = CV_Error)) +
  geom_line(aes(group = 1), color = "steelblue", linewidth = 1) +  
  geom_point(color = "darkred", size = 3) +                  
  geom_text(aes(label = round(RMSE, 2)), vjust = -0.5, size = 4) +  
  labs(
    title = "Błąd walidacji krzyżowej 10-krotnej dla różnych modeli",
    x = "Model",
    y = "RMSE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

print(cv_data)





#  ----------------------- BOOTSTRAP -----------------------







### Teraz wykorzystamy bootstrap do estymacji odchylenia standardowego współczynników w naszym modelu.

boot.fn <- function(data, index)
coef(glm(price ~ .-bedrooms , data = data, subset = index))

len = length(data$price) # wartość opisująca ilość obserwacji
boot.fn(data, 1:len)  # Estymacja współczynników na podstawie pełnego zbioru danych

# Dla przykładu bootstrapowa wersja współczynników:

boot.fn(data, sample(len, len, replace = T))[[1]] # jednokrotny bootstrap dla współczynnika intercept

# Estymacja odchyleń standardowych metodą bootstrap:

boot = boot(data, boot.fn, 1000)

plot(boot)

summary(glm(price ~ .-bedrooms , data = data))$coef

boot

# Sprawdzenie czy odchylenia standardowe uzyskane metodą bootstrap są porównywalne do tych uzyskanych funkcją glm

# Wyniki GLM
glm_results <- summary(glm(price ~ . - bedrooms, data = data))$coef
glm_se <- glm_results[, "Std. Error"]

# Wyniki boostrapu
bootstrap_se <- apply(boot$t, 2, sd)

# Ramka danych do ggplota
comparison_data <- data.frame(
  Variable = rownames(glm_results),
  GLM_SE = glm_se,
  Bootstrap_SE = bootstrap_se
)

comparison_long <- pivot_longer(
  comparison_data,
  cols = c(GLM_SE, Bootstrap_SE),
  names_to = "Source",
  values_to = "Std_Error"
)

# Wykres porównujący błędy standardowe Bootstrapu i GLM
ggplot(comparison_long, aes(x = Variable, y = Std_Error, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    fill = NULL, 
    title = "Porównanie błędów standardowych (GLM vs Bootstrap)",
    x = "Zmienna",
    y = "Błąd standardowy"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )





#  ----------------------- WYBÓR ZMIENNYCH NIEZALEŻNYCH -----------------------






# Best Subset Selection dla problemu estymacji zmiennej price
regfit.full <- regsubsets(price ~ ., data = data, nvmax = 16) 
reg.summary <- summary(regfit.full)
reg.summary

# Narysowanie wykresów dla wszystkich miar ułatwi wybór najlepszego modelu
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Liczba zmiennych",
     ylab = "RSS", type = "l")
which.min(reg.summary$rss)
points(16, reg.summary$rss[16], col = "red", cex = 2, pch = 20)


plot(reg.summary$adjr2, xlab = "Liczba zmiennych",
     ylab = "Adjusted RSq", type = "l")
which.max(reg.summary$adjr2)
points(16, reg.summary$adjr2[16], col = "red", cex = 2, pch = 20)


plot(reg.summary$cp, xlab = "Liczba zmiennych", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(16, reg.summary$cp[16], col = "red", cex = 2, pch = 20)


plot(reg.summary$bic, xlab = "Liczba zmiennych",
     ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(15, reg.summary$bic[15], col = "red", cex = 2, pch = 20)

# Każda z metod wskazuje że najlepszy to model z 15 lub 16 zmiennymi 

par(mfrow = c(1, 1))
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

coef(regfit.full, 16) # współczynniki dokładnie takie same jak w modelu m_3 czyli RMSE będzie takie samo



# Selekcja krokowa

regfit.fwd <- regsubsets(price ~ ., data = data,
                         nvmax = 16, method = "forward")
summary(regfit.fwd)

regfit.bwd <- regsubsets(price ~ ., data = data,
                         nvmax = 16, method = "backward")
summary(regfit.bwd)

coef(regfit.full, 15)
coef(regfit.fwd, 15)
coef(regfit.bwd, 15)

coef(regfit.full, 16)
coef(regfit.fwd, 16)
coef(regfit.bwd, 16)

# widzimy, że (odpowiednio dla 15 i 16) każda z metod (full, fwd, bwd) wzieła dokładnie te same zmienne do modelu oraz, że daje dokładnie te same współczynniki.




# Alternatywnie spróbujemy znaleźć najlepszy model metodą walidacji krzyżowej

# Robimy 10-krotną walidację krzyżową
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[ , xvars] %*% coefi
}

k <- 10
n <- nrow(data)

folds <- sample(rep(1:k, length = n))

nvmax <- 16 
cv.errors <- matrix(NA, k, nvmax, dimnames = list(NULL, paste(1:nvmax)))

# Pętla k-krotnej walidacji
for(j in 1:k) {
  best.fit <- regsubsets(price ~ ., data = data[folds != j, ], nvmax = nvmax)
  
  for(i in 1:nvmax) {
    pred <- predict(best.fit, data[folds == j, ], id = i)
    cv.errors[j, i] <- mean((data$price[folds == j] - pred)^2)
  }
}
cv.errors

# Obliczenie średniego błędu CV dla każdego modelu
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

# Wykres średnich błędów
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b", xlab = "Liczba zmiennych", ylab = "Średni błąd walidacyjny MSE", 
     main = "Błąd walidacyjny k-krotnej walidacji")

# Znalezienie najlepszego modelu
best.model <- which.min(mean.cv.errors)
best.model

# Dopasowanie najlepszego modelu na całym zbiorze danych
reg.best <- regsubsets(price ~ ., data = data, nvmax = nvmax)
coef(reg.best, best.model)

# Dla 10-krotnej walidacji krzyżowej najlepszy wyszedł model z 16 zmiennymi
mean.cv.errors[16]
Model_5=sqrt(mean.cv.errors[16]) # błąd policzony z 10-krotnej walidacji krzyżowej na całym zbiorze (model pełny)
Model_5




#  ----------------------- REGRESJA GRZBIETOWA I REGRESJA LASSO -----------------------



# Przygotowanie macierzy x i wektora y
x_all <- model.matrix(price ~ ., data = data)[, -1]
y_all <- data$price


# Ridge regression
cv.ridge <- cv.glmnet(x_all, y_all, alpha = 0)
plot(cv.ridge)

# Wybór najlepszej lambdy
best_lambda_ridge <- cv.ridge$lambda.min
best_lambda_ridge

# MSE dla najlepszej lambdy:
mse_cv_ridge = min(cv.ridge$cvm) 
mse_cv_ridge
Model_6=sqrt(mse_cv_ridge)
Model_6


# Lasso Regression


# Szukamy zatem lambdy za pomocą CV:
cv.lasso <- cv.glmnet(x_all, y_all, alpha = 1)   
plot(cv.lasso)
best_lambda_lasso <- cv.lasso$lambda.min
best_lambda_lasso

# Liczymy MSE dla najlepszej lambdy:
mse_cv_lasso=min(cv.lasso$cvm)
mse_cv_lasso
sqrt(mse_cv_lasso)

# Uwaga! może warto rozważyć model z 13 zmiennymi, bo MSE jest niewiele mniejszy a model się upraszcza

gorsza_lambda <- cv.lasso$lambda.1se
gorsza_lambda
cv.lasso$cvm
cv.lasso$cvm[42]

# Liczymy MSE dla gorszej lambdy:
mse_cv_lasso2=min(cv.lasso$cvm[42])
mse_cv_lasso2
Model_7=sqrt(mse_cv_lasso2)
Model_7

out <- glmnet(x_all, y_all, alpha = 1)
lasso.coef <- predict(out, type = "coefficients", s = gorsza_lambda)[1:17, ]
lasso.coef

# Widzimy że współczynniki przy zmiennych bedrooms, real_bathrooms i grade2 się wyzerowały






#  ----------------------- REDUKCJA WYMIARÓW -----------------------




# PCA Regresja głównych składowych

pcr.fit <- pcr(price ~., data = data, scale = TRUE, validation = "CV")
summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP") # małe błędy dla liczby zmiennych 14,15,16

RMSEP(pcr.fit)$val

Errors = NULL
for(i in seq(1, 34, by = 2)){
  Errors[i] = RMSEP(pcr.fit)$val[i]
  
}
Errors=na.omit(Errors)
Errors
Model_8=Errors[15] # indeks 15 ale to oznacza 14 zmiennych w modelu bo jeszcze wchodzi Intercept
Model_8




# PLS metoda cząstkowych najmniejszych kwadratów

pls.fit <- plsr(price ~ ., data = data, scale = TRUE , validation = "CV")
summary(pls.fit)

validationplot(pls.fit, val.type = "MSEP")

RMSEP(pls.fit)$val

Errors2 = NULL
for(i in seq(1, 34, by = 2)){
  Errors2[i] = RMSEP(pls.fit)$val[i]
  
}
Errors2=na.omit(Errors2)
Errors2
Model_9=Errors2[5]  # 4 zmienne w modelu 
Model_9





#  ----------------------- DRZEWA REGRESYJNE -----------------------



tree.data <- tree(price ~ ., data)
summary(tree.data)

# Tworzymy duże drzewo pozwalamy mu się nawet przeuczyć
control <- tree.control(nobs = nrow(data), mindev = 0.00001)
large_tree <- tree(price ~ ., data, control = control)
summary(large_tree)
plot(large_tree)
text(large_tree, pretty = 0)

# Za pomocą CV chcemy znaleźć optymalny rozmiar drzewa
cv.large_tree <- cv.tree(large_tree, K = 10)
cv.large_tree
which.min(cv.large_tree$dev)
optimal_size <- cv.large_tree$size[which.min(cv.large_tree$dev)]
optimal_size # wychodzi że trzeba wziąć maksymalne drzewo mimo tego że ma 1255 liści
plot(cv.large_tree$size, cv.large_tree$dev, type = "b")

prune_large_tree <- prune.tree(large_tree, best = optimal_size)
plot(prune_large_tree)
text(prune_large_tree, pretty = 0)

pred <- predict(prune_large_tree, newdata = data)
mse <- mean((data$price - pred)^2)
rmse <- sqrt(mse)
rmse



# Robimy mniejsze drzewo, nie przeuczamy go tak bardzo

control2 <- tree.control(nobs = nrow(data), mindev = 0.001)
large_tree2 <- tree(price ~ ., data, control = control2)
summary(large_tree2)
plot(large_tree2)
text(large_tree2, pretty = 0)

# Za pomocą CV chcemy znaleźć optymalny rozmiar drzewa
cv.large_tree2 <- cv.tree(large_tree2, K = 10)
cv.large_tree2
which.min(cv.large_tree2$dev)
optimal_size2 <- cv.large_tree2$size[which.min(cv.large_tree2$dev)]
optimal_size2 # wychodzi że trzeba wziąć maksymalne drzewo mimo tego że ma 1255 liści
plot(cv.large_tree2$size, cv.large_tree2$dev, type = "b")

# Tutaj kod na przyciananie ale u nas nic nie przytnie
prune_large_tree2 <- prune.tree(large_tree2, best = optimal_size2)
plot(prune_large_tree2)
text(prune_large_tree2, pretty = 0)

pred <- predict(prune_large_tree2, newdata = data)
mse <- mean((data$price - pred)^2)
Model_10=sqrt(mse)
Model_10





#  ----------------------- BAGGING -----------------------



# Trochę się liczy
bag.data <- randomForest(price ~ ., data = data, mtry = 11, importance = TRUE)
Model_11=mean(sqrt(bag.data$mse))
Model_11





#  ----------------------- RANDOM FOREST -----------------------




# Biorę 3 zmienne, sprawdzane było jeszcze dla 4 ale błąd był wyższy
rf.data <- randomForest(price ~ ., data = data, mtry = 3, importance = TRUE)

rf.data

Model_12=mean(sqrt(rf.data$mse))
Model_12

importance(rf.data)
varImpPlot(rf.data)







#  ----------------------- BOOSTING -----------------------





# Sprawdzamy różne modele z różnym parametrem uczenia Shrinkage

boost.data <- gbm(price ~ ., data = data,
                    distribution = "gaussian", n.trees = 5000,
                    interaction.depth = 4, cv.folds=10)

summary(boost.data)

mean(sqrt(boost.data$cv.error))



boost2.data <- gbm(price ~ ., data = data,
                  distribution = "gaussian", n.trees = 5000,
                  interaction.depth = 4, shrinkage = 0.2, cv.folds=10)

summary(boost2.data)

mean(sqrt(boost2.data$cv.error))



boost3.data <- gbm(price ~ ., data = data,
                   distribution = "gaussian", n.trees = 5000,
                   interaction.depth = 4, shrinkage = 0.3, cv.folds=10)

summary(boost3.data)

mean(sqrt(boost3.data$cv.error))


boost4.data <- gbm(price ~ ., data = data,
                   distribution = "gaussian", n.trees = 5000,
                   interaction.depth = 4, shrinkage = 0.05, cv.folds=10)

summary(boost4.data)

Model_13=mean(sqrt(boost4.data$cv.error)) #dla tego najlepszy wychodzi
Model_13


boost5.data <- gbm(price ~ ., data = data,
                   distribution = "gaussian", n.trees = 5000,
                   interaction.depth = 4, shrinkage = 0.01, cv.folds=10)

summary(boost5.data)

mean(sqrt(boost5.data$cv.error))






#  ----------------------- BART -----------------------





# Przygotowujemy dane do BART

# Musimy zamienić zmienne jakościowe na dummy
grade_dummy <- model.matrix(~ grade - 1, data)
quartile_zone_dummy <- model.matrix(~ quartile_zone - 1, data)

# Usuwamy oryginalne zmienne 'grade' i 'quartile_zone' z danych
data <- data[, !(names(data) %in% c("grade", "quartile_zone"))]

# Łączymy nowe zmienne dummy z oryginalnymi danymi
data <- cbind(data, grade_dummy, quartile_zone_dummy)

# Wyodrębniamy zmienne numeryczne oprócz zmiennej objaśnianej (price)
numeric_cols <- sapply(data, is.numeric)
numeric_cols <- setdiff(names(data)[numeric_cols], "price")

# Identyfikujemy zmienne binarne (0/1), które nie wymagają skalowania
binary_cols <- names(data)[sapply(data, function(col) all(col %in% c(0, 1)))]

# Tworzymy listę zmiennych do skalowania (wszystkie numeryczne oprócz binarnych)
cols_to_scale <- setdiff(numeric_cols, binary_cols)

# Skalujemy wybrane zmienne
data[cols_to_scale] <- scale(data[cols_to_scale])

str(data)



# Teraz możemy już przejść do algorytmu

x <- (data[, 2:18])
y <- data[, "price"]


# Prosta funkcja do obliczenia RMSE
compute_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Piszemy ręcznie 10-krotną walidację krzyżową
k <- 10
folds <- createFolds(y, k = k, list = TRUE)

rmse_list <- numeric(k)

# Trochę się liczy
for (i in seq_along(folds)) {
  test_idx <- folds[[i]]
  train_idx <- setdiff(seq_along(y), test_idx)
  
  x_train <- x[train_idx, , drop = FALSE]
  y_train <- y[train_idx]
  x_test <- x[test_idx, , drop = FALSE]
  y_test <- y[test_idx]
  
  bart_model <- bart(x_train, y_train, x.test = x_test)
  
  y_pred <- bart_model$yhat.test.mean
  
  rmse_list[i] <- compute_rmse(y_test, y_pred)
}

mean_rmse <- mean(rmse_list, na.rm = TRUE)
Model_14=mean_rmse
Model_14






#  ----------------------- XGBOOST -----------------------




# Używamy danych, które przekształciliśmy w metodzie BART!

# Przygotowujemy dane do użycia funcji xgboost
x <- data.matrix(data[, -1])  
y <- data[, 1]

xgb_train <- xgb.DMatrix(data = x, label = y)


params <- list(
  objective = "reg:squarederror",  
  max_depth = 4,                  
  eta = 0.01                       
)

cv_results <- xgb.cv(
  params = params,
  data = xgb_train,
  nfold = 10,                
  nrounds = 1000,              
  metrics = "rmse",           
  verbose = TRUE,             
)

cv_results

Model_15=min(cv_results$evaluation_log$test_rmse_mean)
Model_15



# Wykres wyników RMSE dla danych treningowych i testowych

# Przygotowanie danych do wykresu
cv_results_df <- data.frame(
  Iteration = 1:nrow(cv_results$evaluation_log),
  Train_RMSE = cv_results$evaluation_log$train_rmse_mean,
  Test_RMSE = cv_results$evaluation_log$test_rmse_mean
)
ggplot(cv_results_df, aes(x = Iteration)) +
  geom_line(aes(y = Train_RMSE, color = "Train RMSE"), size = 1) +
  geom_line(aes(y = Test_RMSE, color = "Test RMSE"), size = 1) +
  labs(
    title = "Wyniki RMSE dla treningu i testu w walidacji krzyżowej",
    x = "Iteracja",
    y = "RMSE",
    color = "Zbiór danych"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )








#  ----------------------- PORÓWNANIE MODELI -----------------------


wyniki <- data.frame(
  Model = paste0("Model_", 1:15),  
  RMSE = c(Model_1, Model_2, Model_3, Model_4, Model_5, Model_6, Model_7, Model_8, Model_9, Model_10, 
            Model_11, Model_12, Model_13, Model_14, Model_15),  
  Metoda = c("GLM", "GLM", "GLM", "GLM", "Selekcja Krokowa",
              "Regresja Grzbietowa", "Regresja Lasso", "PCR", "PLS", "Drzewo Regresyjne",
              "Bagging", "Random Forest", "Boosting", "BART", "XGBoost")  
)

# Wyświetlenie tabeli 
wyniki

kable(wyniki, caption = "Porównanie modeli") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(2, color = "blue", bold = TRUE)  


