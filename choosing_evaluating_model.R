setwd("/home/jmgonzalezro/Documentos/PDSwR2-master/Spambase/")

spamD <- read.table('spamD.tsv',header=T,sep='\t')
spamTrain <- subset(spamD,spamD$rgroup >= 10)

spamTest <- subset(spamD,spamD$rgroup < 10)
spamVars <- setdiff(colnames(spamD), list('rgroup','spam'))

spamFormula <- as.formula(paste('spam == "spam"',
                                paste(spamVars, collapse = ' + '),sep = ' ~ '))
spamModel <- glm(spamFormula,family = binomial(link = 'logit'),
                 data = spamTrain)
spamTrain$pred <- predict(spamModel,newdata = spamTrain,
                          type = 'response')
spamTest$pred <- predict(spamModel,newdata = spamTest,
                         type = 'response')


# spam classifications
sample <- spamTest[c(7,35,224,327), c("spam", "pred")]
print(sample)

# Confussion matrix
confmat_spam <- table(truth = spamTest$spam,
                      prediction = ifelse(spamTest$pred > 0.5,
                                          "spam", "non-spam"))
print(confmat_spam)


# Entering the akismet confusion matrix
confmat_akismet <- as.table(matrix(data=c(288-1,17,1,13882-17),nrow=2,ncol=2))
rownames(confmat_akismet) <- rownames(confmat_spam)
colnames(confmat_akismet) <- colnames(confmat_spam)
print(confmat_akismet)

# accuracy de 0.9987297
(confmat_akismet[1,1] + confmat_akismet[2,2]) / sum(confmat_akismet)


# Evaluating score models
crickets <- read.csv("/home/jmgonzalezro/Documentos/PDSwR2-master/cricketchirps/crickets.csv")

cricket_model <- lm(temperatureF ~ chirp_rate, data = crickets)
crickets$temp_pread <- predict(cricket_model, newdata = crickets)

# root mean square error
error_sq <- (crickets$temp_pread - crickets$temperatureF)^2
( RMSE <-  sqrt(mean(error_sq)))
# 3.56, está indicando que el modelo se desvía normalmente unos 3.6 grados
# farenheit de la temperatura actual.

# r² o el coeficiente de determinación
error_sq <- (crickets$temp_pread - crickets$temperatureF)^2
numerator <- sum(error_sq)

delta_sq <- (mean(crickets$temperatureF) - crickets$temperatureF)^2 # calcula el error cuadrado del modelo null
denominator = sum(delta_sq) # calcula la varianza
(R2 <- 1 - numerator/denominator) # calcula el R2


# Ejemplo de LIME
iris <- iris
iris$class <- as.numeric(iris$Species == "setosa")
set.seed(2345)
intrain <- runif(nrow(iris)) < 0.75 # aquí le mete el 75% del data para train

train <- iris[intrain,]
test <- iris[!intrain,]
head(train)

library(xgboost)
source("/home/jmgonzalezro/Documentos/PDSwR2-master/LIME_iris/lime_iris_example.R")
fit_iris_example = function(variable_matrix, labelvec) {
  
  cv = xgb.cv(variable_matrix, label = labelvec,
              params=list(
                objective="binary:logistic"
              ),
              nfold=5,
              nrounds=100,
              print_every_n=10,
              metrics="logloss")
  
  evalframe = as.data.frame(cv$evaluation_log)
  NROUNDS = which.min(evalframe$test_logloss_mean)
  
  model = xgboost(data=variable_matrix, label=labelvec,
                  params=list(
                    objective="binary:logistic"
                  ),
                  nrounds=NROUNDS,
                  verbose=FALSE)
  
  model
}

input <- as.matrix(train[, 1:4]) # aquí está convirtiendo las 4 columnas del data de train
# como una matrix para el objeto input y poderla usar como el fitting
model <- fit_iris_example(input, train$class) # da error al realizar esto

# evualiacion
predictions <- predict(model, newdata = as.matrix(train[, 1:4]))  
teframe <- data.frame(isSetosa = ifelse(test$class == 1,
                                        "setosa",
                                        "not setosa"),
                      pred = ifelse(predictions > 0.5,
                                    "setosa",
                                    "not setosa"))
with(teframe, table(truth = isSetosa, pred = pred))

# Esto se pued ehacer más fácil con el paquete LIME
library(lime)
explainer <- lime(train[, 1:4], #creamos un explainer
                  model = model,
                  bin_continuous = TRUE, # las bins serán variables continuas
                  n_bins = 10) # usa 10 bins

# trabajamos con iris
(example <- test[5, 1:4, drop = FALSE])
test$class[5] # este ejemplo es un setosa
round(predict(model, newdata = as.matrix(example))) # el modelo predice una setosa

# lo mejor es usarla con lime::explain porque dplyr tiene una función explain
explanation <- lime::explain(example,
                             explainer,
                             n_labels = 1, # número de etiquetas a epxlicar
                             n_features = 4) # número de features que se usan para fitear

plot_features(explanation) # aquí lo podemos plotear
# este explainer va a predecir el ejemplo de setosa como label 1.

# más ejemplos y explicación del modelo
(example <- test[c(13, 24), 1:4])
test$class[c(13,24)]
round(predict(model, newdata = as.matrix(example)))
explanation <- lime::explain(example,
                             explainer,
                             n_labels = 1,
                             n_features = 4,
                             kernel_width = 0.5)
plot_features(explanation)

# Ejemplo con IMDB

library(zeallot) # esta librería sirve para unpackar la lista en 2 elementos, 1 son las revies
# y otro elemento son las notas, 1 positiva 0 negativa como etiqueta.
c(texts, labels) %<-% readRDS("/home/jmgonzalezro/Documentos/PDSwR2-master/IMDB/IMDBtrain.RDS")
list(text = texts[1], label = labels[1])

library(wrapr)
library(xgboost)
library(text2vec)

create_pruned_vocabulary <- function(texts) {
  it_train <- itoken(texts,
                     preprocessor = tolower,
                     tokenizer = word_tokenizer,
                     ids = names(texts),
                     progressbar = FALSE)
  
  stop_words <- qc(the, a, an, this, that, those, i, you)
  vocab <- create_vocabulary(it_train, stopwords = stop_words)

  pruned_vocab <- prune_vocabulary(
    vocab,
    doc_proportion_max = 0.5,
    doc_proportion_min = 0.001,
    vocab_term_max = 10000
  )
  
  pruned_vocab
}

make_matrix <- function(texts, vocab) {
  iter <- itoken(texts,
                 preprocessor = tolower,
                 tokenizer = word_tokenizer,
                 ids = names(texts),
                 progressbar = FALSE)
  create_dtm(iter, vocab_vectorizer(vocab))
}

fit_imdb_model <- function(dtm_train, labels) {
  NROUNDS <- 371
  
  model <- xgboost(data=dtm_train, label=labels,
                   params=list(
                     objective="binary:logistic"
                   ),
                   nrounds=NROUNDS,
                   verbose=FALSE)
  
  model
}

# Convirtiendo el texto y fitteando el modelo
source("/home/jmgonzalezro/Documentos/PDSwR2-master/IMDB/lime_imdb_example.R")
vocab <- create_pruned_vocabulary(texts) # crea el vocabulario para el training data
dtm_train <- make_matrix(texts, vocab) # crea el documento de la matrz del corpus de texto
model <- fit_imdb_model(dtm_train, labels) #fittea el modelo


# evaluamos el clasificador de reviews
setwd("/home/jmgonzalezro/Documentos/PDSwR2-master/IMDB")
c(test_txt, test_labels) %<-% readRDS("IMDBtrain.RDS")
dtm_test <- make_matrix(test_txt, vocab)
predicted <- predict(model, newdata = dtm_test)
teframe <- data.frame(true_label = test_labels, # hace un datafrem con true y las etiquetas de preddcion
                      pred = predicted)
(cmat <- with(teframe, table(truth = true_label, pred = pred > 0.5))) # hace la matriz de confusion

sum(diag(cmat))/sum(cmat) # hace un accuracy de 0.99248

library(WVPlots)
DoubleDensityPlot(teframe, "pred", "true_label",
                  "Distribution of the test prediction scores")
# hace el plot de doble densidad donde puede verse perfectamente el clasificador

# Para poder hacer explicaciones usamos lime
explainer <- lime(texts, model = model,
                  preprocess = function(x) make_matrix(x, vocab))

# Ahora lo aplicamos a una review
casename <- "test_19552";
sample_case <- test_txt[casename]
pred_prob <- predict(model, make_matrix(sample_case, vocab))
list(text = sample_case,
     label = test_labels[casename],
     prediction = round(pred_prob) )

# explicando el modelo y la predicción
explanation <- lime::explain(sample_case,
                             explainer,
                             n_labels = 1,
                             n_features = 5)
plot_features(explanation)
plot_text_explanations(explanation)

# examinbando dos casos más
casenames <- c("test_12034", "test_10294")
sample_cases <- test_txt[casenames]
pred_probs <- predict(model, make_matrix(sample_cases, vocab))
list(text = sample_cases,
     label = test_labels[casenames],
     prediction = round(pred_probs))














