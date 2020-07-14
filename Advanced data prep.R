library(vroom)
library(vtreat) #preparación del dato
library(wrapr)

setwd("/home/jmgonzalezro/Documentos/PDSwR2-master/KDD2009/")


d <- read.table('orange_small_train.data.gz',
                header = TRUE,
                sep = '\t',
                na.strings = c('NA', ''))

churn <- read.table('orange_small_train_churn.labels.txt',
                    header = FALSE, sep = '\t')
d$churn <- churn$V1
set.seed(729375)
rgroup <- base::sample(c('train', 'calibrate', 'test'),
                       nrow(d),
                       prob = c(0.8, 0.1, 0.1),
                       replace = TRUE)
dTrain <- d[rgroup == 'train', , drop = FALSE]
dCal <- d[rgroup == 'calibrate', , drop = FALSE]
dTrainAll <- d[rgroup %in% c('train', 'calibrate'), , drop = FALSE]
dTest <- d[rgroup == 'test', , drop = FALSE]
outcome <- 'churn'
vars <- setdiff(colnames(dTrainAll), outcome)
rm(list=c('d', 'churn', 'rgroup'))

outcome_summary <- table(
  churn = dTrain[, outcome],
  useNA = 'ifany')
knitr::kable(outcome_summary)

outcome_summary["1"] / sum(outcome_summary)

# Intentando modelar sin preparar ----
library(wrapr)

outcome <- "churn"
vars <- setdiff(colnames(dTrainAll), outcome)

formula1 <- mk_formula("churn", vars, outcome_target = 1)
model1 <- glm(formula1, data = dTrainAll, family = binomial)

model2 <- glm((churn == 1) ~ Var1, data = dTrainAll, family = binomial)
summary(model2)
dim(dTrainAll)


# Basic data preparation ----
library(vtreat)

(parallel_cluster <- parallel::makeCluster(parallel::detectCores()))

treatment_plan <- vtreat::designTreatmentsC(
  dTrain,
  varlist = vars,
  outcomename = "churn",
  outcometarget = 1,
  verbose = FALSE,
  parallelCluster = parallel_cluster
)

dTrain_treated <- prepare(treatment_plan,
                          dTrain,
                          parallelCluster = parallel_cluster)

head(colnames(dTrain))
head(colnames(dTrain_treated))

# designTreatmentsC() devuelve un plan de tratamiento de datos
# La variable score frame tiene la preparación de los datos y un summary y critica inicial
score_frame <- treatment_plan$scoreFrame
t(subset(score_frame, origName %in% c("Var126", "Var189")))

comparison <- data.frame(original218 = dTrain$Var218,
                         impact218 = dTrain_treated$Var218_catB)
head(comparison)

t(subset(score_frame, origName == "Var218"))


# Usando bien "treatment plan" ----
dCal_treated <- prepare(treatment_plan,
                        dCal,
                        parallelCluster = parallel_cluster)

library("sigr")
calcAUC(dTrain_treated$Var200_catB, dTrain_treated$churn)
calcAUC(dCal_treated$Var200_catB, dCal_treated$churn)


# Haciendo crossframes ----
library(vtreat)
parallel_cluster <- parallel::makeCluster(parallel::detectCores())

cross_frame_experiment <- vtreat::mkCrossFrameCExperiment(
  dTrainAll,
  varlist = vars,
  outcomename = "churn",
  outcometarget = 1,
  verbose = FALSE,
  parallelCluster = parallel_cluster
)

dTrainAll_treated <- cross_frame_experiment$crossFrame
treatment_plan <- cross_frame_experiment$treatments
score_frame <- treatment_plan$scoreFrame
dTest_treated <- prepare(treatment_plan,
                         dTest,
                         parallelCluster = parallel_cluster)

library("sigr")
calcAUC(dTrainAll_treated$Var200_catB, dTrainAll_treated$churn)
calcAUC(dTest_treated$Var200_catB, dTest_treated$churn)


# Creación de un modelo ----

# Selección de variables
k <- 1
(significance_cutoff <- k / nrow(score_frame))
score_frame$selected <- score_frame$sig < significance_cutoff
suppressPackageStartupMessages(library("dplyr"))

score_frame %>% 
  group_by(., code, selected) %>% 
  summarize(.,
            count = n()) %>% 
  ungroup(.) %>% 
  cdata::pivot_to_rowrecs(.,
                          columnToTakeKeysFrom = "selected",
                          columnToTakeValuesFrom = "count",
                          rowKeyColumns = "code",
                          sep = "=")


# Creación de un modelo multivariable ----
# Regresión logística

library(wrapr)
newvars <- score_frame$varName[score_frame$selected]

f <- mk_formula("churn", newvars, outcome_target = 1)
model <- glm(f, data = dTrainAll_treated, family = binomial)

library("sigr")
dTest_treated$glm_pred <- predict(model,
                                  newdata = dTest_treated,
                                  type = "response")

calcAUC(dTest_treated$glm_pred, dTest_treated$churn == 1)
permTestAUC(dTest_treated, "glm_pred", "churn", yTarget = 1)

var_aucs <- vapply(newvars,
                   function(vi) {
                     calcAUC(dTrainAll_treated[[vi]], dTrainAll_treated$churn == 1)
                   }, numeric(1))
(best_train_aucs <- var_aucs[var_aucs >= max(var_aucs)])
# Var216_catB 
# 0.5892631 

# Hciendo la regresión logística en un modelo de clasificación
table(prediction = dTest_treated$glm_pred >= 0.5,
      truth = dTest$churn)

WVPlots::DoubleDensityPlot(dTest_treated, "glm_pred", "churn",
                           "glm prediction on test, double density plot")
WVPlots::PRTPlot(dTest_treated, "glm_pred", "churn",
                 "glm prediction on test, enrichment plot",
                 truthTarget = 1,
                 plotvars = c("enrichment", "recall"),
                 thresholdrange = c(0, 1.0))

# Preparando los datos para modelos de regresión

auto_mpg <- readRDS("/home/jmgonzalezro/Documentos/PDSwR2-master/auto_mpg/auto_mpg.RDS")
knitr::kable(head(auto_mpg))

library("wrapr")
vars <- c("cylinders", "displacement", "horsepower", "weight", "acceleration",
          "model_year", "origin")
f <- mk_formula("mpg", vars)
model <- lm(f, data = auto_mpg)

auto_mpg$prediction <- predict(model, newdata = auto_mpg)
str(auto_mpg[!complete.cases(auto_mpg), , drop = FALSE]) # en la columna de prediccion
# podemos ver como hay NA porque no hay predicción, tampoco hay horsepower

# Todo ello porque el dataset tiene missings

library(vtreat)
cfe <- mkCrossFrameNExperiment(auto_mpg, vars, "mpg", verbose = FALSE)
treatment_plan <- cfe$treatments
auto_mpg_treated <- cfe$crossFrame
score_frame <- treatment_plan$scoreFrame
new_vars <- score_frame$varName

newf <- mk_formula("mpg", new_vars)
new_model <- lm(newf, data = auto_mpg_treated)

auto_mpg$prediction <- predict(new_model, newdata = auto_mpg_treated)
str(auto_mpg[!complete.cases(auto_mpg), , drop = FALSE])
# ahora si hay una predicción en la última columna después de haber hecho un 
# treatment plan incluso teniendo missings en horsepower todavía













