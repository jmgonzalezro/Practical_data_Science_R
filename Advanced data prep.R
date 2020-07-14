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




# Basic data preparation ----
library(wrapr)
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
model <- glm(f, data = dTrainAll, family = binomial)











