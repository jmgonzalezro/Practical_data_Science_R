setwd("/home/jmgonzalezro/Documentos/PDSwR2-master/PUMS")

psub <- readRDS("psub.RDS")
set.seed(3454351)
gp <- runif(nrow(psub))

dtrain <- subset(psub, gp >= 0.5)
dtest <- subset(psub, gp < 0.5) # aquí hacemos un split 50/50

model <- lm(log10(PINCP) ~ AGEP + SEX + COW + SCHL, data = dtrain) # modelo linear inicial a mano
dtest$predLogPINCP <- predict(model, newdata = dtest)
dtrain$predlogPINCP <- predict(model, newdata = dtrain)

library('ggplot2')
ggplot(data = dtest, aes(x = predLogPINCP, y = log10(PINCP))) +
  geom_point(alpha = 0.2, color = "darkgray") +
  geom_smooth(color = "darkblue") +
  geom_line(aes(x = log10(PINCP),
                y = log10(PINCP)),
            color = "blue", linetype = 2) +
  coord_cartesian(xlim = c(4, 5.25),
                  ylim = c(3.5, 5.5))

# plotting de residuos como función del income logartimico predecido
ggplot(data = dtest, aes(x = predLogPINCP,
                         y = predLogPINCP - log10(PINCP))) +
  geom_point(alpha = 0.2, color = "darkgray") +
  geom_smooth(color = "darkblue") +
  ylab("residual error (prediction - actual)")

# R² y RMSE
rsq <- function(y, f) {
  
  1 - sum((y - f)^2)/sum((y - mean(y))^2)
  
}

rsq(log10(dtrain$PINCP), dtrain$predLogPINCP)
rsq(log10(dtest$PINCP), dtest$predLogPINCP)

# root mean square error
rmse <- function(y, f) {
  sqrt(mean( (y - f)^2 ))
}

rmse(log10(dtrain$PINCP), dtrain$predLogPINCP)
rmse(log10(dtest$PINCP), dtest$predLogPINCP)

#######################
# Regresión
#       Logística 
######################

setwd("/home/jmgonzalezro/Documentos/PDSwR2-master/CDC/")
load("NatalRiskData.rData")
train <- sdata[sdata$ORIGRANDGROUP <= 5,]
test <- sdata[sdata$ORIGRANDGROUP > 5,]

# Variable dependiente es atRisk

complications <- c("ULD_MECO", "ULD_PRECIP", "ULD_BREECH")
riskfactors <- c("URF_DIAB", "URF_CHYPER", "URF_PHYPER", "URF_ECLAM")
y <- "atRisk"
x <- c("PWGT", "UPREVIS", "CIG_REC", "GESTREC3", "DPLURAL", complications, riskfactors)
library(wrapr)
fmla <- mk_formula(y, x)

print(fmla)
model <- glm(fmla, data = train, family = binomial(link = "logit"))

# aplicamos la regresión logística al modelo
train$pred <- predict(model, newdata = train, type = "response") # response le dice que tiene que
# devolver las probabilidades predichas sobre y, sino, devolverá logit(y)
test$pred <- predict(model, newdata = test, type = "response")

sum(train$atRisk == TRUE) # cuenta el número de bebes en riesgo
sum(train$pred) # suma las probabilidades predichas sobre el train

premature <- subset(train, GESTREC3 == "< 37 weeks")
sum(premature$atRisk == TRUE)
sum(premature$pred)

# Plotting
library("WVPlots")
DoubleDensityPlot(train, "pred", "atRisk",
                  title = "Distribution of natality risk scores") # nos quedan sobrepuestos

# Podemos ver como hay pérdidas y ganancias cambiando el treshold
library(ggplot2)
plt <- PRTPlot(train, "pred", "atRisk", TRUE,
               plotvars = c("enrichment", "recall"),
               thresholdrange = c(0, 0.05),
               title = "Enrichment / recall vs. threshold for natality model")
plt + geom_vline(xintercept = 0.02, color = "red", linteype = 2)

# Evaluando el modelo elegido
( ctab.test <- table(pred = test$pred > 0.02, atRisk = test$atRisk) ) # construye la confusion matrix

( precision <- ctab.test[2,2] / sum(ctab.test[2,]) )
( recall <- ctab.test[2,2] / sum(ctab.test[,2]))
( enrichment <- precision / mean(as.numeric(test$atRisk)) )

summary(model)

# model call
glm(formula = fmla, family = binomial(link = "logit"), data = train)

# pseudo R-squared




























