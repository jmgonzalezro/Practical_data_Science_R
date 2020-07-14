library("ggplot2")
summary(iris)

ggplot(iris, aes(x = Petal.Length, y = Petal.Width,
                 shape = Species, color = Species)) +
    geom_point(size = 2) +
    ggtitle("Petal dimensiones by iris species: All measurements")

columnas_1 <- c("Petal.Length", "Petal.Width", "Species")
filas_1 <- iris$Petal.Length > 2

head(iris)

iris_base <- iris[filas_1, columnas_1, drop = FALSE]
head(iris_base)


# Selección de columnas en data.table
library("data.table")
iris_data.table <- as.data.table(iris)

columnas_1 <- c("Petal.Length", "Petal.Width", "Species")
filas_1 <- iris$Petal.Length > 2

iris_data.table <- iris_data.table[filas_1, ..columnas_1]
# estos dos puntos le dicen a data.table que es una variable
# no el nombre de una columna
head(iris_data.table)


# Selección de columnas con dplyr
library(tidyverse)
iris_dplyr <- iris %>% 
  select(.,
         Petal.Length, Petal.Width, Species) %>% 
  filter(.,
         Petal.Length > 2)

head(iris_dplyr)

# Missing Data ----

data(msleep)
str(msleep)
summary(msleep)

clean_base_1 <- msleep[complete.cases(msleep), , drop = FALSE]
summary(msleep)
nrow(clean_base_1)

clean_base_2 = na.omit(msleep)
nrow(clean_base_2)
# tienen las mismas rows, de ahí que na.omit lo haga del tirón

# data.table solution
msleep_data.table <- as.data.table(msleep)
clean_data.table = msleep_data.table[complete.cases(msleep_data.table)]
nrow(clean_data.table) # 20

# Dplyr solution
clean_dplyr <- msleep %>% 
  filter(., complete.cases(.))
nrow(clean_dplyr) # 20


# Ordenando Columnas ----
purchases <- wrapr::build_frame(
  "day", "hour", "n_purchase" |
    1 , 9 , 5 |
    2 , 9 , 3 |
    2 , 11 , 5 |
    1 , 13 , 1 |
    2 , 13 , 3 |
    1 , 14 , 1 )
View(purchases)

# Reordenar las columnas

# Base
order_index <- with(purchases, order(day, hour))
purchases_ordered <- purchases[order_index, , drop = FALSE]
purchases_ordered$running_total <- cumsum(purchases_ordered$n_purchase)

purchases_ordered

# Data table solution
DT_purchases <- as.data.table(purchases)
order_cols <- c("day", "hour")
setorderv(DT_purchases, order_cols)

DT_purchases[ , running_total := cumsum(n_purchase)]

print(DT_purchases)


# Dplyr solution

# arrange to order
# mutate to add new column
res <- purchases %>% 
  arrange(., day, hour) %>% 
  mutate(., running_total = cumsum(n_purchase))

print(res)


# Advanced use of ordering
#Dplyr solution
res <- purchases %>% 
  arrange(., day, hour) %>% 
  group_by(., day) %>% 
  mutate(., running_total = cumsum(n_purchase)) %>% 
  ungroup(.)

print(res)
# aquí el ungroup() es importante cuando has hecho operaciones
# con grupos previos.

# Añadir nuevas columnas ----
# Dplyr solution
library("datasets")
library(tidyverse)
library(lubridate)
library(wrapr)
summary(airquality)
airquality_with_date <- airquality

# create a function to make the date string
datestr = function(day, month, year) {
  paste(day, month, year, sep = "-")
}

# tenemos que convertir la fecha no estandarizada en una nueva, más
# útil columna para los queries y el plotting

airquality_with_date2 <- airquality %>%
  mutate(., date = dmy(datestr(Day, Month, 1973))) %>%
  select(., Ozone, date)

head(airquality_with_date2)

# Missing data ----
library("zoo")
airquality_with_date2 %>% 
  mutate(., ozoneCorrected = na.locf(Ozone, na.rm = FALSE)) %>% 
  summary(.)

# na.locf -> lo que hace es rellenar los missing values
# Nos está mostrando un ozone sin corregir con 37 NA y un
# ozonecorrected sin NA

# Aggregating transforms
# Combinando multiples filas en filas summary

iris_summary <- iris %>% 
  group_by(., Species) %>% 
  summarize(.,
            Petal.Length = mean(Petal.Length),
            Petal.Width = mean(Petal.Width)) %>% 
  ungroup(.)
print(iris_summary)

# también podemos añadir las columnas en la tabla sin ahcer join
iris_dplyr <- iris %>% 
  group_by(., Species) %>% 
  mutate(.,
         mean_Petal.Lenght = mean(Petal.Lenght),
         mean_Petal.Width = mean(Petal.Width)) %>% 
  ungroup(.)
head(iris_dplyr)


# Transformaciones multitabla de datos
# Combinación de dos o más data frames ordenados rápidamente
# en base se haría con rbind

productTable <- wrapr::build_frame(
  "productID", "price" |
    "p1" , 9.99 |
    "p2" , 16.29 |
    "p3" , 19.99 |
    "p4" , 5.49 |
    "p5" , 24.49 )
salesTable <- wrapr::build_frame(
  "productID", "sold_store", "sold_online" |
    "p1" , 6 , 64 |
    "p2" , 31 , 1 |
    "p3" , 30 , 23 |
    "p4" , 31 , 67 |
    "p5" , 43 , 51 )
productTable2 <- wrapr::build_frame(
  "productID", "price" |
    "n1" , 25.49 |
    "n2" , 33.99 |
    "n3" , 17.99 )

productTable$productID <- factor(productTable$productID)
productTable2$productID <- factor(productTable2$productID)

# -> dplyer
bind_rows(list(productTable,
               productTable2))

# Separar tablas
# dplyr no tiene su propia implementación, pero se pued ehacer con groupby

productTable_marked <- productTable
productTable_marked$table <- "productTable"
productTable2_marked <- productTable2
productTable2_marked$table <- "productTable2"

rbind_base <- rbind(productTable_marked,
                    productTable2_marked)

rbind_base %>% 
  group_by(., table) %>% 
  mutate(., max_price = max(price)) %>% 
  ungroup(.)


# Añadiendo columnas
cbind(productTable, salesTable[, -1])
# solución de dplyr
dplyr::bind_cols(list(productTable, salesTable[, -1]))

# Métodos principales de unión de tablas ----

# LEFT JOIN

productTable <- wrapr::build_frame(
  "productID", "price" |
    "p1" , 9.99 |
    "p3" , 19.99 |
    "p4" , 5.49 |
    "p5" , 24.49 )
salesTable <- wrapr::build_frame(
  "productID", "unitsSold" |
    "p1" , 10 |
    "p2" , 43 |
    "p3" , 55 |
    "p4" , 8 )

left_join(productTable, salesTable, by = "productID")
# right join sería con los argumentos al revés

# INNER JOIN
# deja solo los datos que existan en las dos tablas.
inner_join(productTable, salesTable, by = "productID")

# FULL JOIN
# mantiene todos los datos de ambas tablas
full_join(productTable, salesTable, by = "productID")


# Reshaping aka pivoting ----
# Combina el summary del data y la transformación de tamaño.

# Mover los datos de ancho a alto
library("xts")
dates <- index(as.xts(time(Seatbelts)))
Seatbetls <- data.frame(Seatbelts)
Seatbelts$date <- dates
Seatbelts <- Seatbelts[ (Seatbelts$date >= as.yearmon("Jan 1982")) &
                          (Seatbelts$date <= as.yearmon("Dec 1983")),
                        , drop = FALSE]

# por hacer






# Modeling Methods ----















