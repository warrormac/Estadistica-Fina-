install.packages("ggplot2")  # Para visualización
install.packages("dplyr") 
install.packages("lubridate")
install.packages("igraph")
install.packages("caret")
install.packages("lattice")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("recommenderlab")
install.packages("tidyr")

library(ggplot2)
library(dplyr)
library(lubridate)
library(igraph)
library(caret)
library(rpart)
library(rpart.plot)
library(recommenderlab)
library(tidyr)


data <- read.csv("sales.csv", stringsAsFactors = FALSE)
head(data)


# Preprocesamiento de datos
# Convertir columnas a numéricas si es necesario
data$Unit.price <- as.numeric(data$Unit.price)
data$Quantity <- as.numeric(data$Quantity)
data$gross.income <- as.numeric(data$gross.income)


# Convertir la columna de fecha a tipo Date
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")

# Ajustar el modelo de regresión
modelo <- lm(gross.income ~ Unit.price + Quantity, data = data)

# Resumen del modelo
summary(modelo)


# Graficar los resultados de la regresión
ggplot(data, aes(x = Unit.price, y = gross.income)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Relación entre Precio Unitario e Ingreso Bruto",
       x = "Precio Unitario",
       y = "Ingreso Bruto") +
  theme_minimal()

# Gráfico para la cantidad
ggplot(data, aes(x = Quantity, y = gross.income)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Relación entre Cantidad e Ingreso Bruto",
       x = "Cantidad",
       y = "Ingreso Bruto") +
  theme_minimal()


# Adjusted regression model summary
model_summary <- summary(modelo)

# Print the full summary
print(model_summary)

# Print specific values
cat("Regression Coefficients:\n")
print(coef(modelo))

cat("\nR-squared:\n")
print(model_summary$r.squared)

cat("\nP-values:\n")
print(model_summary$coefficients[, 4])





# Serie Teporal

# Agrupar por fecha y sumar el ingreso bruto
daily_income <- data %>%
  group_by(Date) %>%
  summarise(Total_Gross_Income = sum(gross.income, na.rm = TRUE))

# Convertir a un objeto de serie temporal
# Puedes usar ts() o zoo() dependiendo de tus necesidades, aquí usamos ts()
income_ts <- ts(daily_income$Total_Gross_Income, start = c(2019, 1), frequency = 365)


ggplot(daily_income, aes(x = Date, y = Total_Gross_Income)) +
  geom_line(color = "blue") +
  labs(title = "Ingreso Bruto Diario", x = "Fecha", y = "Ingreso Bruto") +
  theme_minimal()

summary(daily_income)







# Generacion de una red
relationships <- data %>%
  select(Customer.type, Product.line) %>%
  distinct()

# Crear un grafo a partir del dataframe de relaciones
g <- graph_from_data_frame(relationships, directed = FALSE)

# Visualizar la red
plot(g, vertex.label = V(g)$name, vertex.color = "lightblue", 
     vertex.size = 30, edge.color = "gray", main = "Red de Clientes y Productos")

# Print basic information about the graph
cat("Number of Nodes:", gorder(g), "\n")  # Number of nodes
cat("Number of Edges:", gsize(g), "\n")  # Number of edges

# Print a summary of the graph
summary(g)

# Print the degree of each node
cat("\nDegrees of Nodes:\n")
print(degree(g))

# Optionally, you can also print a few specific details, like the adjacency list
cat("\nAdjacency List:\n")
print(as_data_frame(g, what = "edges"))






# Preprocesamiento de Datos
str(data)
data <- distinct(data)

# Manejo de valores faltantes
# Por ejemplo, eliminar filas con valores NA en columnas clave
data <- na.omit(data)

data$Date <- as.Date(data$Date, format = "%d/%m/%Y")

data$Unit.price <- as.numeric(data$Unit.price)
data$Quantity <- as.numeric(data$Quantity)
data$Total <- as.numeric(data$Total)

# Crear nuevas variables si es necesario
# Por ejemplo, calcular el ingreso bruto (cogs) si no está presente
data$cogs <- data$Unit.price * data$Quantity

summary(data)



# Clasificacion
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
data$Unit.price <- as.numeric(data$Unit.price)
data$Quantity <- as.numeric(data$Quantity)

# Convertir la variable objetivo a factor
data$Customer.type <- as.factor(data$Customer.type)

# Dividir los datos en conjunto de entrenamiento y prueba
set.seed(123)  # Para reproducibilidad
trainIndex <- createDataPartition(data$Customer.type, p = .8, 
                                  list = FALSE, 
                                  times = 1)
dataTrain <- data[trainIndex, ]
dataTest <- data[-trainIndex, ]

# Crear el modelo de árbol de decisión
modelo_arbol <- rpart(Customer.type ~ Unit.price + Quantity + Total + Rating, 
                      data = dataTrain, 
                      method = "class")

rpart.plot(modelo_arbol)

predicciones <- predict(modelo_arbol, dataTest, type = "class")

confusionMatrix(predicciones, dataTest$Customer.type)





# Sistema de Recomendacion

transactions <- data %>%
  group_by(Invoice.ID, Product.line) %>%
  summarize(n = n()) %>%
  spread(Product.line, n, fill = 0)

# Convertir la matriz a un objeto de clase "realRatingMatrix"
trans_matrix <- as(transactions[, -1], "matrix")  # Quitamos Invoice.ID
rownames(trans_matrix) <- transactions$Invoice.ID
rating_matrix <- as(trans_matrix, "realRatingMatrix")

# Crear un modelo de recomendación usando el método de filtrado colaborativo
rec_model <- Recommender(rating_matrix, method = "UBCF")  # User-Based Collaborative Filtering

# Hacer recomendaciones para todos los usuarios
n_recommendations <- 5  # Número de recomendaciones por usuario
recomendaciones <- predict(rec_model, rating_matrix, n = n_recommendations)

# Convertir las recomendaciones a un formato legible
recomendaciones_list <- as(recomendaciones, "list")

# Imprimir recomendaciones para cada usuario
print(recomendaciones_list)
print(rating_matrix)
summary(data)

transactions <- data %>%
  group_by(Customer.type, Product.line) %>%
  summarize(n = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Product.line, values_from = n, values_fill = 0)


