# Cargando el  dataset
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(patchwork) 

setwd("C:/Users/adema/OneDrive/Escritorio/FCEA/EstadisticaDescriptiva/Practico/ProyectoEstDesc2025/GitHub/data")
df = read_csv("data.csv")

# Vision general de los datos
str(df)               # Estructura de variables
summary(df)           # Resumen estadístico rápido
colSums(is.na(df))    # Cuenta de NAs por columna

# Clean data
df = df[!(is.na(df$exp_sf) & is.na(df$linea_sf) & is.na(df$deuda_sf)), ] # Eliminar las filas que tienen NA en: exp_sf, linea_sf y deuda_sf
mediana_exp = median(df$exp_sf, na.rm = TRUE) # Calcular la mediana de exp_sf
df$exp_sf_clean = ifelse(is.na(df$exp_sf), mediana_exp, df$exp_sf) # Imputar los valores NA de exp_sf por su mediana en una nueva columna
df$linea_sf_clean = ifelse(is.na(df$linea_sf), 0, df$linea_sf) # Imputar los NA linea_sf por 0
df$deuda_sf_clean = ifelse(is.na(df$deuda_sf), 0, df$deuda_sf) # Imputar los NA deuda_sf por 0

colSums(is.na(df)) # Validar la imputacion de NA en las columnas clean

# Recodificar categorías: educación, vivienda y zona
df$nivel_educ_clean = dplyr::recode(df$nivel_educ,
                                    "SIN EDUCACION" = "Baja o menos",
                                    "SECUNDARIA" = "Baja o menos",
                                    "TECNICA" = "Media",
                                    "UNIVERSITARIA" = "Alta",
                                    .default = "Otra")

df$vivienda_clean = dplyr::recode(df$vivienda,
                                  "PROPIA" = "Propia",
                                  "FAMILIAR" = "No Propia",
                                  "ALQUILADA" = "No Propia",
                                  .default = "Otra")

zonas_frecuentes = names(which(table(df$zona) > 250))  # elegimos zonas con más de 250 casos
df$zona_clean = ifelse(df$zona %in% zonas_frecuentes,
                             df$zona,
                             "OTRAS")

# Comparativa de datos No Clean y Clean
# Transformar datos para comparar visualmente
df_long_exp = df %>%
  select(exp_sf, exp_sf_clean) %>%
  pivot_longer(cols = everything(), names_to = "versión", values_to = "valor")

# Boxplot comparativo
ggplot(df_long_exp, aes(x = versión, y = valor, fill = versión)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Comparación de experiencia financiera (exp_sf)", y = "Meses de experiencia") +
  theme_minimal()

df_long_linea = df %>%
  select(linea_sf, linea_sf_clean) %>%
  pivot_longer(cols = everything(), names_to = "versión", values_to = "valor")

ggplot(df_long_linea, aes(x = versión, y = valor, fill = versión)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Comparación de línea de crédito (linea_sf)", y = "Monto en soles") +
  theme_minimal()

df_long_deuda <- df %>%
  select(deuda_sf, deuda_sf_clean) %>%
  pivot_longer(cols = everything(), names_to = "versión", values_to = "valor")

ggplot(df_long_deuda, aes(x = versión, y = valor, fill = versión)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Comparación de deuda (deuda_sf)", y = "Monto en soles") +
  theme_minimal()

# Etiquetando los outliers
# Función para detectar outliers usando la regla del IQR

detectar_outliers = function(x) {
  Q1 = quantile(x, 0.25, na.rm = TRUE)
  Q3 = quantile(x, 0.75, na.rm = TRUE)
  IQR = Q3 - Q1
  lim_inf = Q1 - 1.5 * IQR
  lim_sup = Q3 + 1.5 * IQR
  return(x < lim_inf | x > lim_sup)
}

# Aplicar la función a cada variable limpia

df$exp_sf_outlier = detectar_outliers(df$exp_sf_clean)
df$linea_sf_outlier = detectar_outliers(df$linea_sf_clean)
df$deuda_sf_outlier = detectar_outliers(df$deuda_sf_clean)

# Ver cantidad de outliers en cada variable
outlier_counts = data.frame(
  Variable = c("exp_sf_clean", "linea_sf_clean", "deuda_sf_clean"),
  Outliers = c(
    sum(df$exp_sf_outlier, na.rm = TRUE),
    sum(df$linea_sf_outlier, na.rm = TRUE),
    sum(df$deuda_sf_outlier, na.rm = TRUE)
  )
)

print(outlier_counts)

# Comparativas entre variables categoricas 

# Función genérica: gráfico individual sin facet
graficar_version = function(df, columna, titulo, color_barras, etiqueta) {
  resumen = df %>%
    count(categoria = .data[[columna]]) %>%
    mutate(categoria = factor(categoria, levels = categoria[order(-n)]))
  
  ggplot(resumen, aes(x = categoria, y = n)) +
    geom_bar(stat = "identity", fill = color_barras) +
    geom_text(aes(label = n), vjust = -0.5, size = 3.5) +
    labs(title = titulo, x = etiqueta, y = "Cantidad") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Gráfico para cada variable

# NIVEL EDUCATIVO
g1 = graficar_version(df, "nivel_educ", "Nivel educativo original", "#F8766D", "Nivel educativo")
g2 = graficar_version(df, "nivel_educ_clean", "Nivel educativo recodificado", "#00BFC4", "Nivel educativo clean")
g1 / g2  # muestra ambos apilados

# VIVIENDA
g3 = graficar_version(df, "vivienda", "Tipo de vivienda original", "#F8766D", "Tipo de vivienda")
g4 = graficar_version(df, "vivienda_clean", "Tipo de vivienda recodificada", "#00BFC4", "Tipo de vivienda clean")
g3 / g4

# ZONA
g5 = graficar_version(df, "zona", "Zona original", "#F8766D", "Zona")
g6 = graficar_version(df, "zona_clean", "Zona recodificada", "#00BFC4", "Zona recodificada")
g5 / g6


# Validando la cantidad de outliers en cada una de las categorias numericas que se limpiaron

# Crear data en formato largo para las columnas *_outlier
df_outliers = df %>%
  select(exp_sf_outlier, linea_sf_outlier, deuda_sf_outlier) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "es_outlier")

# Renombrar variables para que se lean bien en el gráfico
df_outliers$variable = recode(df_outliers$variable,
                              "exp_sf_outlier" = "Experiencia financiera",
                              "linea_sf_outlier" = "Línea de crédito",
                              "deuda_sf_outlier" = "Deuda actual")

# Crear gráfico
ggplot(df_outliers, aes(x = es_outlier, fill = es_outlier)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.3, size = 3.5) +
  facet_wrap(~ variable, scales = "free_x") +
  scale_fill_manual(values = c("TRUE" = "#F8766D", "FALSE" = "#00BFC4")) +
  labs(title = "Cantidad de outliers por variable",
       x = "¿Es outlier?",
       y = "Cantidad de observaciones",
       fill = "Outlier") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5))

# Validación de todo el Cleaning
colSums(is.na(df))  # Suma la cantidad de NA (en las columnas 'clean', debe ser 0)
sapply(df[, grepl("_clean$", names(df))], function(x) sum(is.na(x))) # Verificar consistencia de nuevas variables (debe ser 0)
df$zona_clean = as.factor(df$zona_clean) # Valida que la variable zona es factor
df$nivel_educ_clean = as.factor(df$nivel_educ_clean)  # Valida que la variable de nivel educativo es factor
df$vivienda_clean = as.factor(df$vivienda_clean)  # Valida que la variable tipo de vivienda es factor
unique(df$zona_clean) # Valida que no haya valores extraños
unique(df$nivel_educ_clean) # Valida que no haya valores extraños
unique(df$vivienda_clean) # Valida que no haya valores extraños
table(df$mora) # Valida la distribucion de 'Mora'

# Guardar el dataset limpio en un archivo CSV para que puedan trabajar desde aquí
write.csv(df, file = "C:/Users/adema/OneDrive/Escritorio/FCEA/EstadisticaDescriptiva/Practico/ProyectoEstDesc2025/GitHub/data/data_limpio.csv", 
          row.names = FALSE)
