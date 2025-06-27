# Cargando el  dataset
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(patchwork) 

setwd("C:/Users/adema/OneDrive/Escritorio/FCEA/EstadisticaDescriptiva/Practico/ProyectoEstDesc2025/GitHub/Morosidad-con-R/data")
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
# Calcular la mediana de deuda_sf solo para mora == 1 y deuda_sf > 0
mediana_deuda = median(df$deuda_sf[df$mora == 1 & df$deuda_sf > 0], na.rm = TRUE)

# Imputar correctamente todos los casos para deuda_sf_clean
df$deuda_sf_clean = case_when(
  df$mora == 0 ~ 0,
  is.na(df$deuda_sf) & df$mora == 1 ~ mediana_deuda,
  df$deuda_sf == 0 & df$mora == 1 ~ mediana_deuda,
  TRUE ~ df$deuda_sf
)

sum(df$mora == 1 & df$deuda_sf == 0 & df$deuda_sf_clean == 0)  #  debería dar 0


colSums(is.na(df)) # Validar la imputacion de NA en las columnas clean

# Recodificar categorías: educación, vivienda y zona
df$nivel_educ_clean = dplyr::recode(df$nivel_educ,
                                    "SIN EDUCACION" = "Media o menos",
                                    "SECUNDARIA" = "Media o menos",
                                    "TECNICA" = "Media o menos",
                                    "UNIVERSITARIA" = "Alta",
                                    .default = "Otra")

df$vivienda_clean = dplyr::recode(df$vivienda,
                                  "PROPIA" = "Propia",
                                  "FAMILIAR" = "No Propia",
                                  "ALQUILADA" = "No Propia",
                                  .default = "Otra")

zonas_frecuentes = names(which(table(df$zona) > 600))  # elegimos zonas con más de 250 casos
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
write.csv(df, file = "C:/Users/adema/OneDrive/Escritorio/FCEA/EstadisticaDescriptiva/Practico/ProyectoEstDesc2025/data_limpio.csv", 
          row.names = FALSE)
#EXPLORACION DE DATOS
#traigo la data
datos = read.csv("C:/Users/adema/OneDrive/Escritorio/FCEA/EstadisticaDescriptiva/Practico/ProyectoEstDesc2025/data_limpio.csv")
#Extraigo las columas a las que se le pueden hacer estas variables
datos_mdr = subset(datos, select = c(2,4,5,6,7,8,9,10,11,13,15,16,17))
#Calculo la media a cada variable y redondeo
media = apply(na.exclude(datos_mdr), MARGIN = 2, FUN = mean)
media_redondeada <- round(media, digits=1)
#Calculo la mediana a cada variable y redondeo
mediana = apply(na.exclude(datos_mdr), MARGIN = 2, FUN = median)
mediana_redondeada <- round(mediana, digits=1)
#Creo la funcion moda y la calculo a cada variable
moda <- function(x) {
  freq <- table(x)      
  moda_val <- names(freq)[freq == max(freq)]
  return(moda_val)}
apply(datos_mdr, MARGIN = 2, FUN = moda)
#Calculo desvio a cada variable y redondeo
desvio = apply(na.exclude(datos_mdr), MARGIN = 2, FUN = sd)
desvio_redondeado <- round(desvio, digits=1)
#Hago Histogramas para cada uno

num_vars <- datos_mdr[sapply(datos_mdr, is.numeric)]
for (colname in names(num_vars)) {
  hist(num_vars[[colname]], 
       main = paste("Histograma de", colname),
       xlab = colname,
       col = "skyblue", 
       border = "white")
}
#Hago Boxplot para cada unos
for (colname in names(num_vars)) {
  boxplot(num_vars[[colname]], 
          main = paste("Boxplot de", colname),
          xlab = colname)
}
#Tablas de frecuencia de zona y graficos de barras
datos_tdf = subset(datos, select = c(3,12,14,18,19,20,21,22,23))
#Tablas de frecuancia de zona
tabla_zona_abs = table(datos$zona)
tabla_zona_rel = prop.table(tabla_zona_abs)
tabla_zona_pct <- round(tabla_zona_rel * 100, 2)
tabla_frec_zona <- data.frame(
  Categoria = names(tabla_zona_abs),
  Frecuencia = as.vector(tabla_zona_abs),
  Proporcion = round(tabla_zona_rel, 3),
  Porcentaje = tabla_zona_pct)
#Graficos de Barras 
datos_barras = subset(datos_tdf, select = c(1,3,4,5))
#Nivel educativo clean barplot
barplot(table(datos_barras$nivel_educ_clean),
        col = "lightblue",
        main = "Gráfico de Barras de Nivel Educativo",
        xlab = "Nivel educativo",
        ylab = "Frecuencia")
#Vivienda clean
barplot(table(datos_barras$vivienda_clean),
        col = "lightblue",
        main = "Gráfico de Barras de Tipo de Vivienda",
        xlab = "Tipo de Vivienda",
        ylab = "Frecuencia")
# Analisis Bivariado
#Dos variables numericas 
#(Deuda e Ingreso)
ggplot(datos, aes(x = datos$deuda_sf_clean, y = datos$ingreso)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relación entre deuda e ingreso",
       x = "Deuda", y = "Ingreso") +
  theme_minimal()
#(Deuda y Atraso)
ggplot(datos, aes(x = datos$deuda_sf_clean, y = datos$atraso)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relación entre deuda y atraso",
       x = "Deuda", y = "Atraso") +
  theme_minimal()
#BoxPlot (Edad y Atraso)
ggplot(datos, aes(x = datos$edad, y = datos$atraso)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relación entre edad y atraso",
       x = "Edad", y = "Atraso") +
  theme_minimal()
#BoxPlot (zona e ingreso)
ggplot(datos, aes(x = datos$zona_clean, y = datos$ingreso)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Ingreso por zona",
       x = "Zona",
       y = "Ingreso") +
  theme_minimal()
#BoxPlot (Zona y Deuda)
ggplot(datos, aes(x = datos$zona_clean, y = datos$deuda_sf_clean)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Deuda por zona",
       x = "Zona",
       y = "Deuda") +
  theme_minimal()

#Tabla de fecuencia (Tipo de Vivienda y Mora)
vivienda_mora= table(datos$vivienda_clean, datos$mora)
prop.table(vivienda_mora)


casos_mora_cero = df %>%
  filter(mora == 1, deuda_sf_clean == 0)

nrow(casos_mora_cero) 

View(casos_mora_cero)

# Seteo de directorio de trabajo
setwd("C:/Users/adema/OneDrive/Escritorio/FCEA/EstadisticaDescriptiva/Practico/ProyectoEstDesc2025/GitHub/Morosidad-con-R/data")

# Cargar dataset
df <- read_csv("data_limpio.csv")

### 1. Prevalencia de la mora
df_aux <- df %>% 
  mutate(mora = factor(mora, levels = c(0,1), labels = c("No Moroso", "Moroso"))) %>%
  count(mora) %>% 
  mutate(pct = round(n / sum(n) * 100))

ggplot(df_aux, aes(x = "", y = n, fill = mora)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribución de la mora", y = "Frecuencia", x = "", fill = "Mora") +
  scale_fill_manual(values = c("No Moroso" = "#9BCD9B", "Moroso" = "#9c5641")) +
  geom_text(aes(label = paste0(pct, "%")), position = position_stack(vjust = 0.5)) +
  theme_bw()

### 2. Proporción de atrasos severos
df_aux <- df %>%
  filter(atraso > 0) %>%
  mutate(atraso = cut(atraso, breaks = c(0,21,245), labels = c("Moderados/Estándares", "Severos"))) %>%
  count(atraso) %>%
  mutate(pct = round(n/sum(n)*100))

ggplot(df_aux, aes(x = atraso, y = n, fill = atraso)) +
  geom_bar(stat = "identity") +
  labs(title = "Severidad de atrasos", y = "Frecuencia", fill = "Atraso") +
  scale_fill_manual(values = c("Moderados/Estándares" = "#CAFF70", "Severos" = "#9BCD9B")) +
  geom_text(aes(label = paste0(pct, "%")), position = position_stack(vjust = 0.5))

### 3. Mora según nivel de ingreso
df_aux <- df %>%
  mutate(
    mora = factor(mora, levels = c(0,1), labels = c("No Moroso", "Moroso")),
    nivelDeIngreso = cut(ingreso, breaks = c(0, 1360, 4900, 30000),
                         labels = c("Ingresos Bajos", "Ingresos Medios", "Ingresos Altos"))
  ) %>%
  count(mora, nivelDeIngreso)

ggplot(df_aux, aes(x = "", y = n, fill = mora)) +
  facet_wrap(~nivelDeIngreso) +
  geom_bar(stat = "identity") +
  labs(title = "Prevalencia de la mora según nivel de ingreso", y = "Frecuencia", x = "", fill = "Mora") +
  scale_fill_manual(values = c("No Moroso" = "#CAFF70", "Moroso" = "#9BCD9B")) +
  theme_bw()

### 4. Mora según nivel educativo
df_aux <- df %>%
  mutate(mora = factor(mora, levels = c(0,1), labels = c("No Moroso", "Moroso")),
         nivelEducativo = factor(nivel_educ_clean, levels = c("Alta", "Media o menos"),
                                 labels = c("Nivel Educativo: Alto","Nivel Educativo: Medio o bajo")))

ggplot(df_aux, aes(x = nivelEducativo, fill = mora)) +
  geom_bar(position = "fill") +
  labs(title = "Morosidad según Nivel educativo", x = "Nivel Educativo", y = "Frecuencia relativa", fill = "Mora") +
  scale_fill_manual(values = c("No Moroso" = "#CAFF70", "Moroso" = "#9BCD9B"))

### 5. Mora según score y experiencia financiera
df_aux <- df %>%
  mutate(
    exp_sf_clean = cut(exp_sf_clean, breaks = c(0, 7, 50, 300), include.lowest = TRUE,
                       labels = c("Baja", "Media", "Alta")),
    score = cut(score, breaks = 4),
    mora = factor(mora, levels = c(0,1), labels = c("No Moroso", "Moroso"))
  ) %>%
  filter(exp_sf_clean != "Media")

ggplot(df_aux, aes(x = score, fill = mora)) +
  geom_bar() +
  labs(title = "Morosidad según score y experiencia financiera", x = "Score", y = "Frecuencia", fill = "Mora") +
  facet_wrap(~exp_sf_clean) +
  scale_fill_manual(values = c("No Moroso" = "#CAFF70", "Moroso" = "#9BCD9B"))

### 6. Atraso según clasificación crediticia

df_aux <- df %>% filter(atraso > 0)
ggplot(df_aux, aes(x = as.factor(clasif_sbs), y = log(atraso), fill = factor(clasif_sbs))) +
  geom_boxplot() +
  labs(title = "Atraso según clasificación crediticia", x = "Clasificación crediticia", y = "Días de atraso (log)") +
  scale_fill_manual(values = c("lightblue", "lightblue1", "lightblue2", "lightblue3", "lightblue4"))

# ------- Mora según nivel educativo -------
general <- df %>% 
  select(nivelEducativo = nivel_educ_clean, mora) %>% 
  mutate(nivelEducativo = factor("General"), mora = factor(mora, levels = c(0,1), labels = c("No Moroso", "Moroso")))

dist <- df %>% 
  select(nivelEducativo = nivel_educ_clean, mora) %>% 
  mutate(
    nivelEducativo = factor(nivelEducativo, levels = c("Alta", "Media o menos"),
                            labels = c("Nivel Educativo: Alto", "Nivel Educativo: Medio o bajo")),
    mora = factor(mora, levels = c(0,1), labels = c("No Moroso", "Moroso"))
  )

aux <- rbind(general, dist)

print(
  ggplot(aux, aes(x = "", fill = mora)) +
    geom_bar(position = "fill") + coord_polar(theta = "y") +
    facet_wrap(~ nivelEducativo) +
    labs(title = "Morosidad: Nivel Educativo", y = "", fill = "Mora") +
    scale_fill_manual(values = c("No Moroso" = "#CAFF70", "Moroso" = "#9BCD9B"))
)

print(
  ggplot(aux, aes(x = nivelEducativo, fill = mora)) +
    geom_bar(position = "fill") +
    labs(title = "Morosidad según Nivel educativo", x = "", y = "Frecuencia", fill = "Mora") +
    scale_fill_manual(values = c("No Moroso" = "#CAFF70", "Moroso" = "#9BCD9B"))
)

# ------- Mora según ingreso y educación -------
df_aux <- df %>% 
  mutate(
    nivelDeIngreso = cut(ingreso, breaks = c(0, 1360, 4900, 30000), labels = c("Ingresos Bajos", "Ingresos Medios", "Ingresos Altos")),
    mora = factor(mora, levels = c(0,1), labels = c("No Moroso", "Moroso"))
  )

print(
  ggplot(df_aux, aes(x = nivelDeIngreso, fill = mora)) +
    geom_bar() +
    facet_wrap(~ nivel_educ_clean) +
    labs(title = "Mora según nivel de ingreso y nivel educativo", x = "Nivel de ingresos", y = "Cantidad", fill = "Mora") +
    scale_fill_manual(values = c("No Moroso" = "#CAFF70", "Moroso" = "#9BCD9B"))
)

# ------- Mora según score y experiencia -------
df_aux <- df %>%
  mutate(
    exp_sf_clean = cut(exp_sf_clean, breaks = c(0, 7, 50, 300), include.lowest = TRUE, right = FALSE, labels = c("Baja", "Media", "Alta")),
    mora = factor(mora, levels = c(0,1), labels = c("No Moroso", "Moroso")),
    score = cut(score, breaks = 4)
  ) %>%
  filter(exp_sf_clean != "Media")

print(
  ggplot(df_aux, aes(x = score, fill = mora)) +
    geom_bar() +
    facet_wrap(~exp_sf_clean) +
    labs(title = "Morosidad según score y experiencia financiera", x = "Score", y = "Frecuencia", fill = "Mora") +
    scale_fill_manual(values = c("No Moroso" = "#CAFF70", "Moroso" = "#9BCD9B"))
)

# ---------------------------------------------
# Clasificación según deuda
# ---------------------------------------------
df_aux <- df %>% 
  filter(mora == 1) %>% 
  select(deuda_sf_clean, clasif_sbs) %>% 
  mutate(catDeuda = cut(deuda_sf_clean, 
                        breaks = quantile(deuda_sf_clean, probs = c(0, 0.15, 0.85, 1), na.rm = TRUE),
                        labels = c("baja", "media", "alta"),
                        include.lowest = TRUE)) %>% 
  filter(catDeuda != "media") %>% 
  count(catDeuda, clasif_sbs) %>% 
  mutate(n = ifelse(catDeuda == "baja", -n, n))


# Gráfico

ggplot(df_aux, aes(x = clasif_sbs, y = n, fill = catDeuda)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(title = "Clasificación según tipo de deuda", y = "", x = "Clasificación financiera", fill = "Magnitud de deuda") + 
  scale_fill_manual(values = c("yellow3", "green4"))


# ---------------------------------------------
# Clasificación según ingreso
# ---------------------------------------------
df_aux <- df %>% 
  filter(mora == 1) %>% 
  select(ingreso, clasif_sbs) %>% 
  mutate(catIngreso = cut(ingreso, 
                          breaks = quantile(ingreso, probs = c(0, 0.15, 0.85, 1), na.rm = TRUE),
                          labels = c("baja", "media", "alta"),
                          include.lowest = TRUE)) %>% 
  filter(catIngreso != "media") %>% 
  count(catIngreso, clasif_sbs) %>% 
  mutate(n = ifelse(catIngreso == "baja", -n, n))


ggplot(df_aux, aes(x = clasif_sbs, y = n, fill = catIngreso)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(title = "Clasificación según nivel de ingreso", y = "", x = "Clasificación financiera", fill = "Nivel de ingreso") + 
  scale_fill_manual(values = c("pink1", "pink4"))


# ---------------------------------------------
# Score según deuda
# ---------------------------------------------
df_aux <- df %>% 
  filter(mora == 1) %>% 
  select(score, deuda_sf_clean) %>% 
  mutate(score = cut(score, breaks = 5)) %>% 
  mutate(catDeuda = cut(deuda_sf_clean, 
                        breaks = quantile(deuda_sf_clean, probs = c(0, 0.15, 0.85, 1), na.rm = TRUE),
                        labels = c("baja", "media", "alta"),
                        include.lowest = TRUE)) %>% 
  filter(catDeuda != "media") %>% 
  count(score, catDeuda) %>% 
  mutate(n = ifelse(catDeuda == "baja", -n, n))


ggplot(df_aux, aes(x = score, y = n, fill = catDeuda)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(title = "Score según tipo de deuda", y = "", x = "Score", fill = "Magnitud de deuda") + 
  scale_fill_manual(values = c("yellow3", "green4"))


# ---------------------------------------------
# Score según ingreso
# ---------------------------------------------
df_aux <- df %>% 
  filter(mora == 1) %>% 
  select(score, ingreso) %>% 
  mutate(score = cut(score, breaks = 5)) %>% 
  mutate(catIngreso = cut(ingreso, 
                          breaks = quantile(ingreso, probs = c(0, 0.15, 0.85, 1), na.rm = TRUE),
                          labels = c("baja", "media", "alta"),
                          include.lowest = TRUE)) %>% 
  filter(catIngreso != "media") %>% 
  count(score, catIngreso) %>% 
  mutate(n = ifelse(catIngreso == "baja", -n, n))


ggplot(df_aux, aes(x = score, y = n, fill = catIngreso)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(title = "Score según nivel de ingreso", y = "", x = "Score", fill = "Nivel de ingreso") + 
  scale_fill_manual(values = c("yellow3", "green4"))


# ---------------------------------------------
# Morosidad según zona
# ---------------------------------------------
df_aux <- df %>% 
  select(zona_clean, mora)

ggplot(df_aux, aes(x = zona_clean, fill = as.factor(mora))) + 
  geom_bar(position = "fill") + 
  labs(title = "Morosidad según zona", x = "", y = "frecuencia", fill = "Morosidad") +
  scale_fill_manual(values = c("plum3", "orange2"))


# ---------------------------------------------
# Severidad de atraso según zona
# ---------------------------------------------
df_aux <- df %>% 
  filter(atraso > 0) %>% 
  select(zona_clean, atraso) %>% 
  mutate(atraso = log(atraso))


ggplot(df_aux, aes(x = zona_clean, y = atraso, fill = zona_clean)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("lightblue", "lightblue1")) +
  labs(title = "Atrasos según zona", x = "", y = "Días de atraso (log)", fill = "Zona")

# Vivienda, línea de crédito y score

df_aux <- df %>% 
  select(vivienda_clean, linea_sf_clean, score) %>% 
  filter(linea_sf_clean > 0) %>% 
  mutate(linea_sf_clean = log(linea_sf_clean))

ggplot(df_aux, aes(x = vivienda_clean, y = linea_sf_clean, fill = vivienda_clean)) + 
  geom_boxplot() +
  labs(title = "Línea de crédito según vivienda", x = "Tipo de vivienda", y = "Línea de crédito (log)") + 
  scale_fill_manual(values = c("lightblue", "lightblue1"))

ggplot(df_aux, aes(x = vivienda_clean, y = score, fill = vivienda_clean)) + 
  geom_boxplot() +
  labs(title = "Score según vivienda", x = "Tipo de vivienda", y = "Score") + 
  scale_fill_manual(values = c("lightblue", "lightblue1"))


# Ahorro y severidad de mora

df_aux <- df %>% 
  filter(atraso > 0, mora == 1) %>% 
  select(atraso, nivel_ahorro) %>% 
  mutate(
    atraso = log(atraso),
    nivel_ahorro = factor(nivel_ahorro, levels = 0:12,
                          labels = c("Bajo", "Bajo", 0, 0, 0, "Medio", "Medio", "Medio", 0, 0, 0, "Alto", "Alto"))
  ) %>% 
  filter(nivel_ahorro != 0)

ggplot(df_aux, aes(x = as.factor(nivel_ahorro), y = atraso, fill = nivel_ahorro)) + 
  geom_boxplot() + 
  labs(title = "Atraso según nivel de ahorro", x = "Nivel de ahorro", y = "Días de atraso (log)") +
  scale_fill_manual(values = c("palevioletred2", "palevioletred3", "palevioletred4"))


# Score según nivel de ahorro

df_aux <- df %>% 
  select(score, nivel_ahorro) %>% 
  mutate(
    nivel_ahorro = factor(nivel_ahorro, levels = 0:12,
                          labels = c("Bajo", "Bajo", 0, 0, 0, "Medio", "Medio", "Medio", 0, 0, 0, "Alto", "Alto"))
  ) %>% 
  filter(nivel_ahorro != 0)

ggplot(df_aux, aes(x = as.factor(nivel_ahorro), y = score, fill = nivel_ahorro)) +
  geom_boxplot() +
  labs(title = "Score según nivel de ahorro", x = "Nivel de ahorro", y = "Score") +
  scale_fill_manual(values = c("palevioletred2", "palevioletred3", "palevioletred4"))


# Score y línea de crédito según edad

df_aux <- df %>% 
  select(linea_sf_clean, score, edad) %>% 
  filter(linea_sf_clean > 0) %>% 
  mutate(
    linea_sf_clean = log(linea_sf_clean),
    score = cut(score, breaks = 4)
  )

ggplot(df_aux, aes(x = as.factor(score), y = linea_sf_clean, fill = score)) + 
  geom_boxplot() + 
  labs(title = "Línea de crédito según score", x = "Score", y = "Línea de crédito (log)") + 
  scale_fill_manual(values = c("peachpuff", "peachpuff2", "peachpuff3", "peachpuff4"))

# Segmentado por edad

df_aux1 <- df_aux %>% filter(edad <= 30)
df_aux2 <- df_aux %>% filter(edad >= 40)

ggplot(df_aux1, aes(x = as.factor(score), y = linea_sf_clean, fill = score)) + 
  geom_boxplot() + 
  labs(title = "Línea de crédito (edad ≤ 30)", x = "Score", y = "Línea de crédito (log)") + 
  scale_fill_manual(values = c("lightsalmon1", "lightsalmon2", "lightsalmon3", "lightsalmon4"))

ggplot(df_aux2, aes(x = as.factor(score), y = linea_sf_clean, fill = score)) + 
  geom_boxplot() + 
  labs(title = "Línea de crédito (edad ≥ 40)", x = "Score", y = "Línea de crédito (log)") + 
  scale_fill_manual(values = c("lightsalmon1", "lightsalmon2", "lightsalmon3", "lightsalmon4"))


# Morosidad y atraso según días laborales

df_aux <- df %>% 
  select(dias_lab, mora, atraso, ingreso) %>% 
  filter(atraso > 0) %>% 
  mutate(
    dias_lab = cut(dias_lab, breaks = quantile(dias_lab, probs = c(0, 0.1, 0.45, 0.55, 0.9, 1), na.rm = TRUE),
                   labels = c("Pocos", "1", "Media", "1", "Muchos"), include.lowest = TRUE),
    mora = factor(mora, levels = c(0,1), labels = c("No moroso", "Moroso")),
    atraso = log(atraso)
  ) %>% 
  filter(dias_lab != "1")

ggplot(df_aux, aes(x = dias_lab, fill = mora)) + 
  geom_bar(position = "fill") + 
  labs(title = "Morosidad según días de trabajo", x = "Días de trabajo", y = "Frecuencia", fill = "Morosidad") +
  scale_fill_manual(values = c("No moroso" = "#CAFF70", "Moroso" = "#9BCD9B"))

ggplot(df_aux, aes(x = dias_lab, y = atraso, fill = dias_lab)) + 
  geom_boxplot() + 
  labs(title = "Atraso según días de trabajo", x = "Días de trabajo", y = "Días de atraso (log)") + 
  scale_fill_manual(values = c("#CAFF70", "#9BCD9B", "plum4"))

ggplot(df_aux, aes(x = "", fill = mora)) + 
  geom_bar(position = "fill") + 
  coord_polar(theta = "y") + 
  facet_wrap(~dias_lab) + 
  labs(title = "Prevalencia de la mora según días de trabajo", x = "", y = "", fill = "Morosidad") + 
  scale_fill_manual(values = c("No moroso" = "#CAFF70", "Moroso" = "#9BCD9B"))


# Exploración geom_point: score vs linea_sf

df_aux <- df %>% 
  select(mora, ingreso, linea_sf, deuda_sf_clean, atraso, score) %>% 
  filter(atraso > 0) %>% 
  mutate(
    atraso = log(atraso),
    atraso_cat = cut(atraso, breaks = quantile(atraso, na.rm = TRUE), labels = c("Bajo", "Medio", "Medio", "Alto")),
    mora = factor(mora, levels = c(0,1), labels = c("No Moroso", "Moroso"))
  ) %>% 
  filter(atraso_cat != "Medio")

ggplot(df_aux, aes(x = score, y = linea_sf, colour = mora)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Relación entre score y línea de crédito", x = "Score", y = "Línea de crédito", colour = "Morosidad")