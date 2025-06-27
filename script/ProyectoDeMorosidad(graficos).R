# Cargando librerías
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(patchwork)

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