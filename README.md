# Morosidad con R
Proyecto de R para la materia Estadística Descriptiva. Es sobre Morosidad.

# 🧹 Limpieza y Transformación de Datos - Proyecto de Morosidad

Este repositorio contiene el resultado de la etapa de limpieza (ETL) del dataset utilizado en el proyecto final del curso de Estadística Descriptiva - FCEA, primer semestre 2025. El objetivo fue preparar los datos de morosidad financiera para su posterior análisis estadístico, asegurando integridad, consistencia y trazabilidad.

---

## 📁 Dataset original

- **Nombre:** `data.csv`
- **Fuente:** Kaggle - [Bank Default Analysis Dataset](https://www.kaggle.com/datasets/luishcaldernb/morosidad)
- **Observaciones:** 8399 registros
- **Variables:** 14 variables socioeconómicas, demográficas y financieras relacionadas con morosidad (`mora`).

---

## 🔍 Objetivos de esta etapa

- Detectar y manejar valores faltantes.
- Generar versiones limpias (`*_clean`) sin alterar la variable original.
- Estandarizar variables categóricas para su análisis.
- Etiquetar y cuantificar valores atípicos.
- Documentar y exportar un dataset limpio y validado para análisis descriptivo y exploratorio.

---

## 📦 Librerías utilizadas

| Librería     | Rol técnico |
|--------------|-------------|
| `readr`      | Importación eficiente de archivos CSV. |
| `dplyr`      | Transformación de datos (filtrado, imputación, recodificación). |
| `tidyr`      | Reestructuración de datos para comparaciones (`pivot_longer`). |
| `ggplot2`    | Generación de visualizaciones comparativas y exploratorias. |
| `patchwork`  | Composición visual de múltiples gráficos en una misma visualización. |

---

## ⚙️ Principales transformaciones realizadas

### 1. **Manejo de valores faltantes**

- **Variables categóricas faltantes** (`NA`) fueron **eliminadas** (registro completo removido si faltaban en `exp_sf`, `linea_sf`, `deuda_sf`).
- **Variables numéricas:**
  - `exp_sf` imputada con **mediana** → nueva columna: `exp_sf_clean`
  - `linea_sf` y `deuda_sf` imputadas con **cero** → `linea_sf_clean` y `deuda_sf_clean`

> 💡 Las imputaciones fueron seleccionadas en base a la naturaleza de cada variable:  
> - La mediana minimiza el efecto de outliers en experiencia financiera.  
> - El cero representa ausencia de línea de crédito o deuda.

---

### 2. **Recodificación de variables categóricas**

Se generaron versiones limpias (`*_clean`) con categorías consolidadas para reducir ruido analítico:

- `nivel_educ_clean`  
  - `"SIN EDUCACION"` y `"SECUNDARIA"` → `"Baja o menos"`  
  - `"TECNICA"` → `"Media"`  
  - `"UNIVERSITARIA"` → `"Alta"`  
- `vivienda_clean`  
  - `"PROPIA"` → `"Propia"`  
  - `"FAMILIAR"`, `"ALQUILADA"` → `"No Propia"`
- `zona_clean`  
  - Agrupación de zonas con menos de 250 registros bajo `"OTRAS"`

---

### 3. **Detección de Outliers**

Se implementó una función `detectar_outliers()` bajo la regla de Tukey (1.5 * IQR) sobre:

- `exp_sf_clean` → `exp_sf_outlier`
- `linea_sf_clean` → `linea_sf_outlier`
- `deuda_sf_clean` → `deuda_sf_outlier`

> ✔️ Esto permite a los futuros analistas decidir si tratar los outliers o analizarlos como subgrupo.

---

## 📊 Visualizaciones generadas

Se generaron comparativas entre variables originales y limpias mediante:

- **Boxplots** para evaluar el efecto de la imputación.
- **Gráficos de barras** para comparar frecuencias categóricas antes y después de la limpieza.
- **Facets** para observar la cantidad de outliers por variable.

Las visualizaciones fueron fundamentales para validar decisiones y mostrar trazabilidad del proceso de limpieza.

---

## ✅ Validaciones aplicadas

- `colSums(is.na(df))` sobre columnas limpias: **0 NAs**
- `duplicated(df)` → **0 duplicados**
- `unique()` sobre variables recodificadas → sin valores erróneos
- `str()` → verificación de tipos `factor` para análisis categórico
- `table(mora)` → validación de distribución de la variable respuesta

---

## 📤 Exportación

El dataset final fue guardado como:

```r
write.csv(df, file = "data_limpio.csv", row.names = FALSE)
