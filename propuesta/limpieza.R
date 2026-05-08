# ============================================================
# Bayes Project — Ronda 1: Exploración inicial del dataset MAL
# ============================================================

# --- Paquetes ---
library(tidyverse)

# --- Cargar datos ---
ruta <- "E:/UC/bayes project/dataset/mal_anime.csv"
df <- read_csv(ruta, show_col_types = FALSE)

cat("\n========== 1. ESTRUCTURA BÁSICA ==========\n")
cat("Dimensiones:", nrow(df), "filas x", ncol(df), "columnas\n\n")
cat("Nombres de columnas:\n")
print(colnames(df))

cat("\n========== 2. TIPOS DE DATOS ==========\n")
print(sapply(df, class))

cat("\n========== 3. PRIMERAS 3 FILAS (transpuestas para legibilidad) ==========\n")
print(t(head(df, 3)))

cat("\n========== 4. VALORES FALTANTES Y 'N/A' / 'Unknown' COMO STRING ==========\n")
# Algunos campos numéricos están guardados como string con "N/A" o "Unknown"
faltantes <- df %>%
  summarise(across(everything(), ~ sum(is.na(.) | . %in% c("N/A", "Unknown", "")))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_faltantes") %>%
  mutate(pct_faltantes = round(100 * n_faltantes / nrow(df), 1)) %>%
  arrange(desc(pct_faltantes))
print(faltantes, n = 30)

cat("\n========== 5. VARIABLES NUMÉRICAS CLAVE (convertidas) ==========\n")
# Convertir variables numéricas que vienen como string
df_num <- df %>%
  mutate(
    Score_num      = suppressWarnings(as.numeric(Score)),
    Members_num    = suppressWarnings(as.numeric(Members)),
    Favorites_num  = suppressWarnings(as.numeric(Favorites)),
    Episodes_num   = suppressWarnings(as.numeric(Episodes)),
    Year_num       = suppressWarnings(as.numeric(Released_Year)),
    Ranked_num     = suppressWarnings(as.numeric(Ranked)),
    Popularity_num = suppressWarnings(as.numeric(Popularity))
  )

resumen_num <- df_num %>%
  select(Score_num, Members_num, Favorites_num, Episodes_num, Year_num) %>%
  summary()
print(resumen_num)

cat("\n========== 6. DISTRIBUCIÓN DE SCORE (variable de respuesta del problema 1) ==========\n")
score_stats <- df_num %>%
  filter(!is.na(Score_num)) %>%
  summarise(
    n           = n(),
    media       = round(mean(Score_num), 3),
    mediana     = round(median(Score_num), 3),
    sd          = round(sd(Score_num), 3),
    minimo      = round(min(Score_num), 3),
    maximo      = round(max(Score_num), 3),
    p1          = round(quantile(Score_num, 0.01), 3),
    p99         = round(quantile(Score_num, 0.99), 3),
    n_score_lt2 = sum(Score_num < 2),
    n_score_gt9 = sum(Score_num > 9)
  )
print(score_stats)

cat("\nCuantiles detallados de Score:\n")
print(quantile(df_num$Score_num, probs = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), na.rm = TRUE))

cat("\n========== 7. MEMBERS Y FAVORITES (problema 2) ==========\n")
mf_stats <- df_num %>%
  filter(!is.na(Members_num), !is.na(Favorites_num), Members_num > 0) %>%
  mutate(tasa_conversion = Favorites_num / Members_num) %>%
  summarise(
    n                   = n(),
    members_mediana     = median(Members_num),
    members_media       = round(mean(Members_num)),
    members_max         = max(Members_num),
    favorites_mediana   = median(Favorites_num),
    favorites_media     = round(mean(Favorites_num)),
    favorites_max       = max(Favorites_num),
    conversion_mediana  = round(median(tasa_conversion), 5),
    conversion_media    = round(mean(tasa_conversion), 5),
    conversion_max      = round(max(tasa_conversion), 5)
  )
print(mf_stats)

cat("\nCuantiles de Members:\n")
print(quantile(df_num$Members_num, probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.99), na.rm = TRUE))

cat("\nCuantiles de Favorites:\n")
print(quantile(df_num$Favorites_num, probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.99), na.rm = TRUE))

cat("\n========== 8. VARIABLES CATEGÓRICAS: FRECUENCIAS ==========\n")

cat("\n--- Type ---\n")
print(df %>% count(Type, sort = TRUE))

cat("\n--- Status ---\n")
print(df %>% count(Status, sort = TRUE))

cat("\n--- Source (top 10) ---\n")
print(df %>% count(Source, sort = TRUE) %>% head(10))

cat("\n--- Demographic ---\n")
print(df %>% count(Demographic, sort = TRUE))

cat("\n--- Rating ---\n")
print(df %>% count(Rating, sort = TRUE))

cat("\n--- Released_Season ---\n")
print(df %>% count(Released_Season, sort = TRUE))

cat("\n========== 9. STUDIOS: ¿CUÁNTA DISPERSIÓN HAY? ==========\n")
# Studios puede ser multi-valor separado por coma — primero veamos como viene
cat("Ejemplo de valores de Studios (5 al azar):\n")
set.seed(123)
print(df %>% filter(!is.na(Studios), Studios != "Unknown") %>%
        sample_n(5) %>% pull(Studios))

cat("\nFrecuencia de Studios (separando por coma, top 20):\n")
studios_split <- df %>%
  filter(!is.na(Studios), Studios != "Unknown", Studios != "") %>%
  separate_rows(Studios, sep = ",\\s*") %>%
  count(Studios, sort = TRUE)
print(head(studios_split, 20))

cat("\nDistribución de número de obras por Studio:\n")
print(quantile(studios_split$n, probs = c(0.5, 0.75, 0.9, 0.95, 0.99)))
cat("Total de studios únicos:", nrow(studios_split), "\n")
cat("Studios con >=10 obras:", sum(studios_split$n >= 10), "\n")
cat("Studios con >=20 obras:", sum(studios_split$n >= 20), "\n")
cat("Studios con >=50 obras:", sum(studios_split$n >= 50), "\n")

cat("\n========== 10. GENRES: ¿CUÁLES SON LOS PRINCIPALES? ==========\n")
cat("Ejemplo de valores de Genres:\n")
print(df %>% filter(!is.na(Genres), Genres != "Unknown") %>%
        sample_n(5) %>% pull(Genres))

cat("\nTop 20 Genres más frecuentes:\n")
genres_split <- df %>%
  filter(!is.na(Genres), Genres != "Unknown", Genres != "") %>%
  separate_rows(Genres, sep = ",\\s*") %>%
  count(Genres, sort = TRUE)
print(head(genres_split, 20))
cat("Total de Genres únicos:", nrow(genres_split), "\n")

cat("\n========== 11. INTERSECCIÓN DE VARIABLES CLAVE (sin valores faltantes) ==========\n")
# ¿Cuántas filas son utilizables para los modelos?
df_completo_p1 <- df_num %>%
  filter(!is.na(Score_num), !is.na(Studios), Studios != "Unknown",
         !is.na(Source), Source != "Unknown",
         !is.na(Type), Type != "Unknown")
cat("Filas con Score, Studios, Source, Type todos presentes (problema 1):",
    nrow(df_completo_p1), "\n")

df_completo_p2 <- df_num %>%
  filter(!is.na(Members_num), !is.na(Favorites_num), Members_num > 0,
         !is.na(Genres), Genres != "Unknown",
         !is.na(Year_num), !is.na(Type), Type != "Unknown")
cat("Filas con Members, Favorites, Genres, Year, Type todos presentes (problema 2):",
    nrow(df_completo_p2), "\n")

cat("\n========== FIN DE LA RONDA 1 ==========\n")




















# ============================================================
# Ronda 1.5: Análisis complementario
# ============================================================

cat("\n========== 8. VARIABLES CATEGÓRICAS ==========\n")

cat("\n--- Type ---\n")
print(df %>% count(Type, sort = TRUE))

cat("\n--- Status ---\n")
print(df %>% count(Status, sort = TRUE))

cat("\n--- Source ---\n")
print(df %>% count(Source, sort = TRUE))

cat("\n--- Demographic ---\n")
print(df %>% count(Demographic, sort = TRUE))

cat("\n--- Rating ---\n")
print(df %>% count(Rating, sort = TRUE))

cat("\n========== 9. STUDIOS — DISPERSIÓN ==========\n")
cat("Ejemplo de valores de Studios:\n")
set.seed(123)
print(df %>% filter(!is.na(Studios), Studios != "Unknown") %>%
        sample_n(5) %>% pull(Studios))

studios_split <- df %>%
  filter(!is.na(Studios), Studios != "Unknown", Studios != "") %>%
  separate_rows(Studios, sep = ",\\s*") %>%
  count(Studios, sort = TRUE)

cat("\nTop 25 Studios:\n")
print(head(studios_split, 25))
cat("\nDistribución de obras por Studio:\n")
print(quantile(studios_split$n, probs = c(0.5, 0.75, 0.9, 0.95, 0.99)))
cat("Total studios únicos:", nrow(studios_split), "\n")
cat("Studios con >=10 obras:", sum(studios_split$n >= 10), "\n")
cat("Studios con >=20 obras:", sum(studios_split$n >= 20), "\n")
cat("Studios con >=50 obras:", sum(studios_split$n >= 50), "\n")
cat("Studios con >=100 obras:", sum(studios_split$n >= 100), "\n")

cat("\n========== 10. GENRES ==========\n")
genres_split <- df %>%
  filter(!is.na(Genres), Genres != "Unknown", Genres != "") %>%
  separate_rows(Genres, sep = ",\\s*") %>%
  count(Genres, sort = TRUE)
cat("\nTop 25 Genres:\n")
print(head(genres_split, 25))
cat("Total Genres únicos:", nrow(genres_split), "\n")

cat("\n========== 11. RECUPERACIÓN DE AÑO DESDE 'Premiered' ==========\n")
# Premiered tiene formato como "Fall 2023" o "Spring 1998"
cat("Ejemplos de valores de Premiered (no nulos):\n")
print(df %>% filter(!is.na(Premiered), Premiered != "Unknown") %>%
        sample_n(10) %>% pull(Premiered))

# Intento de extracción de año desde Premiered
df_year <- df %>%
  mutate(
    Year_from_Premiered = suppressWarnings(
      as.numeric(stringr::str_extract(Premiered, "\\d{4}"))
    )
  )

cat("\nComparación: Released_Year vs Year extraído de Premiered\n")
cat("  Filas con Released_Year no nulo:",
    sum(!is.na(df_year$Released_Year)), "\n")
cat("  Filas con Year extraído de Premiered no nulo:",
    sum(!is.na(df_year$Year_from_Premiered)), "\n")
cat("  Filas donde ambos coinciden:",
    sum(!is.na(df_year$Released_Year) & !is.na(df_year$Year_from_Premiered) &
          df_year$Released_Year == df_year$Year_from_Premiered, na.rm = TRUE), "\n")

# Si Premiered no ayuda, ver si el campo "Status" o "title" da información
cat("\n¿Qué tipos de anime tienen año faltante?\n")
print(df_year %>%
        filter(is.na(Year_from_Premiered) & is.na(Released_Year)) %>%
        count(Type, sort = TRUE))

cat("\n========== 12. SAMPLES UTILIZABLES POR PROBLEMA ==========\n")

# Conversión de variables numéricas
df_num <- df %>%
  mutate(
    Score_num     = suppressWarnings(as.numeric(Score)),
    Members_num   = suppressWarnings(as.numeric(Members)),
    Favorites_num = suppressWarnings(as.numeric(Favorites)),
    Year_recovered = suppressWarnings(
      as.numeric(stringr::str_extract(Premiered, "\\d{4}"))
    )
  )

# Problema 1: Score + Studios + Source + Type
df_p1 <- df_num %>%
  filter(!is.na(Score_num),
         !is.na(Studios), Studios != "Unknown",
         !is.na(Source), Source != "Unknown",
         !is.na(Type), Type != "Unknown")
cat("Problema 1 (Score + Studio + Source + Type):", nrow(df_p1), "filas\n")

# Problema 2 sin año
df_p2_noyear <- df_num %>%
  filter(!is.na(Members_num), !is.na(Favorites_num), Members_num > 0,
         !is.na(Genres), Genres != "Unknown",
         !is.na(Type), Type != "Unknown")
cat("Problema 2 sin año (Members + Favorites + Genres + Type):",
    nrow(df_p2_noyear), "filas\n")

# Problema 2 con año recuperado
df_p2_year <- df_p2_noyear %>%
  filter(!is.na(Year_recovered))
cat("Problema 2 con año recuperado (de Premiered):",
    nrow(df_p2_year), "filas\n")

# Filtros adicionales para Problema 2 (eliminar zero-inflation extrema)
df_p2_filtered <- df_p2_year %>%
  filter(Members_num >= 1000)
cat("Problema 2 con filtro Members >= 1000:", nrow(df_p2_filtered), "filas\n")

df_p2_filtered2 <- df_p2_year %>%
  filter(Members_num >= 5000)
cat("Problema 2 con filtro Members >= 5000:", nrow(df_p2_filtered2), "filas\n")

cat("\n========== FIN ==========\n")

























# ============================================================
# Ronda 1.5: Investigación de problemas de calidad
# ============================================================

cat("\n========== A. ¿QUÉ ES 'add some'? ==========\n")

# Ver qué tipo de anime tienen este "estudio"
cat("Tipos de anime con Studio = 'add some':\n")
print(df %>% filter(Studios == "add some") %>% count(Type, sort = TRUE))

cat("\nEjemplos de anime con Studio = 'add some':\n")
print(df %>% filter(Studios == "add some") %>%
        select(title, Type, Score, Members) %>%
        head(10))

cat("\n¿Tienen Score estos anime?\n")
print(df %>% filter(Studios == "add some") %>%
        mutate(Score_num = suppressWarnings(as.numeric(Score))) %>%
        summarise(
          n_total       = n(),
          n_con_score   = sum(!is.na(Score_num)),
          score_mediana = median(Score_num, na.rm = TRUE)
        ))

cat("\n========== B. OTROS VALORES SOSPECHOSOS EN STUDIOS ==========\n")
# Los valores con minúscula al inicio y sin espacios pueden ser sospechosos
sospechosos <- df %>%
  filter(!is.na(Studios), Studios != "Unknown") %>%
  separate_rows(Studios, sep = ",\\s*") %>%
  count(Studios, sort = TRUE) %>%
  filter(n >= 50,
         (str_detect(Studios, "^[a-z]") | !str_detect(Studios, " ")))

cat("Studios con >=50 obras pero nombre sospechoso (minúscula inicial o sin espacios):\n")
print(sospechosos, n = 30)

cat("\n========== C. RECUPERACIÓN DE AÑO DESDE 'Premiered' ==========\n")

cat("Ejemplos de Premiered no nulos:\n")
print(df %>% filter(!is.na(Premiered), Premiered != "Unknown") %>%
        sample_n(10) %>% pull(Premiered))

df_year <- df %>%
  mutate(
    Year_from_Premiered = suppressWarnings(
      as.numeric(stringr::str_extract(Premiered, "\\d{4}"))
    )
  )

cat("\nResumen recuperación de año:\n")
cat("  Filas con Released_Year no nulo:",
    sum(!is.na(df_year$Released_Year)), "\n")
cat("  Filas con año extraído de Premiered no nulo:",
    sum(!is.na(df_year$Year_from_Premiered)), "\n")
cat("  Coinciden cuando ambos no son nulos:",
    sum(df_year$Released_Year == df_year$Year_from_Premiered, na.rm = TRUE),
    "de",
    sum(!is.na(df_year$Released_Year) & !is.na(df_year$Year_from_Premiered)), "\n")

cat("\n¿Qué Type tienen los anime SIN año en ningún campo?\n")
print(df_year %>%
        filter(is.na(Released_Year), is.na(Year_from_Premiered)) %>%
        count(Type, sort = TRUE))

cat("\n========== D. MUESTRA EFECTIVA DESPUÉS DE LIMPIEZA ==========\n")

# Limpieza completa: excluir "add some" y otros valores sospechosos básicos
df_limpio <- df %>%
  mutate(
    Score_num     = suppressWarnings(as.numeric(Score)),
    Members_num   = suppressWarnings(as.numeric(Members)),
    Favorites_num = suppressWarnings(as.numeric(Favorites)),
    Year_recovered = suppressWarnings(
      as.numeric(stringr::str_extract(Premiered, "\\d{4}"))
    )
  ) %>%
  # Excluir Hentai y filas con Studios sospechoso
  filter(!str_detect(Rating, "Hentai") | is.na(Rating))

cat("Después de excluir Hentai:", nrow(df_limpio), "filas\n")

# Problema 1: limpio
df_p1 <- df_limpio %>%
  filter(!is.na(Score_num),
         !is.na(Studios), Studios != "Unknown", Studios != "add some",
         !is.na(Source), Source != "Unknown",
         !is.na(Type), Type != "Unknown")
cat("\nProblema 1 después de limpieza completa:", nrow(df_p1), "filas\n")

# Problema 2: con año recuperado
df_p2 <- df_limpio %>%
  filter(!is.na(Members_num), !is.na(Favorites_num), Members_num > 0,
         !is.na(Genres), Genres != "Unknown",
         !is.na(Year_recovered),
         !is.na(Type), Type != "Unknown")
cat("Problema 2 con año recuperado:", nrow(df_p2), "filas\n")

# Problema 2 con filtros adicionales para reducir zero-inflation
cat("\nProblema 2 con filtro adicional Members >= 500:",
    sum(df_p2$Members_num >= 500), "filas\n")
cat("Problema 2 con filtro adicional Members >= 1000:",
    sum(df_p2$Members_num >= 1000), "filas\n")
cat("Problema 2 con filtro adicional Members >= 5000:",
    sum(df_p2$Members_num >= 5000), "filas\n")

# Cuántos studios "buenos" sobreviven
cat("\nDespués de limpieza, distribución de obras por Studio (Problema 1):\n")
studios_p1 <- df_p1 %>%
  separate_rows(Studios, sep = ",\\s*") %>%
  filter(Studios != "add some") %>%
  count(Studios, sort = TRUE)
cat("Total studios:", nrow(studios_p1), "\n")
cat("Con >=10 obras:", sum(studios_p1$n >= 10), "\n")
cat("Con >=20 obras:", sum(studios_p1$n >= 20), "\n")
cat("Con >=30 obras:", sum(studios_p1$n >= 30), "\n")
cat("\nTop 30 studios reales:\n")
print(head(studios_p1, 30))

cat("\n========== FIN ==========\n")




