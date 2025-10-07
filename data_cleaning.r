setwd("~/Desktop/Master/I semicuatri/Statistics/Project/Datasets/ESS11")

datos <- read.csv("data/ESS11.csv", sep = ",", dec = ".", stringsAsFactors = FALSE)
# Definir vector de variables

names(datos)
vars_wanted <- c(
  "agea","gndr","marsts","eduyrs","edulvlb","hinctnta","domicil","ctzcntr",
  "rlgdgr","stflife","happy","health","sclmeet","ppltrst",
  "impsafea","impfreea","imptrada","impfuna","atchctr","rlgatnd","pray","sclact",
  "netusoft","netustm"
)

vars_ok <- intersect(vars_wanted, names(datos))
vars_missing <- setdiff(vars_wanted, names(datos))

df <- datos[, vars_ok, drop = FALSE]

library(dplyr)

df <- 
  
  df <- df %>%
  rename(
    # ---- SOCIODEMOGRÁFICAS ----
    age             = agea,        # Edad del entrevistado
    gender          = gndr,        # Sexo
    marital_status  = marsts,      # Estado civil
    years_edu       = eduyrs,      # Años de educación
    educ_level      = edulvlb,     # Nivel educativo (ISCED)
    income_decile   = hinctnta,    # Decil de ingresos del hogar
    residence_size  = domicil,     # Tamaño del lugar de residencia
    citizenship     = ctzcntr,     # País de ciudadanía
    
    # ---- RELIGIÓN Y VALORES ----
    religiosity     = rlgdgr,      # Grado de religiosidad
    attend_relig    = rlgatnd,     # Frecuencia de asistencia religiosa
    pray_freq       = pray,        # Frecuencia de oración
    value_safe      = impsafea,    # Valor: seguridad
    value_free      = impfreea,    # Valor: libertad personal
    value_tradition = imptrada,    # Valor: tradiciones
    value_fun       = impfuna,     # Valor: disfrutar la vida
    
    # ---- BIENESTAR Y SALUD ----
    life_satisfaction = stflife,   # Satisfacción con la vida
    happiness          = happy,    # Felicidad general
    health_status      = health,   # Salud autopercibida
    
    # ---- RELACIONES SOCIALES ----
    social_meetings  = sclmeet,    # Frecuencia reuniones sociales
    social_activity  = sclact,     # Participación en actividades/voluntariado
    trust_people     = ppltrst,    # Confianza interpersonal
    
    # ---- CULTURA Y TECNOLOGÍA ----
    patriotism_level  = atchctr,    # Asistencia a actividades culturales
    internet_use     = netusoft,   # Frecuencia de uso de internet
    internet_time    = netustm     # Tiempo diario en internet
  )

# Confirmar los nuevos nombres
names(df)

# =========================================================
# 🧹 RECODIFICAR CÓDIGOS ESPECIALES ESS11 → NA
# =========================================================

# ---- EDUCACIÓN ----
df$years_edu[df$years_edu %in% c(77, 88, 99)] <- NA
df$educ_level[df$educ_level %in% c(5555, 7777, 8888, 99)] <- NA

# ---- INGRESOS ----
df$income_decile[df$income_decile %in% c(77, 88, 99)] <- NA

# ---- RELIGIÓN ----
df$religiosity[df$religiosity %in% c(77, 88, 99)] <- NA
df$attend_relig[df$attend_relig %in% c(88, 99)] <- NA
df$pray_freq[df$pray_freq %in% c(77, 88, 99)] <- NA  # por consistencia

# ---- VALORES (0–6 o 0–10, según la escala) ----
df$value_safe[df$value_safe %in% c(66, 77, 88, 99)] <- NA
df$value_free[df$value_free %in% c(66, 77, 88, 99)] <- NA
df$value_tradition[df$value_tradition %in% c(66, 77, 88, 99)] <- NA
df$value_fun[df$value_fun %in% c(66, 77, 88, 99)] <- NA

# ---- BIENESTAR Y SALUD ----
df$life_satisfaction[df$life_satisfaction %in% c(77, 88, 99)] <- NA
df$happiness[df$happiness %in% c(77, 88, 99)] <- NA
df$health_status[df$health_status %in% c(7, 8, 9)] <- NA

# ---- SOCIAL ----
df$social_meetings[df$social_meetings %in% c(77, 88, 99)] <- NA
df$social_activity[df$social_activity %in% c(7, 8, 9)] <- NA
df$trust_people[df$trust_people %in% c(77, 88, 99)] <- NA

# ---- CULTURA Y TECNOLOGÍA ----
df$patriotism_level[df$patriotism_level %in% c(7, 8, 9)] <- NA
df$internet_use[df$internet_use %in% c(7, 8, 9)] <- NA
df$internet_time[df$internet_time %in% c(6666, 7777, 8888, 9999)] <- NA

# ---- SOCIODEMOGRÁFICAS ----
df$gender[df$gender %in% c(9)] <- NA
df$marital_status[df$marital_status %in% c(66, 77, 88, 99)] <- NA
df$residence_size[df$residence_size %in% c(7, 8, 9)] <- NA
df$citizenship[df$citizenship %in% c(7777, 8888, 9999)] <- NA
df$age[df$age < 10 | df$age > 120] <- NA  # por si hubiera errores de entrada



cat("\n✅ Recodificación completada: todos los códigos especiales se convirtieron en NA.\n")

df <- df %>%
  select(-c(patriotism_level, residence_size, citizenship))


write.csv(df, file = "data/clean_data_na.csv", row.names = FALSE)
write.csv(df_sin_na, file = "data/clean_data.csv", row.names = FALSE)
