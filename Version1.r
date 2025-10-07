setwd("~/Desktop/Master/I semicuatri/Statistics/Project/Datasets/ESS11")

datos <- read.csv("ESS11.csv", sep = ",", dec = ".", stringsAsFactors = FALSE)
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

datos_filtrado <- datos[, vars_ok, drop = FALSE]