
### Indicadores pobreza y desigualdad
### Unidad de Métodos y Acceso a datos
### Observatorio Uruguay

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library(rio)
library(survey)
library(srvyr)
library(tidyverse)
library(laeken)
library(statar)
library(xlsx)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Carga de bases ###

ech            <- rio::import("Bases/ECH_implantacion_2023.Rdata")

#banco_mundial <- rio::import("Bases/Data_Banco_Mundial/Headcount_table_sp_Datos_completos_data.csv") # Descarga de: https://www.bancomundial.org/es/topic/poverty/lac-equity-lab1/poverty/head-count

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###

#IPC

ech <- ech %>% dplyr::mutate(bc_ipc_tot = case_when(
  mes == 1  ~ 0.286995721,
  mes == 2  ~ 0.282605309,
  mes == 3  ~ 0.279798078,
  mes == 4  ~ 0.277298982,
  mes == 5  ~ 0.275240151,
  mes == 6  ~ 0.275267734,
  mes == 7  ~ 0.276549203,
  mes == 8  ~ 0.277553197,
  mes == 9  ~ 0.277076807,
  mes == 10 ~ 0.275395538,
  mes == 11 ~ 0.273682943,
  mes == 12 ~ 0.272746995))



# Ingresos
ech <- ech %>% dplyr::mutate(y_pc       =  HT11 / HT19 )                      #Ingreso per-cápita
ech <- ech %>% dplyr::mutate(y_pc_svl   =  (HT11 - HT13) / HT19)              #Ingreso per-cápita sin valor locativo
ech <- ech %>% dplyr::mutate(y_pc_d     =  (HT11 / HT19) * bc_ipc_tot)          #Ingreso per-cápita deflactado
ech <- ech %>% dplyr::mutate(y_pc_svl_d =  (HT11 - HT13) / HT19 * bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado

# Quintil de ingresos
ech_h <- ech %>% distinct(ID, .keep_all = TRUE)
ech_h <- ech_h %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = W_ANO))
ech_h <- ech_h[,c("ID","quintilesy")]
ech <- merge(ech, ech_h, by = "ID")

ech <- ech %>% dplyr::mutate(quintilesy_obs = statar::xtile(y_pc, n=5, wt = W_ANO))

# Decil de ingresos
ech_h <- ech %>% distinct(ID, .keep_all = TRUE)
ech_h <- ech_h %>% dplyr::mutate(decilesy = statar::xtile(y_pc, n=10, wt = W_ANO))
ech_h <- ech_h[,c("ID","decilesy")]
ech <- merge(ech, ech_h, by = "ID")

ech <- ech %>% dplyr::mutate(decilesy_obs = statar::xtile(y_pc, n=10, wt = W_ANO))


# Pobreza auxiliar
ech <- ech %>% dplyr::mutate(pobre_aux = case_when(pobre06 == 0 ~ 2,
                                                   pobre06 == 1 ~ 1))
                                                   
# Región
ech <- ech %>% dplyr::mutate(bd_region = case_when(REGION_4 == 1 | REGION_4 == 2 ~ 1,
                                                   REGION_4 == 3 ~ 2,
                                                   REGION_4 == 4 ~ 3))

# Sexo
ech <- ech %>% dplyr::mutate(bc_pe2 = e26)

# Ascendencia afro
ech <- ech %>% dplyr::mutate(bd_e29_1 = e29_1)

# Tramo de edad
ech <- ech %>% dplyr::mutate(bc_pe3 = e27)
ech <- ech %>% dplyr::mutate(tramo_edad = case_when(bc_pe3>=0  & bc_pe3<=5  ~ 1,
                                                      bc_pe3>=6  & bc_pe3<=12 ~ 2,
                                                      bc_pe3>=13 & bc_pe3<=18 ~ 3,
                                                      bc_pe3>=19 & bc_pe3<=24 ~ 4,
                                                      bc_pe3>=25 & bc_pe3<=29 ~ 5,
                                                      bc_pe3>=30 & bc_pe3<=64 ~ 6,
                                                      bc_pe3>=65 ~ 7))


# Sexo del jefe/a de hogar
ech <- ech[order(ech$ID, ech$e30, decreasing = FALSE), ]
ech_h <- ech %>% distinct(ID, .keep_all = TRUE)
ech_h <- ech_h %>% dplyr::mutate(sexojefe = case_when(e30 == 1 & e26 == 1 ~ 1,
                                                        e30 == 1 & e26 == 2 ~ 2,
                                                        e30 != 1 ~ 99))
ech_h <- ech_h[,c("ID","sexojefe")]
ech <- merge(ech, ech_h, by = "ID")

# NBI - Hacinamiento
ech <- ech %>% dplyr::mutate(aux1 = HT19/d9, 
                                 aux2 = HT19/d10,
                                 bd_hacinamiento1  = ifelse(aux1>2, 1, 0),
                                 bd_hacinamiento2  = ifelse(aux2>2, 1, 0))

# NBI - Materialidad vivienda
ech <- ech %>% dplyr::mutate(NBI_materialidad11 = ifelse(c2==6 | c3==6 | c4==5, 1, 0))


# NBI - Espacio apropiado para cocinar
ech <- ech %>% dplyr::mutate(bd_d19 = d19)
ech <- ech %>% dplyr::mutate(NBI_cocina11 = ifelse(bd_d19==3, 1, 0))


# NBI - Abastecimiento de agua potable
ech <- ech %>% dplyr::mutate(bd_d11 = case_when(d11==1 ~ 1, 
                                                    d11==2 | d11==3 ~ 2, 
                                                    d11>=4 & d11<=6 ~ 3))

ech <- ech %>% dplyr::mutate(bd_d12 = case_when(d12==1 ~ 1, 
                                 d12==2 | d12==3 ~ 2,
                                 d12==4 ~ 3))

ech <- ech %>% dplyr::mutate(NBI_agua11 = ifelse((bd_d12==2 | bd_d12==3) | bd_d11==3, 1, 0))



# NBI - Servicio higiénico de calidad
ech <- ech %>% dplyr::mutate(bd_d13 = as.numeric(d13))
ech <- ech %>% dplyr::mutate(bd_d14 = as.numeric(d14))
ech <- ech %>% dplyr::mutate(bd_d15 = as.numeric(d15))
ech <- ech %>% dplyr::mutate(bd_d16 = as.numeric(case_when(d16==0 ~ 0,
                                                               d16==1 ~ 1, 
                                                               d16==2 ~ 2, 
                                                               d16>=3 & d16<=4 ~ 3)))

ech <- ech %>% dplyr::mutate(NBI_servhigien11 = case_when(bd_d14==0 | bd_d15==2 | bd_d16==3 | bd_d16==4  ~ 1, 
                                                              bd_d14!=0 & bd_d15==1 & (bd_d16==1 | bd_d16==2)  ~ 0,
                                                              bd_d15==0 | bd_d16==0 ~ 99))

# NBI - Energía eléctrica
ech <- ech %>% dplyr::mutate(bd_d18 = d18)
ech <- ech %>% dplyr::mutate(NBI_energia11 = ifelse(bd_d18>2, 1, 0))


# NBI - Artefactos básicos de cofnort
ech <- ech %>% dplyr::mutate(bd_d260 = ifelse(d260 ==6, 2, 1))
ech <- ech %>% dplyr::mutate(bd_d21_1 = d21_3)
ech <- ech %>% dplyr::mutate(bd_d21_2 = case_when(d21_1==1 | d21_2==1 ~ 1,
                                                      d21_1==2 & d21_2==2 ~ 2))
                                                          

ech <- ech %>% dplyr::mutate(NBI_artefactos11 = ifelse(bd_d260==2 | bd_d21_1==2 | bd_d21_2==2, 1, 0))


# NBI - Educación

ech <- ech %>% dplyr::mutate(niños_na = case_when(
  e27<4 | e27>17 ~ 0,
  e27>=4 & e27<=17 & e49==3 ~ 0,
  e27>=4 & e27<=17 & (e49==1 | e49==2) & (e201_1c==1 | e201_1d==1) ~ 0,  
  e27>=4 & e27<=17 & (e49==1 | e49==2) & (e201_1c!=1 & e201_1d!=1) ~ 1))


base_niños_na <- ech[,c("ID","niños_na")]
base_niños_na <- base_niños_na %>% dplyr::mutate(NBI_educacion11 = niños_na)
base_niños_na <- base_niños_na[order(base_niños_na$ID, base_niños_na$NBI_educacion11, decreasing = TRUE), ]
base_niños_na <- base_niños_na %>% distinct(ID, .keep_all = TRUE)
base_niños_na <- base_niños_na[,c("ID","NBI_educacion11")]

ech <- merge(ech, base_niños_na, by = "ID")



# NBI Total

ech <- ech %>% dplyr::mutate(NBI_vivienda11= ifelse(bd_hacinamiento1==1 | NBI_materialidad11==1 | NBI_cocina11==1, 1, 0))
ech <- ech %>% dplyr::mutate(NBI_2011= ifelse(NBI_vivienda11==1 | NBI_agua11==1 | NBI_servhigien11==1 | NBI_energia11==1 | NBI_artefactos11==1 | NBI_educacion11==1, 1, 0))
                                                                                              
ech <- ech %>% dplyr::mutate(NBI_cant= NBI_vivienda11 + NBI_agua11 + NBI_servhigien11 + NBI_energia11 + NBI_artefactos11 + NBI_educacion11)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Bases a nivel hogar ###                               

ech_h <- ech %>% distinct(ID, .keep_all = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ###

ech_svy           <- srvyr::as_survey_design(ech, ids = ID, weights = W_ANO)
ech_h_svy         <- srvyr::as_survey_design(ech_h, ids = ID, weights = W_ANO)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Estimación de indicadores ###


### 111 Promedio de ingreso per-cápita de los hogares (cte. base diciembre 2006)

#_____ Ver deflactor Iecon y reprocesar toda la serie con ingresos INE

## País Urbano


# Total

a_est <- ech_h_svy %>%
  srvyr::filter(bd_region == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(y_pc_d))
a_ano<- mean(as.numeric(a_est$colname))

c_ano <- a_ano


# Sexo del jefe

a_jefe <- function(x) {
  x <- ech_h_svy %>%
    filter(sexojefe == x & bd_region == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(y_pc_d))
  x <- mean(x$colname)
}       

a_e_jefe <- numeric()

for(i in 1:2){
  a_e_jefe[i] <- a_jefe(x = i)
}     

c_jefe <- as.data.frame(a_e_jefe)

# Quintil de ingreso del hogar

a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x & bd_region == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(y_pc_d))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(a_e_quintil)


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""


CODIND       <- 111
NOMINDICADOR <- "Promedio de ingreso per-cápita de los hogares (Cte. Base diciembre 2006)"
CATEGORIA    <- "Ingresos y desigualdad"
PESTAÑA      <- "País Urbano"
CORTE_NUEVA  <- c("Total",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Sexo del jefe(a)",
                  "Sexo del jefe(a)")
REGION       <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
SEXOJEFATURA <-c("Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Jefe Varón",
                 "Jefa Mujer")
POBREZA      <- c("Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos")
QUINTIL      <- c("Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos")
URBANORURALUY<- c("Urbano (más de 5.000 habitantes)",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (más de 5.000 habitantes)")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_quintil, a_e_jefe)
RESPONSABLE  <- "JIMENA PANDOLFI"


m111_pu <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY, PAIS,	ANIO,	VALOR,	RESPONSABLE)	


## Total País


# Total

a_est <- ech_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(y_pc_d))
a_ano<- mean(as.numeric(a_est$colname))

c_ano <- a_ano


# Region

a_region <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(y_pc_d))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}     

c_region <- as.data.frame(a_e_region)

# Sexo del jefe

a_jefe <- function(x) {
  x <- ech_h_svy %>%
    filter(sexojefe == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(y_pc_d))
  x <- mean(x$colname)
}       

a_e_jefe <- numeric()

for(i in 1:2){
  a_e_jefe[i] <- a_jefe(x = i)
}     

c_jefe <- as.data.frame(a_e_jefe)

# Quintil de ingreso del hogar

a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x) %>%

    srvyr::summarise(colname = srvyr::survey_mean(y_pc_d))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(a_e_quintil)

# Situación de pobreza del hogar

a_pobreza <- function(x) {
  x <- ech_h_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(y_pc_d))
  x <- mean(x$colname)
}       

a_e_pobreza <- numeric()

for(i in 1:2){
  a_e_pobreza[i] <- a_pobreza(x = i)
}     

c_pobreza <- as.data.frame(a_e_pobreza)

# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""


CODIND       <- 111
NOMINDICADOR <- "Promedio de ingreso per-cápita de los hogares (Cte. Base diciembre 2006)"
CATEGORIA    <- "Ingresos y desigualdad"
PESTAÑA      <- "Total País"
CORTE_NUEVA  <- c("Total",
                  "Región",
                  "Región",
                  "Región",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Pobreza",
                  "Pobreza",
                  "Sexo del jefe(a)",
                  "Sexo del jefe(a)")
REGION       <- c("Todos",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
SEXOJEFATURA <-c("Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Jefe Varón",
                 "Jefa Mujer")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Pobre",
                  "No pobre",
                  "Todos",
                  "Todos")
QUINTIL      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
URBANORURALUY<- c("Total País",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_region, a_e_quintil, a_e_pobreza, a_e_jefe)
RESPONSABLE  <- "JIMENA PANDOLFI"

ASCENDENCIA   <- c("")
SEXO          <- c("")
TRAMO         <- c("")
SEXO          <- c("")
CORTE         <- c("")
DEPARTAMENTOUY <- c("")
URBANORURALUY <- c("")
DECIL         <- c("")

m111_tp <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	

m111 <- rbind(m111_pu, m111_tp)


### 113 Mediana de ingreso per-cápita de los hogares (cte. base diciembre 2006)

#_____ Ver deflactor Iecon y reprocesar toda la serie con ingresos INE

## País Urbano


# Total

a_est <- ech_h_svy %>%
  srvyr::filter(bd_region == 1) %>%

  srvyr::summarise(colname = srvyr::survey_median(y_pc_d))
a_ano<- mean(as.numeric(a_est$colname))

c_ano <- a_ano


# Sexo del jefe

a_jefe <- function(x) {
  x <- ech_h_svy %>%
    filter(sexojefe == x & bd_region == 1) %>%

    srvyr::summarise(colname = srvyr::survey_median(y_pc_d))
  x <- mean(x$colname)
}       

a_e_jefe <- numeric()

for(i in 1:2){
  a_e_jefe[i] <- a_jefe(x = i)
}     

c_jefe <- as.data.frame(a_e_jefe)

# Quintil de ingreso del hogar

a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x & bd_region == 1) %>%

    srvyr::summarise(colname = srvyr::survey_median(y_pc_d))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(a_e_quintil)


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""


CODIND       <- 113
NOMINDICADOR <- "Mediana de ingreso per-cápita de los hogares (Cte. Base diciembre 2006)"
CATEGORIA    <- "Ingresos y desigualdad"
PESTAÑA      <- "País Urbano"
CORTE_NUEVA  <- c("Total",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Sexo del jefe(a)",
                  "Sexo del jefe(a)")
REGION       <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
SEXOJEFATURA <-c("Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Jefe Varón",
                 "Jefa Mujer")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
QUINTIL      <- c("Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos")
URBANORURALUY<- c("Urbano (más de 5.000 habitantes)",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (más de 5.000 habitantes)")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_quintil, a_e_jefe)
RESPONSABLE  <- "JIMENA PANDOLFI"


m113_pu <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	


## Total País


# Total

a_est <- ech_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_median(y_pc_d))
a_ano<- mean(as.numeric(a_est$colname))

c_ano <- a_ano


# Region

a_region <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_median(y_pc_d))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}     

c_region <- as.data.frame(a_e_region)

# Sexo del jefe

a_jefe <- function(x) {
  x <- ech_h_svy %>%
    filter(sexojefe == x) %>%
    srvyr::summarise(colname = srvyr::survey_median(y_pc_d))
  x <- mean(x$colname)
}       

a_e_jefe <- numeric()

for(i in 1:2){
  a_e_jefe[i] <- a_jefe(x = i)
}     

c_jefe <- as.data.frame(a_e_jefe)

# Quintil de ingreso del hogar

a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_median(y_pc_d))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(a_e_quintil)

# Situación de pobreza del hogar

a_pobreza <- function(x) {
  x <- ech_h_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::summarise(colname = srvyr::survey_median(y_pc_d))
  x <- mean(x$colname)
}       

a_e_pobreza <- numeric()

for(i in 1:2){
  a_e_pobreza[i] <- a_pobreza(x = i)
}     

c_pobreza <- as.data.frame(a_e_pobreza)

# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""


CODIND       <- 113
NOMINDICADOR <- "Mediana de ingreso per-cápita de los hogares (Cte. Base diciembre 2006)"
CATEGORIA    <- "Ingresos y desigualdad"
PESTAÑA      <- "Total País"
CORTE_NUEVA  <- c("Total",
                  "Región",
                  "Región",
                  "Región",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Pobreza",
                  "Pobreza",
                  "Sexo del jefe(a)",
                  "Sexo del jefe(a)")
REGION       <- c("Todos",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
SEXOJEFATURA <-c("Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Jefe Varón",
                 "Jefa Mujer")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Pobre",
                  "No pobre",
                  "Todos",
                  "Todos")
QUINTIL      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
URBANORURALUY<- c("Total País",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_region, a_e_quintil, a_e_pobreza, a_e_jefe)
RESPONSABLE  <- "JIMENA PANDOLFI"

ASCENDENCIA   <- c("")
SEXO          <- c("")
TRAMO         <- c("")
SEXO          <- c("")
CORTE         <- c("")
DEPARTAMENTOUY <- c("")
URBANORURALUY <- c("")
DECIL         <- c("")

m113_tp <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY, PAIS,	ANIO,	VALOR,	RESPONSABLE)	

m113 <- rbind(m113_pu, m113_tp)


### 121	Hogares en situación de pobreza (% de la población total) (INE) ------------------------------------

# Total

a_est <- ech_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(pobre06))

a_ano<- mean(as.numeric(a_est$colname))

c_ano <- a_ano

# Region

a_region <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre06))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}     

c_region <- as.data.frame(a_e_region)


# Sexo del jefe

a_jefe <- function(x) {
  x <- ech_h_svy %>%
    filter(sexojefe == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre06))
  x <- mean(x$colname)
}       

a_e_jefe <- numeric()

for(i in 1:2){
  a_e_jefe[i] <- a_jefe(x = i)
}     

c_jefe <- as.data.frame(a_e_jefe)


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 121
NOMINDICADOR <- "Hogares en situación de pobreza (% de la población total) (INE)"
CATEGORIA    <- "Pobreza"
CORTE_NUEVA  <- c("Total",
                  "Total",
                  "Total",
                  "Total",
                  "Sexo del jefe(a)",
                  "Sexo del jefe(a)")
REGION       <- c("Total País",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos")
SEXOJEFATURA <-c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Jefe Varón",
                  "Jefa Mujer")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_region, a_e_jefe)
RESPONSABLE  <- "JIMENA PANDOLFI"




m121 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	



### 122	Personas en situación de pobreza (% de la población total) (INE) ----------------------------------

# Total

a_est <- ech_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(pobre06))

a_ano<- mean(as.numeric(a_est$colname))

c_ano <- a_ano

# Region

a_region <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x) %>%

    srvyr::summarise(colname = srvyr::survey_mean(pobre06))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}     

c_region <- as.data.frame(a_e_region)


# Edad

a_edad <- function(x) {
  x <- ech_svy %>%
    filter(tramo_edad == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre06))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:7){
  a_e_edad[i] <- a_edad(x = i)
}     

c_edad <- as.data.frame(a_e_edad)


# Sexo

a_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x) %>%

    srvyr::summarise(colname = srvyr::survey_mean(pobre06))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

c_sexo <- as.data.frame(a_e_sexo)

# Ascendencia afro

a_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre06))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

c_afro <- as.data.frame(a_e_afro)


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 122
NOMINDICADOR <- "Personas en situación de pobreza (% de la población total) (INE)"
CATEGORIA    <- "Pobreza"
CORTE_NUEVA  <- c("Total",
                  "Total",
                  "Total",
                  "Total",
                  "Edad",
                  "Edad",
                  "Edad",
                  "Edad",
                  "Edad",
                  "Edad",
                  "Edad",
                  "Sexo",
                  "Sexo",
                  "Ascendencia étnico-racial",
                  "Ascendencia étnico-racial")
REGION       <- c("Total País",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
TRAMO         <- c("Todos", 
                  "Todos",
                  "Todos",
                  "Todos",
                  "De 0 a 5 años",
                  "De 6 a 12 años",
                  "De 13 a 18 años",
                  "De 19 a 24 años",
                  "De 25 a 29 años",
                  "De 30 a 64 años",
                  "De 65 años y más",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
SEXO          <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Varones",
                  "Mujeres",
                  "Todos",
                  "Todos")
ASCENDENCIA  <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Afro",
                   "No afro")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_region, a_e_edad, a_e_sexo, a_e_afro)
RESPONSABLE  <- "JIMENA PANDOLFI"



m122 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	




### 123	Hogares en situación de indigencia (% de la población total) (INE) --------------------------------

# Total

a_est <- ech_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(indig06))

a_ano<- mean(as.numeric(a_est$colname))

c_ano <- a_ano

# Region

a_region <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(indig06))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}     

c_region <- as.data.frame(a_e_region)


# Sexo del jefe

a_jefe <- function(x) {
  x <- ech_h_svy %>%
    filter(sexojefe == x) %>%

    srvyr::summarise(colname = srvyr::survey_mean(indig06))
  x <- mean(x$colname)
}       

a_e_jefe <- numeric()

for(i in 1:2){
  a_e_jefe[i] <- a_jefe(x = i)
}     

c_jefe <- as.data.frame(a_e_jefe)


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 123
NOMINDICADOR <- "Hogares en situación de indigencia (% de la población total) (INE)"
CATEGORIA    <- "Pobreza"
CORTE_NUEVA  <- c("Total",
                  "Total",
                  "Total",
                  "Total",
                  "Sexo del jefe(a)",
                  "Sexo del jefe(a)")
REGION       <- c("Total País",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos")
SEXOJEFATURA <-c("Todos",
                 "Todos",
                 "Todos",
                 "Todos",
                 "Jefe Varón",
                 "Jefa Mujer")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_region, a_e_jefe)
RESPONSABLE  <- "JIMENA PANDOLFI"


m123 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	

### 124	Personas en situación de indigencia (% de la población total) (INE) --------------------------------

# Total

a_est <- ech_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(indig06))

a_ano<- mean(as.numeric(a_est$colname))

c_ano <- a_ano

# Region

a_region <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x) %>%

    srvyr::summarise(colname = srvyr::survey_mean(indig06))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}     

c_region <- as.data.frame(a_e_region)


# Edad

a_edad <- function(x) {
  x <- ech_svy %>%
    filter(tramo_edad == x) %>%

    srvyr::summarise(colname = srvyr::survey_mean(indig06))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:7){
  a_e_edad[i] <- a_edad(x = i)
}     

c_edad <- as.data.frame(a_e_edad)

# Sexo

a_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(indig06))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

c_sexo <- as.data.frame(a_e_sexo)


# Ascendencia afro

a_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(indig06))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

c_afro <- as.data.frame(a_e_afro)


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 124
NOMINDICADOR <- "Personas en situación de indigencia (% de la población total) (INE)"
CATEGORIA    <- "Pobreza"
CORTE_NUEVA  <- c("Total",
                  "Total",
                  "Total",
                  "Total",
                  "Edad",
                  "Edad",
                  "Edad",
                  "Edad",
                  "Edad",
                  "Edad",
                  "Edad",
                  "Sexo",
                  "Sexo",
                  "Ascendencia étnico-racial",
                  "Ascendencia étnico-racial")
REGION       <- c("Total País",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
TRAMO         <- c("Todos", 
                   "Todos",
                   "Todos",
                   "Todos",
                   "De 0 a 5 años",
                   "De 6 a 12 años",
                   "De 13 a 18 años",
                   "De 19 a 24 años",
                   "De 25 a 29 años",
                   "De 30 a 64 años",
                   "De 65 años y más",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos")
SEXO          <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Varones",
                   "Mujeres",
                   "Todos",
                   "Todos")
ASCENDENCIA  <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Afro",
                  "No afro")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_region, a_e_edad, a_e_sexo, a_e_afro)
RESPONSABLE  <- "JIMENA PANDOLFI"



m124 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	




### 125	Severidad de la pobreza
#-- Manualmente a partir de Indicadores sobre distribución del ingreso - INE --#



### 126	Brecha de pobreza a usd 1,90 por día (2011 ppp) (%)

#Total País
#m126_bm_tp  <- banco_mundial %>% filter(`Area Sp`== "Nacional" & `Countryname Sp` == "Uruguay" & `Indicator Sp` == "Brecha de pobreza" & `Pline Sp` == "Pobreza $1.9 (2011 PPP)")

# Base motor

# CODIND  <- ""
# NOMINDICADOR  <- ""
# CATEGORIA  <- ""
# PESTAÑA  <- ""
# CORTE  <- ""
# CORTE_NUEVA  <- ""
# REGION  <- ""
# TRAMO  <- ""
# SEXOJEFATURA  <- ""
# POBREZA  <- ""
# SEXO  <- ""
# ASCENDENCIA  <- ""
# DECIL  <- ""
# QUINTIL  <- ""
# DEPARTAMENTOUY  <- ""
# 
# PAIS  <- ""
# ANIO  <- ""
# VALOR  <- ""
# RESPONSABLE  <- ""
# 
# CODIND       <- 126
# NOMINDICADOR <- "Brecha de pobreza a usd 1,90 por día (2011 ppp) (%)"
# CATEGORIA    <- "Pobreza"
# CORTE_NUEVA  <- "Total"
# REGION       <- "Todos"
# PAIS         <- "Uruguay"
# ANIO         <- m126_bm_tp$Year
# VALOR        <- m126_bm_tp$Rate
# RESPONSABLE  <- "JIMENA PANDOLFI"
# 
# m126_tp <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	
# 
# 
# #País urbano
# #Total País
# m126_bm_pu  <- banco_mundial %>% filter(`Area Sp`== "Urbano" & `Countryname Sp` == "Uruguay" & `Indicator Sp` == "Brecha de pobreza" & `Pline Sp` == "Pobreza $1.9 (2011 PPP)")
# 
# # Base motor
# 
# CODIND  <- ""
# NOMINDICADOR  <- ""
# CATEGORIA  <- ""
# PESTAÑA  <- ""
# CORTE  <- ""
# CORTE_NUEVA  <- ""
# REGION  <- ""
# TRAMO  <- ""
# SEXOJEFATURA  <- ""
# POBREZA  <- ""
# SEXO  <- ""
# ASCENDENCIA  <- ""
# DECIL  <- ""
# QUINTIL  <- ""
# DEPARTAMENTOUY  <- ""
# 
# PAIS  <- ""
# ANIO  <- ""
# VALOR  <- ""
# RESPONSABLE  <- ""
# 
# CODIND       <- 126
# NOMINDICADOR <- "Brecha de pobreza a usd 1,90 por día (2011 ppp) (%)"
# CATEGORIA    <- "Pobreza"
# CORTE_NUEVA  <- "Región"
# REGION       <- "Urbano (menos de 5.000 habitantes)"
# PAIS         <- "Uruguay"
# ANIO         <- m126_bm_pu$Year
# VALOR        <- m126_bm_pu$Rate
# RESPONSABLE  <- "JIMENA PANDOLFI"
# 
# m126_pu <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	
# 
# m126 <- rbind(m126_pu,m126_tp)
# 
# 
# 
# ### 127	Brecha de pobreza a usd 3,20 por día (2011 ppp) (%)
# 
# m127_bm_tp  <- banco_mundial %>% filter(`Area Sp`== "Nacional" & `Countryname Sp` == "Uruguay" & `Indicator Sp` == "Brecha de pobreza" & `Pline Sp` == "Pobreza $3.2 (2011 PPP)")
# 
# # Base motor
# 
# CODIND  <- ""
# NOMINDICADOR  <- ""
# CATEGORIA  <- ""
# PESTAÑA  <- ""
# CORTE  <- ""
# CORTE_NUEVA  <- ""
# REGION  <- ""
# TRAMO  <- ""
# SEXOJEFATURA  <- ""
# POBREZA  <- ""
# SEXO  <- ""
# ASCENDENCIA  <- ""
# DECIL  <- ""
# QUINTIL  <- ""
# DEPARTAMENTOUY  <- ""
# PAIS  <- ""
# ANIO  <- ""
# VALOR  <- ""
# RESPONSABLE  <- ""
# 
# CODIND       <- 127
# NOMINDICADOR <- "Brecha de pobreza a usd 3,20 por día (2011 ppp) (%)"
# CATEGORIA    <- "Pobreza"
# CORTE_NUEVA  <- "Total"
# REGION       <- "Todos"
# PAIS         <- "Uruguay"
# ANIO         <- m127_bm_tp$Year
# VALOR        <- m127_bm_tp$Rate
# RESPONSABLE  <- "JIMENA PANDOLFI"
# 
# m127_tp <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	
# 
# 
# #País urbano
# 
# m127_bm_pu  <- banco_mundial %>% filter(`Area Sp`== "Urbano" & `Countryname Sp` == "Uruguay" & `Indicator Sp` == "Brecha de pobreza" & `Pline Sp` == "Pobreza $3.2 (2011 PPP)")
# 
# # Base motor
# 
# CODIND  <- ""
# NOMINDICADOR  <- ""
# CATEGORIA  <- ""
# PESTAÑA  <- ""
# CORTE  <- ""
# CORTE_NUEVA  <- ""
# REGION  <- ""
# TRAMO  <- ""
# SEXOJEFATURA  <- ""
# POBREZA  <- ""
# SEXO  <- ""
# ASCENDENCIA  <- ""
# DECIL  <- ""
# QUINTIL  <- ""
# DEPARTAMENTOUY  <- ""
# PAIS  <- ""
# ANIO  <- ""
# VALOR  <- ""
# RESPONSABLE  <- ""
# 
# CODIND       <- 127
# NOMINDICADOR <- "Brecha de pobreza a usd 3,20 por día (2011 ppp) (%)"
# CATEGORIA    <- "Pobreza"
# CORTE_NUEVA  <- "Total"
# REGION       <- "Todos"
# PAIS         <- "Uruguay"
# ANIO         <- m127_bm_pu$Year
# VALOR        <- m127_bm_pu$Rate
# RESPONSABLE  <- "JIMENA PANDOLFI"
# 
# m127_pu <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	
# 
# 
# m127 <- rbind (m127_pu, m127_tp)
# 
# ### 128	Brecha de pobreza a usd 5,50 por día (2011 ppp) (%)
# 
# 
# m128_bm_tp  <- banco_mundial %>% filter(`Area Sp`== "Nacional" & `Countryname Sp` == "Uruguay" & `Indicator Sp` == "Brecha de pobreza" & `Pline Sp` == "Pobreza $5.5 (2011 PPP)")
# 
# # Base motor
# 
# CODIND  <- ""
# NOMINDICADOR  <- ""
# CATEGORIA  <- ""
# PESTAÑA  <- ""
# CORTE  <- ""
# CORTE_NUEVA  <- ""
# REGION  <- ""
# TRAMO  <- ""
# SEXOJEFATURA  <- ""
# POBREZA  <- ""
# SEXO  <- ""
# ASCENDENCIA  <- ""
# DECIL  <- ""
# QUINTIL  <- ""
# DEPARTAMENTOUY  <- ""
# PAIS  <- ""
# ANIO  <- ""
# VALOR  <- ""
# RESPONSABLE  <- ""
# 
# CODIND       <- 128
# NOMINDICADOR <- "Brecha de pobreza a usd 5,50 por día (2011 ppp) (%)"
# CATEGORIA    <- "Pobreza"
# CORTE_NUEVA  <- "Total"
# REGION       <- "Todos"
# PAIS         <- "Uruguay"
# ANIO         <- m128_bm_tp$Year
# VALOR        <- m128_bm_tp$Rate
# RESPONSABLE  <- "JIMENA PANDOLFI"
# 
# m128_tp <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	
# 
# 
# 
# #Pais urbano
# 
# m128_bm_pu  <- banco_mundial %>% filter(`Area Sp`== "Urbano" & `Countryname Sp` == "Uruguay" & `Indicator Sp` == "Brecha de pobreza" & `Pline Sp` == "Pobreza $5.5 (2011 PPP)")
# 
# # Base motor
# 
# CODIND  <- ""
# NOMINDICADOR  <- ""
# CATEGORIA  <- ""
# PESTAÑA  <- ""
# CORTE  <- ""
# CORTE_NUEVA  <- ""
# REGION  <- ""
# TRAMO  <- ""
# SEXOJEFATURA  <- ""
# POBREZA  <- ""
# SEXO  <- ""
# ASCENDENCIA  <- ""
# DECIL  <- ""
# QUINTIL  <- ""
# DEPARTAMENTOUY  <- ""
# PAIS  <- ""
# ANIO  <- ""
# VALOR  <- ""
# RESPONSABLE  <- ""
# 
# CODIND       <- 128
# NOMINDICADOR <- "Brecha de pobreza a usd 5,50 por día (2011 ppp) (%)"
# CATEGORIA    <- "Pobreza"
# CORTE_NUEVA  <- "Total"
# REGION       <- "Todos"
# PAIS         <- "Uruguay"
# ANIO         <- m128_bm_pu$Year
# VALOR        <- m128_bm_pu$Rate
# RESPONSABLE  <- "JIMENA PANDOLFI"
# 
# m128_pu <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	
# 
# 
# 
# m128 <- rbind(m128_tp, m128_pu)
# 
# 

### 131	Índice de Gini -------------------------------------------------------------------------------------

#-- Manualmente a partir de INE --#



### 132 Distribución porcentual de Ingresos medios per cápita apropiado según quintil

a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy_obs == x & bd_region == 1) %>%

    srvyr::summarise(colname = srvyr::survey_mean(y_pc))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

m132_pu <- a_e_quintil/sum(a_e_quintil)

a_e_quintil_pu <- as.data.frame(a_e_quintil)


a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy_obs == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(y_pc))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

m132_tp <- a_e_quintil/sum(a_e_quintil)
a_e_quintil_tp <- as.data.frame(a_e_quintil)


###BASE MOTOR
CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 132
NOMINDICADOR <- "Distribución porcentual de Ingresos medios per cápita apropiado según quintil"
CATEGORIA    <- "Ingresos y desigualdad"
PESTAÑA      <- c("País Urbano",
                  "País Urbano",
                  "País Urbano",
                  "País Urbano",
                  "País Urbano",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País")
CORTE <- "Quintil de ingreso y región"
CORTE_NUEVA <- "Quintil de ingreso"
REGION       <- c("Urbano (más de 5.000 habitantes)",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (más de 5.000 habitantes)",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País")
URBANORURALUY       <- c("Urbano (más de 5.000 habitantes)",
                         "Urbano (más de 5.000 habitantes)",
                         "Urbano (más de 5.000 habitantes)",
                         "Urbano (más de 5.000 habitantes)",
                         "Urbano (más de 5.000 habitantes)",
                         "Total País",
                         "Total País",
                         "Total País",
                         "Total País",
                         "Total País")
POBREZA      <- c("Todos")
QUINTIL      <- c("Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(m132_pu, m132_tp)*100
RESPONSABLE  <- "JIMENA PANDOLFI"


m132 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	


### 133 Ingreso total acumulado apropiado según decil

a_decil <- function(x) {
  x <- ech_h_svy %>%
    filter(decilesy_obs == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(y_pc))
  x <- mean(x$colname)
}       

a_e_decil <- numeric()

for(i in 1:10){
  a_e_decil[i] <- a_decil(x = i)
}     

m133_tp <- a_e_decil/sum(a_e_decil)

a_e_decil_tp <- as.data.frame(a_e_decil)

##País urbano (insumo para indicador 134)
a_decil <- function(x) {
  x <- ech_h_svy %>%
    filter(decilesy_obs == x & bd_region == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(y_pc))
  x <- mean(x$colname)
}       

a_e_decil <- numeric()

for(i in 1:10){
  a_e_decil[i] <- a_decil(x = i)
}     

m133_pu <- a_e_decil/sum(a_e_decil)

a_e_decil_pu <- as.data.frame(a_e_decil)


###BASE MOTOR
CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""


CODIND       <- 133
NOMINDICADOR <- "Ingreso total acumulado apropiado según decil"
CATEGORIA    <- "Ingresos y desigualdad"
PESTAÑA      <- c("Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País")
CORTE <- "Decil de ingreso"
CORTE_NUEVA <- "Decil de ingreso"
REGION       <- c("Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País",
                  "Total País")
URBANORURALUY       <- c("Total País",
                         "Total País",
                         "Total País",
                         "Total País",
                         "Total País",
                         "Total País",
                         "Total País",
                         "Total País",
                         "Total País",
                         "Total País")
DECIL      <- c("Decil 1",
                "Decil 2",
                "Decil 3",
                "Decil 4",
                "Decil 5",
                "Decil 6",
                "Decil 7",
                "Decil 8",
                "Decil 9",
                "Decil 10")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(m133_tp)
RESPONSABLE  <- "JIMENA PANDOLFI"


m133 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY, PAIS,	ANIO,	VALOR,	RESPONSABLE)	


### 134 Relación entre el Ingreso medio per cápita del primer y décimo decil
m134_pu <- a_e_decil_pu[10,1]/a_e_decil_pu[1,1]
m134_tp <- a_e_decil_tp[10,1]/a_e_decil_tp[1,1]

###BASE MOTOR
CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""


CODIND       <- 134
NOMINDICADOR <- "Relación entre el Ingreso medio per cápita del primer y décimo decil"
CATEGORIA    <- "Ingresos y desigualdad"
PESTAÑA      <- c("Total País",
                  "País Urbano")
CORTE <- "Región"
CORTE_NUEVA <- "Total"
REGION       <- c("Total País",
                  "Urbano (más de 5.000 habitantes)")
URBANORURALUY       <- c("Total País",
                         "Urbano (más de 5.000 habitantes)")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(m134_tp, m134_pu)
RESPONSABLE  <- "JIMENA PANDOLFI"

m134 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	




### 135 Relación entre el Ingreso medio per cápita del primer y quinto quintil
m135_pu <- a_e_quintil_pu[5,1]/a_e_quintil_pu[1,1]
m135_tp <- a_e_quintil_tp[5,1]/a_e_quintil_tp[1,1]

###BASE MOTOR
CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""


CODIND       <- 135
NOMINDICADOR <- "Relación entre el Ingreso medio per cápita del primer y quinto quintil"
CATEGORIA    <- "Ingresos y desigualdad"
PESTAÑA      <- c("Total País",
                  "País Urbano")
CORTE <- "Región"
CORTE_NUEVA <- "Total"
REGION       <- c("Total País",
                  "Urbano (más de 5.000 habitantes)")
URBANORURALUY       <- c("Total País",
                         "Urbano (más de 5.000 habitantes)")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(m135_tp, m135_pu)
RESPONSABLE  <- "JIMENA PANDOLFI"

m135 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY, PAIS,	ANIO,	VALOR,	RESPONSABLE)	


### 141	Hogares con necesidades básicas insatisfechas (met 2011)(% de la población total) ------------------

## Total País

# Total

a_ano <- ech_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_2011))
a_ano <- as.numeric(a_ano$colname)


# Region

a_region <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_2011))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}         


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_2011))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         


# Situación de pobreza del hogar

a_pobreza <- function(x) {
  x <- ech_h_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_2011))
  x <- mean(x$colname)
}       

a_e_pobreza <- numeric()

for(i in 1:2){
  a_e_pobreza[i] <- a_pobreza(x = i)
}         


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 141
NOMINDICADOR <- "Hogares con necesidades básicas insatisfechas (met 2011)(% de la población total)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "Total País"
CORTE_NUEVA  <- c("Total",
                  "Región",
                  "Región",
                  "Región",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Pobreza",
                  "Pobreza")
REGION       <- c("Todos",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Pobre",
                  "No pobre")
QUINTIL      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(a_ano, a_e_region, a_e_quintil, a_e_pobreza)
RESPONSABLE  <- "JIMENA PANDOLFI"


m141 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	



### 142	Personas en hogares con necesidades básicas insatisfechas (met 2011) (% de la población total) (INE)------------------


## Total País

# Total

a_ano <- ech_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_2011))
a_ano <- as.numeric(a_ano$colname)


# Region

a_region <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_2011))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}         


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_2011))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         


# Situación de pobreza del hogar

a_pobreza <- function(x) {
  x <- ech_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_2011))
  x <- mean(x$colname)
}       

a_e_pobreza <- numeric()

for(i in 1:2){
  a_e_pobreza[i] <- a_pobreza(x = i)
}         


# Tramo de edad

a_edad <- function(x) {
  x <- ech_svy %>%
    filter(tramo_edad == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_2011))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:7){
  a_e_edad[i] <- a_edad(x = i)
}         

# Sexo

a_sexo <- function(x) {
  x <- ech_svy %>%
    filter(e26 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_2011))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}    


# Ascendencia afro

a_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_2011))
  x <- mean(x$colname)
}          

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}    


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 142
NOMINDICADOR <- "Personas en hogares con necesidades básicas insatisfechas (met 2011) (% de la población total) (INE)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "Total País"
CORTE_NUEVA  <- c("Total",
                  "Región",
                  "Región",
                  "Región",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Pobreza",
                  "Pobreza",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Sexo",
                  "Sexo",
                  "Ascendencia étnico-racial",
                  "Ascendencia étnico-racial")
REGION       <- c("Todos",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Pobre",
                  "No pobre",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
QUINTIL      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
TRAMO        <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "De 0 a 5 años",
                  "De 6 a 12 años",
                  "De 13 a 18 años",
                  "De 19 a 24 años",
                  "De 25 a 29 años",
                  "De 30 a 64 años",
                  "De 65 años y más",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
SEXO          <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "varones",
                  "Mujeres",
                  "Todos",
                  "Todos")
ASCENDENCIA   <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Afro",
                   "No afro")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(a_ano, a_e_region, a_e_quintil, a_e_pobreza, a_e_edad, a_e_sexo, a_e_afro)
RESPONSABLE  <- "JIMENA PANDOLFI"


m142 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	




### 143	Hogares con cinco o más nbi(% de la población total)
### 143	Hogares con cuatro nbi(% de la población total)
### 143	Hogares con dos nbi(% de la población total)
### 143	Hogares con tres nbi(% de la población total)
### 143	Hogares con una nbi(% de la población total)
### 143	Hogares sin nbi(% de la población total)



### 145	Hogares con hacinamiento (% de la población total) (def. CELADE) -----------------------------------------------------------

## Total País


# Total

a_ano <- ech_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento2))
a_ano <- as.numeric(a_ano$colname)


# Region

a_region <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento2))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}         


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento2))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         


# Situación de pobreza del hogar

a_pobreza <- function(x) {
  x <- ech_h_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento2))
  x <- mean(x$colname)
}       

a_e_pobreza <- numeric()

for(i in 1:2){
  a_e_pobreza[i] <- a_pobreza(x = i)
}         


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 145
NOMINDICADOR <- "Hogares con hacinamiento (% de la población total) (def. CELADE)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "Total País"
CORTE_NUEVA  <- c("Total",
                  "Región",
                  "Región",
                  "Región",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Pobreza",
                  "Pobreza")
REGION       <- c("Todos",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Pobre",
                  "No pobre")
QUINTIL      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(a_ano, a_e_region, a_e_quintil, a_e_pobreza)
RESPONSABLE  <- "JIMENA PANDOLFI"


m145_tp <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	


## País Urbano


# Total

a_ano <- ech_h_svy %>%
  srvyr::filter(bd_region == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento2))
a_ano <- as.numeric(a_ano$colname)


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_h_svy %>%
    srvyr::filter(bd_region == 1) %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento2))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         



# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 145
NOMINDICADOR <- "Hogares con hacinamiento (% de la población total) (def. CELADE)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "País Urbano"
CORTE_NUEVA  <- c("Total",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso")
REGION       <- ""
QUINTIL      <- c("Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5")
POBREZA       <- ""
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(a_ano, a_e_quintil)
RESPONSABLE  <- "JIMENA PANDOLFI"


m145_pu <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	

m145 <- rbind(m145_pu, m145_tp)


### 146	Personas en hogares con hacinamiento (% de la población total) (def. CELADE) ---------------------------------------------


## Total País

# Total

a_ano <- ech_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento2))
a_ano <- as.numeric(a_ano$colname)


# Region

a_region <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento2))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}         


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento2))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         


# Situación de pobreza del hogar

a_pobreza <- function(x) {
  x <- ech_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento2))
  x <- mean(x$colname)
}       

a_e_pobreza <- numeric()

for(i in 1:2){
  a_e_pobreza[i] <- a_pobreza(x = i)
}         


# Tramo de edad

a_edad <- function(x) {
  x <- ech_svy %>%
    filter(tramo_edad == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento2))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:7){
  a_e_edad[i] <- a_edad(x = i)
}         

# Sexo

a_sexo <- function(x) {
  x <- ech_svy %>%
    filter(e26 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento2))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}    


# Ascendencia afro

a_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento2))
  x <- mean(x$colname)
}          

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}    


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 146
NOMINDICADOR <- "Personas en hogares con hacinamiento (% de la población total) (def. CELADE)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "Total País"
CORTE_NUEVA  <- c("Total",
                  "Región",
                  "Región",
                  "Región",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Pobreza",
                  "Pobreza",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Sexo",
                  "Sexo",
                  "Ascendencia étnico-racial",
                  "Ascendencia étnico-racial")
REGION       <- c("Todos",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Pobre",
                  "No pobre",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
QUINTIL      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
TRAMO        <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "De 0 a 5 años",
                  "De 6 a 12 años",
                  "De 13 a 18 años",
                  "De 19 a 24 años",
                  "De 25 a 29 años",
                  "De 30 a 64 años",
                  "De 65 años y más",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
SEXO          <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "varones",
                   "Mujeres",
                   "Todos",
                   "Todos")
ASCENDENCIA   <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Afro",
                   "No afro")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(a_ano, a_e_region, a_e_quintil, a_e_pobreza, a_e_edad, a_e_sexo, a_e_afro)
RESPONSABLE  <- "JIMENA PANDOLFI"

m146_tp <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	



## País urbano

# Total

a_ano <- ech_svy %>%
  srvyr::filter(bd_region == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento2))
a_ano <- as.numeric(a_ano$colname)


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x) %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento2))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         


# Tramo de edad

a_edad <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == 1) %>%
    filter(tramo_edad == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento2))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:7){
  a_e_edad[i] <- a_edad(x = i)
}         

# Sexo

a_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == 1) %>%
    filter(e26 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento2))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}    



# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 146
NOMINDICADOR <- "Personas en hogares con hacinamiento (% de la población total) (def. CELADE)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "País Urbano"
CORTE_NUEVA  <- c("Total",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Sexo",
                  "Sexo")
REGION       <- c("")
POBREZA      <- c("")
QUINTIL      <- c("Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
TRAMO        <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "De 0 a 5 años",
                  "De 6 a 12 años",
                  "De 13 a 18 años",
                  "De 19 a 24 años",
                  "De 25 a 29 años",
                  "De 30 a 64 años",
                  "De 65 años y más",
                  "Todos",
                  "Todos")
SEXO          <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "varones",
                   "Mujeres")
ASCENDENCIA   <- c("")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(a_ano, a_e_quintil, a_e_edad, a_e_sexo)
RESPONSABLE  <- "JIMENA PANDOLFI"

m146_pu <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	

m146 <- rbind(m146_pu, m146_tp)


### 147	Hogares con hacinamiento (% de la población total) (def. INE) --------------------------------------------------------------------


## Total País


# Total

a_ano <- ech_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento1))
a_ano <- as.numeric(a_ano$colname)


# Region

a_region <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento1))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}         


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento1))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         


# Situación de pobreza del hogar

a_pobreza <- function(x) {
  x <- ech_h_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento1))
  x <- mean(x$colname)
}       

a_e_pobreza <- numeric()

for(i in 1:2){
  a_e_pobreza[i] <- a_pobreza(x = i)
}         


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 147
NOMINDICADOR <- "Hogares con hacinamiento (% de la población total) (def. INE)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "Total País"
CORTE_NUEVA  <- c("Total",
                  "Región",
                  "Región",
                  "Región",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Pobreza",
                  "Pobreza")
REGION       <- c("Todos",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Pobre",
                  "No pobre")
QUINTIL      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(a_ano, a_e_region, a_e_quintil, a_e_pobreza)
RESPONSABLE  <- "JIMENA PANDOLFI"


m147_tp <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	


## País Urbano


# Total

a_ano <- ech_h_svy %>%
  srvyr::filter(bd_region == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento1))
a_ano <- as.numeric(a_ano$colname)


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_h_svy %>%
    srvyr::filter(bd_region == 1) %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento1))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         



# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 147
NOMINDICADOR <- "Hogares con hacinamiento (% de la población total) (def. INE)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "País Urbano"
CORTE_NUEVA  <- c("Total",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso")
REGION       <- ""
QUINTIL      <- c("Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5")
POBREZA       <- ""
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(a_ano, a_e_quintil)
RESPONSABLE  <- "JIMENA PANDOLFI"

m147_pu <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	

m147 <- rbind(m147_pu, m147_tp)


### 148	Personas en hogares con hacinamiento (% de la población total) (def. INE) --------------------------------------------------------


## Total País

# Total

a_ano <- ech_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento1))
a_ano <- as.numeric(a_ano$colname)


# Region

a_region <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento1))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}         


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento1))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         


# Situación de pobreza del hogar

a_pobreza <- function(x) {
  x <- ech_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento1))
  x <- mean(x$colname)
}       

a_e_pobreza <- numeric()

for(i in 1:2){
  a_e_pobreza[i] <- a_pobreza(x = i)
}         


# Tramo de edad

a_edad <- function(x) {
  x <- ech_svy %>%
    filter(tramo_edad == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento1))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:7){
  a_e_edad[i] <- a_edad(x = i)
}         

# Sexo

a_sexo <- function(x) {
  x <- ech_svy %>%
    filter(e26 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento1))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}    


# Ascendencia afro

a_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento1))
  x <- mean(x$colname)
}          

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}    


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 148
NOMINDICADOR <- "Personas en hogares con hacinamiento (% de la población total) (def. INE)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "Total País"
CORTE_NUEVA  <- c("Total",
                  "Región",
                  "Región",
                  "Región",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Pobreza",
                  "Pobreza",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Sexo",
                  "Sexo",
                  "Ascendencia étnico-racial",
                  "Ascendencia étnico-racial")
REGION       <- c("Todos",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Pobre",
                  "No pobre",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
QUINTIL      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
TRAMO        <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "De 0 a 5 años",
                  "De 6 a 12 años",
                  "De 13 a 18 años",
                  "De 19 a 24 años",
                  "De 25 a 29 años",
                  "De 30 a 64 años",
                  "De 65 años y más",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
SEXO          <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "varones",
                   "Mujeres",
                   "Todos",
                   "Todos")
ASCENDENCIA   <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Afro",
                   "No afro")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(a_ano, a_e_region, a_e_quintil, a_e_pobreza, a_e_edad, a_e_sexo, a_e_afro)
RESPONSABLE  <- "JIMENA PANDOLFI"


m148_tp <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	


## País urbano

# Total

a_ano <- ech_svy %>%
  filter(bd_region == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento1))
a_ano <- as.numeric(a_ano$colname)


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == 1) %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento1))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         


# Tramo de edad

a_edad <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == 1) %>%
    filter(tramo_edad == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento1))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:7){
  a_e_edad[i] <- a_edad(x = i)
}         

# Sexo

a_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == 1) %>%
    filter(e26 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_hacinamiento1))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}    



# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 148
NOMINDICADOR <- "Personas en hogares con hacinamiento (% de la población total) (def. INE)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "País Urbano"
CORTE_NUEVA  <- c("Total",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Sexo",
                  "Sexo")
REGION       <- c("")
POBREZA      <- c("")
QUINTIL      <- c("Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
TRAMO        <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "De 0 a 5 años",
                  "De 6 a 12 años",
                  "De 13 a 18 años",
                  "De 19 a 24 años",
                  "De 25 a 29 años",
                  "De 30 a 64 años",
                  "De 65 años y más",
                  "Todos",
                  "Todos")
SEXO          <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "varones",
                   "Mujeres")
ASCENDENCIA   <- c("")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(a_ano, a_e_quintil, a_e_edad, a_e_sexo)
RESPONSABLE  <- "JIMENA PANDOLFI"

m148_pu <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	

m148 <- rbind(m148_pu, m148_tp)

### 149	Hogares residiendo en viviendas con paredes o techos de desecho o piso de tierra (% de la población total) -----------------------

## Total País

# Total

a_ano <- ech_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_materialidad11))
a_ano <- as.numeric(a_ano$colname)


# Region

a_region <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_materialidad11))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}         


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_materialidad11))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         


# Situación de pobreza del hogar

a_pobreza <- function(x) {
  x <- ech_h_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_materialidad11))
  x <- mean(x$colname)
}       

a_e_pobreza <- numeric()

for(i in 1:2){
  a_e_pobreza[i] <- a_pobreza(x = i)
}         


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 149
NOMINDICADOR <- "Hogares residiendo en viviendas con paredes o techos de desecho o piso de tierra (% de la población total)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "Total País"
CORTE_NUEVA  <- c("Total",
                  "Región",
                  "Región",
                  "Región",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Pobreza",
                  "Pobreza")
REGION       <- c("Todos",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Pobre",
                  "No pobre")
QUINTIL      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(a_ano, a_e_region, a_e_quintil, a_e_pobreza)
RESPONSABLE  <- "JIMENA PANDOLFI"

m149 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	




### 150	Personas residiendo en viviendas con paredes o techos de desecho o piso de tierra (% de la población total) ----------------------

## Total País

# Total

c_ano <- ech_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_materialidad11))
c_ano <- as.numeric(c_ano$colname)


# Region

b_region <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_materialidad11))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- b_region(x = i)
}         


# Quintil de ingreso del hogar


b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_materialidad11))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- b_quintil(x = i)
}         


# Situación de pobreza del hogar

b_pobreza <- function(x) {
  x <- ech_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_materialidad11))
  x <- mean(x$colname)
}       

a_e_pobreza <- numeric()

for(i in 1:2){
  a_e_pobreza[i] <- b_pobreza(x = i)
}         


# Tramo de edad

b_edad <- function(x) {
  x <- ech_svy %>%
    filter(tramo_edad == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_materialidad11))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:7){
  a_e_edad[i] <- b_edad(x = i)
}         

# Sexo

b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(e26 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_materialidad11))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- b_sexo(x = i)
}    


# Ascendencia afro

b_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_materialidad11))
  x <- mean(x$colname)
}          

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- b_afro(x = i)
}    


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 150
NOMINDICADOR <- "Personas residiendo en viviendas con paredes o techos de desecho o piso de tierra (% de la población total)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "Total País"
CORTE_NUEVA  <- c("Total",
                  "Región",
                  "Región",
                  "Región",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Pobreza",
                  "Pobreza",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Sexo",
                  "Sexo",
                  "Ascendencia étnico-racial",
                  "Ascendencia étnico-racial")
REGION       <- c("Todos",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Pobre",
                  "No pobre",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
QUINTIL      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
TRAMO        <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "De 0 a 5 años",
                  "De 6 a 12 años",
                  "De 13 a 18 años",
                  "De 19 a 24 años",
                  "De 25 a 29 años",
                  "De 30 a 64 años",
                  "De 65 años y más",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
SEXO          <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "varones",
                   "Mujeres",
                   "Todos",
                   "Todos")
ASCENDENCIA   <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Afro",
                   "No afro")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_region, a_e_quintil, a_e_pobreza, a_e_edad, a_e_sexo, a_e_afro)
RESPONSABLE  <- "JIMENA PANDOLFI"

m150 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	





### 151	Hogares sin agua potable (% de la población total) ------------------------------------------------------------------------------- 


# Total

c_ano <- ech_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_agua11))
c_ano <- as.numeric(c_ano$colname)


# Region

b_region <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_agua11))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- b_region(x = i)
}         


# Quintil de ingreso del hogar


b_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_agua11))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- b_quintil(x = i)
}         


# Situación de pobreza del hogar

b_pobreza <- function(x) {
  x <- ech_h_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_agua11))
  x <- mean(x$colname)
}       

a_e_pobreza <- numeric()

for(i in 1:2){
  a_e_pobreza[i] <- b_pobreza(x = i)
}         


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 151
NOMINDICADOR <- "Hogares sin agua potable (% de la población total)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "Total País"
CORTE_NUEVA  <- c("Total",
                  "Región",
                  "Región",
                  "Región",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Pobreza",
                  "Pobreza")
REGION       <- c("Todos",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Pobre",
                  "No pobre")
QUINTIL      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_region, a_e_quintil, a_e_pobreza)
RESPONSABLE  <- "JIMENA PANDOLFI"

m151 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	




### 152	Personas en hogares sin agua potable (% de la población total) -------------------------------------------------------------------

## Total País

# Total

c_ano <- ech_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_agua11))
c_ano <- as.numeric(c_ano$colname)


# Region

a_region <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_agua11))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}         


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_agua11))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         


# Situación de pobreza del hogar

a_pobreza <- function(x) {
  x <- ech_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_agua11))
  x <- mean(x$colname)
}       

a_e_pobreza <- numeric()

for(i in 1:2){
  a_e_pobreza[i] <- a_pobreza(x = i)
}         


# Tramo de edad

a_edad <- function(x) {
  x <- ech_svy %>%
    filter(tramo_edad == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_agua11))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:7){
  a_e_edad[i] <- a_edad(x = i)
}         

# Sexo

a_sexo <- function(x) {
  x <- ech_svy %>%
    filter(e26 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_agua11))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}    


# Ascendencia afro

a_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_agua11))
  x <- mean(x$colname)
}          

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}    


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 152
NOMINDICADOR <- "Personas en hogares sin agua potable (% de la población total)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "Total País"
CORTE_NUEVA  <- c("Total",
                  "Región",
                  "Región",
                  "Región",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Pobreza",
                  "Pobreza",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Sexo",
                  "Sexo",
                  "Ascendencia étnico-racial",
                  "Ascendencia étnico-racial")
REGION       <- c("Todos",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Pobre",
                  "No pobre",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
QUINTIL      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
TRAMO        <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "De 0 a 5 años",
                  "De 6 a 12 años",
                  "De 13 a 18 años",
                  "De 19 a 24 años",
                  "De 25 a 29 años",
                  "De 30 a 64 años",
                  "De 65 años y más",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
SEXO          <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "varones",
                   "Mujeres",
                   "Todos",
                   "Todos")
ASCENDENCIA   <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Afro",
                   "No afro")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_region, a_e_quintil, a_e_pobreza, a_e_edad, a_e_sexo, a_e_afro)
RESPONSABLE  <- "JIMENA PANDOLFI"

m152 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	





### 153	Hogares sin servicio higiénico de calidad(% de la población total) --------------------------------------------------------------- 


# Total

c_ano <- ech_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11))
c_ano <- as.numeric(c_ano$colname)


# Region

a_region <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}         


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         


# Situación de pobreza del hogar

a_pobreza <- function(x) {
  x <- ech_h_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11))
  x <- mean(x$colname)
}       

a_e_pobreza <- numeric()

for(i in 1:2){
  a_e_pobreza[i] <- a_pobreza(x = i)
}         


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 153
NOMINDICADOR <- "Hogares sin servicio higiénico de calidad(% de la población total)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "Total País"
CORTE_NUEVA  <- c("Total",
                  "Región",
                  "Región",
                  "Región",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Pobreza",
                  "Pobreza")
REGION       <- c("Todos",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Pobre",
                  "No pobre")
QUINTIL      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_region, a_e_quintil, a_e_pobreza)
RESPONSABLE  <- "JIMENA PANDOLFI"

m153 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	





### 154	Personas en hogares sin servicio higiénico de calidad(% de la población total) ---------------------------------------------------


## Total País

# Total

c_ano <- ech_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11))
c_ano <- as.numeric(c_ano$colname)


# Region

a_region <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}         


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         


# Situación de pobreza del hogar

a_pobreza <- function(x) {
  x <- ech_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11))
  x <- mean(x$colname)
}       

a_e_pobreza <- numeric()

for(i in 1:2){
  a_e_pobreza[i] <- a_pobreza(x = i)
}         


# Tramo de edad

a_edad <- function(x) {
  x <- ech_svy %>%
    filter(tramo_edad == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:7){
  a_e_edad[i] <- a_edad(x = i)
}         

# Sexo

a_sexo <- function(x) {
  x <- ech_svy %>%
    filter(e26 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}    


# Ascendencia afro

a_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11))
  x <- mean(x$colname)
}          

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}    


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 154
NOMINDICADOR <- "Personas en hogares sin servicio higiénico de calidad(% de la población total)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "Total País"
CORTE_NUEVA  <- c("Total",
                  "Región",
                  "Región",
                  "Región",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Pobreza",
                  "Pobreza",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Sexo",
                  "Sexo",
                  "Ascendencia étnico-racial",
                  "Ascendencia étnico-racial")
REGION       <- c("Todos",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Pobre",
                  "No pobre",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
QUINTIL      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
TRAMO        <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "De 0 a 5 años",
                  "De 6 a 12 años",
                  "De 13 a 18 años",
                  "De 19 a 24 años",
                  "De 25 a 29 años",
                  "De 30 a 64 años",
                  "De 65 años y más",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
SEXO          <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "varones",
                   "Mujeres",
                   "Todos",
                   "Todos")
ASCENDENCIA   <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Afro",
                   "No afro")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_region, a_e_quintil, a_e_pobreza, a_e_edad, a_e_sexo, a_e_afro)
RESPONSABLE  <- "JIMENA PANDOLFI"

m154 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	





### 155	Hogares sin artefactos básicos de confort(% de la población total) ---------------------------------------------------------------



# Total

c_ano <- ech_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos11))
c_ano <- as.numeric(c_ano$colname)


# Region

a_region <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos11))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}         


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos11))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         


# Situación de pobreza del hogar

a_pobreza <- function(x) {
  x <- ech_h_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos11))
  x <- mean(x$colname)
}       

a_e_pobreza <- numeric()

for(i in 1:2){
  a_e_pobreza[i] <- a_pobreza(x = i)
}         


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 155
NOMINDICADOR <- "Hogares sin artefactos básicos de confort(% de la población total)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "Total País"
CORTE_NUEVA  <- c("Total",
                  "Región",
                  "Región",
                  "Región",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Pobreza",
                  "Pobreza")
REGION       <- c("Todos",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Pobre",
                  "No pobre")
QUINTIL      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_region, a_e_quintil, a_e_pobreza)
RESPONSABLE  <- "JIMENA PANDOLFI"

m155 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	





### 156	Personas en hogares sin artefactos básicos de confort(% de la población total) ---------------------------------------------------

## Total País

# Total

c_ano <- ech_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos11))
c_ano <- as.numeric(c_ano$colname)


# Region

a_region <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos11))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}         


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos11))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         


# Situación de pobreza del hogar

a_pobreza <- function(x) {
  x <- ech_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos11))
  x <- mean(x$colname)
}       

a_e_pobreza <- numeric()

for(i in 1:2){
  a_e_pobreza[i] <- a_pobreza(x = i)
}         


# Tramo de edad

a_edad <- function(x) {
  x <- ech_svy %>%
    filter(tramo_edad == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos11))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:7){
  a_e_edad[i] <- a_edad(x = i)
}         

# Sexo

a_sexo <- function(x) {
  x <- ech_svy %>%
    filter(e26 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos11))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}    


# Ascendencia afro

a_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos11))
  x <- mean(x$colname)
}          

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}    


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 156
NOMINDICADOR <- "Personas en hogares sin artefactos básicos de confort(% de la población total)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "Total País"
CORTE_NUEVA  <- c("Total",
                  "Región",
                  "Región",
                  "Región",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Pobreza",
                  "Pobreza",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Sexo",
                  "Sexo",
                  "Ascendencia étnico-racial",
                  "Ascendencia étnico-racial")
REGION       <- c("Todos",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Pobre",
                  "No pobre",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
QUINTIL      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
TRAMO        <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "De 0 a 5 años",
                  "De 6 a 12 años",
                  "De 13 a 18 años",
                  "De 19 a 24 años",
                  "De 25 a 29 años",
                  "De 30 a 64 años",
                  "De 65 años y más",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
SEXO          <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "varones",
                   "Mujeres",
                   "Todos",
                   "Todos")
ASCENDENCIA   <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Afro",
                   "No afro")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_region, a_e_quintil, a_e_pobreza, a_e_edad, a_e_sexo, a_e_afro)
RESPONSABLE  <- "JIMENA PANDOLFI"

m156 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	



### 157	Hogares con miembros de 4 a 17 años que no asisten a centros educativos (% de la población total) --------------------------------


## Total País


# Total

c_ano <- ech_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_educacion11))
c_ano <- as.numeric(c_ano$colname)


# Region

a_region <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_educacion11))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}         


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_educacion11))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         


# Situación de pobreza del hogar

a_pobreza <- function(x) {
  x <- ech_h_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_educacion11))
  x <- mean(x$colname)
}       

a_e_pobreza <- numeric()

for(i in 1:2){
  a_e_pobreza[i] <- a_pobreza(x = i)
}         


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 157
NOMINDICADOR <- "Hogares con miembros de 4 a 17 años que no asisten a centros educativos (% de la población total)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "Total País"
CORTE_NUEVA  <- c("Total",
                  "Región",
                  "Región",
                  "Región",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Pobreza",
                  "Pobreza")
REGION       <- c("Todos",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Pobre",
                  "No pobre")
QUINTIL      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_region, a_e_quintil, a_e_pobreza)
RESPONSABLE  <- "JIMENA PANDOLFI"


m157_tp <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	


## País Urbano


# Total

c_ano <- ech_h_svy %>%
  srvyr::filter(bd_region == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_educacion11))
c_ano <- as.numeric(c_ano$colname)


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_h_svy %>%
    srvyr::filter(bd_region == 1) %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_educacion11))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         



# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""

PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 157
NOMINDICADOR <- "Hogares con miembros de 4 a 17 años que no asisten a centros educativos (% de la población total)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "País Urbano"
CORTE_NUEVA  <- c("Total",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso")
REGION       <- ""
QUINTIL      <- c("Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5")
POBREZA       <- ""
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_quintil)
RESPONSABLE  <- "JIMENA PANDOLFI"

m157_pu <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	

m157 <- rbind(m157_pu, m157_tp)

### 158	Personas en hogares con miembros de 4 a 17 años que no asisten a centros educativos (% de la población total) --------------------


## Total País

# Total

c_ano <- ech_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_educacion11))
c_ano <- as.numeric(c_ano$colname)


# Region

a_region <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_educacion11))
  x <- mean(x$colname)
}       

a_e_region <- numeric()

for(i in 1:3){
  a_e_region[i] <- a_region(x = i)
}         


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_educacion11))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         


# Situación de pobreza del hogar

a_pobreza <- function(x) {
  x <- ech_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_educacion11))
  x <- mean(x$colname)
}       

a_e_pobreza <- numeric()

for(i in 1:2){
  a_e_pobreza[i] <- a_pobreza(x = i)
}         


# Tramo de edad

a_edad <- function(x) {
  x <- ech_svy %>%
    filter(tramo_edad == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_educacion11))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:7){
  a_e_edad[i] <- a_edad(x = i)
}         

# Sexo

a_sexo <- function(x) {
  x <- ech_svy %>%
    filter(e26 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_educacion11))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}    


# Ascendencia afro

a_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_educacion11))
  x <- mean(x$colname)
}          

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}    


# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""
PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 158
NOMINDICADOR <- "Personas en hogares con miembros de 4 a 17 años que no asisten a centros educativos (% de la población total)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "Total País"
CORTE_NUEVA  <- c("Total",
                  "Región",
                  "Región",
                  "Región",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Pobreza",
                  "Pobreza",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Sexo",
                  "Sexo",
                  "Ascendencia étnico-racial",
                  "Ascendencia étnico-racial")
REGION       <- c("Todos",
                  "Urbano (más de 5.000 habitantes)",
                  "Urbano (menos de 5.000 habitantes)",
                  "Rural disperso",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
POBREZA      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Pobre",
                  "No pobre",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
QUINTIL      <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
TRAMO        <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "De 0 a 5 años",
                  "De 6 a 12 años",
                  "De 13 a 18 años",
                  "De 19 a 24 años",
                  "De 25 a 29 años",
                  "De 30 a 64 años",
                  "De 65 años y más",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
SEXO          <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "varones",
                   "Mujeres",
                   "Todos",
                   "Todos")
ASCENDENCIA   <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Afro",
                   "No afro")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_region, a_e_quintil, a_e_pobreza, a_e_edad, a_e_sexo, a_e_afro)
RESPONSABLE  <- "JIMENA PANDOLFI"

m158_tp <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	



## País urbano

# Total

c_ano <- ech_svy %>%
  filter(bd_region == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_educacion11))
c_ano <- as.numeric(c_ano$colname)


# Quintil de ingreso del hogar


a_quintil <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == 1) %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_educacion11))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}         


# Tramo de edad

a_edad <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == 1) %>%
    filter(tramo_edad == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_educacion11))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:7){
  a_e_edad[i] <- a_edad(x = i)
}         

# Sexo

a_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == 1) %>%
    filter(e26 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_educacion11))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}    



# Base motor

CODIND  <- ""
NOMINDICADOR  <- ""
CATEGORIA  <- ""
PESTAÑA  <- ""
CORTE  <- ""
CORTE_NUEVA  <- ""
REGIÓN  <- ""
TRAMO  <- ""
SEXOJEFATURA  <- ""
POBREZA  <- ""
SEXO  <- ""
ASCENDENCIA  <- ""
DECIL  <- ""
QUINTIL  <- ""
DEPARTAMENTOUY  <- ""
PAÍS  <- ""
ANIO  <- ""
VALOR  <- ""
RESPONSABLE  <- ""

CODIND       <- 158
NOMINDICADOR <- "Personas en hogares con miembros de 4 a 17 años que no asisten a centros educativos (% de la población total)"
CATEGORIA    <- "Necesidades Básicas"
PESTAÑA      <- "País Urbano"
CORTE_NUEVA  <- c("Total",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Quintil de ingreso",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Tramo de edad",
                  "Sexo",
                  "Sexo")
REGION       <- c("")
POBREZA      <- c("")
QUINTIL      <- c("Todos",
                  "Quintil 1",
                  "Quintil 2",
                  "Quintil 3",
                  "Quintil 4",
                  "Quintil 5",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos")
TRAMO        <- c("Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "Todos",
                  "De 0 a 5 años",
                  "De 6 a 12 años",
                  "De 13 a 18 años",
                  "De 19 a 24 años",
                  "De 25 a 29 años",
                  "De 30 a 64 años",
                  "De 65 años y más",
                  "Todos",
                  "Todos")
SEXO          <- c("Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "Todos",
                   "varones",
                   "Mujeres")
ASCENDENCIA   <- c("")
PAIS         <- "Uruguay"
ANIO         <- 2023
VALOR        <- c(c_ano, a_e_quintil, a_e_edad, a_e_sexo)
RESPONSABLE  <- "JIMENA PANDOLFI"

m158_pu <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	


m158 <- rbind(m158_pu, m158_tp)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Fusión de base motor ###

## PISO I ##

motor_2023 <- rbind(m111, m113, m121, m122, m123, m124, 
                    m132, m133, m134, 
                    m135, m141, m142, m145, m146 , m147, 
                    m148, m149, m150, m151, m152, m153, 
                    m154, m155, m156, m157, m158 )
motor_2023 <- as.data.frame(motor_2023)
motor_2023 <- motor_2023  %>% mutate(URBANORURALUY = case_when(PESTAÑA == "País Urbano"                                                                            ~ "Urbano (más de 5.000 habitantes)",
                                                               PESTAÑA == "Total País" & CORTE_NUEVA != "Región"                                                   ~  "Total País",
                                                               PESTAÑA == "Total País" & CORTE_NUEVA == "Región" & REGION == "Rural disperso"                      ~ "Rural disperso",
                                                               PESTAÑA == "Total País" & CORTE_NUEVA == "Región" & REGION == "Urbano (más de 5.000 habitantes)"    ~ "Urbano (más de 5.000 habitantes)",
                                                               PESTAÑA == "Total País" & CORTE_NUEVA == "Región" & REGION == "Urbano (menos de 5.000 habitantes)"  ~ "Urbano (menos de 5.000 habitantes)",
                                                               CATEGORIA == "Pobreza" & (REGION == "Todos" |  REGION == "Total País")                              ~  "Total País",
                                                               CATEGORIA == "Pobreza" & (REGION == "Todos" |  REGION == "Total País")                              ~  "Total País",
                                                               CATEGORIA == "Pobreza" &  REGION == "Rural disperso"                                                ~ "Rural disperso",
                                                               CATEGORIA == "Pobreza" &  REGION == "Urbano (más de 5.000 habitantes)"                              ~ "Urbano (más de 5.000 habitantes)",
                                                               CATEGORIA == "Pobreza" &  REGION == "Urbano (menos de 5.000 habitantes)"                            ~ "Urbano (menos de 5.000 habitantes)"))
motor_2023 <- motor_2023  %>% mutate(CORTE = CORTE_NUEVA) 



actualizaINE <- rio::import("Export actualiza 2024/Datos INE_Pobreza_2022y2023.xlsx")
                                       
Base_Motor_Pobreza  <- rio::import("Versión pervia web/Base_Motor_Pobreza.xlsx")

Base_Motor_Pobreza_24  <- rbind(Base_Motor_Pobreza, actualizaINE, motor_2023)

rio::export(Base_Motor_Pobreza_24, "Export actualiza 2024/Base_Motor_Pobreza_08052024.xlsx" )
