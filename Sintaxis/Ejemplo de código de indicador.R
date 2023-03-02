
### Estimación de valores 2021 ###

# Para sintaxix de años anteriores consultar: https://osf.io/dyzg8/files/osfstorage#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Carga de paquetes ###

# Instalar si es necesario:
#install.packages("rio")
#install.packages("srvyr")
#install.packages("tidyverse")

library(rio)
library(srvyr)
library(tidyverse)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Descarga bases ECH 2021 (INE) de repositorio UMAD-FCS ###

download.file(
  url = "https://osf.io/dyzg8/files/osfstorage/63da65232781fc021200aab6", 
  destfile = "ECH2021_sem1.Rdata"
)

download.file(
  url = "https://osf.io/dyzg8/files/osfstorage/63da65616946a002027a4652", 
  destfile = "ECH2021_sem2_implant.Rdata"
)


download.file(
  url = "https://osf.io/dyzg8/files/osfstorage/63da65276946a001fe7a4630", 
  destfile = "ECH2021_sem2_panel.Rdata"
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Carga de bases ###

sem1          <- rio::import("ECH2021_sem1.Rdata")
sem2_implant  <- rio::import("ECH2021_sem2_implant.Rdata")
sem2_panel    <- rio::import("ECH2021_sem2_panel.Rdata")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###

#IPC

sem1 <- sem1 %>% dplyr::mutate(bc_ipc_tot = case_when(mes == 1 ~ 0.335542051,
                                                      mes == 2 ~ 0.330249262,
                                                      mes == 3 ~ 0.327549795,
                                                      mes == 4 ~ 0.32554687,
                                                      mes == 5 ~ 0.323919843,
                                                      mes == 6 ~ 0.322448435))

sem2_implant <- sem2_implant %>% dplyr::mutate(bc_ipc_tot = case_when(
  mes == 7  ~ 0.320314392,
  mes == 8  ~ 0.318657357,
  mes == 9  ~ 0.31596912,
  mes == 10 ~ 0.314515807,
  mes == 11 ~ 0.31128448,
  mes == 12 ~ 0.310493463))


sem1 <- sem1 %>% dplyr::mutate(bc_ipc = case_when(    mes == 1 ~ 0.329708731,
                                                      mes == 2 ~ 0.325120854,
                                                      mes == 3 ~ 0.32193312,
                                                      mes == 4 ~ 0.320163041,
                                                      mes == 5 ~ 0.318357918,
                                                      mes == 6 ~ 0.316774856))

sem2_implant <- sem2_implant %>% dplyr::mutate(bc_ipc = case_when(
  mes == 7 ~  0.314794762,
  mes == 8 ~  0.31306265,
  mes == 9 ~  0.310558149,
  mes == 10 ~ 0.309320866,
  mes == 11 ~ 0.306270364,
  mes == 12 ~ 0.305191859))                                                 


# Ingresos

sem1 <- sem1 %>% dplyr::mutate(y_pc       =  HT11 / ht19 )                      #Ingreso per-cápita
sem1 <- sem1 %>% dplyr::mutate(y_pc_svl   =  (HT11 - HT13) / ht19)              #Ingreso per-cápita sin valor locativo
sem1 <- sem1 %>% dplyr::mutate(y_pc_d     =  (HT11 / ht19) * bc_ipc_tot)          #Ingreso per-cápita deflactado
sem1 <- sem1 %>% dplyr::mutate(y_pc_svl_d =  (HT11 - HT13) / ht19 * bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado

sem2_implant <- sem2_implant %>% dplyr::mutate(y_pc       =  ht11 / ht19 )                      #Ingreso per-cápita
sem2_implant <- sem2_implant %>% dplyr::mutate(y_pc_svl   =  (ht11 - ht13) / ht19)              #Ingreso per-cápita sin valor locativo
sem2_implant <- sem2_implant %>% dplyr::mutate(y_pc_d     =  (ht11 / ht19) * bc_ipc_tot)          #Ingreso per-cápita deflactado
sem2_implant <- sem2_implant %>% dplyr::mutate(y_pc_svl_d =  (ht11 - ht13) / ht19 * bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado

# Región
sem1 <- sem1 %>% dplyr::mutate(bd_region = case_when(region_4 == 1 | region_4 == 2 ~ 1,
                                                     region_4 == 3 ~ 2,
                                                     region_4 == 4 ~ 3))

sem2_implant <- sem2_implant %>% dplyr::mutate(bd_region = case_when(region_4 == 1 | region_4 == 2 ~ 1,
                                                                     region_4 == 3 ~ 2,
                                                                     region_4 == 4 ~ 3))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Bases a nivel hogar ###                               

sem1_h <- sem1 %>% distinct(numero, .keep_all = TRUE)
sem2_implant_h <- sem2_implant %>% distinct(ID, .keep_all = TRUE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ###

sem1 <- sem1 %>% dplyr::mutate(pesosem = pesomen / 6)


sem1_svy           <- srvyr::as_survey_design(sem1, ids = numero, weights = pesomen)

sem1_h_svy         <- srvyr::as_survey_design(sem1_h, ids = numero, weights = pesomen)

sem2_implant_svy   <- srvyr::as_survey_design(sem2_implant, ids = ID, weights = w_sem)

sem2_implant_h_svy <- srvyr::as_survey_design(sem2_implant_h, ids = ID, weights = w_sem)

sem2_panel_svy   <-  srvyr::as_survey_design(sem2_panel, ids = ID, weights = w)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Estimación de indicadores ###


###  Promedio de ingreso per-cápita de los hogares (cte. base diciembre 2006)

a_mes <- sem1_h_svy %>%
  srvyr::filter(bd_region == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(y_pc_d))
a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_h_svy %>%
  srvyr::filter(bd_region == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(y_pc_d))
b_sem <- as.numeric(b_sem$colname)

estimacion2021 <- mean(c(a_sem, b_sem))


### Mediana de ingreso per-cápita de los hogares (cte. base diciembre 2006)


a_mes <- sem1_h_svy %>%
  srvyr::filter(bd_region == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_median(y_pc_d))
a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_h_svy %>%
  srvyr::filter(bd_region == 1) %>%
  srvyr::summarise(colname = srvyr::survey_median(y_pc_d))
b_sem <- as.numeric(b_sem$colname)

estimacion2021 <- mean(c(a_sem, b_sem))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

