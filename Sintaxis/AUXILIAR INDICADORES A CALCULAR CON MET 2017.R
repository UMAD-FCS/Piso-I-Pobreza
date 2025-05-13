### 170	Hogares en situación de pobreza (% de la población total) (INE-Met 2017) ------------------------------------

# Total

a_est <- ech_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(pobre17))

a_ano<- mean(as.numeric(a_est$colname))

c_ano <- a_ano

# Region

a_region <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre17))
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
    srvyr::summarise(colname = srvyr::survey_mean(pobre17))
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

CODIND       <- 170
NOMINDICADOR <- "Hogares en situación de pobreza (% de la población total) (INE-Met 2017)"
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
ANIO         <- 2024
VALOR        <- c(c_ano, a_e_region, a_e_jefe)
RESPONSABLE  <- "JIMENA PANDOLFI"




m170 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	



### 171	Personas en situación de pobreza (% de la población total) (INE-Met 2017) ----------------------------------

# Total

a_est <- ech_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(pobre17))

a_ano<- mean(as.numeric(a_est$colname))

c_ano <- a_ano

# Region

a_region <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x) %>%
    
    srvyr::summarise(colname = srvyr::survey_mean(pobre17))
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
    srvyr::summarise(colname = srvyr::survey_mean(pobre17))
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
    
    srvyr::summarise(colname = srvyr::survey_mean(pobre17))
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
    srvyr::summarise(colname = srvyr::survey_mean(pobre17))
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

CODIND       <- 171
NOMINDICADOR <- "Personas en situación de pobreza (% de la población total) (INE-Met 2017)"
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
ANIO         <- 2024
VALOR        <- c(c_ano, a_e_region, a_e_edad, a_e_sexo, a_e_afro)
RESPONSABLE  <- "JIMENA PANDOLFI"



m171 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	




### 172	Hogares en situación de indigencia (% de la población total) (INE-Met 2017) --------------------------------

# Total

a_est <- ech_h_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(indig17))

a_ano<- mean(as.numeric(a_est$colname))

c_ano <- a_ano

# Region

a_region <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(indig17))
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
    
    srvyr::summarise(colname = srvyr::survey_mean(indig17))
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

CODIND       <- 172
NOMINDICADOR <- "Hogares en situación de indigencia (% de la población total) (INE-Met 2017)"
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
ANIO         <- 2024
VALOR        <- c(c_ano, a_e_region, a_e_jefe)
RESPONSABLE  <- "JIMENA PANDOLFI"


m172 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	

### 173	Personas en situación de indigencia (% de la población total) (INE-Met 2017) --------------------------------

# Total

a_est <- ech_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(indig17))

a_ano<- mean(as.numeric(a_est$colname))

c_ano <- a_ano

# Region

a_region <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x) %>%
    
    srvyr::summarise(colname = srvyr::survey_mean(indig17))
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
    
    srvyr::summarise(colname = srvyr::survey_mean(indig17))
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
    srvyr::summarise(colname = srvyr::survey_mean(indig17))
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
    srvyr::summarise(colname = srvyr::survey_mean(indig17))
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

CODIND       <- 173
NOMINDICADOR <- "Personas en situación de indigencia (% de la población total) (INE-Met 2017)"
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
ANIO         <- 2024
VALOR        <- c(c_ano, a_e_region, a_e_edad, a_e_sexo, a_e_afro)
RESPONSABLE  <- "JIMENA PANDOLFI"



m173 <- cbind(CODIND,	NOMINDICADOR,	CATEGORIA,	PESTAÑA,	CORTE,	CORTE_NUEVA,	REGION,	TRAMO,	SEXOJEFATURA,	POBREZA,	SEXO,	ASCENDENCIA,	DECIL,	QUINTIL,	DEPARTAMENTOUY,	PAIS,	ANIO,	VALOR,	RESPONSABLE)	

