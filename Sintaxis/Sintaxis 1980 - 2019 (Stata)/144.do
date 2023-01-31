
/* ESTE INDICADOR SE HACE MANUAL







































*======================================================================================*
*                       **   POBREZA Y DESIGUALDAD   **
*                            MATRIZ PARA BASE WEB
*
*
* Creación:      21/02/2020
* Institución:   Banco de Datos, FCS-UdelaR
* Responsable:   Jimena Pandolfi
* Descripción:   Construcción de matriz para base web (Piso II - Motores)para 
*				 indicadores de dimiensión temática "pobreza y desigualdad"
*				
*				 Se divide en dos archivos "a" y "b" debido al tamaño de la matriz 
*				 dado por la cantidad de cruces. 
*
*======================================================================================*


*======================================================================================*
*                           UBICACIÓN DE ARCHIVOS      
*======================================================================================*

	cd "C:\Users\Usuario\Dropbox\1. Banco de datos\2019"
	global bases "C:\Users\Usuario\Dropbox\1. Banco de datos\2019\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon"
	global tabulados C:\Users\Usuario\Dropbox\1. Banco de datos\2019\PISO II_MOTORES\Base web\Extracciones parciales\1. Pobreza y desigualdad


*======================================================================================*
*              CARACTERÍSTICAS GENERALES DE LA MATRIZ DE EXPORTACAIÓN      
*======================================================================================*

    global seieURB 6 11 14/18   // Años de serie para país urbano
	global seieTOT 6 11 14/18   // Años de serie para total país

	  

*======================================================================================*
*                 1.2. INSUFICIENCIA DE INGRESOS      
*======================================================================================*

* INDICADOR: Distribución de personas según cantidad de NBI acumuladas 
* CÓDIGO:    144
* GRUPOS:    Región / Ascendencia étnico-racial / Sexo / Tramo de edad / Condición de pobreza del hogar
* FUENTE:    ECH (INE)
* PERÍODO:   2006 2011 2014/2018 (Años en los que es posible construir serie compelta NBI met. 2011)


**

*******************************************************************************************
*******************************************************************************************
* PARTE A * (CRUCES REGIÓN, SEXO Y ASCENDENCIA)
*******************************************************************************************
*******************************************************************************************



**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *TOTAL PAÍS
	local i         = 1
	local codind    = 144                 // Código de indicador
	local grupo     = 7                   // Cantidad de grupos (se excluye tramo de edad y pobreza por tamaño de la matriz)
	local canio     = 7                   // Cantidad de años de la serie
	local categvar  = 6                   // Cantidad de categorías de la variable 

	local filas = (`grupo' + 1) * `canio' * `categvar' // Por ser una distrubución lo multiplico por el total de categorías de la variable (trunco en 5 nbi)
	matrix def       MATR= J(`filas',4,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO CATEGORIA

	foreach anio of numlist $seieTOT  {
    use "$bases\p`anio'.dta", clear
	
	
	replace NBI_2011cant=5 if NBI_2011cant>=5 & NBI_2011cant<=8
	qui: ta NBI_2011cant, g(NBI_AUX_)
	

	
	
	foreach categ of numlist 1/6  {
	
    mean NBI_AUX_`categ' [aw=bc_pesoan]
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
   	matrix MATR  [`i',4]=  `categ'
  	
	
	local i  = `i' + 1
	foreach val of numlist 1/3  {
	mean NBI_AUX_`categ' [aw=bc_pesoan] if region_3==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `val'
	matrix MATR  [`i',3]=  `anio'
   	matrix MATR  [`i',4]=  `categ'
	local i  = `i' + 1
    }
    *
	
	local j  = 	4
	foreach val of numlist 1/2  {
	mean NBI_AUX_`categ' [aw=bc_pesoan] if bc_pe2==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
   	matrix MATR  [`i',4]=  `categ'

	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	6
	foreach val of numlist 1/2  {
	mean NBI_AUX_`categ' [aw=bc_pesoan] if bd_e29_1==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
   	matrix MATR  [`i',4]=  `categ'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *

	}
    *
}
    *
**

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_a.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i         = 1
	local codind    = 144                 // Código de indicador
	local grupo     = 7                   // Cantidad de grupos (se excluye tramo de edad y pobreza por tamaño de la matriz)
	local canio     = 7                   // Cantidad de años de la serie
	local categvar  = 6                   // Cantidad de categorías de la variable 

import  excel "$tabulados\Auxiliares\AUX`codind'_a.xls", firstrow clear
sort AUXILIAR ANIO
drop if ANIO==.

g CODIND                 = 144
g NOMINDICADOR           = "NA"
g SEXO                   = "NA"
g ASCENDENCIA    		 = "NA"
g QUINTIL       		 = "NA"
g DEPARTAMENTOUY		 = "NA"
g URBANORURALUY 		 = "NA"
g PAÍS			 		 = "URUGUAY"
g RESPONSABLE			 = "JIMENA PANDOLFI"

**

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y TÍTULOS DE LA VARIABLES AUXILIARES CORRESPONDIENTES**

replace URBANORURALUY    = "URBANO (MÁS DE 5.000 HABITANTES)"     if AUXILIAR==1
replace URBANORURALUY    = "URBANO (MENOS DE 5.000 HABITANTES)"   if AUXILIAR==2
replace URBANORURALUY    = "RURAL DISPERSO"                       if AUXILIAR==3

replace SEXO    = "VARONES"            if AUXILIAR==4
replace SEXO    = "MUJERES"            if AUXILIAR==5

replace ASCENDENCIA    = "AFRO"        if AUXILIAR==6
replace ASCENDENCIA    = "NO AFRO"     if AUXILIAR==7

replace NOMINDICADOR    = "PERSONAS EN HOGARES SIN NBI(% DE LA POBLACIÓN TOTAL)"             if CATEGORIA==1
replace NOMINDICADOR    = "PERSONAS EN HOGARES CON UNA NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==2
replace NOMINDICADOR    = "PERSONAS EN HOGARES CON DOS NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==3
replace NOMINDICADOR    = "PERSONAS EN HOGARES CON TRES NBI(% DE LA POBLACIÓN TOTAL)"        if CATEGORIA==4
replace NOMINDICADOR    = "PERSONAS EN HOGARES CON CUATRO NBI(% DE LA POBLACIÓN TOTAL)"      if CATEGORIA==5
replace NOMINDICADOR    = "PERSONAS EN HOGARES CON CINCO O MÁS NBI(% DE LA POBLACIÓN TOTAL)" if CATEGORIA==6

** 

save  "$tabulados\Auxiliares\`codind'_a.dta", replace


*******************************************************************************************
*******************************************************************************************
* PARTE B * (CRUCE TRAMO DE EDAD)
*******************************************************************************************
*******************************************************************************************


	local i         = 1
	local codind    = 144                 // Código de indicador
	local grupo     = 7                   // Cantidad de grupos (se excluye tramo de edad y pobreza por tamaño de la matriz)
	local canio     = 7                   // Cantidad de años de la serie
	local categvar  = 6                   // Cantidad de categorías de la variable 

	local filas = `grupo' * `canio' * `categvar' // Por ser una distrubución lo multiplico por el total de categorías de la variable (trunco en 5 nbi)
	matrix def       MATR= J(`filas',4,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO CATEGORIA

	foreach anio of numlist $seieTOT  {
    use "$bases\p`anio'.dta", clear
	
	
	replace NBI_2011cant=5 if NBI_2011cant>5
	qui: ta NBI_2011cant, g(NBI_AUX_)
	
    g tramo_edad=.
	replace tramo_edad=1 if bc_pe3>=0  & bc_pe3<=5
	replace tramo_edad=2 if bc_pe3>=6  & bc_pe3<=12
	replace tramo_edad=3 if bc_pe3>=13 & bc_pe3<=18
	replace tramo_edad=4 if bc_pe3>=19 & bc_pe3<=24
	replace tramo_edad=5 if bc_pe3>=25 & bc_pe3<=29
	replace tramo_edad=6 if bc_pe3>=30 & bc_pe3<=64
	replace tramo_edad=7 if bc_pe3>=65 
	
	
	foreach categ of numlist 1/6  {
	
	local j  = 	8
	foreach val of numlist 1/7  {
	mean NBI_AUX_`categ' [aw=bc_pesoan] if tramo_edad==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
   	matrix MATR  [`i',4]=  `categ'

	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	}
    *
}
    *
**

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_b.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i         = 1
	local codind    = 144                 // Código de indicador
	local grupo     = 7                   // Cantidad de grupos (se excluye tramo de edad y pobreza por tamaño de la matriz)
	local canio     = 7                   // Cantidad de años de la serie
	local categvar  = 6                   // Cantidad de categorías de la variable 

import  excel "$tabulados\Auxiliares\AUX`codind'_b.xls", firstrow clear
sort AUXILIAR ANIO
drop if ANIO==.

g CODIND                 = 144
g NOMINDICADOR           = "NA"
g SEXO                   = "NA"
g ASCENDENCIA    		 = "NA"
g QUINTIL       		 = "NA"
g DEPARTAMENTOUY		 = "NA"
g URBANORURALUY 		 = "NA"
g PAÍS			 		 = "URUGUAY"
g RESPONSABLE			 = "JIMENA PANDOLFI"

**

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y TÍTULOS DE LA VARIABLES AUXILIARES CORRESPONDIENTES**
destring AUXILIAR, replace
destring CATEGORIA, replace

replace NOMINDICADOR    = "PERSONAS DE 0 A 5 AÑOS EN HOGARES SIN NBI(% DE LA POBLACIÓN TOTAL)"             if CATEGORIA==1  & AUXILIAR==8
replace NOMINDICADOR    = "PERSONAS DE 0 A 5 AÑOS EN HOGARES CON UNA NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==2  & AUXILIAR==8
replace NOMINDICADOR    = "PERSONAS DE 0 A 5 AÑOS EN HOGARES CON DOS NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==3  & AUXILIAR==8
replace NOMINDICADOR    = "PERSONAS DE 0 A 5 AÑOS EN HOGARES CON TRES NBI(% DE LA POBLACIÓN TOTAL)"        if CATEGORIA==4  & AUXILIAR==8
replace NOMINDICADOR    = "PERSONAS DE 0 A 5 AÑOS EN HOGARES CON CUATRO NBI(% DE LA POBLACIÓN TOTAL)"      if CATEGORIA==5  & AUXILIAR==8
replace NOMINDICADOR    = "PERSONAS DE 0 A 5 AÑOS EN HOGARES CON CINCO O MÁS NBI(% DE LA POBLACIÓN TOTAL)" if CATEGORIA==6  & AUXILIAR==8

replace NOMINDICADOR    = "PERSONAS DE 6 A 12 AÑOS EN HOGARES SIN NBI(% DE LA POBLACIÓN TOTAL)"             if CATEGORIA==1  & AUXILIAR==9
replace NOMINDICADOR    = "PERSONAS DE 6 A 12 AÑOS EN HOGARES CON UNA NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==2  & AUXILIAR==9
replace NOMINDICADOR    = "PERSONAS DE 6 A 12 AÑOS EN HOGARES CON DOS NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==3  & AUXILIAR==9
replace NOMINDICADOR    = "PERSONAS DE 6 A 12 AÑOS EN HOGARES CON TRES NBI(% DE LA POBLACIÓN TOTAL)"        if CATEGORIA==4  & AUXILIAR==9
replace NOMINDICADOR    = "PERSONAS DE 6 A 12 AÑOS EN HOGARES CON CUATRO NBI(% DE LA POBLACIÓN TOTAL)"      if CATEGORIA==5  & AUXILIAR==9
replace NOMINDICADOR    = "PERSONAS DE 6 A 12 AÑOS EN HOGARES CON CINCO O MÁS NBI(% DE LA POBLACIÓN TOTAL)" if CATEGORIA==6  & AUXILIAR==9

replace NOMINDICADOR    = "PERSONAS DE 13 A 18 AÑOS EN HOGARES SIN NBI(% DE LA POBLACIÓN TOTAL)"             if CATEGORIA==1  & AUXILIAR==10
replace NOMINDICADOR    = "PERSONAS DE 13 A 18 AÑOS EN HOGARES CON UNA NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==2  & AUXILIAR==10
replace NOMINDICADOR    = "PERSONAS DE 13 A 18 AÑOS EN HOGARES CON DOS NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==3  & AUXILIAR==10
replace NOMINDICADOR    = "PERSONAS DE 13 A 18 AÑOS EN HOGARES CON TRES NBI(% DE LA POBLACIÓN TOTAL)"        if CATEGORIA==4  & AUXILIAR==10
replace NOMINDICADOR    = "PERSONAS DE 13 A 18 AÑOS EN HOGARES CON CUATRO NBI(% DE LA POBLACIÓN TOTAL)"      if CATEGORIA==5  & AUXILIAR==10
replace NOMINDICADOR    = "PERSONAS DE 13 A 18 AÑOS EN HOGARES CON CINCO O MÁS NBI(% DE LA POBLACIÓN TOTAL)" if CATEGORIA==6  & AUXILIAR==10

replace NOMINDICADOR    = "PERSONAS DE 19 A 24 AÑOS EN HOGARES SIN NBI(% DE LA POBLACIÓN TOTAL)"             if CATEGORIA==1  & AUXILIAR==11
replace NOMINDICADOR    = "PERSONAS DE 19 A 24 AÑOS EN HOGARES CON UNA NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==2  & AUXILIAR==11
replace NOMINDICADOR    = "PERSONAS DE 19 A 24 AÑOS EN HOGARES CON DOS NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==3  & AUXILIAR==11
replace NOMINDICADOR    = "PERSONAS DE 19 A 24 AÑOS EN HOGARES CON TRES NBI(% DE LA POBLACIÓN TOTAL)"        if CATEGORIA==4  & AUXILIAR==11
replace NOMINDICADOR    = "PERSONAS DE 19 A 24 AÑOS EN HOGARES CON CUATRO NBI(% DE LA POBLACIÓN TOTAL)"      if CATEGORIA==5  & AUXILIAR==11
replace NOMINDICADOR    = "PERSONAS DE 19 A 24 AÑOS EN HOGARES CON CINCO O MÁS NBI(% DE LA POBLACIÓN TOTAL)" if CATEGORIA==6  & AUXILIAR==11

replace NOMINDICADOR    = "PERSONAS DE 25 A 29 AÑOS EN HOGARES SIN NBI(% DE LA POBLACIÓN TOTAL)"             if CATEGORIA==1  & AUXILIAR==12
replace NOMINDICADOR    = "PERSONAS DE 25 A 29 AÑOS EN HOGARES CON UNA NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==2  & AUXILIAR==12
replace NOMINDICADOR    = "PERSONAS DE 25 A 29 AÑOS EN HOGARES CON DOS NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==3  & AUXILIAR==12
replace NOMINDICADOR    = "PERSONAS DE 25 A 29 AÑOS EN HOGARES CON TRES NBI(% DE LA POBLACIÓN TOTAL)"        if CATEGORIA==4  & AUXILIAR==12
replace NOMINDICADOR    = "PERSONAS DE 25 A 29 AÑOS EN HOGARES CON CUATRO NBI(% DE LA POBLACIÓN TOTAL)"      if CATEGORIA==5  & AUXILIAR==12
replace NOMINDICADOR    = "PERSONAS DE 25 A 29 AÑOS EN HOGARES CON CINCO O MÁS NBI(% DE LA POBLACIÓN TOTAL)" if CATEGORIA==6  & AUXILIAR==12

replace NOMINDICADOR    = "PERSONAS DE 30 A 64 AÑOS EN HOGARES SIN NBI(% DE LA POBLACIÓN TOTAL)"             if CATEGORIA==1  & AUXILIAR==13
replace NOMINDICADOR    = "PERSONAS DE 30 A 64 AÑOS EN HOGARES CON UNA NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==2  & AUXILIAR==13
replace NOMINDICADOR    = "PERSONAS DE 30 A 64 AÑOS EN HOGARES CON DOS NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==3  & AUXILIAR==13
replace NOMINDICADOR    = "PERSONAS DE 30 A 64 AÑOS EN HOGARES CON TRES NBI(% DE LA POBLACIÓN TOTAL)"        if CATEGORIA==4  & AUXILIAR==13
replace NOMINDICADOR    = "PERSONAS DE 30 A 64 AÑOS EN HOGARES CON CUATRO NBI(% DE LA POBLACIÓN TOTAL)"      if CATEGORIA==5  & AUXILIAR==13
replace NOMINDICADOR    = "PERSONAS DE 30 A 64 AÑOS EN HOGARES CON CINCO O MÁS NBI(% DE LA POBLACIÓN TOTAL)" if CATEGORIA==6  & AUXILIAR==13

replace NOMINDICADOR    = "PERSONAS DE 65 AÑOS Y MÁS EN HOGARES SIN NBI(% DE LA POBLACIÓN TOTAL)"             if CATEGORIA==1  & AUXILIAR==14
replace NOMINDICADOR    = "PERSONAS DE 65 AÑOS Y MÁS EN HOGARES CON UNA NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==2  & AUXILIAR==14
replace NOMINDICADOR    = "PERSONAS DE 65 AÑOS Y MÁS EN HOGARES CON DOS NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==3  & AUXILIAR==14
replace NOMINDICADOR    = "PERSONAS DE 65 AÑOS Y MÁS EN HOGARES CON TRES NBI(% DE LA POBLACIÓN TOTAL)"        if CATEGORIA==4  & AUXILIAR==14
replace NOMINDICADOR    = "PERSONAS DE 65 AÑOS Y MÁS EN HOGARES CON CUATRO NBI(% DE LA POBLACIÓN TOTAL)"      if CATEGORIA==5  & AUXILIAR==14
replace NOMINDICADOR    = "PERSONAS DE 65 AÑOS Y MÁS EN HOGARES CON CINCO O MÁS NBI(% DE LA POBLACIÓN TOTAL)" if CATEGORIA==6  & AUXILIAR==14

** 

save  "$tabulados\\Auxiliares\\`codind'_b.dta", replace


*******************************************************************************************
*******************************************************************************************
* PARTE C * (CRUCE POBREZA)
*******************************************************************************************
*******************************************************************************************


	local i         = 1
	local codind    = 144                 // Código de indicador
	local grupo     = 2                   // Cantidad de grupos (se excluye tramo de edad y pobreza por tamaño de la matriz)
	local canio     = 7                   // Cantidad de años de la serie
	local categvar  = 6                   // Cantidad de categorías de la variable 

	local filas = `grupo' * `canio' * `categvar' // Por ser una distrubución lo multiplico por el total de categorías de la variable (trunco en 5 nbi)
	matrix def       MATR= J(`filas',4,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO CATEGORIA

	foreach anio of numlist $seieTOT  {
    use "$bases\p`anio'.dta", clear
	
	
	replace NBI_2011cant=5 if NBI_2011cant>5
	qui: ta NBI_2011cant, g(NBI_AUX_)
	

	
	foreach categ of numlist 1/6  {
	
	local j  = 	15
	foreach val of numlist 0/1  {
	mean NBI_AUX_`categ' [aw=bc_pesoan] if pobre06==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
   	matrix MATR  [`i',4]=  `categ'

	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *

	}
    *
	}
    *
**

xml_tab MATR, save("$tabulados\Auxiliares\\AUX`codind'_c.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i         = 1
	local codind    = 144                 // Código de indicador
	local grupo     = 2                   // Cantidad de grupos (se excluye tramo de edad y pobreza por tamaño de la matriz)
	local canio     = 7                   // Cantidad de años de la serie
	local categvar  = 6                   // Cantidad de categorías de la variable 


import  excel "$tabulados\Auxiliares\AUX`codind'_c.xls", firstrow clear
sort AUXILIAR ANIO
drop if ANIO==.

g CODIND                 = 144
g NOMINDICADOR           = "NA"
g SEXO                   = "NA"
g ASCENDENCIA    		 = "NA"
g QUINTIL       		 = "NA"
g DEPARTAMENTOUY		 = "NA"
g URBANORURALUY 		 = "NA"
g PAÍS			 		 = "URUGUAY"
g RESPONSABLE			 = "JIMENA PANDOLFI"

**

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y TÍTULOS DE LA VARIABLES AUXILIARES CORRESPONDIENTES**
destring AUXILIAR, replace
destring CATEGORIA, replace

replace NOMINDICADOR    = "PERSONAS NO EN SITUACIÓN DE POBREZA EN HOGARES SIN NBI(% DE LA POBLACIÓN TOTAL)"             if CATEGORIA==1  & AUXILIAR==15
replace NOMINDICADOR    = "PERSONAS NO EN SITUACIÓN DE POBREZA EN HOGARES CON UNA NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==2  & AUXILIAR==15
replace NOMINDICADOR    = "PERSONAS NO EN SITUACIÓN DE POBREZA EN HOGARES CON DOS NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==3  & AUXILIAR==15
replace NOMINDICADOR    = "PERSONAS NO EN SITUACIÓN DE POBREZA EN HOGARES CON TRES NBI(% DE LA POBLACIÓN TOTAL)"        if CATEGORIA==4  & AUXILIAR==15
replace NOMINDICADOR    = "PERSONAS NO EN SITUACIÓN DE POBREZA EN HOGARES CON CUATRO NBI(% DE LA POBLACIÓN TOTAL)"      if CATEGORIA==5  & AUXILIAR==15
replace NOMINDICADOR    = "PERSONAS NO EN SITUACIÓN DE POBREZA EN HOGARES CON CINCO O MÁS NBI(% DE LA POBLACIÓN TOTAL)" if CATEGORIA==6  & AUXILIAR==15

replace NOMINDICADOR    = "PERSONAS EN SITUACIÓN DE POBREZA EN HOGARES SIN NBI(% DE LA POBLACIÓN TOTAL)"             if CATEGORIA==1  & AUXILIAR==16
replace NOMINDICADOR    = "PERSONAS EN SITUACIÓN DE POBREZA EN HOGARES CON UNA NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==2  & AUXILIAR==16
replace NOMINDICADOR    = "PERSONAS EN SITUACIÓN DE POBREZA EN HOGARES CON DOS NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==3  & AUXILIAR==16
replace NOMINDICADOR    = "PERSONAS EN SITUACIÓN DE POBREZA EN HOGARES CON TRES NBI(% DE LA POBLACIÓN TOTAL)"        if CATEGORIA==4  & AUXILIAR==16
replace NOMINDICADOR    = "PERSONAS EN SITUACIÓN DE POBREZA EN HOGARES CON CUATRO NBI(% DE LA POBLACIÓN TOTAL)"      if CATEGORIA==5  & AUXILIAR==16
replace NOMINDICADOR    = "PERSONAS EN SITUACIÓN DE POBREZA EN HOGARES CON CINCO O MÁS NBI(% DE LA POBLACIÓN TOTAL)" if CATEGORIA==6  & AUXILIAR==16

** 

save  "$tabulados\Auxiliares\\`codind'_c.dta", replace



*******************************************************************************************
*******************************************************************************************
* FUSIÓN *
*******************************************************************************************
*******************************************************************************************

local codind    = 144                 // Código de indicador

use  "$tabulados\Auxiliares\\`codind'_c.dta", clear
append using "$tabulados\Auxiliares\\`codind'_b.dta"
append using "$tabulados\Auxiliares\\`codind'_c.dta"

export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace





