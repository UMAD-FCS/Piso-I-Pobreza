
*======================================================================================*
*                       **   POBREZA Y DESIGUALDAD   **
*                            MATRIZ PARA BASE WEB
*
*
* Creación:      21/02/2020
* Actualización: 30/09/2020
* Institución:   Banco de Datos, FCS-UdelaR
* Responsable:   Jimena Pandolfi
* Descripción:   Construcción de matriz para base web (Piso II - Motores)para 
*				 indicadores de dimiensión temática "pobreza y desigualdad"
*
*
*======================================================================================*


*======================================================================================*
*                           UBICACIÓN DE ARCHIVOS      
*======================================================================================*


	cd "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD"
	global bases "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon\En uso"
	global tabulados C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO II_MOTORES\Base web\1. Pobreza y desigualdad\Extracciones parciales\1. Pobreza y desigualdad


*======================================================================================*
*              CARACTERÍSTICAS GENERALES DE LA MATRIZ DE EXPORTACAIÓN      
*======================================================================================*

    global seieURB 6 11 14/19   // Años de serie para país urbano
	global seieTOT 6 11 14/19   // Años de serie para total país
	  

*======================================================================================*
*                 1.2. INSUFICIENCIA DE INGRESOS      
*======================================================================================*

* INDICADOR: Porcentaje de personas viviendo en hogres con NBI
* CÓDIGO:    142
* GRUPOS:    Región / Ascendencia étnico-racial / Sexo / Tramo de edad / Condición de pobreza / Quintil de ingreso del hogar
* FUENTE:    ECH (INE)
* PERÍODO:   2006 2011 2014/2018 (Años en los que es posible construir serie compelta NBI met. 2011)


**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *TOTAL PAÍS
	local i      = 1
	local codind = 142                 // Código de indicador
	local grupo  = 21                  // Cantidad de grupos
	local canio  = 8                   // Cantidad de años de la serie

	local filas = (1 + `grupo') * `canio' // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',3,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO

	
	
	foreach anio of numlist $seieTOT  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g tramo_edad=.
	replace tramo_edad=1 if bc_pe3>=0  & bc_pe3<=5
	replace tramo_edad=2 if bc_pe3>=6  & bc_pe3<=12
	replace tramo_edad=3 if bc_pe3>=13 & bc_pe3<=18
	replace tramo_edad=4 if bc_pe3>=19 & bc_pe3<=24
	replace tramo_edad=5 if bc_pe3>=25 & bc_pe3<=29
	replace tramo_edad=6 if bc_pe3>=30 & bc_pe3<=64
	replace tramo_edad=7 if bc_pe3>=65 
	
	
	mean NBI_2011 [aw=bc_pesoan]
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
   	
	local i  = `i' + 1
	foreach val of numlist 1/3  {
	mean NBI_2011 [aw=bc_pesoan] if bd_region==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `val'
	matrix MATR  [`i',3]=  `anio'
	local i  = `i' + 1
    }
    *
	
	local j  = 	4
	foreach val of numlist 1/2  {
	mean NBI_2011 [aw=bc_pesoan] if bc_pe2==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	6
	foreach val of numlist 1/7  {
	mean NBI_2011 [aw=bc_pesoan] if tramo_edad==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	13
	foreach val of numlist 1/2  {
	mean NBI_2011 [aw=bc_pesoan] if bd_e29_1==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	15
	foreach val of numlist 1/5  {
	mean NBI_2011 [aw=bc_pesoan] if bd_quintilesy==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	20
	foreach val of numlist 0/1  {
	mean NBI_2011 [aw=bc_pesoan] if pobre06==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	
	}
    *

**
xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 142                 // Código de indicador
	local grupo  = 21                  // Cantidad de grupos
	local canio  = 8                   // Cantidad de años de la serie

import  excel "$tabulados\Auxiliares\AUX`codind'.xls", firstrow clear
sort AUXILIAR ANIO
drop if ANIO==.

g CODIND                 = 142
g NOMINDICADOR           = "PERSONAS EN HOGARES CON NECESIDADES BÁSICAS INSATISFECHAS (MET. 2011) (% DE LA POBLACIÓN TOTAL) (INE)"
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

replace NOMINDICADOR    = "PERSONAS DE 0 A 5 AÑOS EN HOGARES CON NBI(MET. 2011) (% DE LA POBLACIÓN TOTAL) (INE)"    if AUXILIAR==6
replace NOMINDICADOR    = "PERSONAS DE 6 A 12 AÑOS EN HOGARES CON NBI (MET. 2011) (% DE LA POBLACIÓN TOTAL) (INE)"   if AUXILIAR==7
replace NOMINDICADOR    = "PERSONAS DE 13 A 18 AÑOS EN HOGARES CON NBI (MET. 2011) (% DE LA POBLACIÓN TOTAL) (INE)"  if AUXILIAR==8
replace NOMINDICADOR    = "PERSONAS DE 19 A 24 AÑOS EN HOGARES CON NBI (MET. 2011) (% DE LA POBLACIÓN TOTAL) (INE)"  if AUXILIAR==9
replace NOMINDICADOR    = "PERSONAS DE 25 A 29 AÑOS EN HOGARES CON NBI (MET. 2011) (% DE LA POBLACIÓN TOTAL) (INE)"  if AUXILIAR==10
replace NOMINDICADOR    = "PERSONAS DE 30 A 64 AÑOS EN HOGARES CON NBI (MET. 2011) (% DE LA POBLACIÓN TOTAL) (INE)"  if AUXILIAR==11
replace NOMINDICADOR    = "PERSONAS DE 65 AÑOS Y MÁS EN HOGARES CON NBI (MET. 2011) (% DE LA POBLACIÓN TOTAL) (INE)" if AUXILIAR==12

replace ASCENDENCIA    = "AFRO"        if AUXILIAR==13
replace ASCENDENCIA    = "NO AFRO"     if AUXILIAR==14

replace QUINTIL			 = "QUINTIL 1"							  if AUXILIAR==15
replace QUINTIL			 = "QUINTIL 2"							  if AUXILIAR==16
replace QUINTIL			 = "QUINTIL 3"							  if AUXILIAR==17
replace QUINTIL			 = "QUINTIL 4"							  if AUXILIAR==18
replace QUINTIL			 = "QUINTIL 5"							  if AUXILIAR==19

replace NOMINDICADOR    = "PERSONAS NO EN SITUACIÓN DE POBREZA EN HOGARES CON NBI (% DE LA POBLACIÓN TOTAL) (INE)"     if AUXILIAR==20
replace NOMINDICADOR    = "PERSONAS EN SITUACIÓN DE POBREZA EN HOGARES CON NBI (% DE LA POBLACIÓN TOTAL) (INE)"        if AUXILIAR==21


** 

export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace

