
*======================================================================================*
*                       **   POBREZA Y DESIGUALDAD   **
*                            MATRIZ PARA BASE WEB
*
*
* Creación:      11/03/2020
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

    global seieURB 81/99 0/19   // Años de serie para país urbano
	global seieTOT 6/19         // Años de serie para total país

	  

*======================================================================================*
*                 1.4. POBREZA MULTIDIMENSIONAL      
*======================================================================================*

* INDICADOR: Hogares con al menos un integrante en edades comprendidas entre los 4 y 17 años que no asista a ningún centro de educación formal
* CÓDIGO:    157
* GRUPOS:    Región / Condición de pobreza / Quintil de ingreso
* FUENTE:    ECH (INE)
* PERÍODO:   1981-2018 País urbano / 2006-2018 Total país 


**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *TOTAL PAÍS
	local i      = 1
	local codind = 157                  // Código de indicador
	local grupo  = 10                   // Cantidad de grupos
	local canio  = 14                   // Cantidad de años de la serie

	local filas = (1 + `grupo') * `canio' // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',3,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO

	foreach anio of numlist $seieTOT  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	recode NBI_educacion11 (-15=.)
	
		
	mean NBI_educacion11 [aw=bc_pesoan] if bc_pe4==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
   	
	local i  = `i' + 1
	foreach val of numlist 1/3  {
	mean NBI_educacion11 [aw=bc_pesoan] if bc_pe4==1 & bd_region==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `val'
	matrix MATR  [`i',3]=  `anio'
	local i  = `i' + 1
    }
    *
	
	local j  = 	4
	foreach val of numlist 1/5  {
	mean NBI_educacion11 [aw=bc_pesoan] if bc_pe4==1 & bd_quintilesy==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *	
	
	local j  = 	9
	foreach val of numlist 0/1  {
	mean NBI_educacion11 [aw=bc_pesoan] if bc_pe4==1 & pobre06==`val'
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

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_TP.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 157                  // Código de indicador
	local grupo  = 10                   // Cantidad de grupos
	local canio  = 14                   // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_TP.xls", firstrow clear
sort AUXILIAR ANIO
drop if ANIO==.

g CODIND                 = 157
g NOMINDICADOR           = "HOGARES CON MIEMBROS DE 4 A 17 AÑOS QUE NO ASISTEN A CENTROS EDUCATIVOS  (% DE LA POBLACIÓN TOTAL)"
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

replace QUINTIL			 = "QUINTIL 1"							  if AUXILIAR==4
replace QUINTIL			 = "QUINTIL 2"							  if AUXILIAR==5
replace QUINTIL			 = "QUINTIL 3"							  if AUXILIAR==6
replace QUINTIL			 = "QUINTIL 4"							  if AUXILIAR==7
replace QUINTIL			 = "QUINTIL 5"							  if AUXILIAR==8

replace NOMINDICADOR    = "HOGARES NO EN SITUACIÓN DE POBREZA CON MIEMBROS DE 4 A 17 AÑOS QUE NO ASISTEN A CENTROS EDUCATIVOS (% DE LA POBLACIÓN TOTAL)"      if AUXILIAR==9
replace NOMINDICADOR    = "HOGARES EN SITUACIÓN DE POBREZA CON MIEMBROS DE 4 A 17 AÑOS QUE NO ASISTEN A CENTROS EDUCATIVOS (% DE LA POBLACIÓN TOTAL)"         if AUXILIAR==10

** 
** 

save "$tabulados\Auxiliares\\`codind'_TP.dta", replace





************************************************************************************************
************************************************************************************************

    *PAÍS URBANO
	local i      = 1
	local codind = 157                  // Código de indicador
	local grupo  = 6                    // Cantidad de grupos (se agrega un grupo para el total)
	local canio  = 39                   // Cantidad de años de la serie

	local filas = `grupo' * `canio' // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',3,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO

	foreach anio of numlist $seieURB  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	recode NBI_educacion11 (-15=.)
	
	local j  = 	1
	mean NBI_educacion11 [aw=bc_pesoan] if bc_pe4==1 & bc_filtloc==1
 	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'

	local i  = `i' + 1
	

	local j  = 	4
	foreach val of numlist 1/5  {
	mean NBI_educacion11 [aw=bc_pesoan] if bc_pe4==1 & bd_quintilesy==`val' & bc_filtloc==1
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

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_PU.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 157                  // Código de indicador
	local grupo  = 6                    // Cantidad de grupos (se agrega un grupo para el total)
	local canio  = 39                   // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_PU.xls", firstrow clear
sort AUXILIAR ANIO
drop if ANIO==.

g CODIND                 = 157
g NOMINDICADOR           = "HOGARES CON MIEMBROS DE 4 A 17 AÑOS QUE NO ASISTEN A CENTROS EDUCATIVOS(% DE LA POBLACIÓN TOTAL)"
g SEXO                   = "NA"
g ASCENDENCIA    		 = "NA"
g QUINTIL       		 = "NA"
g DEPARTAMENTOUY		 = "NA"
g URBANORURALUY 		 = "URBANO (MÁS DE 5.000 HABITANTES)"
g PAÍS			 		 = "URUGUAY"
g RESPONSABLE			 = "JIMENA PANDOLFI"

**

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y TÍTULOS DE LA VARIABLES AUXILIARES CORRESPONDIENTES**

destring AUXILIAR, replace

replace URBANORURALUY    = "URBANO (MÁS DE 5.000 HABITANTES)"            if AUXILIAR==1

replace QUINTIL			 = "QUINTIL 1"							  if AUXILIAR==4
replace QUINTIL			 = "QUINTIL 2"							  if AUXILIAR==5
replace QUINTIL			 = "QUINTIL 3"							  if AUXILIAR==6
replace QUINTIL			 = "QUINTIL 4"							  if AUXILIAR==7
replace QUINTIL			 = "QUINTIL 5"							  if AUXILIAR==8


** 

save  "$tabulados\Auxiliares\\`codind'_PU.dta", replace



************************************************************************************************
************************************************************************************************
*FUSIÓN PAÍS URBANO Y TOTAL PAÍS

append using "$tabulados\Auxiliares\\`codind'_TP.dta", force
export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace
