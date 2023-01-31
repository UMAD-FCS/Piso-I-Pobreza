
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

* INDICADOR: Distribución de hogares según cantidad de NBI acumuladas
* CÓDIGO:    143
* GRUPOS:    Región / Condición de pobreza del hogar
* FUENTE:    ECH (INE)
* PERÍODO:   2006 2011 2014/2018 (Años en los que es posible construir serie compelta NBI met. 2011)


**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *TOTAL PAÍS
	local i      = 1
	local codind = 143                    // Código de indicador
	local grupo  = 5              		  // Cantidad de grupos
	local canio  = 8                   	  // Cantidad de años de la serie
	local categvar  = 6                   // Cantidad de categorías de la variable 

	local filas = (`grupo' + 1) * `canio' * `categvar' // Por ser una distrubución lo multiplico por el total de categorías de la variable (trunco en 5 nbi)
	matrix def       MATR= J(`filas',4,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO CATEGORIA

	
	
	foreach anio of numlist $seieTOT  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	replace NBI_2011cant=5 if NBI_2011cant>=5 & NBI_2011cant<=8
	qui: ta NBI_2011cant, g(NBI_AUX_)
			
	foreach categ of numlist 1/6  {
	
	qui: mean NBI_AUX_`categ' [aw=bc_pesoan] if bc_pe4==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
   	matrix MATR  [`i',4]=  `categ'

	local i  = `i' + 1
	foreach val of numlist 1/3  {
	qui: mean NBI_AUX_`categ' [aw=bc_pesoan] if bc_pe4==1 & region_3==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `val'
	matrix MATR  [`i',3]=  `anio'
	matrix MATR  [`i',4]=  `categ'
	
	local i  = `i' + 1
    }
    *
	

	local j  = 	4
	foreach val of numlist 0/1  {
	qui: mean NBI_AUX_`categ' [aw=bc_pesoan] if bc_pe4==1 & pobre06==`val'
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

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local codind = 143                    // Código de indicador
	local grupo  = 5              		  // Cantidad de grupos
	local canio  = 8                   	  // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'.xls", firstrow clear
sort AUXILIAR ANIO
drop if ANIO==.

g CODIND                 = 143
g NOMINDICADOR           = "NA"   		  // Lo mantengo como valor perdido porque varía según categoría de la variable
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

replace NOMINDICADOR    = "HOGARES SIN NBI(% DE LA POBLACIÓN TOTAL)"             if CATEGORIA==1
replace NOMINDICADOR    = "HOGARES CON UNA NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==2
replace NOMINDICADOR    = "HOGARES CON DOS NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==3
replace NOMINDICADOR    = "HOGARES CON TRES NBI(% DE LA POBLACIÓN TOTAL)"        if CATEGORIA==4
replace NOMINDICADOR    = "HOGARES CON CUATRO NBI(% DE LA POBLACIÓN TOTAL)"      if CATEGORIA==5
replace NOMINDICADOR    = "HOGARES CON CINCO O MÁS NBI(% DE LA POBLACIÓN TOTAL)" if CATEGORIA==6

replace NOMINDICADOR    = "HOGARES NO EN SITUACIÓN DE POBREZA SIN NBI(% DE LA POBLACIÓN TOTAL)"             if CATEGORIA==1 & AUXILIAR==4
replace NOMINDICADOR    = "HOGARES NO EN SITUACIÓN DE POBREZA CON UNA NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==2 & AUXILIAR==4
replace NOMINDICADOR    = "HOGARES NO EN SITUACIÓN DE POBREZA CON DOS NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==3 & AUXILIAR==4
replace NOMINDICADOR    = "HOGARES NO EN SITUACIÓN DE POBREZA CON TRES NBI(% DE LA POBLACIÓN TOTAL)"        if CATEGORIA==4 & AUXILIAR==4
replace NOMINDICADOR    = "HOGARES NO EN SITUACIÓN DE POBREZA CON CUATRO NBI(% DE LA POBLACIÓN TOTAL)"      if CATEGORIA==5 & AUXILIAR==4
replace NOMINDICADOR    = "HOGARES NO EN SITUACIÓN DE POBREZA CON CINCO O MÁS NBI(% DE LA POBLACIÓN TOTAL)" if CATEGORIA==6 & AUXILIAR==4

replace NOMINDICADOR    = "HOGARES EN SITUACIÓN DE POBREZA SIN NBI(% DE LA POBLACIÓN TOTAL)"        	 if CATEGORIA==1 & AUXILIAR==5
replace NOMINDICADOR    = "HOGARES EN SITUACIÓN DE POBREZA CON UNA NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==2 & AUXILIAR==5
replace NOMINDICADOR    = "HOGARES EN SITUACIÓN DE POBREZA CON DOS NBI(% DE LA POBLACIÓN TOTAL)"         if CATEGORIA==3 & AUXILIAR==5
replace NOMINDICADOR    = "HOGARES EN SITUACIÓN DE POBREZA CON TRES NBI(% DE LA POBLACIÓN TOTAL)"        if CATEGORIA==4 & AUXILIAR==5
replace NOMINDICADOR    = "HOGARES EN SITUACIÓN DE POBREZA CON CUATRO NBI(% DE LA POBLACIÓN TOTAL)"      if CATEGORIA==5 & AUXILIAR==5
replace NOMINDICADOR    = "HOGARES EN SITUACIÓN DE POBREZA CON CINCO O MÁS NBI(% DE LA POBLACIÓN TOTAL)" if CATEGORIA==6 & AUXILIAR==5

replace URBANORURALUY    = "URBANO (MÁS DE 5.000 HABITANTES)"     if AUXILIAR==1
replace URBANORURALUY    = "URBANO (MENOS DE 5.000 HABITANTES)"   if AUXILIAR==2
replace URBANORURALUY    = "RURAL DISPERSO"                       if AUXILIAR==3

** 

export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace

