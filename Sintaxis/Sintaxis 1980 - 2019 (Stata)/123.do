
*======================================================================================*
*                       **   POBREZA Y DESIGUALDAD   **
*                            MATRIZ PARA BASE WEB
*
*
* Creación:      03/02/2020
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
*                 1.2. INSUFICIENCIA DE INGRESOS      
*======================================================================================*

* INDICADOR: Porcentaje de hogares en situación de indigencia
* CÓDIGO:    123
* GRUPOS:    Región / Sexo del jefe(a)
* FUENTE:    ECH (INE)
* PERÍODO:   2006-2018


**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *TOTAL PAÍS
	local i      = 1
	local codind = 123                // Código de indicador
	local grupo  = 5                  // Cantidad de grupos
	local canio  = 14                 // Cantidad de años de la serie

	local filas = (1 + `grupo') * `canio' // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',3,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO

	foreach anio of numlist $seieTOT  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g       jefe_mas   = . 
	replace jefe_mas   = 1 if bc_pe4==1 & bc_pe2==1
	replace jefe_mas   = 2 if bc_pe4==1 & bc_pe2==2
	replace jefe_mas   = 0 if bc_pe4>1  & bc_pe4<=7
	egen    sexojefe   = max (jefe_mas), by (bc_correlat)
	
	mean indigente06 [aw=bc_pesoan] if bc_pe4==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
   	
	local i  = `i' + 1
	foreach val of numlist 1/3  {
	mean indigente06 [aw=bc_pesoan] if bc_pe4==1 & bd_region==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `val'
	matrix MATR  [`i',3]=  `anio'
	local i  = `i' + 1
    }
    *
	
	local j  = 	4
	foreach val of numlist 1/2  {
	mean indigente06 [aw=bc_pesoan] if bc_pe4==1 & sexojefe==`val'
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
	local codind = 123                // Código de indicador
	local grupo  = 5                  // Cantidad de grupos
	local canio  = 14                // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'.xls", firstrow clear
sort AUXILIAR ANIO
drop if ANIO==.

g CODIND                 = 123
g NOMINDICADOR           = "HOGARES EN SITUACIÓN DE INDIGENCIA (% DE LA POBLACIÓN TOTAL) (INE)"
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

replace NOMINDICADOR    = "HOGARES CON JEFATURA MASCULINA EN SITUACIÓN DE INDIGENCIA (% DE LA POBLACIÓN TOTAL) (INE)" if AUXILIAR==4
replace NOMINDICADOR    = "HOGARES CON JEFATURA FEMENINA EN SITUACIÓN DE INDIGENCIA (% DE LA POBLACIÓN TOTAL) (INE)"  if AUXILIAR==5
** 

export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace

