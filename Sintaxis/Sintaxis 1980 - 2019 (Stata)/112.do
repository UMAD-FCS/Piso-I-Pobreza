
*======================================================================================*
*                       **   POBREZA Y DESIGUALDAD   **
*                            MATRIZ PARA BASE WEB
*
*
* Creación:      10/06/2020
* Institución:   UMAD, FCS
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
	global bases "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ENGIH"
	global tabulados C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO II_MOTORES\Base web\Extracciones parciales\1. Pobreza y desigualdad


*======================================================================================*
*                 1.1. NIVEL DE INGRESOS      
*======================================================================================*

* INDICADOR: Promedio de gasto de consumo per-cápita de los hogares
* CÓDIGO:    112
* GRUPOS:    Quintil de ingreso / Sexo del jefe(a)
* FUENTE:    ENGHI(INE)
* PERÍODO:   2006 Y 2016 


**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *TOTAL PAÍS
	local i      = 1
	local codind = 112                  // Código de indicador
	local grupo  = 7                    // Cantidad de grupos
	local canio  = 2                   // Cantidad de años de la serie

	local filas = (1 + `grupo') * `canio' // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',3,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO

	foreach anio of numlist 2006 2016  {
    use "$bases\Hogar_consumos_`anio'.dta", clear
	

	mean gasto_def_p_total [aw=peso]
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
   	
	local i  = `i' + 1

	local j  = 	4
	foreach val of numlist 1/5  {
	mean gasto_def_p_total [aw=peso] if quintilesy==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *	
	

	local j  = 	11
	foreach val of numlist 1/2  {
	mean gasto_def_p_total [aw=peso] if sexojefe==`val'
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
	local codind = 112                  // Código de indicador
	local grupo  = 7                    // Cantidad de grupos
	local canio  = 2                   // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'.xls", firstrow clear
sort AUXILIAR ANIO
drop if ANIO==.

g CODIND                 = 112
g NOMINDICADOR           = "PROMEDIO DE GASTO DE CONSUMO PER-CÁPITA DE LOS HOGARES (CTE. BASE ENERO 2006)"
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

replace QUINTIL			 = "QUINTIL 1"							  if AUXILIAR==4
replace QUINTIL			 = "QUINTIL 2"							  if AUXILIAR==5
replace QUINTIL			 = "QUINTIL 3"							  if AUXILIAR==6
replace QUINTIL			 = "QUINTIL 4"							  if AUXILIAR==7
replace QUINTIL			 = "QUINTIL 5"							  if AUXILIAR==8


replace NOMINDICADOR    = "PROMEDIO DE GASTO DE CONSUMO PER-CÁPITA DE LOS HOGARES (CTE. BASE ENERO 2006) EN HOGARES CON JEFATURA MASCULINA"      if AUXILIAR==11
replace NOMINDICADOR    = "PROMEDIO DE GASTO DE CONSUMO PER-CÁPITA DE LOS HOGARES (CTE. BASE ENERO 2006) EN HOGARES CON JEFATURA FEMENINA"       if AUXILIAR==12

** 

** 

export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace


