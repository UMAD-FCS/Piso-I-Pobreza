
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

* INDICADOR: Porcentaje de hogares en situación de pobreza
* CÓDIGO:    121
* GRUPOS:    Región / Sexo del jefe(a)
* FUENTE:    ECH (INE)
* PERÍODO:   2006-2018


**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *TOTAL PAÍS
	local i      = 1
	local codind = 121                // Código de indicador
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
	
	mean pobre06 [aw=bc_pesoan] if bc_pe4==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
   	
	local i  = `i' + 1
	foreach val of numlist 1/3  {
	mean pobre06 [aw=bc_pesoan] if bc_pe4==1 & bd_region==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `val'
	matrix MATR  [`i',3]=  `anio'
	local i  = `i' + 1
    }
    *
	
	local j  = 	4
	foreach val of numlist 1/2  {
	mean pobre06 [aw=bc_pesoan] if bc_pe4==1 & sexojefe==`val'
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
	local codind = 121                // Código de indicador
	local grupo  = 5                  // Cantidad de grupos
	local canio  = 14                 // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'.xls", firstrow clear
sort AUXILIAR ANIO
drop if ANIO==.

g CODIND                 = 121
g NOMINDICADOR           = "Hogares en situación de pobreza (% de la población total) (INE)"
g CATEGORIA				 = "Ingresos y desigualdad"
g PESTAÑA				 = "Total País"
g CORTE					 = "Total"
g CORTE_NUEVA			 = "Total"
g REGIÓN				 = "Todos"
g TRAMO					 =.
g SEXOJEFATURA			 = "Todos"
g POBREZA				 =.
g SEXO                   =.
g ASCENDENCIA    		 =.
g DECIL					 =.
g QUINTIL       		 =.
g DEPARTAMENTOUY		 =.
g URBANORURALUY 		 = "Total País"
g PAÍS			 		 = "Uruguay"
g RESPONSABLE			 = "JIMENA PANDOLFI"

**

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y TÍTULOS DE LA VARIABLES AUXILIARES CORRESPONDIENTES**

replace REGIÓN    = "Urbano (más de 5.000 habitantes)"     if AUXILIAR==1
replace REGIÓN    = "Urbano (menos de 5.000 habitantes)"   if AUXILIAR==2
replace REGIÓN    = "Rural disperso"                       if AUXILIAR==3

replace URBANORURALUY    = "Urbano (más de 5.000 habitantes)"     if AUXILIAR==1
replace URBANORURALUY    = "Urbano (menos de 5.000 habitantes)"   if AUXILIAR==2
replace URBANORURALUY    = "Rural disperso"                       if AUXILIAR==3

replace SEXOJEFATURA			 = "Jefe Varón"							  if AUXILIAR==4
replace SEXOJEFATURA			 = "Jefa Mujer"							  if AUXILIAR==5

** 

*EDICIÓN DE BASE CON NUEVO FORMATO

replace CORTE = "Total"              if PESTAÑA == "Total País" & SEXOJEFATURA =="Todos" & REGIÓN == "Todos" & URBANORURALUY == "Total País"
replace CORTE = "Sexo del jefe(a)"   if PESTAÑA == "Total País" & SEXOJEFATURA !="Todos" & REGIÓN == "Todos" & URBANORURALUY == "Total País"
replace CORTE = "Región"  			 if PESTAÑA == "Total"

replace CORTE_NUEVA = "Total"              if PESTAÑA == "Total País" & SEXOJEFATURA =="Todos" & REGIÓN == "Todos" 
replace CORTE_NUEVA = "Sexo del jefe(a)"   if PESTAÑA == "Total País" & SEXOJEFATURA !="Todos" & REGIÓN == "Todos" 
replace CORTE_NUEVA = "Región"  		   if PESTAÑA == "Total País" & SEXOJEFATURA =="Todos" & REGIÓN != "Todos" 

recode ANIO (0=2000)
recode ANIO (1=2001)
recode ANIO (2=2002)
recode ANIO (3=2003)
recode ANIO (4=2004)
recode ANIO (5=2005)
recode ANIO (6=2006)
recode ANIO (7=2007)
recode ANIO (8=2008)
recode ANIO (9=2009)
recode ANIO (10=2010)
recode ANIO (11=2011)
recode ANIO (12=2012)
recode ANIO (13=2013)
recode ANIO (14=2014)
recode ANIO (15=2015)
recode ANIO (16=2016)
recode ANIO (17=2017)
recode ANIO (18=2018)
recode ANIO (19=2019)
recode ANIO (81=1981)
recode ANIO (82=1982)
recode ANIO (83=1983)
recode ANIO (84=1984)
recode ANIO (85=1985)
recode ANIO (86=1986)
recode ANIO (87=1987)
recode ANIO (88=1988)
recode ANIO (89=1989)
recode ANIO (90=1990)
recode ANIO (91=1991)
recode ANIO (92=1992)
recode ANIO (93=1993)
recode ANIO (94=1994)
recode ANIO (95=1995)
recode ANIO (96=1996)
recode ANIO (97=1997)
recode ANIO (98=1998)
recode ANIO (99=1999)

order CODIND NOMINDICADOR CATEGORIA PESTAÑA CORTE CORTE_NUEVA REGIÓN TRAMO SEXOJEFATURA POBREZA SEXO ASCENDENCIA DECIL QUINTIL DEPARTAMENTOUY URBANORURALUY PAÍS ANIO VALOR RESPONSABLE
drop A AUXILIAR



export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace

