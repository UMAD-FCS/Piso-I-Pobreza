
*======================================================================================*
*                       **   POBREZA Y DESIGUALDAD   **
*                            MATRIZ PARA BASE WEB
*
*
* Creación:      09/06/2020
* Actualización: 15/10/2021
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
/*
	cd "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD"
	global bases "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon\En uso"
	global tabulados C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO II_MOTORES\Base web\1. Pobreza y desigualdad\Extracciones parciales\1. Pobreza y desigualdad
*/
	cd "C:\Users\Usuario\Dropbox\UMAD"
	global bases "C:\Users\Usuario\Dropbox\UMAD\Bases de datos\Encuesta Continua de Hogares (ECH)\Compatibilizadas IECON\Bases DESCA"
	global tabulados "C:\Users\Usuario\Dropbox\UMAD\Sociodemografico\Pobreza\Tabulados"



*======================================================================================*
*              CARACTERÍSTICAS GENERALES DE LA MATRIZ DE EXPORTACAIÓN      
*======================================================================================*

    global seieURB 4/19   // Años de serie para país urbano
	global seieTOT 6/19        // Años de serie para total país

	  

*======================================================================================*
*                 1.1. NIVEL DE INGRESOS      
*======================================================================================*

* INDICADOR: Promedio de ingresos per cápita de los hogares (precios corrientes)
* CÓDIGO:    111
* GRUPOS:    Región / Condición de pobreza / Quintil de ingreso / Sexo del jefe(a)
* FUENTE:    ECH (INE)
* PERÍODO:   2004-2020 País urbano / 2006-2020 Total país 


**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *TOTAL PAÍS
	local i      = 1
	local codind = 111                  // Código de indicador
	local grupo  = 12                   // Cantidad de grupos
	local canio  = 15                   // Cantidad de años de la serie

	local filas = (1 + `grupo') * `canio' // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',3,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO

	foreach anio of numlist $seieTOT  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g sexojefe=0
	replace sexojefe=1 if bc_pe4==1 & bc_pe2==1
	replace sexojefe=2 if bc_pe4==1 & bc_pe2==2
	
	g ht11_d = ht11*bc_ipc_tot
	gen ht11_d_pc = ht11_d/ht19

	
	tabstat ht11_d_pc [aw=bc_pesoan] if bc_pe4==1, save s(mean)
	matrix MATR  [`i',1]=  r(StatTotal) 
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
   	
	local i  = `i' + 1
	local j  =  1
	foreach val of numlist 1/3  {
	tabstat ht11_d_pc [aw=bc_pesoan] if bc_pe4==1 & bd_region==`val', save s(mean)
	matrix MATR  [`i',1]=  r(StatTotal) 
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  `anio'
	local i  = `i' + 1
    }
    *
	
	local j  = 	4
	foreach val of numlist 1/5  {
	tabstat ht11_d_pc [aw=bc_pesoan] if bc_pe4==1 & bd_quintilesy==`val', save s(mean)
	matrix MATR  [`i',1]=  r(StatTotal) 
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *	
	
	local j  = 	9
	foreach val of numlist 0/1  {
	tabstat ht11_d_pc [aw=bc_pesoan] if bc_pe4==1 & pobre06==`val', save s(mean)
	matrix MATR  [`i',1]=  r(StatTotal) 
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *	
	
	local j  = 	11
	foreach val of numlist 1/2  {
	tabstat ht11_d_pc [aw=bc_pesoan] if bc_pe4==1 & sexojefe==`val', save s(mean)
	matrix MATR  [`i',1]=  r(StatTotal) 
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	
	}
    *

**

use "$bases\fusionada_personasyhogares_20.dta", clear
global anio 20
	mat def aux_2020 = J(12,1,.)

	g sexojefe=0
	replace sexojefe=1 if e30==1 & e26==1
	replace sexojefe=2 if e30==1 & e26==2


	g bc_ipc_tot=.
	g bc_ipc_tot_tot=. 

	replace bc_ipc_tot_tot= 0.367126174	if anio=="2020"&mes==1
	replace bc_ipc_tot=0.360590014 if anio=="2020"&mes==1

	replace bc_ipc_tot_tot=0.359598378	if anio=="2020"&mes==2
	replace bc_ipc_tot=0.353341973 if anio=="2020"&mes==2

	replace bc_ipc_tot_tot=0.357408439	if anio=="2020"&mes==3
	replace bc_ipc_tot=0.350863606 if anio=="2020"&mes==3

	replace bc_ipc_tot_tot=0.352706586	if anio=="2020"&mes==4
	replace bc_ipc_tot=0.346976192 if anio=="2020"&mes==4

	replace bc_ipc_tot_tot=0.345801038	if anio=="2020"&mes==5
	replace bc_ipc_tot=0.341163344 if anio=="2020"&mes==5

	replace bc_ipc_tot_tot=0.343854751	if anio=="2020"&mes==6
	replace bc_ipc_tot=0.339346002 if anio=="2020"&mes==6

	replace bc_ipc_tot_tot=0.343791309	if anio=="2020"&mes==7
	replace bc_ipc_tot=0.339516038 if anio=="2020"&mes==7

	replace bc_ipc_tot_tot=0.341914564	if anio=="2020"&mes==8
	replace bc_ipc_tot=0.3372272 if anio=="2020"&mes==8

	replace bc_ipc_tot_tot=0.339965133	if anio=="2020"&mes==9
	replace bc_ipc_tot=0.334938911 if anio=="2020"&mes==9

	replace bc_ipc_tot_tot=0.337807994	if anio=="2020"&mes==10
	replace bc_ipc_tot=0.332473708 if anio=="2020"&mes==10

	replace bc_ipc_tot_tot=0.33585957	if anio=="2020"&mes==11
	replace bc_ipc_tot=0.330512863 if anio=="2020"&mes==11

	replace bc_ipc_tot_tot=0.334908811	if anio=="2020"&mes==12
	replace bc_ipc_tot=0.329329957 if anio=="2020"&mes==12

	g ht11_d = ht11*bc_ipc_tot
	gen ht11_d_pc = ht11_d/ht19

g uno=1  						//Variabe auxiliar para construir cantidad de personas
replace uno=0 if e30==14 		//Excluyo de la suma al servicio doméstico

egen bd_ht19 = sum (uno), by (numero) 
label var bd_ht19 "Cantidad de pseronas (sin servicio doméstico)"

g ypc1=ht11/bd_ht19 if e30==1
g quintilesy_prev=.
foreach i of numlist 1/12 {
xtile quintilesy_prev`i'= ypc1 [aw=pesomen] if mes==`i', n(5)
replace quintilesy_prev=quintilesy_prev`i' if mes==`i'
drop quintilesy_prev`i'
}

egen bd_quintilesy= max (quintilesy_prev), by (numero)
label var bd_quintilesy "Quintil de ingreso per cápita del hogar"

drop uno ypc1 quintilesy_prev

rename pobre_06 pobre06
	
	cap drop bd_region
	g bd_region=.
	replace bd_region=1 if region_4== 1 | region_4== 2
	replace bd_region=2 if region_4== 3
	replace bd_region=3 if region_4== 4

	rename e30 bc_pe4
		
	
	foreach mes of numlist 1/12 {
	tabstat ht11_d_pc if mes==`mes'&bc_pe4==1 [aw=pesomen], save s(mean)
	matrix aux_2020 [`mes',1]=r(StatTotal)
}

	local i  =  183 // (1 + `grupo') * (`canio'-1) 
	
	
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  $anio
   	
	local i  = `i' + 1 /*NO se procesa país urbano para no duplicar valores*/
	
	local j  = 	1
	foreach val of numlist 1/3  {
foreach mes of numlist 1/12 {
	tabstat ht11_d_pc [aw=pesomen] if mes==`mes' & bd_region==`val' & bc_pe4==1, save s(mean)
	matrix aux_2020 [`mes',1]=r(StatTotal)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
    }
    *
	
	
	local j  = 	4
		foreach val of numlist 1/5  {
foreach mes of numlist 1/12 {
	tabstat ht11_d_pc [aw=pesomen] if mes==`mes' & bd_quintilesy==`val' & bc_pe4==1, save s(mean)
	matrix aux_2020 [`mes',1]=r(StatTotal)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
    }
    *

	
	
	local j  = 	9
	foreach val of numlist 0/1  {
	foreach mes of numlist 1/12 {
	tabstat ht11_d_pc [aw=pesomen] if mes==`mes' & pobre06==`val' & bc_pe4==1, save s(mean)
	matrix aux_2020 [`mes',1]=r(StatTotal)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
    }
    *

	local j  = 	11
	foreach val of numlist 1/2  {
	foreach mes of numlist 1/12 {
	tabstat ht11_d_pc [aw=pesomen] if mes==`mes' & sexojefe==`val' & bc_pe4==1, save s(mean)
	matrix aux_2020 [`mes',1]=r(StatTotal)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
    }
    *

xml_tab MATR, save("$tabulados\AUX111_TP.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 111                  // Código de indicador
	local grupo  = 12                   // Cantidad de grupos
	local canio  = 15                  // Cantidad de años de la serie


import  excel "$tabulados\AUX`codind'_TP.xls", firstrow clear
sort AUXILIAR ANIO
destring ANIO, replace
destring AUXILIAR, replace
drop if ANIO==.

g CODIND                 = 111
g NOMINDICADOR           = "Promedio de ingreso per-cápita de los hogares (Cte. Base diciembre 2006)"
g CATEGORIA				 = "Ingresos y desigualdad"
g PESTAÑA				 = "Total País"
g CORTE					 = "Total"
g CORTE_NUEVA			 = "Total"
g REGIÓN				 = "Todos"
g TRAMO					 =.
g SEXOJEFATURA			 = "Todos"
g POBREZA				 = "Todos"
g SEXO                   =.
g ASCENDENCIA    		 =.
g DECIL					 =.
g QUINTIL       		 = "Todos"
g DEPARTAMENTOUY		 =.
g URBANORURALUY 		 = "Total País"
g PAÍS			 		 = "Uruguay"
g RESPONSABLE			 = "JIMENA PANDOLFI"

**

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y TÍTULOS DE LA VARIABLES AUXILIARES CORRESPONDIENTES**

replace REGIÓN    = "Urbano (más de 5.000 habitantes)"   if AUXILIAR==1
replace REGIÓN    = "Urbano (menos de 5.000 habitantes)"   if AUXILIAR==2
replace REGIÓN    = "Rural disperso"                       if AUXILIAR==3

replace URBANORURALUY    = "Urbano (más de 5.000 habitantes)"   if AUXILIAR==1
replace URBANORURALUY    = "Urbano (menos de 5.000 habitantes)"   if AUXILIAR==2
replace URBANORURALUY    = "Rural disperso"                      if AUXILIAR==3

replace QUINTIL			 = "Quintil 1"							  if AUXILIAR==4
replace QUINTIL			 = "Quintil 2"							  if AUXILIAR==5
replace QUINTIL			 = "Quintil 3"							  if AUXILIAR==6
replace QUINTIL			 = "Quintil 4"							  if AUXILIAR==7
replace QUINTIL			 = "Quintil 5"							  if AUXILIAR==8

replace POBREZA    = "Pobre"            if AUXILIAR==9
replace POBREZA    = "No pobre"         if AUXILIAR==10

replace SEXOJEFATURA			 = "Jefe Varón"							  if AUXILIAR==11
replace SEXOJEFATURA			 = "Jefa Mujer"							  if AUXILIAR==12


** 

** 

save "$tabulados\\`codind'_TP.dta", replace




************************************************************************************************
************************************************************************************************

    *PAÍS URBANO
	local i      = 1
	local codind = 111                  // Código de indicador
	local grupo  = 8                    // Cantidad de grupos (se agrega un grupo para el total)
	local canio  = 17                   // Cantidad de años de la serie

	local filas = `grupo' * `canio' // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',3,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO

	foreach anio of numlist $seieURB  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g sexojefe=0
	replace sexojefe=1 if bc_pe4==1 & bc_pe2==1
	replace sexojefe=2 if bc_pe4==1 & bc_pe2==2
	
	g ht11_d = ht11*bc_ipc_tot
	gen ht11_d_pc = ht11_d/ht19
	
	local j  = 	0
	tabstat ht11_d_pc [aw=bc_pesoan] if bc_pe4==1 & bc_filtloc==1, save s(mean)
 	matrix MATR  [`i',1]=  r(StatTotal)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'

	local i  = `i' + 1
	
	local j  = 	1
	foreach val of numlist 1/5  {
	tabstat ht11_d_pc [aw=bc_pesoan] if bc_pe4==1 & bd_quintilesy==`val' & bc_filtloc==1, save s(mean)
	matrix MATR  [`i',1]=  r(StatTotal)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
	
		local j  = 	6
	foreach val of numlist 1/2  {
	tabstat ht11_d_pc [aw=bc_pesoan] if bc_pe4==1 & sexojefe==`val' & bc_filtloc==1, save s(mean)
	matrix MATR  [`i',1]=  r(StatTotal)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *	
	
	}
    *

**

use "$bases\fusionada_personasyhogares_20.dta", clear
global anio 20
	mat def aux_2020 = J(12,1,.)

	g sexojefe=0
	replace sexojefe=1 if e30==1 & e26==1
	replace sexojefe=2 if e30==1 & e26==2


	g bc_ipc_tot=.
	g bc_ipc_tot_tot=. 

	replace bc_ipc_tot_tot= 0.367126174	if anio=="2020"&mes==1
	replace bc_ipc_tot=0.360590014 if anio=="2020"&mes==1

	replace bc_ipc_tot_tot=0.359598378	if anio=="2020"&mes==2
	replace bc_ipc_tot=0.353341973 if anio=="2020"&mes==2

	replace bc_ipc_tot_tot=0.357408439	if anio=="2020"&mes==3
	replace bc_ipc_tot=0.350863606 if anio=="2020"&mes==3

	replace bc_ipc_tot_tot=0.352706586	if anio=="2020"&mes==4
	replace bc_ipc_tot=0.346976192 if anio=="2020"&mes==4

	replace bc_ipc_tot_tot=0.345801038	if anio=="2020"&mes==5
	replace bc_ipc_tot=0.341163344 if anio=="2020"&mes==5

	replace bc_ipc_tot_tot=0.343854751	if anio=="2020"&mes==6
	replace bc_ipc_tot=0.339346002 if anio=="2020"&mes==6

	replace bc_ipc_tot_tot=0.343791309	if anio=="2020"&mes==7
	replace bc_ipc_tot=0.339516038 if anio=="2020"&mes==7

	replace bc_ipc_tot_tot=0.341914564	if anio=="2020"&mes==8
	replace bc_ipc_tot=0.3372272 if anio=="2020"&mes==8

	replace bc_ipc_tot_tot=0.339965133	if anio=="2020"&mes==9
	replace bc_ipc_tot=0.334938911 if anio=="2020"&mes==9

	replace bc_ipc_tot_tot=0.337807994	if anio=="2020"&mes==10
	replace bc_ipc_tot=0.332473708 if anio=="2020"&mes==10

	replace bc_ipc_tot_tot=0.33585957	if anio=="2020"&mes==11
	replace bc_ipc_tot=0.330512863 if anio=="2020"&mes==11

	replace bc_ipc_tot_tot=0.334908811	if anio=="2020"&mes==12
	replace bc_ipc_tot=0.329329957 if anio=="2020"&mes==12

	g ht11_d = ht11*bc_ipc_tot
	gen ht11_d_pc = ht11_d/ht19

g uno=1  						//Variabe auxiliar para construir cantidad de personas
replace uno=0 if e30==14 		//Excluyo de la suma al servicio doméstico

egen bd_ht19 = sum (uno), by (numero) 
label var bd_ht19 "Cantidad de pseronas (sin servicio doméstico)"

g ypc1=ht11/bd_ht19 if e30==1
g quintilesy_prev=.
foreach i of numlist 1/12 {
xtile quintilesy_prev`i'= ypc1 [aw=pesomen] if mes==`i', n(5)
replace quintilesy_prev=quintilesy_prev`i' if mes==`i'
drop quintilesy_prev`i'
}

egen bd_quintilesy= max (quintilesy_prev), by (numero)
label var bd_quintilesy "Quintil de ingreso per cápita del hogar"

drop uno ypc1 quintilesy_prev

rename pobre_06 pobre06
	
	cap drop bd_region
	g bd_region=.
	replace bd_region=1 if region_4== 1 | region_4== 2
	replace bd_region=2 if region_4== 3
	replace bd_region=3 if region_4== 4

	rename e30 bc_pe4
	g bc_filtloc = region_4<3
		
	
	foreach mes of numlist 1/12 {
	tabstat ht11_d_pc if mes==`mes'&bc_pe4==1& bc_filtloc==1 [aw=pesomen], save s(mean)
	matrix aux_2020 [`mes',1]=r(StatTotal)
}

	local i  =  129 // (1 + `grupo') * (`canio'-1) 
	
	
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  $anio
   	
	local i  = `i' + 1 /*NO se procesa país urbano para no duplicar valores*/
	
	local j  = 	1
		foreach val of numlist 1/5  {
foreach mes of numlist 1/12 {
	tabstat ht11_d_pc [aw=pesomen] if mes==`mes' & bd_quintilesy==`val' & bc_pe4==1& bc_filtloc==1, save s(mean)
	matrix aux_2020 [`mes',1]=r(StatTotal)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
    }
    *

	
	
	local j  = 	6
	foreach val of numlist 1/2  {
	foreach mes of numlist 1/12 {
	tabstat ht11_d_pc [aw=pesomen] if mes==`mes' & sexojefe==`val' & bc_pe4==1 & bc_filtloc==1, save s(mean)
	matrix aux_2020 [`mes',1]=r(StatTotal)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
    }
    *

	
xml_tab MATR, save("$tabulados\AUX111_PU.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 111                  // Código de indicador
	local grupo  = 8                    // Cantidad de grupos
	local canio  = 17                   // Cantidad de años de la serie


import  excel "$tabulados\AUX`codind'_PU.xls", firstrow clear
sort AUXILIAR ANIO
destring ANIO, replace
destring AUXILIAR, replace

g CODIND                 = 111
g NOMINDICADOR           = "Promedio de ingreso per-cápita de los hogares (Cte. Base diciembre 2006)"
g CATEGORIA				 = "Ingresos y desigualdad"
g PESTAÑA				 = "País Urbano"
g CORTE					 = "Total"
g CORTE_NUEVA			 = "Total"
g REGIÓN				 = "Todos"
g TRAMO					 =.
g SEXOJEFATURA			 = "Todos"
g POBREZA				 = "Todos"
g SEXO                   =.
g ASCENDENCIA    		 =.
g DECIL					 =.
g QUINTIL       		 = "Todos"
g DEPARTAMENTOUY		 =.
g URBANORURALUY 		 = "Urbano (más de 5.000 habitantes)"
g PAÍS			 		 = "Uruguay"
g RESPONSABLE			 = "JIMENA PANDOLFI"

**

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y TÍTULOS DE LA VARIABLES AUXILIARES CORRESPONDIENTES**

destring AUXILIAR, replace

replace URBANORURALUY    = "Urbano (más de 5.000 habitantes)"            if AUXILIAR==0


replace QUINTIL			 = "Quintil 1"							  if AUXILIAR==1
replace QUINTIL			 = "Quintil 2"							  if AUXILIAR==2
replace QUINTIL			 = "Quintil 3"							  if AUXILIAR==3
replace QUINTIL			 = "Quintil 4"							  if AUXILIAR==4
replace QUINTIL			 = "Quintil 5"							  if AUXILIAR==5

replace SEXOJEFATURA			 = "Jefe Varón"							  if AUXILIAR==6
replace SEXOJEFATURA			 = "Jefa Mujer"							  if AUXILIAR==7


** 

save  "$tabulados\\`codind'_PU.dta", replace



************************************************************************************************
************************************************************************************************
*FUSIÓN PAÍS URBANO Y TOTAL PAÍS

append using "$tabulados\\`codind'_TP.dta", force


************************************************************************************************
************************************************************************************************
*EDICIÓN DE BASE CON NUEVO FORMATO

replace CORTE = "Total"              if PESTAÑA == "País Urbano" & QUINTIL == "Todos" & SEXOJEFATURA =="Todos"
replace CORTE = "Quintil de ingreso" if PESTAÑA == "País Urbano" & QUINTIL != "Todos" & SEXOJEFATURA =="Todos"
replace CORTE = "Sexo del jefe(a)"   if PESTAÑA == "País Urbano" & QUINTIL == "Todos" & SEXOJEFATURA !="Todos"

replace CORTE_NUEVA = "Total"              if PESTAÑA == "País Urbano" & QUINTIL == "Todos" & SEXOJEFATURA =="Todos"
replace CORTE_NUEVA = "Quintil de ingreso" if PESTAÑA == "País Urbano" & QUINTIL != "Todos" & SEXOJEFATURA =="Todos"
replace CORTE_NUEVA = "Sexo del jefe(a)"   if PESTAÑA == "País Urbano" & QUINTIL == "Todos" & SEXOJEFATURA !="Todos"

replace CORTE = "Total"              if PESTAÑA == "Total País" & QUINTIL == "Todos" & SEXOJEFATURA =="Todos" & REGIÓN == "Todos" & POBREZA =="Todos" & URBANORURALUY == "Total País"
replace CORTE = "Quintil de ingreso" if PESTAÑA == "Total País" & QUINTIL != "Todos" & SEXOJEFATURA =="Todos" & REGIÓN == "Todos" & POBREZA =="Todos" & URBANORURALUY == "Total País"
replace CORTE = "Sexo del jefe(a)"   if PESTAÑA == "Total País" & QUINTIL == "Todos" & SEXOJEFATURA !="Todos" & REGIÓN == "Todos" & POBREZA =="Todos" & URBANORURALUY == "Total País"
replace CORTE = "Pobreza"            if PESTAÑA == "Total País" & QUINTIL == "Todos" & SEXOJEFATURA =="Todos" & REGIÓN == "Todos" & POBREZA !="Todos" & URBANORURALUY == "Total País"
replace CORTE = "Región"  			 if PESTAÑA == "Total"

replace CORTE_NUEVA = "Total"              if PESTAÑA == "Total País" & QUINTIL == "Todos" & SEXOJEFATURA =="Todos" & REGIÓN == "Todos" & POBREZA =="Todos"
replace CORTE_NUEVA = "Quintil de ingreso" if PESTAÑA == "Total País" & QUINTIL != "Todos" & SEXOJEFATURA =="Todos" & REGIÓN == "Todos" & POBREZA =="Todos"
replace CORTE_NUEVA = "Sexo del jefe(a)"   if PESTAÑA == "Total País" & QUINTIL == "Todos" & SEXOJEFATURA !="Todos" & REGIÓN == "Todos" & POBREZA =="Todos"
replace CORTE_NUEVA = "Pobreza"            if PESTAÑA == "Total País" & QUINTIL == "Todos" & SEXOJEFATURA =="Todos" & REGIÓN == "Todos" & POBREZA !="Todos" 
replace CORTE_NUEVA = "Región"  		   if PESTAÑA == "Total País" & QUINTIL == "Todos" & SEXOJEFATURA =="Todos" & REGIÓN != "Todos" & POBREZA =="Todos"

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
recode ANIO (20=2020)
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
drop if ANIO == .
export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace
