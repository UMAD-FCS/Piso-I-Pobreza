
use "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon\En uso\Fusionada_personasyhogares_20.dta", clear

g bd_region=.
replace bd_region=1 if region_4== 1 | region_4== 2
replace bd_region=2 if region_4== 3
replace bd_region=3 if region_4== 4


** Hogares en situación de pobreza (% de la población total) (INE) **

ta pobre06 [aw=pesomen] if e30==1

ta pobre06 bd_region [aw=pesomen] if e30==1, col nof

ta pobre06 [aw=pesomen] if e30==1 & e26==1
ta pobre06 [aw=pesomen] if e30==1 & e26==2

** Personas en situación de pobreza (% de la población total) (INE) **

ta pobre06 [aw=pesomen] 
ta pobre06 bd_region [aw=pesomen], col nof
ta pobre06 e26 [aw=pesomen], col nof

g tramo =. 
replace tramo =1 if e27>=0  & e27<=5
replace tramo =2 if e27>=6  & e27<=12
replace tramo =3 if e27>=13 & e27<=18
replace tramo =4 if e27>=19 & e27<=24
replace tramo =5 if e27>=25 & e27<=29
replace tramo =6 if e27>=30 & e27<=64
replace tramo =7 if e27>=65

ta pobre06 tramo [aw=pesomen], col nof


** Hogares en situación de indigencia (% de la población total) (INE) **

ta indigente06 [aw=pesomen] if e30==1

ta indigente06 bd_region [aw=pesomen] if e30==1, col nof
ta indigente06 [aw=pesomen] if e30==1 & e26==1
ta indigente06 [aw=pesomen] if e30==1 & e26==2


** Personas en situación de indigencia (% de la población total) (INE) **

ta indigente06 [aw=pesomen] 
ta indigente06 bd_region [aw=pesomen], col nof
ta indigente06 e26 [aw=pesomen], col nof

g tramo =. 
replace tramo =1 if e27>=0  & e27<=5
replace tramo =2 if e27>=6  & e27<=12
replace tramo =3 if e27>=13 & e27<=18
replace tramo =4 if e27>=19 & e27<=24
replace tramo =5 if e27>=25 & e27<=29
replace tramo =6 if e27>=30 & e27<=64
replace tramo =7 if e27>=65

ta indigente06 tramo [aw=pesomen], col nof

