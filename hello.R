# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# =============================================================================#
#    *TÍTULO DE LA SINTAXIS:
#            Pobreza por Necesidades Básicas Insatisfechas (NBI)
#    *ENTIDAD:
#            Instituto Nacional de Estadística y Censos (INEC)
#    *UNIDAD TÉCNICA RESPONSABLE:
#            Dirección de Innovación en Métricas y Metodologías (DINME)
#    *RESPONSABLE:
#            ÁNGEL ANDRÉS VARGAS C.
# =============================================================================#

# =============================================================================#
#    Fecha de elaboración: 03 de octubre de 2024
# =============================================================================#



# =============================================================================#
#   			       Pobreza por Necesidades Básicas Insatisfechas
# =============================================================================#

# LIBRERÍAS---------------------------------------------------------------------
library(readr)
library(haven)
library(data.table)
library(openxlsx)
library(labelled)
library(stringr)
library(dplyr)
library(expss)
library(janitor)
library(plotrix)
library(writexl)

# DATA -------------------------------------------------------------------------
# IMPORTAR DATA ---------------------------------------------------------------#
# personas <- fread("E:/Andres/NBI/data/BDD_DATOS/BDDenemdu_personas_2023_anual.csv") %>% data.table()
# vivienda <- fread("E:/Andres/NBI/data/BDD_DATOS/BDDenemdu_vivienda_2023_anual.csv") %>% data.table()

dir_enemdu <- 'D:/Danilo Vera - INEC/djvera/Procesos/ENEMDU/Bases/ENEMDU_2022/'
# dir_enemdu <- 'E:/Andres/Homologacion_ENEMDU_CENSO/2022/ENEMDU_2022/'

personas_ref <- fread(paste0(dir_enemdu, 'BDDenemdu_personas_2022_anual.csv'), dec = ',')
vivienda_rev <- fread(paste0(dir_enemdu, 'BDDenemdu_vivienda_2022_anual.csv'), dec = ',')

personas <- personas_ref
vivienda <- vivienda_rev

NBI_hog <- function(personas, vivienda){

  ## FORMATO ID´s ----------------------------------------------------------------
  # PERSONAS ------------------------------------------------------------#
  personas[, id_upm := paste0(str_pad(ciudad, 6, pad = "0"), str_pad(conglomerado, 6, pad = "0"))]
  personas[, id_viv := paste0(id_upm, "00", str_pad(panelm, 2, pad = "0"), str_pad(vivienda, 2, pad = "0"))]
  personas[, id_hogar := paste0(id_viv, str_pad(hogar, 2, pad = "0"), str_pad(mes, 2, pad = "0"))]


  # VIVIENDA ------------------------------------------------------------#
  vivienda[, id_upm := paste0(str_pad(ciudad, 6, pad = "0"), str_pad(conglomerado, 6, pad = "0"))]
  vivienda[, id_viv := paste0(id_upm, "00", str_pad(panelm, 2, pad = "0"), str_pad(vivienda, 2, pad = "0"))]
  vivienda[, id_hogar := paste0(id_viv, str_pad(hogar, 2, pad = "0"), str_pad(mes, 2, pad = "0"))]


  ## UNION DE BASES: PERSONAS Y VIVIENDA------------------------------------------
  per_viv <- merge(personas, vivienda, by = 'id_hogar', all.x = T,
                   suffixes = c("", ".y"))


  ## CÁLCULO DE PERSONAS POR HOGAR -----------------------------------------------
  per_por_hog <- per_viv[, .(per_por_hog = .N), by = 'id_hogar']
  per_viv <- merge(per_viv, per_por_hog, by = 'id_hogar')

  ##=============================================================================#
  #                         -----CRITERIOS NBI------
  ##=============================================================================#

  # El cálculo de necesidades básicas incluye cinco dimensiones:

  #      1. Dependencia económica del hogar (dim1).
  #      2. Niños en edad escolar  (dim2).
  #      3. Características físicas de la vivienda (dim3).
  #      4. Disponibilidad de servicios básicos de la vivienda (dim4).
  #      5. Estado de hacinamiento del hogar (dim5).

  #==============================================================================#
  ##                   ---DEPENDENCIA ECONÓMICA DEL HOGAR----
  #==============================================================================#

  #--NOTA:-----------------------------------------------------------------------#
  #  Se considera deficitario si:
  #  El representante del hogar tiene 2 o menos años de escolaridad; y si existe
  #  más de 3 personas por cada persona ocupada en el hogar.
  #------------------------------------------------------------------------------#
  # AÑOS DE EDUCACIÓN DEL REPRESENTANTE DEL HOGAR #
  per_viv[, escol := 0]
  per_viv[, escol := fcase(p10a == 1, 0,
                           p10a == 2 & inrange(p10b, 0, 3), 2 * as.numeric(p10b),
                           p10a == 2 & inrange(p10b, 4, 10), 3 + as.numeric(p10b),
                           p10a == 3, 1,
                           p10a == 4, 1 + as.numeric(p10b),
                           p10a == 5, as.numeric(p10b),
                           p10a == 6, 7 + as.numeric(p10b),
                           p10a == 7, 10 + as.numeric(p10b),
                           p10a == 8, 13 + as.numeric(p10b),
                           p10a == 9, 13 + as.numeric(p10b),
                           p10a == 10, 18 + as.numeric(p10b),
                           default = NA)]

  # Dependencia económica#
  # Identificación de perceptores de ingresos#
  per_viv[, ingresos := 0]
  per_viv[(p03 > 14 & condact < 7), ingresos := 1]

  # Conteo de perceptores de ingresos por hogar
  perceptores <- per_viv[, .(percep = sum(ingresos, na.rm = T)), by = 'id_hogar']
  per_viv <- merge(per_viv, perceptores, by = 'id_hogar')

  # Relación miembros del hogar y de perceptores de ingresos#
  per_viv[percep > 0, relac_perc := per_por_hog/percep]

  # Resultado#
  per_viv[, r_dim := 0]
  per_viv[(escol <= 2 & p04 == 1) & relac_perc > 3, r_dim := 1]
  per_viv[(escol <= 2 & p04 == 1) & percep == 0, r_dim := 1]
  per_viv[percep == 0 & (escol <= 2 & p04 == 1), r_dim := 1]

  dim1 <- per_viv[, .(dim1 = sum(r_dim, na.rm = T)), by = 'id_hogar']
  per_viv <- merge(per_viv, dim1, by = 'id_hogar')

  #==============================================================================#
  ##                         ----NIÑOS EN EDAD ESCOLAR----
  #==============================================================================#

  #--NOTA:-----------------------------------------------------------------------#
  #  Se considera deficitario si:
  #  Existen en el hogar niños de 6 a 12 años que no asisten a clases.
  #------------------------------------------------------------------------------#
  # De 6 a 12 años que no asisten a clases
  per_viv[, no_asiste := 0]
  per_viv[p03 %in% 6:12 & p07 == 1, no_asiste := 0]
  per_viv[p03 %in% 6:12 & p07 == 2, no_asiste := 1]
  per_viv[p03 %in% 6:12 & is.na(p07), no_asiste := NA]

  # De 6 a 12 años que no asisten a clases por hogar
  no_asiste <- per_viv[, .(no_asiste_hog = sum(no_asiste, na.rm = T)), by = 'id_hogar']
  per_viv <- merge(per_viv, no_asiste, by = 'id_hogar')

  # Resultado
  per_viv[, dim2 :=0 ]
  per_viv[no_asiste_hog > 0, dim2 := 1]
  per_viv[no_asiste_hog == 0, dim2 := 0]
  per_viv[is.na(no_asiste_hog), dim2 := NA]

  #==============================================================================#
  ##               ----CARACTERÍSTICAS FÍSICAS DE LA VIVIENDA----
  #==============================================================================#

  #--NOTA:-----------------------------------------------------------------------#
  #  Se considera deficitario si:
  #  El material del piso de la vivienda es tierra u otros materiales; o
  #  El material de las paredes exteriores es caña no revestida, otros materiales.
  #------------------------------------------------------------------------------#
  per_viv[, dim3 := 0]
  per_viv[vi04a %in% c(7,8) | vi05a %in% c(6,7), dim3 := 1]
  per_viv[is.na(vi04a) & is.na(vi05a), dim3 := NA]

  #==============================================================================#
  ##         ---- DISPONIBILIDAD DE SERVICIOS BÁSICOS DE LA VIVIENDA----
  #==============================================================================#

  #--NOTA:-----------------------------------------------------------------------#
  #  Se considera deficitario si:
  #  La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado
  #  conectado a pozo ciego, inodoro o escusado con descarga directa al mar, río,
  #  lago o quebrada o tiene letrina; o
  #  Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero
  #  repartidor u otras fuentes (río, vertiente, acequia, canal, grieta o agua
  #  lluvia); o
  #  La vivienda no obtiene agua por tubería dentro de la vivienda.
  #------------------------------------------------------------------------------#
  per_viv[, dim4 := 0]
  per_viv[vi09 %in% c(3:5) | vi10 %in% c(2, 4:7), dim4 := 1]
  per_viv[is.na(vi10) & is.na(vi09), dim4 := NA]

  #==============================================================================#
  ##                  ----ESTADO DE HACINAMIENTO DEL HOGAR----
  #==============================================================================#

  #--NOTA:-----------------------------------------------------------------------#
  #  Se considera deficitario si:
  #  Existen en el hogar en promedio más de tres personas por cuarto utilizado
  #  para dormir.
  #  Cuando en el hogar no existen dormitorios exclusivos para dormir, se asume
  #  que existe uno.
  #------------------------------------------------------------------------------#
  per_viv[vi07 > 0, r_per_dor := per_por_hog/vi07]
  per_viv[vi07 == 0, r_per_dor := per_por_hog]
  per_viv[is.na(vi07), r_per_dor := NA]

  # Resultado
  per_viv[r_per_dor <= 3, dim5  := 0]
  per_viv[r_per_dor > 3, dim5  := 1]
  per_viv[is.na(r_per_dor), dim5  := NA]

  #==============================================================================#
  #             -----CÁLCULO DEL INDICADOR DE POBREZA POR NBI-----
  #==============================================================================#

  # Conteo del número de carencias por hogar
  per_viv[dim1 < 9, knbi := rowSums(cbind(dim1, dim2, dim3, dim4, dim5), na.rm = T)]
  per_viv[dim1 >= 9, knbi := rowSums(cbind(dim2, dim3, dim4, dim5), na.rm = T)]

  # Resultado pobreza por NBI
  per_viv[knbi >= 1, nbi := 1]
  per_viv[knbi == 0 & dim1 < 9, nbi := 0]

  # Resultado pobreza extrema por NBI
  per_viv[knbi >= 2, xnbi := 1]
  per_viv[knbi < 2 & dim1 < 9, xnbi := 0]

  # Descomposición NBI por componentes
  per_viv[, dimk := 0]
  per_viv[knbi == 1 & dim1 == 1, dimk := 1]
  per_viv[knbi == 1 & dim2 == 1, dimk := 2]
  per_viv[knbi == 1 & dim3 == 1, dimk := 3]
  per_viv[knbi == 1 & dim4 == 1, dimk := 4]
  per_viv[knbi == 1 & dim5 == 1, dimk := 5]
  per_viv[knbi > 1, dimk := 6]
  per_viv[knbi == 0 & dim1 < 9, dimk := 0]

  resultado <- per_viv %>% distinct(id_hogar, id_viv, dim1, dim2, dim3, dim4, dim5, knbi, nbi, xnbi,dimk)

  return(resultado)

}

resul_NBI <- NBI_hog(personas_ref, viviendas_rev)




val_lab(per_viv$dimk) = num_lab("
                            0 No Pobre por NBI
                            1 Pobre en dim 1
                            2 Pobre en dim 2
                            3 Pobre en dim 3
                            4 Pobre en dim 4
                            5 Pobre en dim 5
                            6 overlap
                            ")


#==============================================================================#
#                            -----RESULTADOS-----
#==============================================================================#

resultados <- per_viv %>%
  summarise(N = sum(nbi == 1),
            pp_dim1 = weighted.mean(dim1, fexp),
            pp_dim2 = weighted.mean(dim2, fexp),
            pp_dim3 = weighted.mean(dim3, fexp),
            pp_dim4 = weighted.mean(dim4, fexp),
            pp_dim5 = weighted.mean(dim5, fexp),
            pp_dimk = weighted.mean(dimk, fexp),
            pp_knbi = weighted.mean(knbi, fexp),
            pp_nbi  = weighted.mean( nbi, fexp),
  )

#==============================================================================#
# EXPORTAR RESULTADOS#


