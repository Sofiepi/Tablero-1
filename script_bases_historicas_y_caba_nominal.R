##Instalar paquetes, permisos,etc------
#install.packages("googlesheets4")
#install.packages("googledrive")
#install.packages("openxlsx")
#install.packages("treemapify")


##Cargar paquetes
library(googlesheets4)
library(googledrive)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)
library(openxlsx)
library(treemapify)
library(data.table)



##Limpiar el entorno
rm(list = ls())

#SETEAR EL WORKING DIRECTORY -- definir si usamos drive u otra carpeta local
#setwd("C:/Users/")


drive_deauth()
drive_auth(email = "maila@gmail.com")

##Autenticación de google drive
gs4_auth(email = "mail@gmail.com")
drive_auth(email = "mail@gmail.com")


##Cargo TODOS los archivos ------
##Cargo CABA_nominal
CABA_NOMINAL_PROCESADO <- readRDS("CABA_NOMINAL_PROCESADO.rds")


##Cargo los archivos
CABA_NOMINAL_HISTORICO_PROCESADO_2019 <- readRDS("CABA_NOMINAL_HISTORICO_PROCESADO_2019.RDS")
CABA_NOMINAL_HISTORICO_PROCESADO_2020 <- readRDS("CABA_NOMINAL_HISTORICO_PROCESADO_2020.RDS")
CABA_NOMINAL_HISTORICO_PROCESADO_2021 <- readRDS("CABA_NOMINAL_HISTORICO_PROCESADO_2021.RDS")
CABA_NOMINAL_HISTORICO_PROCESADO_2022 <- readRDS("CABA_NOMINAL_HISTORICO_PROCESADO_2022.RDS")
CABA_NOMINAL_HISTORICO_PROCESADO_2023 <- readRDS("CABA_NOMINAL_HISTORICO_PROCESADO_2023.RDS")
CABA_NOMINAL_HISTORICO_PROCESADO_2024 <- readRDS("CABA_NOMINAL_HISTORICO_PROCESADO_2024.RDS")
CABA_NOMINAL_HISTORICO_PROCESADO_2025 <- readRDS("CABA_NOMINAL_HISTORICO_PROCESADO_2025.RDS")

## UNIFICAR TIPOS DE DATOS -----

bases <- list(
  CABA_NOMINAL_HISTORICO_PROCESADO_2019,
  CABA_NOMINAL_HISTORICO_PROCESADO_2020,
  CABA_NOMINAL_HISTORICO_PROCESADO_2021,
  CABA_NOMINAL_HISTORICO_PROCESADO_2022,
  CABA_NOMINAL_HISTORICO_PROCESADO_2023,
  CABA_NOMINAL_HISTORICO_PROCESADO_2024,
  CABA_NOMINAL_HISTORICO_PROCESADO_2025,
  CABA_NOMINAL_PROCESADO
)

# Variables que existen en todas las bases
variables <- unique(unlist(lapply(bases, names)))

# Para cada variable
for (var in variables) {
  
  # Buscar el primer histórico que tenga esa variable
  referencia <- NULL
  
  for (i in 1:6) {
    if (var %in% names(bases[[i]])) {
      referencia <- bases[[i]][[var]]
      break
    }
  }
  
  # Si existe una referencia, usar su tipo
  if (!is.null(referencia)) {
    
    tipo <- class(referencia)[1]
    
    # Aplicar ese tipo a todas las bases
    bases <- lapply(bases, function(base) {
      
      if (var %in% names(base)) {
        
        if (tipo == "numeric" || tipo == "integer") {
          base[[var]] <- as.numeric(base[[var]])
          
        } else if (tipo == "character") {
          base[[var]] <- as.character(base[[var]])
        }
      }
      
      base
    })
  }
}


## Unificar código de ciudadano
bases <- lapply(bases, function(base) {
  if ("CODIGO_CIUDADANO" %in% names(base)) {
    base$CODIGO_CIUDADANO <- as.character(base$CODIGO_CIUDADANO)
  }
  base
})

## Convertir data.table a data.frame
bases <- lapply(bases, as.data.frame)

## UNIR TODAS LAS BASES
CABA_NOMINAL_TOTAL <- dplyr::bind_rows(bases)


##Me fijo si hay duplicados por id evento (porque en el historico del 2024 hay del 2023 tambien, y en el procesado hay del 2024)
duplicados <- CABA_NOMINAL_TOTAL %>%
  group_by(IDEVENTOCASO) %>%
  summarise(n = n()) %>%
  filter(n > 1)



CABA_NOMINAL_TOTAL <- CABA_NOMINAL_TOTAL %>%
  distinct(IDEVENTOCASO, .keep_all = TRUE)

##Bajar la tabla de referencias del drive
carpeta_referencias_snvs <- drive_ls("link")

archivo_referencias_snvs <- carpeta_referencias_snvs %>%
  filter(name == "tablas referencia campos variables SNVS")

tabla_referencia_snvs <- read_sheet(archivo_referencias_snvs, sheet = "ref_EVENTOS_GRUPOS_UNIFICADO")

##Eliminar todas las columnas de recla, habiamos eliminado las variables
#residente_bes, carga_indicador, recla_vigi_respi y tipo_efector, pero esas no estan en
#la tabla de referencias, así que las saque del codigo de abajo
historico_total_sin_recla <- CABA_NOMINAL_TOTAL %>%
  select(-recla_notificacion,-`recla_via_adquisicion/transmision_predominante`,                                
         -evento_humano_animal_recla, -recla_categoria, -recla_grupo_evento, -recla_nombre_agente,
         -recla_agente_subtipo, -recla_agente_etio, -recla_transmisibilidad, 
         -recla_seguimiento, -recla_tipo_vigilancia, -recla_prioridad, -recla_subtipo_vigilancia, -recla_lugar_tto, 
         -recla_temporalidad, -recla_inmunoprevenible, -recla_afectacion_org, -recla_nombre_patologia_CIE10,                  
         -recla_cod_cie_10, -recla_nombre_manual, -clasificacion_bes, -grupo_de_eventos_bes,                          
         -evento_bes, -grupo_indicador, -evento_indicador, -considerar_indicador,                          
         -moni_interes, -pablo_respi_int,-tablero_clasificacion_unida, 
         -tablero_tipo_alerta, -alerta_clasificacion_manual)


colnames(CABA_NOMINAL_PROCESADO)
#otra opcion que falta pulir
#istorico <- CABA_NOMINAL_TOTAL %>% 
 # select(-starts_with("recla_"))

#Hacer el join con el historico procesado y la tabla de referencias del snvs 
#actualizada para evitar que no coinciden y unificar variables
#Hago el left_join solo O
historico_total <- left_join(
  historico_total_sin_recla,
  tabla_referencia_snvs,
  by = c("GRUPO_EVENTO", "EVENTO", "CLASIFICACION_MANUAL")
)

summary(historico_total)

##Para ver los NA de tablero_tipo_alerta. Nos aparecen dos NA de "Hidroarsenicismo Crónico Regional Endémico (HACRE)",
#porque en la tabla esta escrito como "Caso Confirmado" y en la base hay algunos como "Caso confirmado" y otros como
#"Caso Confirmado"
historico_total %>%
  count(tablero_tipo_alerta) %>%
  mutate(porcentaje = round(100 * n / sum(n), 2))

historico_total_NA <- historico_total %>%
  filter(is.na(tablero_tipo_alerta))

unique(historico_total_NA$EVENTO)

unique(historico_total_NA$CLASIFICACION_MANUAL)

unique(historico_total$CLASIFICACION_MANUAL[historico_total$EVENTO == "Hidroarsenicismo Crónico Regional Endémico (HACRE)"])


##bajar el drive de tablas de referencia y ver si cambio alguna recla
##si es diferente volver a bajar el historico
#pegar todos los archivos, volver a hacer un join de las columnas que modificamos para todos, y despues hacer
# el archivo mas liviano para seguir


##Veo si hay duplicados por id de evento
duplicados <- historico_total %>%
  group_by(IDEVENTOCASO) %>%
  filter(n() > 1)

colnames(historico_total)

##Separamos tuberculosis de infecciones respiratorias
historico_total <- historico_total %>% 
    mutate(recla_categoria = case_when(
    EVENTO == "Tuberculosis" ~ "Tuberculosis",
    TRUE ~ recla_categoria
  ))

colnames

##Hago CABA_filtrada -----
#Filtro caba nominal-Selecciono las variables que nos quedamos
caba_filtrada <- historico_total %>%
  select(
    NRO_DOC, IDEVENTOCASO, EVENTO, SEXO, GRUPO_EVENTO, PROVINCIA_CARGA, SINTOMATICO, VACUNA, 
    ANIO_EPI_SINTOMA, FECHA_NACIMIENTO, EDAD_DIAGNOSTICO, CLASIFICACION_MANUAL,
    EMBARAZADA, INTERNADO, ESTABLECIMIENTO_CARGA, EDAD_APERTURA, LOCALIDAD_RESIDENCIA, CALLE_DOMICILIO, NUMERO_DOMICILIO, FECHA_APERTURA,
    CLASIFICACION_ALGORITMO, CLASIFICACION_AUTOMATICA, recla_notificacion, sepi_bes, anio_bes, evento_humano_animal_recla, recla_categoria, recla_grupo_evento, 
    recla_nombre_agente, recla_agente_subtipo, recla_agente_etio, recla_transmisibilidad,
    `recla_via_adquisicion/transmision_predominante`, recla_seguimiento, recla_tipo_vigilancia,
    recla_prioridad, recla_subtipo_vigilancia, recla_lugar_tto, recla_intensidad.x,
    recla_temporalidad, recla_inmunoprevenible, recla_afectacion_org, recla_nombre_manual,
    residente_bes, carga_indicador, recla_vigi_respi, tipo_efector, tablero_tipo_alerta, clasificacion_bes,
    DETERMINACION, RESULTADO, SEPI_APERTURA)

names(historico_total)

##Renombro-cambio los nombres de recla_categoría para que sean más cortos-----
caba_filtrada <- caba_filtrada %>%
  mutate(recla_categoria = case_when(
    recla_categoria == "Enfermedades Febriles Exantemáticas" ~ "EFE",
    recla_categoria == "Intoxicaciones / Exposiciones" ~ "Intoxicaciones/Exposiciones",
    recla_categoria == "Infecciones respiratorias" ~ "Respiratorias",
    recla_categoria == "Infecciones de transmision sexual" ~ "ITS",
    recla_categoria == "Otras enfermedades transmisibles" ~ "Otras transmisibles",
    recla_categoria == "Transmitidas por alimentos, agua o ruta fecal-oral" ~ "ETAs",
    recla_categoria == "Lesiones, salud mental y otros eventos no transmisibles" ~ "Lesiones, salud mental y otros",
    recla_categoria == "Transmisión Materno-Infantil y pesquisa neonatal" ~ "Materno-infantil",
    recla_categoria == "Otros eventos de importancia para la salud publica" ~ "Otros eventos de importancia",
    TRUE ~ recla_categoria
  ))

unique(caba_filtrada$EVENTO)

caba_filtrada <- caba_filtrada %>%
  mutate(
    EDAD_DIAGNOSTICO = as.numeric(EDAD_DIAGNOSTICO)
  )

##Calculo grupos etarios
caba_filtrada <- caba_filtrada %>%
  mutate(
    grupo_etario_calculado = cut(
      EDAD_DIAGNOSTICO,
      breaks = c(seq(0, 65, 5), Inf),  
      right = FALSE,                   
      labels = c(
        "0-4", "5-9", "10-14", "15-19", "20-24",
        "25-29", "30-34", "35-39", "40-44", "45-49",
        "50-54", "55-59", "60-64", "65+"
      ),
      include.lowest = TRUE
    )
  )

#Excluyo cuando establecimiento de carga y residente no es caba----
caba_filtrada <- caba_filtrada %>%
  filter(!(PROVINCIA_CARGA!= "CABA" & residente_bes == "NO RESIDENTE"))

#chequeo 
efector<- caba_filtrada %>%
  select(ESTABLECIMIENTO_CARGA, PROVINCIA_CARGA, residente_bes)

#cambio a caracter la base nominal_procesado

caba_filtrada$recla_agente_subtipo <- as.character(caba_filtrada$recla_agente_subtipo)

##Guardo el archivo como RDS
saveRDS(caba_filtrada, "caba_filtrada.rds")


##Ahora armamos una base nueva pero solo del año en curso y del año anterior
##Determino el anio actual
anio_actual <- as.numeric(format(Sys.Date(), "%Y"))

#Filtro por los ultimos dos años
caba_filtrada_actual <- caba_filtrada %>%
  filter(anio_bes %in% c(anio_actual, anio_actual - 1))



##Ahora hacemos el caba_filtrada_historico
caba_filtrada_historico <- caba_filtrada %>%
  filter(!anio_bes %in% c(anio_actual, anio_actual - 1))

##Guardo el archivo y lo subo:
saveRDS(caba_filtrada_historico, "caba_filtrada_historico.rds")
