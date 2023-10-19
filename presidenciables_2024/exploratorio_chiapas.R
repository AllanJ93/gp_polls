# Preámbulo ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(rjags)

# Insumos -----------------------------------------------------------------

id_bd_chis <- "https://docs.google.com/spreadsheets/d/1SFnwchCBcKzQtcf-4_8hE7u1HfLgj7Ti9B3PZYt0dlM/edit#gid=2045083898"
dir_chis <-  "./chiapas_2024/bd_chiapas_2024.xlsx"
archivo_xlsx <- googledrive::drive_download(googledrive::as_id(id_bd_chis), path = dir_chis, overwrite = TRUE)
2
bd_chiapas_raw <- openxlsx2::read_xlsx(file = dir_chis, sheet = 6) |> 
  as_tibble(.name_repair = "unique") |> 
  janitor::clean_names() |> 
  rename(dif_preferencia = diferencia_10,
         dif_conocimiento = diferencia_15,
         fechaInicio = inicio,
         fechaFin = final,
         fechaPublicacion = fecha_de_publicacion,
         numeroEntrevistas = tamano,
  )

# Preparar base -----------------------------------------------------------

bd_presi_sheinbaum <- bd_chiapas_raw |> 
  filter(tipo == "Externa") |> 
  transmute(casa_encuestadora, fechaInicio, fechaFin, fechaPublicacion,
            numeroEntrevistas,
            resultado = intencion, candidato, careo, metodologia, calidad,
            total_de_entrevistas = numeroEntrevistas,
            error) |> 
  group_by(casa_encuestadora, fechaInicio, fechaFin, error,
           total_de_entrevistas, metodologia, careo) %>%
  mutate(idIntencionVoto = cur_group_id()) %>% 
  mutate(cheinbaum = dplyr::if_else(condition = "Claudia Sheinbaum" %in% candidato,
                                    true = T,
                                    false = F)) |> 
  ungroup() |> 
  filter(cheinbaum == T) |> 
  filter(!careo %in% c(5, 8, 9)) |> 
  mutate(colorHex = case_when(candidato == "Marcelo Ebrard" ~ "orange",
                              candidato == "Ricardo Anaya Cortés" ~ "blue",
                              candidato == "Samuel García" ~ "orange3",
                              candidato == "Manuel Velasco Coello" ~ "green3",
                              candidato == "Otro" ~ "gray50",
                              candidato == "Ns/Nc" ~ "gray70",
                              candidato == "Claudia Sheinbaum" ~ "purple",
                              candidato == "Enrique de la Madrid" ~ "yellow")) |> 
  relocate(idIntencionVoto, .after = casa_encuestadora) 
