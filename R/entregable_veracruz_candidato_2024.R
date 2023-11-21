# Preámbulo ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(rjags)
library(officer)
library(flextable)

source(file = "R/parametros.R")
source(file = "R/funciones.R")

# Insumos -----------------------------------------------------------------

id_bd_gppolls <- "https://docs.google.com/spreadsheets/d/1M4ifUkX3ULaYoc0gdM2PDC6oEAjUPFAdikTU_jhePFs/edit#gid=0"
dir_bd_gppolls <-  "Insumos/bd_gppolls.xlsx"
archivo_xlsx <- googledrive::drive_download(googledrive::as_id(id_bd_gppolls), path = dir_bd_gppolls, overwrite = TRUE)
2
bd_encuestas_raw <- openxlsx2::read_xlsx(file = dir_bd_gppolls, sheet = "Veracruz", cols = seq.int(1:27)) |> 
  as_tibble(.name_repair = "unique") |> 
  janitor::clean_names() |> 
  mutate(casa_encuestadora = stringr::str_trim(string = casa_encuestadora, side = "both"),
         numero_de_entrevistas = as.integer(numero_de_entrevistas),
         intencion_de_voto_por_candidato_bruta = as.double(intencion_de_voto_por_candidato_bruta),
         levantamiento = stringr::str_to_sentence(levantamiento),
         candidato_modelo = stringr::str_trim(string = candidato_modelo, side = "both"),
         calidad = "general") |> 
  rename(metodologia = levantamiento,
         error = error_muestral,
         fechaPublicacion = fecha_de_publicacion,
         fechaInicio = inicio,
         fechaFin = final,
         numeroEntrevistas = numero_de_entrevistas) |> 
  filter(!is.na(id))

# Preparar base -----------------------------------------------------------

bd_preparada <- bd_encuestas_raw |> 
  filter(tipo_de_pregunta == "Intención de voto por candidato-alianza") |> 
  transmute(casa_encuestadora, fechaInicio, fechaFin, fechaPublicacion,
            numeroEntrevistas,
            resultado = intencion_de_voto_por_candidato_bruta,
            candidato = gsub(pattern = "Rocio", replacement = "Rocío", x = candidato_modelo),
            careo, 
            metodologia = stringi::stri_trans_general(metodologia, "Latin-ASCII"), 
            calidad,
            total_de_entrevistas = numeroEntrevistas,
            error) |> 
  group_by(casa_encuestadora, fechaInicio, fechaFin, error, total_de_entrevistas, metodologia, careo) %>%
  mutate(idIntencionVoto = cur_group_id()) %>% 
  ungroup() |> 
  group_by(idIntencionVoto) |> 
  mutate(trackeable = dplyr::if_else(condition = all(c("Rocío Nahle", "Fernando Yunes") %in% candidato),
                                     true = T,
                                     false = F)) |> 
  ungroup() |> 
  filter(trackeable == T) |> 
  mutate(candidato = dplyr::if_else(condition = candidato %in% c("Candidato MC"),
                                    true = "Otro",
                                    false = candidato),
         candidato = dplyr::if_else(condition = candidato %in% c("No sabe", "No sabe/No Respondió"),
                                    true = "Ns/Nc",
                                    false = candidato)) |> 
  mutate(colorHex = case_when(candidato == "Rocío Nahle" ~ color_sheinbaum,
                              candidato == "Fernando Yunes" ~ color_pan,
                              candidato == "Otro" ~ color_otro,
                              candidato == "Ns/Nc" ~ color_nsnc)) |> 
  relocate(idIntencionVoto, .after = casa_encuestadora) |> 
  group_by(casa_encuestadora, idIntencionVoto, fechaInicio, fechaFin, fechaPublicacion, numeroEntrevistas, candidato, metodologia, careo, calidad, total_de_entrevistas, error, colorHex) |> 
  summarise(resultado = sum(resultado), .groups = "drop") |> 
  relocate(resultado, .before = candidato) |> 
  select(!careo) |> 
  mutate(dias_levantamiento = as.numeric(fechaFin - fechaInicio)) %>%
  mutate(fechaInicio = dplyr::if_else(condition = dias_levantamiento == 0,
                                      true = fechaInicio - lubridate::ddays(1),
                                      false = fechaInicio)) |> 
  mutate(dias_levantamiento = as.numeric(fechaFin - fechaInicio)) %>%
  filter(!dias_levantamiento <= 0) 

bd_puntos <- bd_preparada %>%
  select(idIntencionVoto, fecha = fechaFin, resultado, candidato) %>%
  pivot_wider(id_cols = c(idIntencionVoto, fecha), names_from = candidato, values_from = resultado) %>%
  mutate(across(.cols = !c(idIntencionVoto, fecha), .fns = ~ dplyr::if_else(condition = is.na(.x),
                                                                            true = 0.0,
                                                                            false = .x))) |> 
  pivot_longer(cols = -c(idIntencionVoto, fecha),names_to = "candidato", values_to = "resultado") %>%
  left_join(bd_preparada %>% distinct(candidato, color = colorHex), by = c("candidato")) %>%
  left_join(bd_preparada %>% distinct(idIntencionVoto, calidad), by = "idIntencionVoto")

# Pruebas -----------------------------------------------------------------

### Prefferencia bruta
paste("Encuestas: ", bd_preparada %>% distinct(idIntencionVoto) %>% nrow(), sep = "")
bd_preparada %>% count(candidato)
bd_preparada %>% 
  group_by(idIntencionVoto) %>% 
  summarise(suma_de_porcentaje = sum(resultado)) %>%
  print(n = Inf)
bd_preparada %>% naniar::vis_miss()
bd_preparada %>%
  distinct(fechaInicio, fechaFin, fechaPublicacion) %>%
  filter_at(vars(contains("fecha")), any_vars(. > lubridate::today()))
bd_preparada %>% 
  distinct(fechaInicio, fechaFin, fechaPublicacion) %>%
  mutate(dias_levantamiento = fechaFin - fechaInicio) |>
  arrange(desc(dias_levantamiento))
bd_preparada %>%
  distinct(fechaInicio, fechaFin, fechaPublicacion) %>%
  mutate(dias_levantamiento = fechaFin - fechaInicio) %>%
  ggplot() +
  geom_histogram(aes(y = dias_levantamiento)) +
  coord_flip() +
  theme_minimal() +
  labs(y = "Días de levantamiento", x = "Encuestas")

# Cargar modelo y simulaciones -------------------------------------------- 

## Cargar modelos ---------------------------------------------------------

source(file = "R/modelo.R")

fecha_estimacion <- lubridate::today()

modelo_resultado <- modelo_bayesiano(bd = bd_preparada %>% rename(partido = candidato), fechaFin = fecha_estimacion)

modelo_graf <- graficar_modelo(modelo = modelo_resultado[[1]], bd_puntos = bd_puntos)

# graficar_comparativa_ivoto(bd = mod_presidenciables_candidato[[1]])
# 
# bd_prob_triunfo <- obtener_prob_triunfo(modelo_resultados = mod_presidenciables_candidato[[2]], fecha = fecha_estimacion)
# 
# prob_triunfo_graf <- graficar_prob_triunfo(bd = bd_prob_triunfo)

tabla_encuestas <- bd_preparada %>% 
  select(casa_encuestadora, idIntencionVoto) %>%
  distinct() %>%
  left_join(bd_puntos %>% 
              select(-color) %>%
              pivot_wider(names_from = candidato, values_from = resultado), by = "idIntencionVoto") %>%
  left_join(bd_preparada %>%
              distinct(idIntencionVoto, casa_encuestadora, fechaFin, metodologia, numeroEntrevistas, error),
            by = c("idIntencionVoto", "casa_encuestadora")) %>%
  select(!c(idIntencionVoto, fecha)) %>%
  janitor::clean_names() %>%
  arrange(fecha_fin) %>% 
  mutate(diferencia = rocio_nahle - fernando_yunes,
         fecha_fin = format(fecha_fin, "%d-%b-%y"),
         numero_entrevistas = scales::comma(numero_entrevistas),
         error = as.double(error),
         across(where(is.numeric), ~ scales::percent(.x/100, accuracy = .1)),
         diferencia = gsub(pattern = "%", replacement = "", x = diferencia)) %>%
  relocate(casa_encuestadora,
           rocio_nahle,
           fernando_yunes,
           diferencia,
           ns_nc,
           otro,
           fecha_fin,
           numero_entrevistas,
           error,
           metodologia,
           calidad) %>% 
  rename("Casa Encuestadora" = casa_encuestadora,
         "Rocío Nahle" = rocio_nahle,
         "Fernando Yunes" = fernando_yunes,
         "Diferencia\nventaja\n(puntos)" = diferencia,
         "Ns/Nc" = ns_nc,
         "Otro" = otro,
         "Fecha de\ntérmino" = fecha_fin,
         "Total de\nentrevistas" = numero_entrevistas,
         "Error" = error,
         "Tipo de\nlevantamiento" = metodologia,
         "Calidad" = calidad)

dummy_tb <- tibble(casa_encuestadora = "RESULTADO GPPOLLS",
                   fecha_fin = "",
                   metodologia = "",
                   calidad = "",
                   numero_entrevistas = "",
                   error = "")

resultado_gppolls <- modelo_resultado[[1]] %>%
  filter(fecha == fecha_estimacion) %>%
  mutate(across(c(media, ic_025, ic_975), ~.x*100),
         across(c(media, ic_025, ic_975), ~round(.x), .names = "{.col}tt"),
         candidato = gsub("can_", "", candidato)) %>%
  select(!c(dia, color, contains("ic"), fecha, mediatt)) %>%
  pivot_wider(names_from = candidato, values_from = media) %>%
  janitor::clean_names() %>%
  mutate(across(where(is.numeric), ~ round(.x, digits = 0)),
         diferencia = rocio_nahle - fernando_yunes,
         across(where(is.numeric), ~ scales::percent(.x/100, accuracy = 1.)),
         diferencia = gsub(pattern = "%", replacement = "", x = diferencia))

tabla_resultadoGppolls <- resultado_gppolls %>%
  bind_cols(dummy_tb) %>%
  relocate(casa_encuestadora,
           rocio_nahle,
           fernando_yunes,
           diferencia,
           ns_nc,
           otro,
           fecha_fin,
           numero_entrevistas,
           error,
           metodologia,
           calidad) %>% 
  rename("Casa Encuestadora" = casa_encuestadora,
         "Rocío Nahle" = rocio_nahle,
         "Fernando Yunes" = fernando_yunes,
         "Diferencia\nventaja\n(puntos)" = diferencia,
         "Ns/Nc" = ns_nc,
         "Otro" = otro,
         "Fecha de\ntérmino" = fecha_fin,
         "Total de\nentrevistas" = numero_entrevistas,
         "Error" = error,
         "Tipo de\nlevantamiento" = metodologia,
         "Calidad" = calidad)

tabla_completa <- tabla_encuestas |> 
  bind_rows(tabla_resultadoGppolls) |> 
  select(!Calidad) |> 
  tibble::rownames_to_column(var = "N°") %>%
  mutate(`N°` = case_when(`Casa Encuestadora` == "RESULTADO GPPOLLS" ~ "", T ~ `N°`))

# Preparar anexos ---------------------------------------------------------

tabla_completa_anexos <- tibble::tibble(tabla_completa) %>%
  tibble::rownames_to_column(var = "id") %>%
  mutate(sep = ((as.numeric(id) -1) %/% 10) + 1) %>%
  split(.$sep)

tot_encuestas <- bd_preparada %>% 
  distinct(idIntencionVoto) |> 
  nrow()

ultima_encuesta <- bd_preparada %>% select(fechaFin) %>% pull() %>% max()

# Exportar ----------------------------------------------------------------

pptx <- read_pptx("Insumos/plantilla_gpp.pptx")

add_slide(pptx, layout = "portada", master = "Tema de Office") %>%
  ph_with(value = "ENCUESTAS VERACRUZ 2024", location = ph_location_label(ph_label = "titulo")) %>%
  ph_with(value = stringr::str_to_upper(format(lubridate::today(), "%A %d de %B de %Y")), location = ph_location_label(ph_label = "subtitulo"))

add_slide(pptx, layout = "modelo", master = "Tema de Office") %>%
  ph_with(value = paste("Análisis general: ", tot_encuestas, " encuestas", sep = ""), location = ph_location_label(ph_label = "titulo")) %>%
  ph_with(value = paste("Última encuesta: ", format(x = ultima_encuesta, "%d de %B %Y")), location = ph_location_label(ph_label = "subtitulo")) %>%
  ph_with(value = modelo_graf, location = ph_location_label(ph_label = "imagen_principal"))

tabla_completa_anexos %>%
  purrr::walk( ~ add_slide(pptx, layout = "anexos", master = "Tema de Office") %>%
                 ph_with(value = "Encuestas usadas como insumo", location = ph_location_label(ph_label = "titulo")) %>%
                 ph_with(value = .x %>% select(!c(sep, id)) %>%
                           flextable(cwidth = 2, cheight = 1) %>%
                           theme_vanilla() %>%
                           color(j = c(1:4, 6:11), color = color_morena, part = "header") %>%
                           bold(j = c("Diferencia\nventaja\n(puntos)"), bold = TRUE, part = "body") %>%
                           bg(j = c("Diferencia\nventaja\n(puntos)"), bg = "#E2F0D9", part = "body") %>%
                           bg(j = c("Diferencia\nventaja\n(puntos)"), bg = "#E2F0D9", part = "header") %>%
                           bg(i = ~ `Casa Encuestadora` == "RESULTADO GPPOLLS", bg = color_morena, part = "body") %>%
                           color(i = ~ `Casa Encuestadora` == "RESULTADO GPPOLLS", color = "white", part = "body") %>%
                           bold(i = ~ `Casa Encuestadora` == "RESULTADO GPPOLLS", bold = T, part = "body") %>%
                           align(align = "center", part = "header") %>%
                           align(align = "center", part = "body") %>%
                           autofit(), location = ph_location_label(ph_label = "tabla")))

dia_reporte <- format(lubridate::today(), format = "%B_%d")

folder_path <- paste("Entregable/", dia_reporte, "/", sep = "")

dir.create(folder_path)

pptx_path <- paste(folder_path, format(lubridate::today(), "gppolls_veracruz_candidato_%d_%B"), ".pptx", sep = "")
print(pptx, pptx_path)
beepr::beep()