escribir_modelo <- function(max,concentracion){
  inicio <- paste0("model{
  x[1,1:partidos] ~ ddirch(inicial*alpha)",concentracion,
                   "for(i in 1:encuestas){
  y[i,] ~ ddirch("
  )

  fin <- ")
  }
  for(j in 2:dias_tot){
  # x es el valor
  x[j, 1:partidos] ~ ddirch(x[j-1, 1:partidos]*alpha)
  }
  }"
  ifs.modelo <- crear_steps(max)
  model <- paste0(inicio,ifs.modelo,fin)
  return(model)
}

crear_steps <- function(max){
  sumandos <- "((x[dia_enc[i], 1:partidos])"
  sumandos.2 <- paste0(sumandos," * n[i])")
  den <- "1"
  if(max >= 2){
    for(i in 2:max){
      sumandos.aux <- paste0("(x[dia_enc[i] - ",i," + 1, 1:partidos]) * step(dur_lev[i] - ", i,")")
      sumandos <- paste(sumandos, sumandos.aux,sep = " + ")
      den.aux <- paste0("1 * step(dur_lev[i] - ", i,")")
      den <- paste(den, den.aux,sep = " + ")
    }
  }
  texto <- paste0(sumandos,") * n[i]/","(",den,")")
  return(texto)
}

preparar_base <- function(registro, voto, diseno, formulario, colores){
  # Leer la base de datos
  bd <- registro %>%
    inner_join(voto,
               by="idIntencionVoto", suffix=c(".res", ".cues")) %>%
    left_join(diseno,
              by="idFormGeneral", suffix = c(".a",".b")) %>%
    left_join(colores, by = c("partido"="nombrePartido"),suffix = c("a","b")) %>%
    collect() %>%
    group_by(idFormGeneral) %>%
    top_n(fechaAlta.res,n=1) %>%
    inner_join(formulario %>%
                 collect(),by="idFormGeneral",
               suffix=c(".x",".general"))

  #
  return(bd)
}

preparar_datos <- function(bd, fechaFin){
  # Inicio
  fecha_inicio <- as.Date(min(bd$fechaInicio))
  # Días desde el inicio
  bd <- bd %>% mutate(dia_final=as.numeric(as.Date(fechaFin)-fecha_inicio)+1,
                      duracion=as.numeric(fechaFin-fechaInicio),
                      fecha_final=fechaFin
  )
  bd <- bd %>% mutate(numeroEntrevistas = replace_na(numeroEntrevistas, round(mean(bd$numeroEntrevistas,na.rm = T))))
  # y es la info de encuestas
  bd_y <- bd %>% #filter(partido != "INDEPENDIENTE") %>%
    # mutate(partido=partido) %>%
    tidyr::pivot_wider(id_cols=c(idIntencionVoto,dia_final, duracion, fecha_final),
                       names_prefix="can_",
                       names_from = partido,
                       values_from = resultado) %>%
    mutate(id=row_number()#,
           # dia_final=dia_final+duracion+10
    )
  # Resultados encuestas (Falta verificar si con NA o 0 nunca funciona)

  y <- bd_y %>% select(starts_with("can_")) %>%
    mutate(
      across(everything(), ~ tidyr::replace_na(as.numeric(.x), replace = round(mean(.x, na.rm = T), digits = 1)) %>% as.numeric(.x)),
      across(.cols = everything(), .fns =  ~ dplyr::if_else(condition = .x == 0, true = 0.1, false = .x))
    ) 

  # mutate(can_OTRO = if_else(condition = can_OTRO <= 0, true = 0.001, false = can_OTRO))

  # y <- bd_y %>% select(starts_with("can_")) %>%
  #   mutate(
  #     across(everything(), ~ tidyr::replace_na(as.numeric(.x), replace = mean(.x,na.rm = T)) %>% as.numeric(.x))
  #   ) %>% rowwise() %>% mutate(can_OTRO = 100-sum(c_across(starts_with("can_")))) %>% mutate(can_OTRO = if_else(condition = can_OTRO <= 0, true = 0.1, false = can_OTRO))
  #   # filter(across(everything(), ~ .x > 0))

  # Colores
  colores <- bd %>% ungroup %>% select(partido,colorHex) %>% distinct() %>% pull(colorHex)
  # Partidos
  partidos <- ncol(y)
  # Número de encuestas
  encuestas <-  nrow(y)
  # último día de levantamiento de la encuesta
  dia_enc <- bd_y$dia_final
  # dias que duró la encuesta
  dur_lev <- bd_y$duracion
  # número de entrevistas por encuesta
  n <- bd %>% distinct(idIntencionVoto, .keep_all = T) %>% pull(numeroEntrevistas)
  # días totales 10
  dias_tot <- as.numeric(as.Date(fechaFin)-fecha_inicio)
  # probabilidad inicial
  inicial <- rep(1/partidos, partidos)
  # inicial <- c("delfina", "alejandra", 1 - (.221 + .232))
  # inicial <- c(.221, .232, 1 - (.221 + .232))
  # inicial <- c(.488, .511)
  insumos <- list(
    # Número de encuestas
    n = n,
    # Encuestas
    y = y,
    encuestas = nrow(y),
    partidos = ncol(y),
    # Dist inicial
    inicial = inicial,
    # dia
    dia_enc = dia_enc,
    # cuantos días duró
    dur_lev = dur_lev,
    dias_tot=dias_tot,
    colores = colores)
  return(insumos)
}

modelo_bayesiano <- function(bd = NULL, fechaFin, registro, voto, diseno, formulario, colores){
  # Preparar base
  if(is.null(bd)){
    bd <- preparar_base(registro, voto, diseno, formulario, colores)
  }

  datos <- preparar_datos(bd %>% arrange(fechaFin), fechaFin)
  datos$dia_enc[datos$dia_enc< max(datos$dur_lev)] <- max(datos$dur_lev)
  # A priori
  apriori <- "
alpha ~ dnorm(5000,tau)
tau ~ dnorm(.01,1)T(0,)"
  #
  modelo <- escribir_modelo(max(datos$dur_lev), apriori)
  modelo_compilado <-  jags.model( file = textConnection(modelo),
                                   data = datos[-9],
                                   n.adapt = 10000,
                                   quiet = TRUE )

  muestras_posteriores <-  coda.samples( model = modelo_compilado,
                                         variable.names = c("x"),
                                         n.iter = 1000)


  simulaciones <- muestras_posteriores[[1]] %>% as_tibble() %>%
    mutate(sim=row_number()) %>%
    pivot_longer(-sim, names_to=c("dia", "candidato"),
                 values_to="theta",
                 names_prefix = "x\\[",names_sep=",")
  simulaciones <- simulaciones %>%
    mutate(dia=as.numeric(dia),
           aux = as.numeric(gsub(pattern = "\\]",
                                 replacement="", candidato)),
           candidato=names(datos$y)[aux],
           color = datos$colores[aux]) %>% select(-aux)
  resumen <- simulaciones %>%
    group_by(candidato, dia, color) %>%
    summarise(media=mean(theta),
              ic_025=quantile(theta, probs=.025),
              ic_975=quantile(theta, probs=.975)) %>%
    mutate(fecha=as.Date(min(bd$fechaInicio))+lubridate::days(dia)) %>% ungroup

  prob_ganar <- simulaciones %>%
    group_by(dia, sim) %>%
    mutate(rank = dense_rank(-theta)) %>%
    filter(rank == 1) %>%
    mutate(candidato = gsub("can_","",candidato)) %>%
    ungroup %>% count(dia,candidato) %>%
    group_by(dia) %>% mutate(prob = n/sum(n)) %>%
    ungroup %>%
    mutate(fecha=as.Date(min(bd$fechaInicio))+lubridate::days(dia)) %>%
    left_join(bd %>% distinct(candidato = partido, color = colorHex))

  return(list(resumen,prob_ganar, simulaciones, bd))
}
