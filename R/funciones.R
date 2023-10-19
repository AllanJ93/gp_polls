# Estimacion intención de voto (modelo) -----------------------------------

graficar_modelo = function(modelo, bd_puntos, fecha_estimacion = lubridate::today()) {
  
  g <- modelo %>%
    filter(fecha <= fecha_estimacion) %>%
    mutate(across(c(media, ic_025, ic_975), ~.x*100),
           across(c(media, ic_025, ic_975), ~round(.x), .names = "{.col}tt"),
           candidato = gsub("can_","",candidato)) %>%
    ggplot(aes(x = fecha, y = media, color = color, fill = color)) +
    geom_line() +
    geom_ribbon(aes(ymin = ic_025, ymax = ic_975), alpha = .3, size = 0) +
    ggrepel::geom_text_repel(data = . %>% filter(fecha == max(fecha)),
                             aes(label = stringr::str_wrap(paste0(mediatt, "% ", candidato), 10)),
                             hjust = 0, nudge_x = 10, family = "Poppins", size = 5) +
    geom_point(data = bd_puntos, aes(y = resultado), size = 2, shape = 19) +
    geom_vline(aes(xintercept = max(fecha)), size = 1) +
    scale_color_identity() +
    scale_fill_identity() +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    scale_y_continuous(labels = function(x) paste0(x,"%")) +
    labs(x = NULL, y = "Intención de voto") +
    scale_x_date(expand = expansion(add = c(0, 10))) +
    theme(text = element_text(family = "Poppins", size = 12),
          plot.title.position = "plot",
          axis.title.y.right = element_text(family =  "Poppins", size = 12),
          axis.text.x = element_text(size = 12),
          plot.caption = element_text(family = "Poppins", size = 12))
  
  return(g)
  
}

graficar_comparativa_ivoto <- function(bd) {
  
  g <- bd %>%
    filter(fecha >= lubridate::today()) %>%
    mutate(candidato = gsub("can_", "", candidato)) %>%
    mutate(candidato = case_when(candidato == "Alejandra Del Moral" ~ "Alejandra",
                                 candidato == "Delfina Gómez" ~ "Delfina",
                                 candidato == "Juan Zepeda" ~ "Juan",
                                 T ~ candidato)) %>%
    ggplot(aes(x = fecha, y = media, color = color, fill = color)) +
    geom_pointrange(data = . %>% filter(fecha == lubridate::today()),
                    aes(ymin = ic_025, ymax = ic_975), size = 1) +
    # geom_pointrange(data = . %>% filter(fecha == as.Date("2023-06-04")),
    #                 aes(ymin = ic_025, ymax = ic_975), size = 1) +
    geom_ribbon(aes(ymin = ic_025, ymax = ic_975), alpha = 0.3, size = 0) +
    ggrepel::geom_text_repel(data = . %>% filter(fecha == lubridate::today()),
                    aes(label = stringr::str_wrap(paste(round(media*100, digits = 0), "% ", candidato), 10)),
                    hjust = 0, nudge_x = 10, size = 5) +
    # ggrepel::geom_text_repel(data = . %>% filter(fecha == as.Date("2023-06-04")),
    #                 aes(label = stringr::str_wrap(paste(round(media*100, digits = 0), "% ", candidato), 10)),
    #                 hjust = 0, nudge_x = 10, size = 5) +
    # ggrepel::geom_text_repel(data = . %>% filter(fecha == as.Date("2023-06-04")),
    #                 aes(x = fecha,
    #                     y = ic_975,
    #                     color = color,
    #                     fill = color,
    #                     label = stringr::str_wrap(paste(round(ic_975*100, digits = 0), "% "), 10)),
    #                 hjust = 3, nudge_x = 10, size = 3, inherit.aes = F) +
    # geom_text_repel(data = . %>% filter(fecha == as.Date("2023-06-04")),
    #                 aes(x = fecha,
    #                     y = ic_025,
    #                     color = color,
    #                     fill = color,
    #                     label = stringr::str_wrap(paste(round(ic_025*100, digits = 0), "% "), 10)),
    #                 hjust = 3, nudge_x = 10, size = 3, inherit.aes = F) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_date(expand = expansion(add = c(10, 10)),
                 date_breaks = "1 month", date_labels = "%B") +
    scale_y_continuous(labels = scales::percent) +
    labs(x =  NULL, y = NULL) +
    theme_minimal()
  
  return(g)
  
}

# Probabilidad de triunfo -------------------------------------------------

obtener_prob_triunfo <- function(modelo_resultados, fecha) {
  modelo_resultados %>%
    filter(fecha == !!rlang::enquo(fecha)) %>%
    select(candidato, prob, fecha, color)
}

graficar_prob_triunfo <- function(bd) {
  
  g <- bd %>%
    ggplot(aes(x = reorder(candidato, prob), y = prob, fill = color)) +
    ggchicklet::geom_chicklet(width = 0.5) +
    ggfittext::geom_bar_text(aes(label = scales::percent(prob)), contrast = T, size = 14) +
    scale_fill_identity() +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    labs(x = NULL, y =  NULL, title = "Pronóstico de probabilidad de triunfo\nel 4 de junio") +
    theme_minimal()
  
  return(g)
}