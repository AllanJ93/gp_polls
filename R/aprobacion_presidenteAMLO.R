library(dplyr)
library(ggplot2)
library(encuestar)

bd_aprobacion <- openxlsx2::read_xlsx(file = "./Insumos/aprobacino_presidencial.xlsx") |> 
  as_tibble() |> 
  filter(!is.na(fecha)) |> 
  mutate(aprobacion = aprobacion/100)

g_aprobacion <- bd_aprobacion |> 
  ggplot(aes(x = fecha, y = aprobacion, label = scales::percent(x = aprobacion, accuracy = 1.0))) +
  geom_line(color = color_morena) +
  geom_point(color = color_morena) +
  geom_text(vjust = -1.0, size = 10) +
  tema_default() +
  scale_y_continuous(labels = scales::percent, limits = c(0.5, 1.0)) +
  theme(text = element_text(size = 24, family = "Poppins")) +
  labs(title = "Aprobación presidencial")

ggsave("Entregable/aprobacion.png", g_aprobacion, scale = 2, width = 10, bg = "white", dpi = "retina")
