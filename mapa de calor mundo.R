install.packages("ggplot2")
install.packages("maps")
install.packages("mapdata")
install.packages("dplyr")  # Para manipulação de dados
install.packages("extrafont")

library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library (extrafont)

#Trabalhar com as fontes
font_import()
y
loadfonts(device = "win")  # Para Windows

#Fazer o mapa
world_map <- map_data("world")
dados <- data.frame(
  pais = c("China", "USA", "Australia", "India","Spain","Korea", "Brazil", "Italy", "Malaysia", "Egypt"),
  valor = c(13081,2670,2574,2426,2264,1202,928,905,858,819)
)
world_map <- world_map %>%
  left_join(dados, by = c("region" = "pais"))

ggplot(world_map, aes(x = long, y = lat, group = group, fill = valor)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "gray") +
  theme_minimal() +
  labs(title = "Total de citações por País", fill = "Nº de Citações") +
  coord_fixed(1.0) + # Ajusta a proporção do mapa 
theme(text = element_text(family = "Times New Roman"),  # Define a fonte para todo o texto
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5 ),  # Personaliza o título
        axis.title = element_text(size = 14),  # Personaliza os títulos dos eixos
        axis.text = element_text(size = 12)  # Personaliza os rótulos dos eixos
        )

unique(world_map$region) #nome dos países

