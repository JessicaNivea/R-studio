#> PCA no R
#> https://rpubs.com/nionmaron/exemplo-PCA [Interpretação]

library(FactoMineR)  
library(factoextra) 
library(readxl)

data <- read_excel("C:/Users/jessi/OneDrive/Área de Trabalho/teste.xlsx")
head(data)

# Definir a coluna "fonte" como nome das linhas
dados_pca <- data[, -1]  
rownames(dados_pca) <- data$fonte 

# Rodar o PCA
pca.data <- PCA(dados_pca, scale.unit = TRUE, graph = FALSE)

# visualizar a proporção da variância explicada por cada componente principal
fviz_eig(pca.data, addlabels = TRUE, ylim = c(0, 70))

#Gráfico de variáveis
fviz_pca_var(pca.data, 
             col.var = "cos2", 
             gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"),
             repel = TRUE) +
  ggtitle("PCA - Analysis of Variables") +  # Adiciona título ao gráfico
  theme_minimal(base_size = 14) +  # Aplica um tema minimalista
  theme(axis.title.x = element_text(size = 16, face = "bold", color = "#330033"),  # Eixo Dim1 destacado
        axis.title.y = element_text(size = 16, face = "bold", color = "#660033"),  # Eixo Dim2 destacado
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18)) +  # Centraliza o título
  scale_color_gradient(low = "#FFCC00", high = "#330033")

#PCA Individuals com número
fviz_pca_ind(pca.data, col.ind = "cos2", 
             gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"), 
             repel = TRUE)+
  ggtitle("PCA - Individuals Analysis") +  # Adiciona título ao gráfico
  theme_minimal(base_size = 14) +  # Aplica um tema minimalista
  theme(axis.title.x = element_text(size = 16, face = "bold", color = "#330033"),  # Eixo Dim1 destacado
        axis.title.y = element_text(size = 16, face = "bold", color = "#660033"),  # Eixo Dim2 destacado
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18)) +  # Centraliza o título
  scale_color_gradient(low = "#FFCC00", high = "#330033")

#PCA Individuals com nome
fviz_pca_ind(pca.data, col.ind = "cos2", 
             gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"), 
             repel = TRUE,
             label = "var",
             geom.ind = "point")+
  geom_text(aes(label = data$fonte), 
            hjust = -0.1, 
            vjust = -0.1,
            size = 3,  # Reduz o tamanho da fonte
            check_overlap = FALSE) +
  ggtitle("PCA - Individuals Analysis") +  # Adiciona título ao gráfico
  theme_minimal(base_size = 14) +  # Aplica um tema minimalista
  theme(axis.title.x = element_text(size = 16, face = "bold", color = "#330033"),  # Eixo Dim1 destacado
        axis.title.y = element_text(size = 16, face = "bold", color = "#660033"),  # Eixo Dim2 destacado
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18)) +  # Centraliza o título
  scale_color_gradient(low = "#FFCC00", high = "#330033")

#PCA Biplot com números
fviz_pca_biplot(pca.data, col.ind = "cos2", 
  gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"), 
  repel = TRUE,
  label = "var",
geom.ind = "point")+
  ggtitle("PCA - Biplot Analysis") +  # Adiciona título ao gráfico
  theme_minimal(base_size = 14) +  # Aplica um tema minimalista
  theme(axis.title.x = element_text(size = 16, face = "bold", color = "#330033"),  # Eixo Dim1 destacado
        axis.title.y = element_text(size = 16, face = "bold", color = "#660033"),  # Eixo Dim2 destacado
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18)) +  # Centraliza o título
  scale_color_gradient(low = "#FFCC00", high = "#330033")

#PCA Biplot com nomes
fviz_pca_biplot(pca.data, col.ind = "cos2", 
                gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"), 
                repel = TRUE,
                label = "var",
                geom.ind = "point")+
  geom_text(aes(label = data$fonte), 
            hjust = -0.1, 
            vjust = -0.1,
            size = 3,  # Reduz o tamanho da fonte
            check_overlap = FALSE) +
  ggtitle("PCA - Biplot Analysis") +  # Adiciona título ao gráfico
  theme_minimal(base_size = 14) +  # Aplica um tema minimalista
  theme(axis.title.x = element_text(size = 16, face = "bold", color = "#330033"),  # Eixo Dim1 destacado
        axis.title.y = element_text(size = 16, face = "bold", color = "#660033"),  # Eixo Dim2 destacado
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18)) +  # Centraliza o título
  scale_color_gradient(low = "#FFCC00", high = "#330033")
