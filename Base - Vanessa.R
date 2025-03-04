#Instalar pacotes (necessário apenas uma vez)
installed.packages("ggplot2")
installed.packages("dplyr")
installed.packages("bibliometrix")
installed.packages("igraph")
installed.packages("openxlsx")
#Carregar pacotes
library(ggplot2)
library(dplyr)
library(bibliometrix)
library(igraph)
library(openxlsx)

#Estudo próprio

?convert2df #comando para acessar os dbsource e formatos do convert2df

# Carregar os dados (lembre-se de colocar o arquivo na pasta do diretório do R)
file <- "savedrecs.bib"  # Insira o nome do seu arquivo da forma que baixou
M <- convert2df(file, dbsource = "wos", format = "bibtex")  # Para Web of Science pode ser "wos" ou "isi"
# M <- convert2df(file, dbsource = "scopus", format = "bibtex")  # Para Scopus
com <- missingData(M)
com$mandatoryTags
results <- biblioAnalysis(M, sep = ";")
S <- summary(object = results, k = 10, pause = FALSE)

#Manipulando os dados

#OS DEZ AUTORES MAIS PRODUTIVOS
top_authors <- S$MostProdAuthors
top_authors <- as.data.frame(top_authors)
colnames(top_authors) <- c("Author", "Articles")
str(top_authors) #conferir se articles está como numerico
top_authors$Articles <- as.numeric(top_authors$Articles) #converter para numerico
ggplot(top_authors, aes(x = reorder(Author, Articles), y = Articles)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Autores Mais Produtivos",
       x = "AUTOR",
       y = "NÚMERO DE ARTIGOS") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),  # Remove grades principais
    panel.grid.minor = element_blank()   # Remove grades secundárias
  )
#Se não gostar do theme_minimal pode trocar para theme_classic

#10 PRINCIPAIS FONTES (PERIÓDICOS)
top_sources <- S$MostRelSources
top_sources <- as.data.frame(top_sources)
colnames(top_sources) <- c("Sources", "Articles")
str(top_sources)
top_sources$Articles <- as.numeric(top_sources$Articles)
ggplot(top_sources, aes(x = reorder(Sources, Articles), y = Articles)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Periódicos Mais Produtivos",
       x = "PERIÓDICOS",
       y = "NÚMERO DE ARTIGOS") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),  # Remove grades principais
    panel.grid.minor = element_blank()   # Remove grades secundárias
  )

#AUTORES CORRESPONDENTES
top_country <- S$MostProdCountries
top_country <- as.data.frame(top_country)
colnames(top_country) <- c("Country", "Articles", "Freq", "SCP", "MCP", "MCPRatio")
str(top_country)
top_country$Articles <- as.numeric(top_country$Articles)
top_country$Freq <- as.numeric(top_country$Freq)
top_country$SCP <- as.numeric(top_country$SCP)
top_country$MCP <- as.numeric(top_country$MCP)
top_country$MCPRatio <- as.numeric(top_country$MCPRatio)
library(tidyr)
top_countries_long <- pivot_longer(top_country, 
                                   cols = c(SCP, MCP), 
                                   names_to = "Publication_Type", 
                                   values_to = "Count")

ggplot(top_countries_long, aes(x = reorder(Country, Articles), y = Count, fill = Publication_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), 
            position = position_stack(vjust = 0.5), 
            size = 3.5, color = "white") +
  coord_flip() +
  scale_fill_manual(values = c("SCP" = "steelblue", "MCP" = "orange")) +
  labs(title = "PUBLICAÇÕES POR PAÍS",
       x = "PAÍS",
       y = "NÚMERO DE ARTIGOS",
       fill = "Tipo de Publicação") +
  theme_classic() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 16, color = "#2E2E2E"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_blank(),  # Remove grades principais
    panel.grid.minor = element_blank()   # Remove grades secundárias
  )
#SCP: Single Country Publications; MCP: Multiple Country Publications

#COUNTRY COLLABORATION NETWORK

M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = 20, Title = "Colaboração Entre Países",
                type = "star", size=TRUE, remove.multiple=FALSE,labelsize=1)

#Tipos: circle, fruchterman, star, mds
# n = dim(NetMatrix)[1]

#PRODUÇÃO CIENTÍFICA
#Antes de plotar, aviso que a escala dele está personalizada aos meus dados
#Se os eixos ficarem esquisitos, edite (scale_y_continuous e scale_x_continuous)

df_year<-S$AnnualProduction%>%
  transmute(year=as.numeric(as.character(`Year   `)),
            articles=Articles)%>%
  filter(year<2025)
kable(df_year)

ggplot(df_year,aes(x=year, y=articles))+ geom_bar(stat='identity',fill="steelblue")+
  geom_smooth(method = "auto", se = FALSE, color = "red",
              linetype = "dashed" )+
  theme_bw()+
  labs(y='Nº DE ARTIGOS', x=NULL)+
  scale_y_continuous(expand = c(0, 0),limits = c (0,125), breaks = seq (0, 125, 25)) +  # Remove espaçamento extra no eixo Y
  scale_x_continuous(limits = c(1973, 2024), breaks = seq(1973, 2024, 5))+
  theme(
    text = element_text(size =12),
    plot.title = element_text(size = 16, face = "bold"),  # Personaliza o título
    axis.title = element_text(size = 14, face ="bold"),  # Personaliza os títulos dos eixos
    axis.text = element_text(size = 14, color = "black"), # Personaliza os textos dos eixos
    panel.grid.major = element_blank(),  # Remove grades principais
    panel.grid.minor = element_blank()   # Remove grades secundárias
  ) +
  geom_text(aes(x = 2000, y = 100, label = paste("Taxa Anual de Crescimento:", S$AnnualGrowthRate)),
            color = "red", size = 5, fontface = "bold")
# Linetype pode ser dashed ou solid

# Criar um co-citation network

NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", n=25, sep = ";")

# Plotar o co-citation network
net=networkPlot(NetMatrix,n = 25, Title = "Rede de Co-citação", 
                type = "star", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)

# Criar um keyword co-occurrences network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plotar o keyword co-occurrences network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Co-ocorrências de Palavras-chave", 
                type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

# Conceptual Structure usando palavras-chave plus (method="CA")

CS <- conceptualStructure(M,field="ID", method="MCA", minDegree=10, clust=5, stemming=FALSE, labelsize=15, documents=25, graph=FALSE)
plot(CS$graph_terms)
#ID = palavras-chave plus; DE = palavras-chave de autores

#Pesquisador de palavras (para realizar wordcloud)
library(dplyr)
library(tidyr)

# Acessar a tabela de frequências das palavras-chave
palavras_chave_freq <- results$DE
#DE = palavras-chave dos autores e ID = palavras-chave plus

# Converter a tabela em um data.frame
palavras_chave_df <- as.data.frame(palavras_chave_freq, 
                                   stringsAsFactors = FALSE)
colnames(palavras_chave_df) <- c("palavra", "freq")
palavras_especificas <- c("lead", "nickel", "cadmium")

# Filtrar as palavras-chave específicas
contagem_especifica <- palavras_chave_df %>%
  filter(tolower(palavra) %in% tolower(palavras_especificas))

print(contagem_especifica) #Visualizar os dados
