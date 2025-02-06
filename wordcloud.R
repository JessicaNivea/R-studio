#Instalar os pacotes
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")

library(tm)
library(SnowballC)

#Wordcloud1
library(wordcloud)

dados = data.frame(
  palavra = c("phosphate", "phosphate fertilizer", "wastewater", 
              "agricultural waste", "composting", "chicken manure", "chicken litter",
              "pig slurry", "pig manure", "swine manure",
              "fertilizer", "organic fertilizer", "mineral fertilizer",
              "organo-mineral", "limestone", "rock dust", "vinasse",
              "filter cake", "sewage sludge", "pesticide", "urban", "waste","manure"),
freq = c(44,19,44,49,24,17,10,10,10,13,70,46,10,10,47,10,10,10,35,10,10,70,70))


#Wordcloud 2
library(wordcloud2)
nuvem2 = planilhaword

wordcloud2(nuvem2)
wordcloud2(nuvem2, color = "random-dark", backgroundColor = "white")
wordcloud2(nuvem2, minRotation = 0, maxRotation = 0, minSize = 15,
           rotateRatio = 0.5, shape ="circle", size = 0.5)


library(openxlsx)
write.xlsx(dados, file = "planilhaword.xlsx")

