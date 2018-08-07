#Import do Tidyverse
library(tidyverse)
#Importar tabela original
#Importação do data-table
FAC2015<-data.table::fread(input="dados/FAC-2015.CSV",sep=";",header = T, 
                           encoding = "Latin-1", stringsAsFactors = F, dec = ",")

#Apresentar dados em notação normal
options(scipen = 9)
#Summary dos dados pela linha de projetos

#alterar nome das variáveis
nomes<-names(FAC2015)
nomes
nomes2<-gsub(pattern = " ",replacement = "_",x = nomes)
nomes2[c(1:9,14,50:53,56:60)]<-c("NProcesso","NomeProponente","Projeto","Programa",
                              "Linguagem","Modalidade","CPF_CNPJ","Objeto","Valor",
                              "RA_Proponente","Como_Ajudará","Estimativa_Publico",
                              "Empregos_Diretos","Cobrança","Escolaridade","Nivel_Superior",
                              "Ja_Concorreu","Ja_Contemplado","Incubadora_BSB")
nomes<-cbind(nomes,nomes2)
#Renomear FAC2015 para os novos nomes
names(FAC2015)<-nomes[,2]
#USar os nomes antigos como labels
names(nomes)<-names(FAC2015)
FAC2015<-Hmisc::upData(FAC2015, labels = nomes[,1])

#tratamento dos dados de valor, sexo
FAC2015_val <- FAC2015 %>%
  mutate(Valor=gsub(pattern = 'R[$] ',replacement = "",x = Valor)) %>%
  mutate(Valor=gsub(pattern = '[.]',replacement = "",x = Valor)) %>%
  mutate(Valor=gsub(pattern = '[,]',replacement = ".",x = Valor))%>%
  mutate(Valor=as.numeric(Valor)) %>%
  mutate(Natureza_Jurídica=as.factor(Natureza_Jurídica),Sexo=as.factor(Sexo),
         RA_Proponente=as.factor(RA_Proponente),Pagamento=tolower(Pagamento),
         Idade=as.factor(Idade)) %>%
  mutate(Pagamento=as.factor(Pagamento))

#Elaboração dos gráficos
#Importa biblioteca gráfica
library(ggthemes)
grafico_Residencia<-FAC2015_val %>%
  ggplot(aes(x=RA_Proponente))+
  geom_bar(aes(y=stat(count)))+
  coord_flip()
grafico_Residencia

unique(FAC2015_val$Pagamento)

grafico_Valores_Natureza <- FAC2015_val %>%
  filter(Pagamento!="não pago") %>%
  ggplot(aes(x = Natureza_Jurídica, group=Natureza_Jurídica,fill=Natureza_Jurídica))+
  geom_bar() +
  # Aqui o texto vai ser a contagem, a posição vai ser afastada etc.
  geom_text(stat="count", aes(label=..count..),position=position_dodge(width=0.9),hjust=-0.25) +
  ggthemes::theme_fivethirtyeight()+
  coord_flip()

grafico_Valores_Natureza


a<-table(FAC2015_val$Natureza_Jurídica)
a

