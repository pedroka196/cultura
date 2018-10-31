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

unique(FAC2015$RA_Proponente)

#tratamento dos dados de valor, sexo
FAC2015_val <- FAC2015 %>%
  mutate(Valor=gsub(pattern = 'R[$] ',replacement = "",x = Valor)) %>%
  mutate(Valor=gsub(pattern = '[.]',replacement = "",x = Valor)) %>%
  mutate(Valor=gsub(pattern = '[,]',replacement = ".",x = Valor))%>%
  mutate(Valor=as.numeric(Valor)) %>%
  mutate(Natureza_Jurídica=as.factor(Natureza_Jurídica),Sexo=as.factor(Sexo),
         Pagamento=tolower(Pagamento),
         Idade=as.factor(Idade),RA_Proponente=as.factor(RA_Proponente)) %>%
  mutate(Pagamento=as.factor(Pagamento),Estimativa_Publico=as.numeric(Estimativa_Publico),
         Empregos_Diretos=as.numeric(Empregos_Diretos))%>%
  #As variáveis informadas como 0 estão sendo colocadas como "Não informado"
  mutate(Idade=as.factor(Idade),Escolaridade=ifelse(Escolaridade=="0","Não informado",Escolaridade),
         Escolaridade=as.factor(Escolaridade),Cobrança=ifelse(Cobrança=="0","Não informado",Cobrança))


tamanho=3

unique(FAC2015_val$RA_Proponente)

unique(FAC2015_val$Pagamento)

##### Contagens de valores
######
##### Contagens de valores
#filtra valores pela natureza
natureza<-FAC2015_val %>%
  #Ignora valores não pagos
  filter(Pagamento!="não pago") %>%
  #agrupa pela natureza
  group_by(Natureza_Jurídica) %>%
  #faz a soma, pega o desvio, a média, a mediana, a soma do valor e 
  # a quantidade de elements
  #Importante ressaltar que há diversos valores faltantando
  summarise(ValorSD=sd(Valor),ValorMean=mean(Valor),ValorMedian=median(Valor),
            Valor=sum(Valor),Num=n(),Publico=sum(Estimativa_Publico, na.rm = T),
            PublicoMean=mean(Estimativa_Publico, na.rm = T),PublicoSD=sd(Estimativa_Publico, na.rm = T),
            Empregos=sum(Empregos_Diretos, na.rm = T),EmpregosMean=mean(Empregos_Diretos, na.rm = T),
            EmpregosSD=sd(Empregos_Diretos, na.rm = T))

#Valor por linguagem
linguagem<-FAC2015_val %>%
  #Ignora valores não pagos
  filter(Pagamento!="não pago") %>%
  #agrupa pela natureza
  group_by(Linguagem) %>%
  #faz a soma, pega o desvio, a média, a mediana, a soma do valor e 
  # a quantidade de elements
  summarise(ValorSD=sd(Valor),ValorMean=mean(Valor),ValorMedian=median(Valor),
            Valor=sum(Valor),Num=n(),Publico=sum(Estimativa_Publico, na.rm = T),
            PublicoMean=mean(Estimativa_Publico, na.rm = T),PublicoSD=sd(Estimativa_Publico, na.rm = T),
            Empregos=sum(Empregos_Diretos, na.rm = T),EmpregosMean=mean(Empregos_Diretos, na.rm = T),
            EmpregosSD=sd(Empregos_Diretos, na.rm = T))

# Valor por pessoas físicas e raça
cor<-FAC2015_val %>%
  #Ignora valores não pagos
  filter(Pagamento!="não pago",Natureza_Jurídica=="Pessoa Física") %>%
  # altera os valores errados da variável cor
  mutate(Cor=ifelse(Cor_ou_Raça=="0","Não informado",Cor_ou_Raça)) %>%
  group_by(Cor) %>%
  #faz a soma, pega o desvio, a média, a mediana, a soma do valor e 
  # a quantidade de elements
  summarise(ValorSD=sd(Valor),ValorMean=mean(Valor),ValorMedian=median(Valor),
            Valor=sum(Valor),Num=n(),Publico=sum(Estimativa_Publico, na.rm = T),
            PublicoMean=mean(Estimativa_Publico, na.rm = T),PublicoSD=sd(Estimativa_Publico, na.rm = T),
            Empregos=sum(Empregos_Diretos, na.rm = T),EmpregosMean=mean(Empregos_Diretos, na.rm = T),
            EmpregosSD=sd(Empregos_Diretos, na.rm = T))

# Valores por sexo
sexo<-FAC2015_val %>%
  #Ignora valores não pagos
  filter(Pagamento!="não pago",Natureza_Jurídica=="Pessoa Física") %>%
  # altera os valores errados da variável cor
  group_by(Sexo) %>%
  #faz a soma, pega o desvio, a média, a mediana, a soma do valor e 
  # a quantidade de elements
  summarise(ValorSD=sd(Valor),ValorMean=mean(Valor),ValorMedian=median(Valor),
            Valor=sum(Valor),Num=n(),Publico=sum(Estimativa_Publico, na.rm = T),
            PublicoMean=mean(Estimativa_Publico, na.rm = T),PublicoSD=sd(Estimativa_Publico, na.rm = T),
            Empregos=sum(Empregos_Diretos, na.rm = T),EmpregosMean=mean(Empregos_Diretos, na.rm = T),
            EmpregosSD=sd(Empregos_Diretos, na.rm = T))


#### Criação de dados para cidades
cidades<-FAC2015_val %>%
  filter(Pagamento!="não pago") %>%
  group_by(RA_Proponente) %>%
  summarise(ValorSD=sd(Valor, na.rm=T),ValorMean=mean(Valor, na.rm=T),ValorMedian=median(Valor, na.rm=T),
            Valor=sum(Valor, na.rm = T),Num=n(),Publico=sum(Estimativa_Publico, na.rm = T),
            PublicoMean=mean(Estimativa_Publico, na.rm = T),PublicoSD=sd(Estimativa_Publico, na.rm = T),
            Empregos=sum(Empregos_Diretos, na.rm = T),EmpregosMean=mean(Empregos_Diretos, na.rm = T),
            EmpregosSD=sd(Empregos_Diretos, na.rm = T))

#### Criação de dados para cidades que tiveram fotos
cidades_atingidas<-FAC2015_val %>%
  filter(Pagamento!="não pago") %>%
  mutate(Valores=Valor/rowSums(x=select(Aguas_Claras:Fora_do_Distrito_Federal),na.rm = T))
  select(Aguas_Claras:Vicente_Pires,Fora_do_Distrito_Federal,Valor) %>%
    %>%## Separação em uma tabela
  filter(Valores!=Inf) 
  gather(c("Cidades","valores"),Val)



cidades_atingidas<-FAC2015_val %>%
  filter(Pagamento!="não pago") %>%
  select(Aguas_Claras:Fora_do_Distrito_Federal,Valor)

cidades_atingidas <- cidades_atingidas %>%
  mutate(Valores=Valor/rowSums(cidades_atingidas=="X",na.rm = T))%>%
  filter(Valores!=Inf) %>%
  gather("Cidades",Val,factor_key = T)

cidades_atingidas.valores <- cidades_atingidas %>%
  filter(Cidades=="Valores")

cidades_atingidas<-cidades_atingidas %>%
  filter(Cidades!="Valor" & Cidades!= "Valores")

for(i in 1:(length(unique(cidades_atingidas$Cidades)))-1){
  if(i==1){
    total<-cidades_atingidas.valores
  }
  total<-as.data.frame(rbind(total,cidades_atingidas.valores))
  print(i)
  i<-i+1
}
cidades_atingidas.valores<-total
cidades_atingidas2<-cbind(cidades_atingidas,cidades_atingidas.valores)
names(cidades_atingidas2)<-c("Cidades","Num","FAC","Valores")


cidades_atingidas <- cidades_atingidas2 %>%
  select(Cidades,Num,Valores) %>%
  mutate(Num=ifelse(Num=="X",yes=1,no=0), Valores=as.numeric(Valores)) %>%
  group_by(Cidades) %>%
  summarise(ValoresMedios=sum(Num*Valores, na.rm = T),
            Num=sum(Num))
cidades_atingidas$Cidades<-as.factor(gsub(pattern = "_",replacement = " ",cidades_atingidas$Cidades))

unique(FAC2015_val$Escolaridade)
#Valor por Escolaridade
escolaridade<-FAC2015_val %>%
  #Ignora valores não pagos
  filter(Pagamento!="não pago", Natureza_Jurídica!="Pessoa Jurídica") %>%
  #agrupa pela natureza
  group_by(Escolaridade) %>%
  #faz a soma, pega o desvio, a média, a mediana, a soma do valor e 
  # a quantidade de elements
  summarise(ValorSD=sd(Valor),ValorMean=mean(Valor),ValorMedian=median(Valor),
            Valor=sum(Valor),Num=n(),Publico=sum(Estimativa_Publico, na.rm = T),
            PublicoMean=mean(Estimativa_Publico, na.rm = T),PublicoSD=sd(Estimativa_Publico, na.rm = T),
            Empregos=sum(Empregos_Diretos, na.rm = T),EmpregosMean=mean(Empregos_Diretos, na.rm = T),
            EmpregosSD=sd(Empregos_Diretos, na.rm = T))


rm(list=graficos_todos)
dados<-ls()
# Salvar imagem -------------------
save(list = dados, file="dados_finais_salvos.RData",compress = T)

write.table(names(FAC2015_val), 
            file ="nomes.csv",
            sep = ";")
