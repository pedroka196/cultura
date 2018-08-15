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


unique(FAC2015_val$RA_Proponente)

#Elaboração dos gráficos
#Importa biblioteca gráfica
library(ggthemes)
library(ggthemr)
grafico_Residencia<-FAC2015_val %>%
  ggplot(aes(x=RA_Proponente))+
  geom_bar(aes(y=stat(count)))+
  coord_flip()
grafico_Residencia

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
  select(Aguas_Claras:Fora_do_Distrito_Federal,Valor) %>%
  gather("Cidades",Val) %>%
  mutate(Cidades=as.factor(Cidades),Val=ifelse(Val!="X",NA_integer_,1)) %>%
  group_by(Cidades)  %>%
  summarise(Num=sum(Val, na.rm = T)) %>%
  mutate(Cidades = gsub("_"," ",Cidades), Cidades=as.factor(Cidades))


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


# Gráficos Linguagem -----------------------------------------------------------

#Gráficos por linguagem 
#Gráfico valores por linguagem
grafico_Valores_Linguagem<-linguagem %>%
  ggplot(aes(x=Linguagem,y=Valor,fill=Linguagem,group=Linguagem))+
  #Gráfico de colunas
  geom_col() +
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Valor-ValorSD,ymax=Valor+ValorSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("R$ %s", format(Valor,big.mark = ".", decimal.mark = ",")),
                                 y=Valor+ValorSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Valores pagos em reais por linguagem em 2015",ylab("Em reais"))

#Gráfico contagem por linguagem
grafico_Contagem_Linguagem<-linguagem %>%
  ggplot(aes(x=Linguagem,y=Num,fill=Linguagem,group=Linguagem))+
  geom_col() +
<<<<<<< HEAD
  geom_text(stat="identity", aes(label=sprintf("%s", format(Num, big.mark = ".", decimal.mark = ",")),
                                 y=Num),position=position_dodge(width=0.1),vjust=-0.25,size=3) +
  
=======
  geom_text(stat="identity", aes(label=sprintf("%s projetos", format(Num, big.mark = ".", decimal.mark = ",")),
                                 y=Num),position=position_dodge(width=0.1),vjust=-0.25,size=3) +
  ggthemes::theme_fivethirtyeight()+
>>>>>>> ada2f00fe593dc28e1a24ae549ccfae16a3c395a
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  ggtitle("Projetos pagos por linguagem em 2015")

#Gráficos para empregos
grafico_Empregos_Linguagem<-linguagem %>%
  ggplot(aes(x=Linguagem,y=Empregos,fill=Linguagem,group=Linguagem))+
  #Gráfico de colunas
  geom_col() +
  
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,ymax=Empregos+EmpregosSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s", format(Empregos,big.mark = ".", decimal.mark = ",")),
                                 y=Empregos+EmpregosSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Empregos diretos gerados por linguagem em 2015")


#Gráfico de público estiamdo
grafico_Publico_Linguagem<-linguagem %>%
  ggplot(aes(x=Linguagem,y=Publico,fill=Linguagem,group=Linguagem))+
  #Gráfico de colunas
  geom_col() +
  ggthemes::theme_hc() +
  theme(axis.text.x = element_blank(),legend.text = element_text(size=5), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,ymax=Publico+PublicoSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s", format(Publico,big.mark = ".", decimal.mark = ",")),
                                 y=Publico+PublicoSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Público estimado por linguagem em 2015")

#salva apenas estes gráficos


#gráficos para visualização
grafico_Valores_Linguagem
grafico_Contagem_Linguagem
grafico_Empregos_Linguagem
grafico_Publico_Linguagem


# Gráficos Natureza -----------------------------------------------------------

#Gráficos contagem por natureza jurídica
grafico_Contagem_Natureza<-natureza %>%
  ggplot(aes(x=Natureza_Jurídica,y=Num,fill=Natureza_Jurídica,group=Natureza_Jurídica))+
  geom_col() +
  geom_text(stat="identity", aes(label=sprintf("%s", format(Num, big.mark = ".", decimal.mark = ",")),
                                 y=Num),position=position_dodge(width=0.1),vjust=-0.25,size=3) +
  
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7),legend.title = element_blank())+
  ggtitle("Projetos pagos por natureza jurídica em 2015")+
  theme(legend.title = element_blank())

##Gráficos valores por  natureza jurídica
grafico_Valores_Natureza<-natureza %>%
  ggplot(aes(x=Natureza_Jurídica,y=Valor,fill=Natureza_Jurídica,group=Natureza_Jurídica))+
  #Gráfico de colunas
  geom_col() +
  
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Valor-ValorSD,ymax=Valor+ValorSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("R$ %s", format(Valor,big.mark = ".", decimal.mark = ",")),
                                 y=Valor+ValorSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Valores pagos em reais por natureza jurídica em 2015",ylab("Em reais"))

#Gráficos para empregos
grafico_Empregos_Natureza<-natureza %>%
  ggplot(aes(x=Natureza_Jurídica,y=Empregos,fill=Natureza_Jurídica,group=Natureza_Jurídica))+
  #Gráfico de colunas
  geom_col() +
<<<<<<< HEAD
  
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,ymax=Empregos+EmpregosSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s", format(Empregos,big.mark = ".", decimal.mark = ",")),
=======
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,ymax=Empregos+EmpregosSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s empregos", format(Empregos,big.mark = ".", decimal.mark = ",")),
>>>>>>> ada2f00fe593dc28e1a24ae549ccfae16a3c395a
                                 y=Empregos+EmpregosSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Empregos diretos gerados por natureza jurídica em 2015")

#Gráfico de público estiamdo
grafico_Publico_Natureza<-natureza %>%
  ggplot(aes(x=Natureza_Jurídica,y=Publico,fill=Natureza_Jurídica,group=Natureza_Jurídica))+
  #Gráfico de colunas
  geom_col() +
<<<<<<< HEAD
  
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,ymax=Publico+PublicoSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s", format(Publico,big.mark = ".", decimal.mark = ",")),
                                 y=Publico+PublicoSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Público estimado por natureza em 2015")
=======
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,ymax=Publico+PublicoSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s pessoas", format(Publico,big.mark = ".", decimal.mark = ",")),
                                 y=Publico+PublicoSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Publico estimado por natureza em 2015")
>>>>>>> ada2f00fe593dc28e1a24ae549ccfae16a3c395a


grafico_Contagem_Natureza
grafico_Valores_Natureza
grafico_Empregos_Natureza
grafico_Publico_Natureza


# Gráficos Cor ou Raça -----------------------------------------------------------

### Gráficos por cor ou raça
## Neste gráficos haviam dados faltando, e estes serão destacados no gráfico
grafico_Contagem_Cor<-cor %>%
  ggplot(aes(x=Cor,y=Num,fill=Cor,group=Cor))+
  geom_col() +
  geom_text(stat="identity", aes(label=sprintf("%s", format(Num, big.mark = ".", decimal.mark = ",")),
                                 y=Num),position=position_dodge(width=0.1),vjust=-0.25,size=3) +
  
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  ggtitle("Projetos pagos por cor ou raça em 2015")


grafico_Valores_Cor<-cor %>%
  ggplot(aes(x=Cor,y=Valor,fill=Cor,group=Cor))+
  #Gráfico de colunas
  geom_col() +
  
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Valor-ValorSD,ymax=Valor+ValorSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("R$ %s", format(Valor,big.mark = ".", decimal.mark = ",")),
                                 y=Valor+ValorSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Valores pagos em reais por cor ou raça em 2015",ylab("Em reais"))

#Gráficos para empregos
grafico_Empregos_Cor<-cor %>%
  ggplot(aes(x=Cor,y=Empregos,fill=Cor,group=Cor))+
  #Gráfico de colunas
  geom_col() +
<<<<<<< HEAD
  
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,ymax=Empregos+EmpregosSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s", format(Empregos,big.mark = ".", decimal.mark = ",")),
=======
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,ymax=Empregos+EmpregosSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s empregos", format(Empregos,big.mark = ".", decimal.mark = ",")),
>>>>>>> ada2f00fe593dc28e1a24ae549ccfae16a3c395a
                                 y=Empregos+EmpregosSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Empregos diretos gerados por cor em 2015")


#Gráfico de público estimado por cor
grafico_Publico_Cor<-cor %>%
  ggplot(aes(x=Cor,y=Publico,fill=Cor,group=Cor))+
  #Gráfico de colunas
  geom_col() +
<<<<<<< HEAD
  
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,ymax=Publico+PublicoSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s", format(Publico,big.mark = ".", decimal.mark = ",")),
                                 y=Publico+PublicoSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Público estimado por cor ou raça em 2015")
=======
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,ymax=Publico+PublicoSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s pessoas", format(Publico,big.mark = ".", decimal.mark = ",")),
                                 y=Publico+PublicoSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Publico estimado por cor em 2015")
>>>>>>> ada2f00fe593dc28e1a24ae549ccfae16a3c395a




grafico_Contagem_Cor
grafico_Valores_Cor
grafico_Publico_Cor
grafico_Empregos_Cor



# Gráficos Sexo -----------------------------------------------------------

### Gráficos sexo
## Neste gráficos haviam dados faltando, e estes serão destacados no gráfico
grafico_Contagem_Sexo<-sexo %>%
  ggplot(aes(x=Sexo,y=Num,fill=Sexo,group=Sexo))+
  geom_col() +
  geom_text(stat="identity", aes(label=sprintf("%s", format(Num, big.mark = ".", decimal.mark = ",")),
                                 y=Num),position=position_dodge(width=0.1),vjust=-0.25,size=3) +
  
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  ggtitle("Projetos pagos por gênero em 2015")



grafico_Valores_Sexo<-sexo %>%
  ggplot(aes(x=Sexo,y=Valor,fill=Sexo,group=Sexo))+
  #Gráfico de colunas
  geom_col() +
  
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Valor-ValorSD,ymax=Valor+ValorSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("R$ %s", format(Valor,big.mark = ".", decimal.mark = ",")),
                                 y=Valor+ValorSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Valores pagos em reais por gênero em 2015",ylab("Em reais"))

#Gráficos para empregos
<<<<<<< HEAD
grafico_Empregos_Sexo<-sexo %>%
  ggplot(aes(x=Sexo,y=Empregos,fill=Sexo,group=Sexo))+
  #Gráfico de colunas
  geom_col() +
  
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,ymax=Empregos+EmpregosSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s", format(Empregos,big.mark = ".", decimal.mark = ",")),
=======
grafico_Empregos_RA_Proponente<-sexo %>%
  ggplot(aes(x=Sexo,y=Empregos,fill=Sexo,group=Sexo))+
  #Gráfico de colunas
  geom_col() +
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,ymax=Empregos+EmpregosSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s empregos", format(Empregos,big.mark = ".", decimal.mark = ",")),
>>>>>>> ada2f00fe593dc28e1a24ae549ccfae16a3c395a
                                 y=Empregos+EmpregosSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Empregos diretos gerados por gênero jurídica em 2015")

#Gráfico de público estiamdo
grafico_Publico_Sexo<-sexo %>%
  ggplot(aes(x=Sexo,y=Publico,fill=Sexo,group=Sexo))+
  #Gráfico de colunas
  geom_col() +
<<<<<<< HEAD
  
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,ymax=Publico+PublicoSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s", format(Publico,big.mark = ".", decimal.mark = ",")),
                                 y=Publico+PublicoSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Público estimado por gênero em 2015")
=======
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,ymax=Publico+PublicoSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s pessoas", format(Publico,big.mark = ".", decimal.mark = ",")),
                                 y=Publico+PublicoSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Publico estimado por gênero em 2015")
>>>>>>> ada2f00fe593dc28e1a24ae549ccfae16a3c395a

grafico_Contagem_Sexo
grafico_Valores_Sexo
grafico_Publico_Sexo
grafico_Empregos_Sexo
<<<<<<< HEAD


# Gráficos Cidade -----------------------------------------------------------

grafico_Contagem_RA_Proponente<-cidades %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>1) %>%
  ggplot(aes(x=reorder(RA_Proponente,Num),y=Num,fill=RA_Proponente,group=RA_Proponente))+
  geom_col() +
  geom_text(stat="identity", aes(label=sprintf("%s", format(Num, big.mark = ".", decimal.mark = ",")),
                                 y=Num),position=position_dodge(width=0.1),hjust=-0.15,size=3) +
  
  theme(legend.text = element_text(size=7), legend.title = element_blank(),legend.position = "none")+
  ggtitle("Projetos pagos por Região Administrativa em 2015")+
  coord_flip()



grafico_Valores_RA_Proponente<-cidades %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>1) %>%
  ggplot(aes(x=reorder(RA_Proponente,Valor),y=Valor,fill=RA_Proponente,group=RA_Proponente))+
  #Gráfico de colunas
  geom_col() +
  
  theme(legend.text = element_text(size=7), legend.title = element_blank(),legend.position = "none")+
  geom_errorbar(mapping=aes(ymin=Valor-ValorSD,ymax=Valor+ValorSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("R$ %s", format(Valor,big.mark = ".", decimal.mark = ",")),
                                 y=Valor+ValorSD),position=position_dodge(width=0.1),hjust=-0.15,size=3) +
  ggtitle("Valores pagos em reais por Região Administrativa em 2015",ylab("Em reais"))+
  coord_flip()

#Gráficos para empregos
grafico_Empregos_RA_Proponente<-cidades %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>1) %>%
  ggplot(aes(x=reorder(RA_Proponente,Empregos),y=Empregos,fill=RA_Proponente,group=RA_Proponente))+
  #Gráfico de colunas
  geom_col() +
  
  theme(legend.text = element_text(size=7), legend.title = element_blank(),
        legend.position = "none")+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,ymax=Empregos+EmpregosSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s", format(Empregos,big.mark = ".", decimal.mark = ",")),
                                 y=Empregos+EmpregosSD),position=position_dodge(width=0.1),hjust=-0.15,size=3) +
  ggtitle("Empregos diretos gerados por Região Administrativa em 2015")+
  coord_flip()

# Gráfico de público estimado
grafico_Publico_RA_Proponente<-cidades %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>2) %>%
  #Optou-se por colocar por ordem
  ggplot(aes(x=reorder(RA_Proponente,Publico),y=Publico,fill=RA_Proponente,group=RA_Proponente))+
  #Gráfico de colunas
  geom_col() +
  
  theme(legend.title = element_blank(),
        legend.position = "none")+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,ymax=Publico+PublicoSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s", format(Publico,big.mark = ".", decimal.mark = ",")),
                                 y=Publico+PublicoSD),position=position_dodge(width=0.1),hjust=-0.15,size=3) +
  ggtitle("Público estimado por Região Administrativa em 2015")+
  coord_flip()

grafico_Contagem_RA_Proponente
grafico_Valores_RA_Proponente
grafico_Publico_RA_Proponente
grafico_Empregos_RA_Proponente

# Gráficos Cidades Atingidas -----------------------------------------------------------

grafico_Contagem_RA_Atingidas<-cidades_atingidas %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>1) %>%
  ggplot(aes(x=reorder(Cidades, Num),y=Num,fill=Cidades,group=Cidades))+
  geom_col() +
  geom_text(stat="identity", aes(label=sprintf("%s", format(Num, big.mark = ".", decimal.mark = ",")),
                                 y=Num),position=position_dodge(width =0.1),hjust=-0.15,size=3) +
  
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank(), legend.position = "none")+
  ggtitle("Número de vezes que uma RA foi alvo por um projeto")+
  coord_flip()

grafico_Contagem_RA_Atingidas


# Gráfico de escolaridade ----------------------------------------
## Neste gráficos haviam dados faltando, e estes serão destacados no gráfico
grafico_Contagem_Escolaridade<-escolaridade %>%
  ggplot(aes(x=reorder(Escolaridade,Num),y=Num,fill=Escolaridade,group=Escolaridade))+
  geom_col() +
  geom_text(stat="identity", aes(label=sprintf("%s", format(Num, big.mark = ".", decimal.mark = ",")),
                                 y=Num),position=position_dodge(width=0.1),hjust=-0.25,size=3) +
  
  theme(legend.text = element_text(size=7), legend.title = element_blank(),legend.position = "none")+
  coord_flip() +
  ggtitle("Projetos pagos por escolaridade em 2015")

grafico_Valores_Escolaridade<-escolaridade %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>1) %>%
  ggplot(aes(x=reorder(Escolaridade,Valor),y=Valor,fill=Escolaridade,group=Escolaridade))+
  #Gráfico de colunas
  geom_col() +
  
  theme(legend.text = element_text(size=7), legend.title = element_blank(),legend.position = "none")+
  geom_errorbar(mapping=aes(ymin=Valor-ValorSD,ymax=Valor+ValorSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("R$ %s", format(Valor,big.mark = ".", decimal.mark = ",")),
                                 y=Valor+ValorSD),position=position_dodge(width=0.1),hjust=-0.15,size=3) +
  ggtitle("Valores pagos em reais por escolaridade em 2015",xlab("Em reais"))+
  coord_flip()

#Gráficos para empregos
grafico_Empregos_Escolaridade<-escolaridade %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>1) %>%
  ggplot(aes(x=reorder(Escolaridade,Empregos),y=Empregos,fill=Escolaridade,group=Escolaridade))+
  #Gráfico de colunas
  geom_col() +
  
  theme(legend.position = "none",legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,ymax=Empregos+EmpregosSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s", format(Empregos,big.mark = ".", decimal.mark = ",")),
                                 y=Empregos+EmpregosSD),position=position_dodge(width=0.1),hjust=-0.15,size=3) +
  ggtitle("Empregos diretos gerados por escolaridade em 2015")+
  coord_flip()

# Gráfico de público estimado
grafico_Publico_Escolaridade<-escolaridade %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>2) %>%
  ggplot(aes(x=reorder(Escolaridade,Publico),y=Publico,fill=Escolaridade,group=Escolaridade))+
  #Gráfico de colunas
  geom_col() +
  
  #Tirou-se a legenda dos projetos e optou-se apenas por deixar o rótulo y de cada escolaridade
  theme(legend.position = "none",legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,ymax=Publico+PublicoSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s", format(Publico,big.mark = ".", decimal.mark = ",")),
                                 y=Publico+PublicoSD),position=position_dodge(width=0.1),hjust=-0.15,size=3) +
  ggtitle("Público estimado por escolaridade em 2015")+
  coord_flip()

grafico_Contagem_Escolaridade
grafico_Valores_Escolaridade
grafico_Publico_Escolaridade
grafico_Empregos_Escolaridade


# Salvar imagem -------------------
save.image(file="dados_finais_salvos", compress = T)

rm(list = rm())

load("dados_finais_salvos")

grafico_Residencia
=======


# Gráficos Cidade -----------------------------------------------------------

grafico_Contagem_RA_Proponente<-cidades %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>1) %>%
  ggplot(aes(x=RA_Proponente,y=Num,fill=RA_Proponente,group=RA_Proponente))+
  geom_col() +
  geom_text(stat="identity", aes(label=sprintf("%s", format(Num, big.mark = ".", decimal.mark = ",")),
                                 y=Num),position=position_dodge(height=0.1),hjust=-0.15,size=3) +
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  ggtitle("Projetos pagos por Região Administrativa em 2015")+
  coord_flip()



grafico_Valores_RA_Proponente<-cidades %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>1) %>%
  ggplot(aes(x=RA_Proponente,y=Valor,fill=RA_Proponente,group=RA_Proponente))+
  #Gráfico de colunas
  geom_col() +
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Valor-ValorSD,ymax=Valor+ValorSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("R$ %s", format(Valor,big.mark = ".", decimal.mark = ",")),
                                 y=Valor+ValorSD),position=position_dodge(width=0.1),hjust=-0.15,size=3) +
  ggtitle("Valores pagos em reais por gênero em 2015",ylab("Em reais"))+
  coord_flip()

#Gráficos para empregos
grafico_Empregos_RA_Proponente<-cidades %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>1) %>%
  ggplot(aes(x=RA_Proponente,y=Empregos,fill=RA_Proponente,group=RA_Proponente))+
  #Gráfico de colunas
  geom_col() +
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,ymax=Empregos+EmpregosSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s empregos", format(Empregos,big.mark = ".", decimal.mark = ",")),
                                 y=Empregos+EmpregosSD),position=position_dodge(width=0.1),hjust=-0.15,size=3) +
  ggtitle("Empregos diretos gerados por gênero em 2015")+
  coord_flip()

# Gráfico de público estimado
grafico_Publico_RA_Proponente<-cidades %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>2) %>%
  ggplot(aes(x=RA_Proponente,y=Publico,fill=RA_Proponente,group=RA_Proponente))+
  #Gráfico de colunas
  geom_col() +
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,ymax=Publico+PublicoSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s pessoas", format(Publico,big.mark = ".", decimal.mark = ",")),
                                 y=Publico+PublicoSD),position=position_dodge(width=0.1),hjust=-0.15,size=3) +
  ggtitle("Publico estimado por gênero em 2015")+
  coord_flip()

grafico_Contagem_RA_Proponente
grafico_Valores_RA_Proponente
grafico_Publico_RA_Proponente
grafico_Empregos_RA_Proponente

# Gráficos Cidades Atingidas -----------------------------------------------------------

grafico_Contagem_RA_Atingidas<-cidades_atingidas %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>2) %>%
  arrange(Num) %>%
  ggplot(aes(x=reorder(Cidades, Num),y=Num,fill=Cidades,group=Cidades))+
  geom_col() +
  geom_text(stat="identity", aes(label=sprintf("%s", format(Num, big.mark = ".", decimal.mark = ",")),
                                 y=Num),position=position_dodge(width =0.1),hjust=-0.15,size=3) +
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank(), legend.position = "none")+
  ggtitle("Número de vezes que uma RA foi alvo por um projeto")+
  coord_flip()

  grafico_Contagem_RA_Atingidas
  
  
# Gráfico de escolaridade ----------------------------------------
  ## Neste gráficos haviam dados faltando, e estes serão destacados no gráfico
  grafico_Contagem_Escolaridade<-escolaridade %>%
    ggplot(aes(x=Escolaridade,y=Num,fill=Escolaridade,group=Escolaridade))+
    geom_col() +
    geom_text(stat="identity", aes(label=sprintf("%s projetos", format(Num, big.mark = ".", decimal.mark = ",")),
                                   y=Num),position=position_dodge(width=0.1),hjust=-0.25,size=3) +
    ggthemes::theme_fivethirtyeight()+
    theme(axis.text.y = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
    coord_flip() +
    ggtitle("Projetos pagos por escolaridade em 2015")
  
  grafico_Valores_Escolaridade<-escolaridade %>%
    #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
    filter(Num>1) %>%
    ggplot(aes(x=Escolaridade,y=Valor,fill=Escolaridade,group=Escolaridade))+
    #Gráfico de colunas
    geom_col() +
    ggthemes::theme_fivethirtyeight()+
    theme(axis.text.y = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
    geom_errorbar(mapping=aes(ymin=Valor-ValorSD,ymax=Valor+ValorSD),width=0) +
    #### 
    geom_text(stat="identity", aes(label=sprintf("R$ %s", format(Valor,big.mark = ".", decimal.mark = ",")),
                                   y=Valor+ValorSD),position=position_dodge(width=0.1),hjust=-0.15,size=3) +
    ggtitle("Valores pagos em reais por escolaridade em 2015",ylab("Em reais"))+
    coord_flip()
  
  #Gráficos para empregos
  grafico_Empregos_Escolaridade<-escolaridade %>%
    #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
    filter(Num>1) %>%
    ggplot(aes(x=Escolaridade,y=Empregos,fill=Escolaridade,group=Escolaridade))+
    #Gráfico de colunas
    geom_col() +
    ggthemes::theme_fivethirtyeight()+
    theme(axis.text.y = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
    geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,ymax=Empregos+EmpregosSD),width=0) +
    #### 
    geom_text(stat="identity", aes(label=sprintf("%s empregos", format(Empregos,big.mark = ".", decimal.mark = ",")),
                                   y=Empregos+EmpregosSD),position=position_dodge(width=0.1),hjust=-0.15,size=3) +
    ggtitle("Empregos diretos gerados por escolaridade em 2015")+
    coord_flip()
  
  # Gráfico de público estimado
  grafico_Publico_Escolaridade<-escolaridade %>%
    #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
    filter(Num>2) %>%
    ggplot(aes(x=Escolaridade,y=Publico,fill=Escolaridade,group=Escolaridade))+
    #Gráfico de colunas
    geom_col() +
    ggthemes::theme_fivethirtyeight()+
    theme(axis.text.y = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
    geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,ymax=Publico+PublicoSD),width=0) +
    #### 
    geom_text(stat="identity", aes(label=sprintf("%s pessoas", format(Publico,big.mark = ".", decimal.mark = ",")),
                                   y=Publico+PublicoSD),position=position_dodge(width=0.1),hjust=-0.15,size=3) +
    ggtitle("Público estimado por escolaridade em 2015")+
    coord_flip()
  
  grafico_Contagem_Escolaridade
  grafico_Valores_Escolaridade
  grafico_Publico_Escolaridade
  grafico_Empregos_Escolaridade
  
>>>>>>> ada2f00fe593dc28e1a24ae549ccfae16a3c395a
