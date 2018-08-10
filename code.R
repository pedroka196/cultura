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

# Gráficos Linguagem -----------------------------------------------------------

#Gráficos por linguagem 
#Gráfico valores por linguagem
grafico_Valores_Linguagem<-linguagem %>%
  ggplot(aes(x=Linguagem,y=Valor,fill=Linguagem,group=Linguagem))+
  #Gráfico de colunas
  geom_col() +
  ggthemes::theme_fivethirtyeight()+
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
  geom_text(stat="identity", aes(label=sprintf("%s projetos", format(Num, big.mark = ".", decimal.mark = ",")),
                                 y=Num),position=position_dodge(width=0.1),vjust=-0.25,size=3) +
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  ggtitle("Projetos pagos por linguagem em 2015")

#Gráficos para empregos
grafico_Empregos_Linguagem<-linguagem %>%
  ggplot(aes(x=Linguagem,y=Empregos,fill=Linguagem,group=Linguagem))+
  #Gráfico de colunas
  geom_col() +
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,ymax=Empregos+EmpregosSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s empregos", format(Empregos,big.mark = ".", decimal.mark = ",")),
                                 y=Empregos+EmpregosSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Empregos diretos gerados por linguagem em 2015")


#Gráfico de público estiamdo
grafico_Publico_Linguagem<-linguagem %>%
  ggplot(aes(x=Linguagem,y=Publico,fill=Linguagem,group=Linguagem))+
  #Gráfico de colunas
  geom_col() +
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,ymax=Publico+PublicoSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s pessoas", format(Publico,big.mark = ".", decimal.mark = ",")),
                                 y=Publico+PublicoSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Publico estimado por linguagem em 2015")


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
  geom_text(stat="identity", aes(label=sprintf("%s projetos", format(Num, big.mark = ".", decimal.mark = ",")),
                                 y=Num),position=position_dodge(width=0.1),vjust=-0.25,size=3) +
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7),legend.title = element_blank())+
  ggtitle("Projetos pagos por natureza jurídica em 2015")+
  theme(legend.title = element_blank())

##Gráficos valores por  natureza jurídica
grafico_Valores_Natureza<-natureza %>%
  ggplot(aes(x=Natureza_Jurídica,y=Valor,fill=Natureza_Jurídica,group=Natureza_Jurídica))+
  #Gráfico de colunas
  geom_col() +
  ggthemes::theme_fivethirtyeight()+
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
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,ymax=Empregos+EmpregosSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s empregos", format(Empregos,big.mark = ".", decimal.mark = ",")),
                                 y=Empregos+EmpregosSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Empregos diretos gerados por natureza jurídica em 2015")

#Gráfico de público estiamdo
grafico_Publico_Natureza<-natureza %>%
  ggplot(aes(x=Natureza_Jurídica,y=Publico,fill=Natureza_Jurídica,group=Natureza_Jurídica))+
  #Gráfico de colunas
  geom_col() +
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,ymax=Publico+PublicoSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s pessoas", format(Publico,big.mark = ".", decimal.mark = ",")),
                                 y=Publico+PublicoSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Publico estimado por natureza em 2015")


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
  geom_text(stat="identity", aes(label=sprintf("%s projetos", format(Num, big.mark = ".", decimal.mark = ",")),
                                 y=Num),position=position_dodge(width=0.1),vjust=-0.25,size=3) +
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  ggtitle("Projetos pagos por cor ou raça em 2015")


grafico_Valores_Cor<-cor %>%
  ggplot(aes(x=Cor,y=Valor,fill=Cor,group=Cor))+
  #Gráfico de colunas
  geom_col() +
  ggthemes::theme_fivethirtyeight()+
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
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,ymax=Empregos+EmpregosSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s empregos", format(Empregos,big.mark = ".", decimal.mark = ",")),
                                 y=Empregos+EmpregosSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Empregos diretos gerados por cor em 2015")


#Gráfico de público estiamado por cor
grafico_Publico_Cor<-cor %>%
  ggplot(aes(x=Cor,y=Publico,fill=Cor,group=Cor))+
  #Gráfico de colunas
  geom_col() +
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,ymax=Publico+PublicoSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s pessoas", format(Publico,big.mark = ".", decimal.mark = ",")),
                                 y=Publico+PublicoSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Publico estimado por cor em 2015")




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
  geom_text(stat="identity", aes(label=sprintf("%s projetos", format(Num, big.mark = ".", decimal.mark = ",")),
                                 y=Num),position=position_dodge(width=0.1),vjust=-0.25,size=3) +
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  ggtitle("Projetos pagos por gênero em 2015")



grafico_Valores_Sexo<-sexo %>%
  ggplot(aes(x=Sexo,y=Valor,fill=Sexo,group=Sexo))+
  #Gráfico de colunas
  geom_col() +
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Valor-ValorSD,ymax=Valor+ValorSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("R$ %s", format(Valor,big.mark = ".", decimal.mark = ",")),
                                 y=Valor+ValorSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Valores pagos em reais por gênero em 2015",ylab("Em reais"))

#Gráficos para empregos
grafico_Empregos_RA_Proponente<-sexo %>%
  ggplot(aes(x=Sexo,y=Empregos,fill=Sexo,group=Sexo))+
  #Gráfico de colunas
  geom_col() +
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,ymax=Empregos+EmpregosSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s empregos", format(Empregos,big.mark = ".", decimal.mark = ",")),
                                 y=Empregos+EmpregosSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Empregos diretos gerados por gênero jurídica em 2015")

#Gráfico de público estiamdo
grafico_Publico_Sexo<-sexo %>%
  ggplot(aes(x=Sexo,y=Publico,fill=Sexo,group=Sexo))+
  #Gráfico de colunas
  geom_col() +
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_blank(),legend.text = element_text(size=7), legend.title = element_blank())+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,ymax=Publico+PublicoSD),width=0) +
  #### 
  geom_text(stat="identity", aes(label=sprintf("%s pessoas", format(Publico,big.mark = ".", decimal.mark = ",")),
                                 y=Publico+PublicoSD),position=position_dodge(width=0.1),vjust=-0.5,size=3) +
  ggtitle("Publico estimado por gênero em 2015")

grafico_Contagem_Sexo
grafico_Valores_Sexo
grafico_Publico_Sexo
grafico_Empregos_Sexo


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
  ggtitle("Empregos diretos gerados por gênero jurídica em 2015")+
  coord_flip()

#Gráfico de público estiamdo
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
