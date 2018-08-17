library(ggplot2)
library(ggthemes)
library(ggthemr)
library(ggrepel)

# Gráficos Linguagem -----------------------------------------------------------
tamanho=2
options(scipen = 2)
#Gráficos por linguagem 
#Gráfico valores por linguagem
grafico_Valores_Linguagem<-linguagem %>%
  ggplot(aes(x=Linguagem,
             y=Valor,
             fill=Linguagem, 
             group=Linguagem))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  theme(axis.text.x = element_blank(),
        legend.title = element_blank(),
        axis.title=element_blank())+
  geom_errorbar(mapping=aes(ymin=Valor-ValorSD,
                            ymax=Valor+ValorSD),
                width=0) +
  #### Optei pelo ggrepel porque ele coloca labels em cada plot, ficando mais fácil de visualizar
  # ggrepel::geom_label_repel(stat="identity", 
  #                           aes(label=sprintf("R$ %s", 
  #                                             format(Valor,
  #                                                    big.mark = ".", 
  #                                                    decimal.mark = ",")),
  #                                                y=Valor+ValorSD),
  #                           direction = "y",
  #                           size=tamanho, 
  #                           show.legend = F, 
  #                           box.padding = unit(x = 0, 
  #                                              units = "pt"))+
  ggtitle("Valores pagos em reais por linguagem em 2015",
          ylab("Em reais"))

#Gráfico contagem por linguagem
grafico_Contagem_Linguagem<-linguagem %>%
  ggplot(aes(x=Linguagem,
             y=Num,
             fill=Linguagem))+
  geom_col(position = position_dodge(width = 0)) +
  # ggrepel::geom_label_repel(stat="identity", 
  #                           aes(label=sprintf("%s", 
  #                                             format(Num, big.mark = ".", 
  #                                                    decimal.mark = ",")),
  #                                y=Num),
  #                           size=tamanho,
  #                           show.legend = F, 
  #                           box.padding = unit(x = 0, units = "pt")) +
  theme(axis.text.x = element_blank(),
        legend.title = element_blank(),
        axis.title=element_blank())+
  ggtitle("Projetos pagos por linguagem em 2015")

#Gráficos para empregos
grafico_Empregos_Linguagem<-linguagem %>%
  ggplot(aes(x=Linguagem,
             y=Empregos,
             fill=Linguagem,
             group=Linguagem))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  theme(axis.text.x = element_blank(),
         
        legend.title = element_blank(), 
        axis.title=element_blank())+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,
                            ymax=Empregos+EmpregosSD),
                width=0) +
  # ggrepel::geom_label_repel(stat="identity",
  #                           aes(label=sprintf("%s", 
  #                                             format(Empregos,
  #                                                    big.mark = ".",
  #                                                    decimal.mark = ","))),
  #                           size=tamanho, 
  #                           direction = "y",
  #                           show.legend = F, 
  #                           box.padding = unit(x = 0, units = "pt")) +
  ggtitle("Empregos diretos gerados por linguagem em 2015")


#Gráfico de público estiamdo
grafico_Publico_Linguagem<-linguagem %>%
  ggplot(aes(x=Linguagem,
             y=Publico,
             fill=Linguagem,
             group=Linguagem))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  #ggthemes::theme_hc() +
  theme(axis.text.x = element_blank(),

        legend.title = element_blank(),
        axis.title=element_blank())+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,
                            ymax=Publico+PublicoSD),
                width=0) +
  #### 
  # ggrepel::geom_label_repel(stat="identity", 
  #                           aes(label=sprintf("%s", 
  #                                             format(Publico,big.mark = ".",
  #                                                    decimal.mark = ",")),
  #                                y=Publico+PublicoSD),
  #                           size=tamanho, 
  #                           show.legend = F,
  #                           box.padding = unit(x = 0, units = "pt")) +
  ggtitle("Público estimado por linguagem em 2015")

#salva apenas estes gráficos



# Gráficos Natureza -----------------------------------------------------------

#Gráficos contagem por natureza jurídica
grafico_Contagem_Natureza<-natureza %>%
  ggplot(aes(x=Natureza_Jurídica,
             y=Num,
             fill=Natureza_Jurídica,
             group=Natureza_Jurídica))+
  geom_col(position = position_dodge(width = 0)) +
  # ggrepel::geom_label_repel(stat="identity", 
  #                           aes(label=sprintf("%s", 
  #                                             format(Num,
  #                                                    big.mark = ".",
  #                                                    decimal.mark = ",")),
  #                               y=Num),
  #                           size=tamanho, 
  #                           show.legend = F,
  #                           box.padding = unit(x = 0, units = "pt")) +
  
  theme(axis.text.x = element_blank(),
        legend.title = element_blank(), 
        axis.title=element_blank())+
  ggtitle("Projetos pagos por natureza jurídica em 2015")+
  theme(legend.title = element_blank(), 
        axis.title=element_blank())

##Gráficos valores por  natureza jurídica
grafico_Valores_Natureza<-natureza %>%
  ggplot(aes(x=Natureza_Jurídica,
             y=Valor,
             fill=Natureza_Jurídica,
             group=Natureza_Jurídica))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  theme(axis.text.x = element_blank(),
        legend.title = element_blank(), 
        axis.title=element_blank())+
  geom_errorbar(mapping=aes(ymin=Valor-ValorSD,
                            ymax=Valor+ValorSD),
                width=0) +
  #### 
  # ggrepel::geom_label_repel(stat="identity",
  #                           aes(label=sprintf("R$ %s", 
  #                                             format(Valor,
  #                                                    big.mark = ".",
  #                                                    decimal.mark = ",")),
  #                                y=Valor+ValorSD),
  #                           size=tamanho, 
  #                           show.legend = F,
  #                           box.padding = unit(x = 0, units = "pt")) +
  ggtitle("Valores pagos em reais por natureza jurídica em 2015",
          ylab("Em reais"))

#Gráficos para empregos
grafico_Empregos_Natureza<-natureza %>%
  ggplot(aes(x=Natureza_Jurídica,
             y=Empregos,
             fill=Natureza_Jurídica,
             group=Natureza_Jurídica))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  
  theme(axis.text.x = element_blank(),
         
        legend.title = element_blank(), axis.title=element_blank())+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,
                            ymax=Empregos+EmpregosSD),
                width=0) +
  #### 
  # ggrepel::geom_label_repel(stat="identity", 
  #                           aes(label=sprintf("%s", 
  #                                             format(Empregos,
  #                                                    big.mark = ".", 
  #                                                    decimal.mark = ",")),
  #                                y=Empregos+EmpregosSD),
  #                           size=tamanho, 
  #                           show.legend = F, 
  #                           box.padding = unit(x = 0, 
  #                                              units = "pt")) +
  ggtitle("Empregos diretos gerados por natureza jurídica em 2015")

#Gráfico de público estiamdo
grafico_Publico_Natureza<-natureza %>%
  ggplot(aes(x=Natureza_Jurídica,
             y=Publico,
             fill=Natureza_Jurídica,
             group=Natureza_Jurídica))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  theme(axis.text.x = element_blank(),
        
        legend.title = element_blank(),
        axis.title=element_blank())+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,
                            ymax=Publico+PublicoSD),
                width=0) +
  #### 
  # ggrepel::geom_label_repel(stat="identity",
  #                           aes(label=sprintf("%s",
  #                                             format(Publico,
  #                                                    big.mark = ".",
  #                                                    decimal.mark = ",")),
  #                                y=Publico+PublicoSD),
  #                           size=tamanho, show.legend = F, 
  #                           box.padding = unit(x = 0, 
  #                                              units = "pt")) +
  ggtitle("Público estimado por natureza em 2015")


# Gráficos Cor ou Raça -----------------------------------------------------------

### Gráficos por cor ou raça -----------------
## Neste gráficos haviam dados faltando, e estes serão destacados no gráfico
grafico_Contagem_Cor<-cor %>%
  ggplot(aes(x=Cor,
             y=Num,
             fill=Cor,
             group=Cor))+
  geom_col(position = position_dodge(width = 0)) +
  # ggrepel::geom_label_repel(stat="identity",
  #                           aes(label=sprintf("%s",
  #                                             format(Num,
  #                                                    big.mark = ".",
  #                                                    decimal.mark = ",")),
  #                                y=Num),
  #                           size=tamanho, 
  #                           show.legend = F, 
  #                           box.padding = unit(x = 0,
  #                                              units = "pt")) +
  theme(axis.text.x = element_blank(),
         
        legend.title = element_blank(),
        axis.title=element_blank())+
  ggtitle("Projetos pagos por cor ou raça em 2015")


grafico_Valores_Cor<-cor %>%
  ggplot(aes(x=Cor,
             y=Valor,
             fill=Cor,
             group=Cor))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  
  theme(axis.text.x = element_blank(),
        
        legend.title = element_blank(), 
        axis.title=element_blank())+
  geom_errorbar(mapping=aes(ymin=Valor-ValorSD,
                            ymax=Valor+ValorSD),
                width=0) +
  #### 
  # ggrepel::geom_label_repel(stat="identity",
  #                           aes(label=sprintf("R$ %s",
  #                                             format(Valor,
  #                                                    big.mark = ".",
  #                                                    decimal.mark = ",")),
  #                                y=Valor+ValorSD),
  #                           size=tamanho, 
  #                           show.legend = F,
  #                           box.padding = unit(x = 0, units = "pt")) +
  ggtitle("Valores pagos em reais por cor ou raça em 2015",ylab("Em reais"))

#Gráficos para empregos
grafico_Empregos_Cor<-cor %>%
  ggplot(aes(x=Cor,
             y=Empregos,
             fill=Cor,
             group=Cor))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  
  theme(axis.text.x = element_blank(),
        
        legend.title = element_blank(), 
        axis.title=element_blank())+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,
                            ymax=Empregos+EmpregosSD),
                width=0) +
  #### 
  # ggrepel::geom_label_repel(stat="identity", 
  #                           aes(label=sprintf("%s", 
  #                                             format(Empregos,
  #                                                    big.mark = ".", 
  #                                                    decimal.mark = ",")),
  #                                y=Empregos+EmpregosSD),
  #                           size=tamanho, 
  #                           show.legend = F, 
  #                           box.padding = unit(x = 0, 
  #                                              units = "pt")) +
  ggtitle("Empregos diretos gerados por cor em 2015")


#Gráfico de público estimado por cor
grafico_Publico_Cor<-cor %>%
  ggplot(aes(x=Cor,
             y=Publico,
             fill=Cor,
             group=Cor))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  
  theme(axis.text.x = element_blank(),
        
        legend.title = element_blank(),
        axis.title=element_blank())+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,
                            ymax=Publico+PublicoSD),
                width=0) +
  #### 
  # ggrepel::geom_label_repel(stat="identity",
  #                           aes(label=sprintf("%s",
  #                                             format(Publico,
  #                                                    big.mark = ".",
  #                                                    decimal.mark = ",")),
  #                                y=Publico+PublicoSD),
  #                           size=tamanho, 
  #                           show.legend = F, 
  #                           box.padding = unit(x = 0, 
  #                                              units = "pt")) +
  ggtitle("Público estimado por cor ou raça em 2015")




# Gráficos Sexo -----------------------------------------------------------

### Gráficos sexo
## Neste gráficos haviam dados faltando, e estes serão destacados no gráfico
grafico_Contagem_Sexo<-sexo %>%
  ggplot(aes(x=Sexo,
             y=Num,
             fill=Sexo,
             group=Sexo))+
  geom_col(position = position_dodge(width = 0)) +
  # ggrepel::geom_label_repel(stat="identity",
  #                           aes(label=sprintf("%s",
  #                                             format(Num, 
  #                                                    big.mark = ".", 
  #                                                    decimal.mark = ",")),
  #                                y=Num),
  #                           size=tamanho, 
  #                           show.legend = F,
  #                           box.padding = unit(x = 0,
  #                                              units = "pt")) +
  
  theme(axis.text.x = element_blank(),
         
        legend.title = element_blank(), 
        axis.title=element_blank())+
  ggtitle("Projetos pagos por gênero em 2015")



grafico_Valores_Sexo<-sexo %>%
  ggplot(aes(x=Sexo,
             y=Valor,
             fill=Sexo,
             group=Sexo))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  
  theme(axis.text.x = element_blank(),
        
        legend.title = element_blank(), 
        axis.title=element_blank())+
  geom_errorbar(mapping=aes(ymin=Valor-ValorSD,
                            ymax=Valor+ValorSD),
                width=0) +
  #### 
  # ggrepel::geom_label_repel(stat="identity",
  #                           aes(label=sprintf("R$ %s",
  #                                             format(Valor,
  #                                                    big.mark = ".", 
  #                                                    decimal.mark = ",")),
  #                                y=Valor+ValorSD),
  #                           size=tamanho, 
  #                           show.legend = F,
  #                           box.padding = unit(x = 0, 
  #                                              units = "pt")) +
  ggtitle("Valores pagos em reais por gênero em 2015",ylab("Em reais"))

#Gráficos para empregos
grafico_Empregos_Sexo<-sexo %>%
  ggplot(aes(x=Sexo,
             y=Empregos,
             fill=Sexo,
             group=Sexo))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  
  theme(axis.text.x = element_blank(),
         
        legend.title = element_blank(), 
        axis.title=element_blank())+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,
                            ymax=Empregos+EmpregosSD),
                width=0) +
  #### 
  # ggrepel::geom_label_repel(stat="identity", 
  #                           aes(label=sprintf("%s",
  #                                             format(Empregos,
  #                                                    big.mark = ".", 
  #                                                    decimal.mark = ",")),
  #                                y=Empregos+EmpregosSD),
  #                           size=tamanho, 
  #                           show.legend = F, 
  #                           box.padding = unit(x = 0, 
  #                                              units = "pt")) +
  ggtitle("Empregos diretos gerados por gênero em 2015")

#Gráfico de público estiamdo
grafico_Publico_Sexo<-sexo %>%
  ggplot(aes(x=Sexo,
             y=Publico,
             fill=Sexo,
             group=Sexo))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  
  theme(axis.text.x = element_blank(),
         
        legend.title = element_blank(), 
        axis.title=element_blank())+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,
                            ymax=Publico+PublicoSD),
                width=0) +
  #### 
  # ggrepel::geom_label_repel(stat="identity", 
  #                           aes(label=sprintf("%s", 
  #                                             format(Publico,
  #                                                    big.mark = ".",
  #                                                    decimal.mark = ",")),
  #                                y=Publico+PublicoSD),
  #                           size=tamanho, 
  #                           show.legend = F, 
  #                           box.padding = unit(x = 0, 
  #                                              units = "pt")) +
  ggtitle("Público estimado por gênero em 2015")


# Gráficos Cidade -----------------------------------------------------------

grafico_Contagem_RA_Proponente<-cidades %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>2) %>%
  ggplot(aes(x=reorder(RA_Proponente,Num),
             y=Num,
             fill=RA_Proponente,
             group=RA_Proponente))+
  geom_col(position = position_dodge(width = 0)) +
  # ggrepel::geom_label_repel(stat="identity",
  #                           aes(label=sprintf("%s", 
  #                                             format(Num, 
  #                                                    big.mark = ".", 
  #                                                    decimal.mark = ",")),
  #                                y=Num),
  #                           size=tamanho,
  #                           show.legend = F, 
  #                           box.padding = unit(x = 0, 
  #                                              units = "pt")) +
  
  theme( 
        legend.title = element_blank(),
        legend.position = "none")+
  ggtitle("Projetos pagos por RA em 2015")+
  coord_flip()



grafico_Valores_RA_Proponente<-cidades %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>2) %>%
  ggplot(aes(x=reorder(RA_Proponente,
                       Valor),
             y=Valor,fill=RA_Proponente,
             group=RA_Proponente))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  
  theme( 
        legend.title = element_blank(),
        legend.position = "none")+
  geom_errorbar(mapping=aes(ymin=Valor-ValorSD,
                            ymax=Valor+ValorSD),
                width=0) +
  #### 
  # ggrepel::geom_label_repel(stat="identity", 
  #                           aes(label=sprintf("R$ %s",
  #                                             format(Valor,
  #                                                    big.mark = ".", 
  #                                                    decimal.mark = ",")),
  #                                y=Valor+ValorSD),
  #                           size=tamanho, 
  #                           show.legend = F, 
  #                           box.padding = unit(x = 0, 
  #                                              units = "pt")) +
  ggtitle("Valores pagos em reais por RA do proponente em 2015",
          ylab("Em reais"))+
  coord_flip()

#Gráficos para empregos
grafico_Empregos_RA_Proponente<-cidades %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>2) %>%
  ggplot(aes(x=reorder(RA_Proponente,Empregos),
             y=Empregos,
             fill=RA_Proponente,
             group=RA_Proponente))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  
  theme(
        legend.title = element_blank(),
        legend.position = "none")+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,
                            ymax=Empregos+EmpregosSD),
                width=0) +
  #### 
  # ggrepel::geom_label_repel(stat="identity", 
  #                           aes(label=sprintf("%s", 
  #                                             format(Empregos,
  #                                                    big.mark = ".",
  #                                                    decimal.mark = ",")),
  #                                y=Empregos+EmpregosSD),
  #                           size=tamanho, 
  #                           show.legend = F, 
  #                           box.padding = unit(x = 0, 
  #                                              units = "pt")) +
  ggtitle("Empregos diretos gerados por RA do proponente em 2015")+
  coord_flip()

# Gráfico de público estimado
grafico_Publico_RA_Proponente<-cidades %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>2) %>%
  #Optou-se por colocar por ordem
  ggplot(aes(x=reorder(RA_Proponente,Publico),
             y=Publico,
             fill=RA_Proponente,
             group=RA_Proponente))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  
  theme(legend.title = element_blank(),
        legend.position = "none")+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,
                            ymax=Publico+PublicoSD),
                width=0) +
  #### 
  # ggrepel::geom_label_repel(stat="identity", 
  #                           aes(label=sprintf("%s",
  #                                             format(Publico,
  #                                                    big.mark = ".", 
  #                                                    decimal.mark = ",")),
  #                                y=Publico+PublicoSD),
  #                           size=tamanho, 
  #                           show.legend = F, 
  #                           box.padding = unit(x = 0,
  #                                              units = "pt")) +
  ggtitle("Público estimado por RA do proponente em 2015")+
  coord_flip()

# Gráficos Cidades Atingidas -----------------------------------------------------------

grafico_Contagem_RA_Atingidas<-cidades_atingidas %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>2) %>%
  ggplot(aes(x=reorder(Cidades, Num),
             y=Num,
             fill=Cidades,
             group=Cidades))+
  geom_col(position = position_dodge(width = 0)) +
  # ggrepel::geom_label_repel(stat="identity", 
  #                           aes(label=sprintf("%s",
  #                                             format(Num,
  #                                                    big.mark = ".",
  #                                                    decimal.mark = ",")),
  #                                y=Num),
  #                           size=tamanho, 
  #                           show.legend = F,
  #                           box.padding = unit(x = 0, 
  #                                              units = "pt")) +
  
  theme(axis.text.x = element_blank(),
        
        legend.title = element_blank(),
        legend.position = "none")+
  ggtitle("Número de vezes que uma RA do proponente foi alvo por um projeto")+
  coord_flip()


# Gráfico de escolaridade ----------------------------------------
## Neste gráficos haviam dados faltando, e estes serão destacados no gráfico
grafico_Contagem_Escolaridade<-escolaridade %>%
  filter(Num>2) %>%
  ggplot(aes(x=reorder(Escolaridade,Num),
             y=Num,
             fill=Escolaridade,
             group=Escolaridade))+
  geom_col(position = position_dodge(width = 0)) +
  # ggrepel::geom_label_repel(stat="identity",
  #                           aes(label=sprintf("%s",
  #                                             format(Num, 
  #                                                    big.mark = ".", 
  #                                                    decimal.mark = ",")),
  #                                y=Num),
  #                           size=tamanho, 
  #                           show.legend = F, 
  #                           box.padding = unit(x = 0, 
  #                                              units = "pt")) +
  
  theme( 
        legend.title = element_blank(),
        legend.position = "none")+
  coord_flip() +
  ggtitle("Projetos pagos por escolaridade em 2015")

grafico_Valores_Escolaridade<-escolaridade %>%
  filter(Num>2) %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  ggplot(aes(x=reorder(Escolaridade,Valor),
             y=Valor,
             fill=Escolaridade,
             group=Escolaridade))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  
  theme( 
        legend.title = element_blank(),
        legend.position = "none")+
  geom_errorbar(mapping=aes(ymin=Valor-ValorSD,
                            ymax=Valor+ValorSD),
                width=0) +
  #### 
  # ggrepel::geom_label_repel(stat="identity",
  #                           aes(label=sprintf("R$ %s",
  #                                             format(Valor,
  #                                                    big.mark = ".", 
  #                                                    decimal.mark = ",")),
  #                                y=Valor+ValorSD),
  #                           size=tamanho, 
  #                           show.legend = F,
  #                           box.padding = unit(x = 0, 
  #                                              units = "pt")) +
  ggtitle("Valores pagos em reais por escolaridade em 2015",
          xlab("Em reais"))+
  coord_flip()

#Gráficos para empregos
grafico_Empregos_Escolaridade<-escolaridade %>%
  filter(Num>2) %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  ggplot(aes(x=reorder(Escolaridade,Empregos),
             y=Empregos,
             fill=Escolaridade,
             group=Escolaridade))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  theme(legend.position = "none",
        
        legend.title = element_blank(), 
        axis.title=element_blank())+
  geom_errorbar(mapping=aes(ymin=Empregos-EmpregosSD,
                            ymax=Empregos+EmpregosSD),
                width=0) +
  #### 
  # ggrepel::geom_label_repel(stat="identity", 
  #                           aes(label=sprintf("%s",
  #                                             format(Empregos,
  #                                                    big.mark = ".", 
  #                                                    decimal.mark = ",")),
  #                                y=Empregos+EmpregosSD),
  #                           size=tamanho, 
  #                           show.legend = F, 
  #                           box.padding = unit(x = 0, 
  #                                              units = "pt")) +
  ggtitle("Empregos diretos gerados por escolaridade em 2015")+
  coord_flip()

# Gráfico de público estimado
grafico_Publico_Escolaridade<-escolaridade %>%
  filter(Num>2) %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  ggplot(aes(x=reorder(Escolaridade,Publico),
             y=Publico,
             fill=Escolaridade,
             group=Escolaridade))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  
  #Tirou-se a legenda dos projetos e optou-se apenas por deixar o rótulo y de cada escolaridade
  theme(legend.position = "none",
        
        legend.title = element_blank(),
        axis.title=element_blank())+
  geom_errorbar(mapping=aes(ymin=Publico-PublicoSD,
                            ymax=Publico+PublicoSD),
                width=0) +
  # ggrepel::geom_label_repel(stat="identity", 
  #                           aes(label=sprintf("%s", format(Publico,
  #                                                          big.mark = ".",
  #                                                          decimal.mark = ",")),
  #                                y=Publico+PublicoSD),
  #                           size=tamanho, 
  #                           show.legend = F, 
  #                           box.padding = unit(x = 0, units = "pt")) +
  ggtitle("Público estimado por escolaridade em 2015")+
  coord_flip()


#### Contagem RA_Atingida
grafico_Contagem_RA_Atingida<-cidades_atingidas %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>2) %>%
  ggplot(aes(x=reorder(Cidades,Num),
             y=Num,
             fill=Cidades,
             group=Cidades))+
  geom_col(position = position_dodge(width = 0)) +
  # ggrepel::geom_label_repel(stat="identity", 
  #                           aes(label=sprintf("%s", format(Num,
  #                                                          big.mark = ".",
  #                                                          decimal.mark = ",")),
  #                                y=Num),
  #                           size=tamanho, 
  #                           show.legend = F, 
  #                           box.padding = unit(x = 0, units = "pt")) +
  
  theme(
        legend.title = element_blank(),
        legend.position = "none",
        axis.title = element_blank())+
  ggtitle("Projetos pagos por RA atingida em 2015")+
  coord_flip()



grafico_Valores_RA_Atingida<-cidades_atingidas %>%
  #optei por colocar apenas RAs com mais de um projeto executado, para deixar o gráfico mais limpo
  filter(Num>2) %>%
  ggplot(aes(x=reorder(Cidades,ValoresMedios),
             y=ValoresMedios,
             fill=Cidades))+
  #Gráfico de colunas
  geom_col(position = position_dodge(width = 0)) +
  theme(
        legend.title = element_blank(),
        legend.position = "none", 
        axis.title = element_blank())+
  # ggrepel::geom_label_repel(#stat="identity",
  #   aes(label=ifelse(ValoresMedios>100000,
  #                    yes=prettyNum(x=ValoresMedios, 
  #                              big.mark = ".", 
  #                              decimal.mark = ","), 
  #                    no=NA)),
  #   #nudge_y = cidades_atingidas$ValoresMedios,
  #   size=tamanho, #y=ValoresMedios),
  #   show.legend = F,
  #   direction = "x",
  #   nudge_y = 25000,
  #   box.padding = unit(x = 0, units = "pt"),
  #   label.padding = unit(x = 1,units = "pt"),
  #   label.size = 0.1,
  #   na.rm = T) +
  ggtitle("Valores médios pagos em reais por RA atingida em 2015",
          ylab("Em reais"))+
  coord_flip()


# graficos salvos ------------------------------


graficos_todos<-c("grafico_Valores_Linguagem","grafico_Contagem_Linguagem","grafico_Empregos_Linguagem",
                  "grafico_Publico_Linguagem","grafico_Valores_Natureza","grafico_Contagem_Natureza",
                  "grafico_Empregos_Natureza","grafico_Publico_Natureza","grafico_Valores_Cor","grafico_Contagem_Cor",
                  "grafico_Empregos_Cor","grafico_Publico_Cor","grafico_Valores_Sexo","grafico_Contagem_Sexo",
                  "grafico_Empregos_Sexo","grafico_Publico_Sexo","grafico_Valores_Escolaridade",
                  "grafico_Contagem_Escolaridade","grafico_Empregos_Escolaridade","grafico_Publico_Escolaridade",
                  "grafico_Valores_RA_Proponente","grafico_Contagem_RA_Proponente","grafico_Empregos_RA_Proponente",
                  "grafico_Publico_RA_Proponente","grafico_Valores_RA_Atingida","grafico_Contagem_RA_Atingida")



# Vwr graficos---------------------------

#gráficos para linguagem
grafico_Valores_Linguagem
grafico_Contagem_Linguagem
grafico_Empregos_Linguagem
grafico_Publico_Linguagem

#graficos RA atingidas
grafico_Valores_RA_Atingida
grafico_Contagem_RA_Atingida

#graficos cor ou raça
grafico_Contagem_Cor
grafico_Valores_Cor
grafico_Publico_Cor
grafico_Empregos_Cor


#graficos escolaridade
grafico_Contagem_Escolaridade
grafico_Valores_Escolaridade
grafico_Publico_Escolaridade
grafico_Empregos_Escolaridade

#graficos RA proponentes
grafico_Contagem_RA_Proponente
grafico_Valores_RA_Proponente
grafico_Publico_RA_Proponente
grafico_Empregos_RA_Proponente

#grafico sexo
grafico_Contagem_Sexo
grafico_Valores_Sexo
grafico_Publico_Sexo
grafico_Empregos_Sexo

#grafico natureza
grafico_Contagem_Natureza
grafico_Valores_Natureza
grafico_Empregos_Natureza
grafico_Publico_Natureza




# Salva os dados em RDS -------------------
save(list = graficos_todos,file = "graficos_dados.RData")

rm(list=graficos_todos)


