#Script adaptado de @mercelsantos
#https://www.mercelsantos.com/ 

rm(list = ls())       #Remove todos os objetos da memória
library('readxl')     # para ler arquivos do excel
library(dplyr)        # para pocessar dados
library(ggplot2)      # para plotar os dados
library(geobr)        # Para acessar os dados do IBGE
library(lubridate)   # Para manipulação de datas
library(viridis)      #para usar a paleta de cores Magma

#dados do covid:
#https://covid.saude.gov.br/

setwd("~/Desktop/analises-covid")

estados  <- read_state(code_state = "all")

#para fazer dos municípios de um estado:
#municipio <- read_municipality(code_muni = "PR") %>% 
#  mutate(codmun=substr(as.character(code_muni),1,6))

#carregando os dados
estrut.dados <- c(rep('text',7),'date',rep('text',9))
covid.dados <- read_excel("HIST_PAINEL_COVIDBR_12jul2020.xlsx", 
                          sheet = 1,col_types = estrut.dados) %>% #Leitura do arquivo
  dplyr::mutate(casosAcumulado=as.numeric(casosAcumulado), # Convertendo as classes dos dados
                data=as.Date(data)) %>%  
  dplyr::select(data,estado,casosAcumulado) %>% # Selecionando colunas de dados
  as.data.frame() %>% rename(abbrev_state=estado) #renomeia coluna dos estados pra ficar igual aos dados do IBGE

#definindo categorias dos dados
meus.breaks <- c(-Inf,0,100,500,1000,5000,10000,50000,100000, 200000,+Inf)
meus.labels <- c('0','1 a 100','101 a 500','501 a mil','1.001 a 5 mil','5.001 a 10 mil','10.001 a 50 mil','50.001 a 100 mil','100.001 a 200 mil','>200 mil')
covid.dados$categoria <- cut(covid.dados$casosAcumulado,breaks=meus.breaks,
                             labels = meus.labels)

#definindo paleta de cores
minha.paleta <- colorRampPalette(c('#ffffff', rev(magma(9))))

#organizando as datas
datas <- covid.dados$data %>% unique() 
inicio <- sort(datas)[1]
datas.org <- inicio + days(0:(length(datas)-1))

#gerando as imagens dos mapas
for(i in seq(1, length(datas))){
  print(i)
    
  #gerando os mapas
  data.atual=datas.org[i]
  covid.atual <- filter(covid.dados, data==data.atual)
  juntos <- full_join(estados, covid.atual, by="abbrev_state")
  
  #Criando arquivo de imagem 
  caminho<-paste("./mapas/casos-dia-",i, sep="")
  caminho<-paste(caminho,".png", sep="")
  png(caminho, width = 600, height = 600)
  
  #Plotando os dados 
  plot <- ggplot(juntos)+
    geom_sf(aes(fill=categoria))+
    scale_fill_manual(values = rev(minha.paleta(12))[-(10:11)],   #Customizando a paleta de cores
                      limits = rev(meus.labels))+
    theme_bw()+
    labs(fill="Casos\nAcumulados", #Definindo Títulos e Legendas
         x=NULL,
         y=NULL,
         title = paste0("Total de casos em ",
                        day(data.atual),"/",
                        month(data.atual),"/",
                        year(data.atual)))+
    theme(panel.grid = element_blank(), # Removendo as grades
          panel.border = element_blank(), # Removendo a borda
          panel.background = element_rect(fill = '#FFFFFF'), # Definindo cor de fundo do painel 
          plot.title = element_text(size = 20, face="bold"), #formatação do título
          plot.margin =  margin(t=.4, 0, b=.4, 0, "cm"),     # Definindo as Margens
          axis.text = element_blank(), #Removendo o texto dos eixos
          axis.ticks = element_blank(),#Removendo os Eixos
          legend.position = c(.15,.23),#Definindo a posição da legenda
          legend.key.size = unit(.6,'line'),#Definindo o tamanho da legenda
          legend.text = element_text(size = 12), #Definindo o tamanho do texto da legenda
          legend.title = element_text(size = 13), #Definindo o tamanho do título da legenda
          legend.background = element_blank(), #Definindo a cor do fundo da legenda
          plot.background = element_rect(fill = '#FFFFFF')) #Definindo a cor do fundo do gráfico
  print(plot)
  
  # Limpando plot
  dev.off()
}

#crf - qualidade do vídeo, 0 é a melhor qualidade, 51 a pior
#-y sobrescrever vídeos já salvos sem perguntar
rstudioapi::terminalExecute("ffmpeg -framerate 5 -i ./mapas/casos-dia-%000d.png -crf 1 -y ./casos.avi")

#usando o pacote animation
library(animation)   
saveGIF(ani.height=600,ani.width=600, #Salvando a animação como GIF
        ani.res=120,interval=.10,{
          
          #for 
        })