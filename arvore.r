library(ggimage)
library(ggtree)

setwd("~/Desktop/analises-covid/bandeiras")

#carregando arvore
tree = read.tree("arvore-brasil.tree")

estados <- tree$tip.label

caminhoImagens<-paste(lapply(estados, tolower), '.png', sep = "")
info<- as.data.frame(cbind(estados, caminhoImagens))
colnames(info)<- c('estados', 'caminho')

#documentação:
#https://rdrr.io/bioc/ggtree/man/geom_tiplab.html
p=ggtree(tree,branch.length = "none", ) %<+% info + xlim(NA, 13) + ylim(NA, 27)
p + geom_tiplab(aes(image= caminho), geom="image", offset=0.5, align=T, size=.03, hjust=0,  asp = 1.6) +
  geom_tiplab() +geom_tiplab(geom="label")
#offset: distância da bandeira até a sigla    
#size: tamanho da imagem
#hjust: alinhamento horizontal da bandeira