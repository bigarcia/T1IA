

cidades <- c("A", "B", "C", "D","F", "G", "L", "M","O", "P", "R", "S", "T", "U", "Z") #Cidades de romenia, conforme exemplo dado em aula
## Ignorou-se as cidades E,H,I,N,V do mapa de Romenia,
## uma vez que não é possível partir de A e chegar em B pela primeira vez
## passando por essas cidades

distanciareta<- function(){
  ##Distância em linha reta de todas as cidades até a cidade B
  distancias <- matrix(c(366,0,160,242,178,77,244,241,380,98,193,253,329,80,374),
                       nrow=15,
                       ncol=1,
                       byrow= FALSE)
  rownames(distancias) <- cidades
  colnames(distancias) <-"Distãncia em linha reta até B"
  return(distancias)
}
#print(distanciareta())

#matriz que mapeia a distancia em km entre as cidades
distanciakm <- function() {
  pesos<- matrix(c(
    0,0,0,0,0,0,0,0,0,0,0,140,118,0,75,  #Distancia entre A e as outras cidades
    0,0,0,0,211,90,0,0,0,101,0,0,0,85,0, #Distancia entre B e as outras cidades
    0,0,0,120,0,0,0,0,0,138,146,0,0,0,0, #Distancia entre C e as outras cidades
    0,0,120,0,0,0,0,75,0,0,0,0,0,0,0,    #Distancia entre D e as outras cidades
    0,211,0,0,0,0,0,0,0,0,0,99,0,0,0,    #Distancia entre F e as outras cidades
    0,90,0,0,0,0,0,0,0,0,0,0,0,0,0,      #Distancia entre G e as outras cidades
    0,0,0,0,0,0,0,70,0,0,0,0,111,0,0,    #Distancia entre L e as outras cidades
    0,0,0,75,0,0,70,0,0,0,0,0,0,0,0,     #Distancia entre M e as outras cidades
    0,0,0,0,0,0,0,0,0,0,0,151,0,0,71,    #Distancia entre O e as outras cidades
    0,101,138,0,0,0,0,0,0,0,97,0,0,0,0,  #Distancia entre P e as outras cidades
    0,0,146,0,0,0,0,0,0,97,0,80,0,0,0,   #Distancia entre R e as outras cidades
    140,0,0,0,99,0,0,0,151,0,80,0,0,0,0, #Distancia entre S e as outras cidades
    118,0,0,0,0,0,111,0,0,0,0,0,0,0,0,   #Distancia entre T e as outras cidades
    0,0,85,0,0,0,0,0,0,0,0,0,0,0,0,      #Distancia entre U e as outras cidades
    75,0,0,0,0,0,0,0,71,0,0,0,0,0,0),    #Distancia entre Z e as outras cidades
    nrow=15,                             #15 cidades
    ncol=15,                             #15 cidades
    byrow = TRUE
    )
  rownames(pesos) <- cidades
  colnames(pesos) <- cidades 
  return(pesos)
}
#print(distanciakm())

debugSource("Cidade.R")
debugSource("buscaInformada.R")

### Cidade inicial
inicial <- Cidade(desc= "A", custo = distanciakm())

### Cidade objetivo
objetivo <- Cidade(desc= "B", custo = distanciakm())



cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))


##Será utilizado o algoritmo A*, onde é utilizado a função de custo e heurística. 
cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))