source("Estado.R")

## Classe e métodos para o problema do trajeto entre duas cidades
Cidade <- function(desc = NULL, pai = NULL, custo = NULL) {

  e <- environment()
  
  assign("desc", desc, envir = e) #estado descendente
  assign("pai", pai, envir = e)   #estado pai
  assign("custo", custo, envir = e)
  assign("g", 0, envir = e)       #custo
  assign("h", Inf, envir = e)     #heurística
  assign("f", Inf, envir = e)     #função avaliativa
  
  class(e) <- c("Cidade", "Estado")
  
  return(e)
  
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.Cidade = function(obj1, obj2) {
  if (.Generic == "==") {
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica "print" do R
print.Cidade <- function(obj) {
  cat("Cidade: ",obj$desc,"\n")
  cat("G(n): ", obj$g)
  cat("   H(n): ", obj$h)
  cat("   F(n): ", obj$f)
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.Cidade <- function(atual) {
  if (is.null(atual$desc))
    return(Inf)
  ## h(obj) = distância em linha reta entre cada cidade e a cidade destino
  cidades <- c("A", "B", "C", "D","F", "G", "L", "M","O", "P", "R", "S", "T", "U", "Z") #Cidades de romenia, conforme exemplo dado em aula
  h <- matrix(c(366,0,160,242,178,77,244,241,380,98,193,253,329,80,374),nrow=15,ncol=1,byrow= FALSE)
  rownames(h) <- cidades
  colnames(h) <-"Distãncia em linha reta até B"
  return(h[atual$desc,1])
  
}



geraFilhos.Cidade <- function(obj) {
  filhosDesc <- list()
  filhos <- list()
  vizinhas <- obj$custo[obj$desc,]
  vizinhas <- vizinhas[vizinhas != 0]
  #print("vizinhas")
  #print(vizinhas)
  
  filhosDesc <- names(vizinhas)
  #arr.ind array indices be returned when x is an array
 
  ## gera os objetos Cidade para os filhos
  for(filhoDesc in filhosDesc){
    filho <- Cidade(desc = filhoDesc, pai = obj, custo = obj$custo)
    filho$h <- heuristica(filho)
    filho$g <- obj$g + vizinhas[filhoDesc] #custo do atual mais o custo da vizinha
    filho$f <- filho$h + filho$g
    filhos <- c(filhos, list(filho))
  }
  
  return(filhos)
}