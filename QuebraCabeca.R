source("Estado.R")

## Classe e métodos para o problema do QuebraCabeca
QuebraCabeca <- function(desc = NULL, pai = NULL) {

  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("QuebraCabeca", "Estado")
  
  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.QuebraCabeca = function(obj1, obj2) {
  if (.Generic == "==") {
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica "print" do R
print.QuebraCabeca <- function(obj) {
  print(obj$desc)
  cat("G(n): ", obj$g)
  cat("   H(n): ", obj$h)
  cat("   F(n): ", obj$f)
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.QuebraCabeca <- function(atual) {
  if (is.null(atual$desc))
    return(Inf)
  ## h(obj) = Soma da distância de Manhattan
  matriz = atual$desc
  
  objetivo = matrix(
    c(1, 2, 3, 8, 0, 4, 7, 6, 5),
    nrow = 3,
    ncol = 3,
    byrow = TRUE
  )
  
  h = 0
  for (i in 1:nrow(matriz)) {
    for (j in 1:ncol(matriz)) {
      valor = matriz[i, j]
      pos1 = c(row = i, col = j)
      pos2 = which(objetivo == valor, arr.ind = T)
      h = h + abs(pos1[1] - pos2[1]) + abs(pos1[2] - pos2[2]) # Cálculo da distãncia
    }
  }
  return(h)
}

matrixCopy <- function(obj) {
  copy = matrix(data = NA,
                nrow = nrow(obj),
                ncol = ncol(obj))
  for (i in 1:nrow(obj)) {
    for (j in 1:ncol(obj)) {
      copy[i, j] = obj[i, j]
    }
  }
  return(copy)
}

geraFilhos.QuebraCabeca <- function(obj) {
  filhos <- list()
  filhosDesc <- list()
  desc <- obj$desc
  
  posZero = which(desc == 0, arr.ind = T)
  rowZero = posZero[1]
  colZero = posZero[2]
  
  ### Troca com o de cima
  if (rowZero > 1) {
    newDesc = matrixCopy(desc)
    newDesc[rowZero - 1, colZero] = desc[rowZero, colZero]
    newDesc[rowZero, colZero] = desc[rowZero - 1, colZero]
    filhosDesc = c(filhosDesc, list(newDesc))
  }
  ### Troca com o de baixo
  if (rowZero < 3) {
    newDesc = matrixCopy(desc)
    newDesc[rowZero + 1, colZero] = desc[rowZero, colZero]
    newDesc[rowZero, colZero] = desc[rowZero + 1, colZero]
    filhosDesc = c(filhosDesc, list(newDesc))
  }
  ### Troca com o da esquerda
  if (colZero > 1) {
    newDesc = matrixCopy(desc)
    newDesc[rowZero, colZero - 1] = desc[rowZero, colZero]
    newDesc[rowZero, colZero] = desc[rowZero, colZero - 1]
    filhosDesc = c(filhosDesc, list(newDesc))
  }
  ### Troca com o da direita
  if (colZero < 3) {
    newDesc = matrixCopy(desc)
    newDesc[rowZero, colZero + 1] = desc[rowZero, colZero]
    newDesc[rowZero, colZero] = desc[rowZero, colZero + 1]
    filhosDesc = c(filhosDesc, list(newDesc))
  }
  
  ## gera os objetos QuebraCabeca para os filhos
  for(filhoDesc in filhosDesc){
    filho <- QuebraCabeca(desc = filhoDesc, pai = obj)
    filho$h <- heuristica(filho)
    filho$g <- obj$g + 1
    filhos <- c(filhos, list(filho))
  }
  return(filhos)
}