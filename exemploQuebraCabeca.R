debugSource("QuebraCabeca.R")
debugSource("buscaInformada.R")

### Matriz inicial
inicial = matrix(
  c(2, 8, 3, 1, 6, 4, 7, 0, 5),
  nrow = 3,
  ncol = 3,
  byrow = TRUE
)
print(inicial)
### Matriz objetivo
final = matrix(
  c(1, 2, 3, 8, 0, 4, 7, 6, 5),
  nrow = 3,
  ncol = 3,
  byrow = TRUE
)
print(final)
inicial <- QuebraCabeca(desc = inicial)

objetivo <- QuebraCabeca()
objetivo$desc <- final

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))
 
cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))