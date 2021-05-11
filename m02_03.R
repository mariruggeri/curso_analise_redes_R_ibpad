## Módulo 02 
## Matriz de adjacência

library(igraph)
library(igraphdata)

# Exemplo de matriz de adjacencia
data("karate")
plot(karate)

# transformar grafo em matriz de adjacência
karate
igraph::as_adjacency_matrix(karate)
m <- as.matrix(as_adjacency_matrix(karate))

graph_from_adjacency_matrix()

# Exemplo de matriz de adjacênciaL análise de matriz de correção
# 

data(mtcars)

?mtcars

head(mtcars)
mat_cor <- cor(mtcars)
mat_cor

# remover correlações não-significantes 
mat_cor[abs(mat_cor) < 0.4] <- 0
mat_cor

# criar grafo a partir de matriz de adjacencia
g <- graph_from_adjacency_matrix(mat_cor,
                                 weighted = TRUE, 
                                 mode = "undirected",
                                 diag = FALSE)
g

# plotar grafo
set.seed(123)
plot(g)
l <- layout_in_circle(g)
plot(g, layout = 1)

