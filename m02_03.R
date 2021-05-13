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
plot(g, layout = l)

# detacar correlações negativas e positivas
E(g)$sinal_correl <- ifelse(E(g)$weight < 0, "red", "blue")
g
E(g)$sinal_correl

plot(g, layout = l, edge.color = E(g)$sinal_correl)


# destacar arestas de acordo coma força da correção

plot(g, layout = l, edge.color = E(g)$sinal_correl,
     edge.width = abs(E(g)$weight)*5)
