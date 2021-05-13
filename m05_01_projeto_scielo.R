library(tidyverse)
library(igraph)

# baixe o arquivo do link abaixo:
# https://static.scielo.org/tabs/tabs_bra.zip
# descompacte o arquivo zipado e mude o endereço abaixo para onde está...
# o arquivo document_authors.csv
df <- read_csv("documents_authors.csv")
glimpse(df)

# extrair colunas de interesse

# remover espacos dos nomes
names(df) <- str_replace_all(names(df), " ", "_")

# montar dataframe de arestas que representam colaboracoes entre universidades
df <- df %>% 
  select(
    doc_id = `document_publishing_ID_(PID_SciELO)`,
    doc_uni = document_author_institution,
    doc_ano = document_publishing_year
  ) %>% 
  filter(!is.na(doc_uni) & doc_uni != "") %>% 
  # manter anos mais recentes
  filter(between(doc_ano, 1990, 2017))
  
df_arestas <- inner_join(df, df, by = c("doc_id", "doc_ano"))
# mudar posicao das colunas
df_arestas <- df_arestas %>% select(doc_uni.x, doc_uni.y, doc_id, doc_ano)
head(df_arestas)  


### remover redundancias
nrow(df_arestas)
# linhas onde uni.x == uni.y
df_arestas <- df_arestas %>% filter(doc_uni.x != doc_uni.y)
# remover duplicatas
df_arestas <- df_arestas %>%
  unique()

# redundancia 3: mais de uma linha para cada par de universidades
df_arestas_cnt <- df_arestas %>% 
  ungroup() %>% 
  count(doc_uni.x, doc_uni.y, sort = TRUE)

# redundancia 4: duplicatas
# ex: para um ano, exista uma linha onde uni.x = A e uni.y = B...
# e outra linha onde uni.x = B e uni.y = A
system.time({
  df_arestas_cnt <- df_arestas_cnt %>% 
    rowwise() %>% 
    mutate(concat = paste0(sort(c(doc_uni.x, doc_uni.y)), collapse = "_")) %>% 
    distinct(concat, .keep_all = TRUE) %>% 
    select(-concat)
  #select(doc_id, concat) %>% 
})

# ver resultado
df_arestas_cnt
# analisar distribuicao da frequencia de colaboracoes
summary(df_arestas_cnt$n)
# analisar percentis
quantile(df_arestas_cnt$n, seq(0, 1, 0.01))
lim_inf <- quantile(df_arestas_cnt$n, prob = 0.99)
# filtrar os 5% pares de colaboracoes mais comuns
df_arestas_cnt_reduzido <- df_arestas_cnt %>% filter(n > lim_inf)

g_reduzido <- graph_from_data_frame(df_arestas_cnt_reduzido, directed = FALSE)

plot(g_reduzido, vertex.label = NA, vertex.size = 2, vertex.color = "red",
     edge.width = 0.5, edge.color = "gray", edge.lty = "dotted")

#### construimos um grafo com a rede reduzida apenas para plotar
# agora vamos construir um grafo com o dataframe inteiro

g_completo <- graph_from_data_frame(df_arestas_cnt, directed = FALSE)
# adicionar atributo de peso do edge
E(g_completo)$weight <- E(g_completo)$n

# salvar plot em pdf
# system.time({
#   pdf("grafo-unis-colab.pdf")
#   plot(g_completo,
#        vertex.label = NA,
#        vertex.size = 2,
#        vertex.color = "red",
#        edge.width = 0.5,
#        edge.color = "gray",
#        edge.lty = "dotted",
#        main = "Rede de colaboração entre universidades")
#   dev.off()
# })


#### analise das propriedades da rede ####
igraph::graph.density(g_reduzido)
igraph::cohesion(g_reduzido)
#igraph::mean_distance(g_completo) # pode demorar bastante
#igraph::diameter(g_completo) # nao faz sentido pq a rede nao é toda conectada (tem buracos)
igraph::transitivity(g_reduzido)

#### analise dos vertices ####
igraph::degree(g_reduzido) %>% sort() %>% tail(10) # top 10
igraph::betweenness(g_reduzido) %>% sort() %>% tail(10) # top 10
igraph::closeness(g_reduzido) %>% sort() %>% tail(10) # top 10


#### como essas propriedades mudaram ao longo do tempo ? ####
# voltando ao nosso dataframe, vamos criar uma coluna de artigos por ano
df_arestas
df_arestas_cnt_ano <- df_arestas %>% 
  count(doc_uni.x, doc_uni.y, doc_ano) %>% 
  group_by(doc_ano) %>% 
  # filtrar percentil por ano
  filter(n >= quantile(n, prob = 0.70)) %>% 
  # remover redundancias
  rowwise() %>% 
  mutate(concat = paste0(sort(c(doc_uni.x, doc_uni.y)), collapse = "_")) %>% 
  distinct(concat, .keep_all = TRUE) %>% 
  select(-concat)


lst_df_por_ano <- split(df_arestas_cnt_ano, df_arestas_cnt_ano$doc_ano)
lapply(lst_df_por_ano, nrow) # conferir split
# para cada dataframe presente na lista, construir um grafo
lst_grafo_por_ano <- lapply(lst_df_por_ano, graph_from_data_frame, directed = FALSE)
# para cada grafo na lista, extrair metricas da rede
dens_ano <- unlist(lapply(lst_grafo_por_ano, graph.density))
dens_ano
sort(dens_ano)
plot(dens_ano, xaxt = "n", type = "l")
axis(1, at = seq_along(dens_ano), labels = names(dens_ano))

# com o ggplot2
data.frame(ano = as.numeric(names(dens_ano)), densidade = dens_ano) %>% 
  ggplot(aes(x = ano, y = densidade)) + 
    geom_line() + geom_point() + 
    labs(x = NULL, y = 'Densidade da rede (%)') + 
    scale_x_continuous(breaks = seq(1990, 2017, 2))

# plotar rede com maior densidade vs com menor
# construir funcao para automatizar processo
plotar_grafo <- function(grafo, titulo){
  plot(grafo, vertex.label = NA, vertex.size = 2, vertex.color = "red",
       edge.width = 0.5, edge.color = "gray", edge.lty = "dotted",
       main = titulo)
}

par(mfrow = c(1, 2))
plotar_grafo(lst_grafo_por_ano$`2017`, '2017')
plotar_grafo(lst_grafo_por_ano$`1990`, '1990')
dev.off()

## a importancia da USP  ao longo dos anos
norm_betweenness_usp <- function(grafo) {
  out <- betweenness(grafo, normalized = TRUE)
  out <- out["Universidade de São Paulo"]
  return(unname(out))
}

# exemplo para um ano:
norm_betweenness_usp(lst_grafo_por_ano$`1990`)
# aplicar em todos os anos
btn_usp <- unlist(lapply(lst_grafo_por_ano, norm_betweenness_usp))
btn_usp

# plotar grafico de linha
data.frame(ano = as.numeric(names(btn_usp)), y = btn_usp) %>% 
  ggplot(aes(x = ano, y =   y)) + 
  geom_line() + geom_point() + 
  labs(x = NULL, y = 'Betweenness',
       title = "Centralidade da USP na produção acadêmica") + 
  scale_x_continuous(breaks = seq(1990, 2017, 2))

# outras possiveis analises dependeriam de mais dados,
# como analise da assortatividade


