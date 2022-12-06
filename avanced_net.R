##################################################################
############ Esempio più avanzato di Network
##################################################################

##################################################################
############ Importa i pacchetti
##################################################################

library(readr)
library(igraph)
library(minet)

##################################################################
############ File in input
##################################################################

expr = read_csv("https://raw.githubusercontent.com/Luigi-Ferraro/Lezione_network/main/networks_input/expr.csv")

##################################################################
############ Coespressione
##################################################################

############ Avendo una matrice di geni e samples
############ la coespressione rappresenta quanto 
############ due pattern di espressione tra i vari samples
############ siano simili
############ Viene calcolata come correlazione per ogni coppia di geni
############ ATTENZIONE: la correlazione è interessante se lontana da 0
############ sia se positiva che negativa! 
############ Noi la prendiamo in valore assoluto

coexpr = abs(cor(expr, use = "pairwise.complete.obs"))

###### trasformiamo la matrice di correlazione in una matrice di adiacenza
###### applicando un threshold
Ad = coexpr
threshold = 0.7

Ad[Ad >= threshold] = 1
Ad[Ad < threshold] = 0

###### Non siamo interessati ad avere loop
###### Quindi impostiamo la diagonale a 0
diag(Ad) = 0

Gr = graph.adjacency(Ad, mode = "undirected", add.colnames = NULL, diag = FALSE)

###### Eliminiamo tutti i nodi che sono isolati
de = degree(Gr, loops = FALSE)
Ad = Ad[which(de > 0), which(de > 0)]
net_coexpr = graph.adjacency(Ad, mode = "undirected", add.colnames = NULL, diag = FALSE)

plot(net_coexpr, vertex.label = NA, edge.width = 5, vertex.size = 5)

######################
############ Proprietà
######################
net = net_coexpr

prop_coexpr = list(edge_density = edge_density(net, loops=F),
                   transitivity = transitivity(net),
                   diameter = diameter(net),
                   centr_degree = centr_degree(net, mode="in", normalized=T)$centralization,
                   centr_clo = centr_clo(net, mode="all", normalize=T)$centralization,
                   centr_betw = centr_betw(net, directed=T, normalized=T)$centralization,
                   mean_distance = mean_distance(net))


hist(degree(net), breaks=1:vcount(net)-1)



vcol <- rep("grey80", vcount(net))
vcol[unlist(largest_cliques(net))] <- "gold"
plot(net, vertex.label=NA, vertex.color=vcol,
     edge.width = 5, vertex.size = 5)


ceb <- cluster_edge_betweenness(net) 
dendPlot(ceb, mode="hclust")
plot(ceb, net, vertex.label=NA, edge.width = 5, vertex.size = 5) 

clp <- cluster_label_prop(net)
plot(clp, net, vertex.label=NA, edge.width = 5, vertex.size = 5)

##################################################################
############ Mutua informazione
##################################################################

###### La mutua informazione rappresenta quanto informazione
###### si può ottenere da una variabile casuale osservando 
###### un'altra variabile casuale 

presimil = build.mim(expr, estimator = "mi.shrink", disc = "globalequalwidth")
simil = sqrt(1 - exp(-2 * presimil))
simil[which(is.na(simil))] = 0

###### Similmente a prima, costruiamo il grafo
Ad = simil
threshold = 0.7

Ad[Ad >= threshold] = 1
Ad[Ad < threshold] = 0

diag(Ad) = 0

Gr = graph.adjacency(Ad, mode = "undirected", add.colnames = NULL, diag = FALSE)

de = degree(Gr,loops = FALSE)
Ad = Ad[which(de > 0), which(de > 0)]
net_mutinf = graph.adjacency(Ad, mode = "undirected", add.colnames = NULL, diag = FALSE)


plot(net_mutinf, vertex.label = NA, edge.width = 5, vertex.size = 5)



######################
############ Proprietà
######################
net = net_mutinf

prop_mutinf = list(edge_density = edge_density(net, loops=F),
                   transitivity = transitivity(net),
                   diameter = diameter(net),
                   centr_degree = centr_degree(net, mode="in", normalized=T)$centralization,
                   centr_clo = centr_clo(net, mode="all", normalize=T)$centralization,
                   centr_betw = centr_betw(net, directed=T, normalized=T)$centralization,
                   mean_distance = mean_distance(net))


hist(degree(net), breaks=1:vcount(net)-1)



vcol <- rep("grey80", vcount(net))
vcol[unlist(largest_cliques(net))] <- "gold"
plot(net, vertex.label=NA, vertex.color=vcol,
     edge.width = 5, vertex.size = 5)


ceb <- cluster_edge_betweenness(net) 
dendPlot(ceb, mode="hclust")
plot(ceb, net, vertex.label=NA, edge.width = 5, vertex.size = 5) 

clp <- cluster_label_prop(net)
plot(clp, net, vertex.label=NA, edge.width = 5, vertex.size = 5)




inter = graph.intersection(net_coexpr, net_mutinf, keep.all.vertices = F)
plot(inter, vertex.label = NA, edge.width = 5, vertex.size = 5)


##################################################################
############ ARACNE
##################################################################

############ ARACNE fa uso della mutua informazione
############ ma è un algoritmo più stringente
###### Sfruttando la mutua informazione calcolata prima
###### applichiamo aracne
Ad = aracne(simil)


###### Similmente a prima, costruiamo il grafo
threshold = 0.7

Ad[Ad >= threshold] = 1
Ad[Ad < threshold] = 0

diag(Ad) = 0

Gr = graph.adjacency(Ad, mode = "undirected", add.colnames = NULL, diag = FALSE)

de = degree(Gr,loops = FALSE)
Ad = Ad[which(de > 0), which(de > 0)]
net_aracne = graph.adjacency(Ad, mode = "undirected", add.colnames = NULL, diag = FALSE)

plot(net_aracne, vertex.label = NA, edge.width = 5, vertex.size = 5)



proprieta = data.frame(rbind(prop_coexpr, prop_mutinf))
proprieta
######################
############ Proprietà
######################
net = net_aracne


prop_aracne = list(edge_density = edge_density(net, loops=F),
                   transitivity = transitivity(net),
                   diameter = diameter(net),
                   centr_degree = centr_degree(net, mode="in", normalized=T)$centralization,
                   centr_clo = centr_clo(net, mode="all", normalize=T)$centralization,
                   centr_betw = centr_betw(net, directed=T, normalized=T)$centralization,
                   mean_distance = mean_distance(net))


hist(degree(net), breaks=1:vcount(net)-1)



vcol <- rep("grey80", vcount(net))
vcol[unlist(largest_cliques(net))] <- "gold"
plot(net, vertex.label=NA, vertex.color=vcol,
     edge.width = 5, vertex.size = 5)


ceb <- cluster_edge_betweenness(net) 
dendPlot(ceb, mode="hclust")
plot(ceb, net, vertex.label=NA, edge.width = 5, vertex.size = 5) 

clp <- cluster_label_prop(net)
plot(clp, net, vertex.label=NA, edge.width = 5, vertex.size = 5)



proprieta = data.frame(rbind(prop_coexpr, prop_mutinf, prop_aracne))
proprieta




inter = graph.intersection(net_aracne, net_mutinf, keep.all.vertices = F)
plot(inter, vertex.label = NA, edge.width = 5, vertex.size = 5)

inter = graph.intersection(net_aracne, net_coexpr, keep.all.vertices = F)
plot(inter, vertex.label = NA, edge.width = 5, vertex.size = 5)
