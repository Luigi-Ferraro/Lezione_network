##################################################################
############ Network Analisys
##################################################################
# Based on https://kateto.net/networks-r-igraph

##################################################################
############ Importa i pacchetti
##################################################################

library(igraph)
library(readr)



##################################################################
############ Creazione di grafi semplici con igraph
##################################################################



############ Creazione di un GRAFO DIRETTO
############ Gli archi avranno una direzione (a b) != (b a)
###### Gli archi sono disposti a coppie di nodi
###### Se non si indica il numero di nodi, si assumerà che non esistano
###### nodi isolati
directed_graph = graph( edges = c(c(1,2), 
                                  c(2,3), 
                                  c(3,1)) )
###### Un plot semplice del grafo
###### Poi vedremo come renderlo complesso
plot(directed_graph)

directed_graph = graph( edges = c(c(1,2), 
                                  c(2,3), 
                                  c(3,1)), n = 10 )
plot(directed_graph)


############ Creazione di un GRAFO INDIRETTO
############ Gli archi non avranno una direzione (a b) == (b a)

undirected_graph = graph( edges = c(c(1,2), 
                                    c(2,3), 
                                    c(3,1)), directed=F  )
plot(undirected_graph)

############ Notare la classe dell'oggetto grafo
class(undirected_graph)


############ Possiamo creare un grafo usano dei vertici nominati
named_graph = graph( c(c("John", "Jim"),
                       c("Jim", "Jill"),
                       c("Jill", "John")) )
plot(named_graph)

###### Per creare nodi isolati in questo caso
###### c'è bisogno di un argomento aggiuntivo
###### passando una lista di nomi
named_graph = graph( c(c("John", "Jim"),
                       c("Jim", "Jack"),
                       c("Jim", "Jack"),
                       c("John", "John")),
                     isolates = c("Jesse", "Janis", "Jennifer", "Justin"))
plot(named_graph)


##################################################################
############ ARCHI E NODI di una network
##################################################################

############ Accesso agli archi e nodi
E(named_graph)
V(named_graph)

############ Accesso al grafo, MATRICE DI ADIACENZA
###### Gli 1 rappresentano l'esistenza di un arco tra i due nodi
###### Se la matrice di adiacenza riporta numeri diversi da 0 e 1
###### siamo in presenza di un GRAFO PESATO
named_graph[]
named_graph["John"]

###### Per ritornare la matrice di adiacenza come una matrice R
###### usare as.matrix()
as.matrix(named_graph[])



############ Attributi dei nodi
###### Possiamo aggiungere diversi attributi ai nodi
###### Lo stesso nome dei nodi risulta un atributo
V(named_graph)$name

V(named_graph)$capelli <- c("mori", "biondi", "mori", "mori", "biondi", "biondi", "mori")

vertex_attr(named_graph)

############ Attributi degli archi
###### Similmente agli archi
E(named_graph)$type <- "amico" 
###### Per aggiungere un peso all'arco
E(named_graph)$weight <- 10    

edge_attr(named_graph)



########### Descrizione di un oggetto igraph
named_graph

###### D o U per indicare se è un grafo diretto o indiretto
###### N per un grafo coi vertici nominati
###### W per un grafo pesato
###### B per un grafo bipartito (grafi con un attributo type assegnato ai nodi)
###### I numeri che seguono indicano il numero di nodi e di archi
###### attr rappresenta gli attributi dei nodi (v/c) e degli archi (e/n)



##################################################################
############ Semplici plot di network
##################################################################



plot(named_graph, edge.arrow.size=.1, 
     vertex.color="gold", vertex.size=15, 
     vertex.frame.color="gray", vertex.label.color="black", 
     vertex.label.cex=1.5, vertex.label.dist=2) 



plot(named_graph, edge.arrow.size=0.1, 
     vertex.label.color="black", 
     vertex.label.dist=1.5,
     vertex.color=c( "brown", "yellow")[1+(V(named_graph)$capelli=="biondi")] ) 


############ Loop e archi duplicati
###### In questo grafo possiamo notare un loop che va da John a se stesso
###### e due archi duplicati che vanno da Jim a Jack


############ Semplificazione del grafo
###### remove.multiple per rimuovere i duplicati
###### remove.loops per rimuovere i loop
###### edge.attr.comb indica cosa fare degli attributi dei doppioni
###### in questo caso diciamo di sommare i pesi 
###### di ignorare e quindi eliminare il tipo
named_graph_s <- simplify( named_graph, remove.multiple = T, remove.loops = F, 
                 edge.attr.comb=list(weight="sum", type="ignore") )

plot(named_graph_s, vertex.label.dist=1.5, edge.arrow.size=0.1)





##################################################################
############ Creazioni di network specifiche
##################################################################


############ Grafo vuoto
###### senza archi, solo nodi
eg <- make_empty_graph(40)
plot(eg, vertex.size=10, vertex.label=NA)

############ Grafo pieno, ovvero tutti i nodi solo collegati agli altri
###### tutti i nodi solo collegati agli altri
fg <- make_full_graph(40)
plot(fg, vertex.size=10, vertex.label=NA)

############ Grafo a stella 
###### un solo nodo è collegato a tutti gli altri nodi
st <- make_star(40)
plot(st, vertex.size=10, vertex.label=NA) 

############ Grafo ad albero
###### grafo con un nodo collegato a n figli, e poi ricorsivo
tr <- make_tree(40, children = 3, mode = "undirected")
plot(tr, vertex.size=10, vertex.label=NA) 

############ Grafo ad anello
###### ogni nodo è collegato a soli due altri nodi
###### in modo da creare una catena
rn <- make_ring(40)
plot(rn, vertex.size=10, vertex.label=NA)


############ Grafo randomico di Erdos-Renyi
###### n è il numero di nodi, m il numero di archi
er <- sample_gnm(n=100, m=40) 
plot(er, vertex.size=6, vertex.label=NA)  

############ Network small-world di Watts-Strogatz 
####### Genera una network con properietà di small world
####### come ad esempio average path length e high clustering
sw <- sample_smallworld(dim=2, size=10, nei=1, p=0.1)
plot(sw, vertex.size=6, vertex.label=NA, layout=layout_in_circle)
 
############  igraph contiene anche grafi molto usati
###### come ad esempio Zachary karate club
zach <- graph("Zachary") 
plot(zach, vertex.size=10, vertex.label=NA)


############ Combinazione di grafi
###### unione disgiunta: %du%
plot(rn, vertex.size=10, vertex.label=NA) 
plot(tr, vertex.size=10, vertex.label=NA) 
plot(rn %du% tr, vertex.size=10, vertex.label=NA) 

  
 
##################################################################
############ Creare network a partire da file
##################################################################
library(readr)

############ Lista di archi
#nodes <- read_csv("C:/Users/luigi/Desktop/Lezioni/nets/preliminary/Media_nodi.csv")
nodes <- read_csv("https://raw.githubusercontent.com/Luigi-Ferraro/Lezione_network/main/networks_input/Media_nodi.csv")
links <- read_csv("https://raw.githubusercontent.com/Luigi-Ferraro/Lezione_network/main/networks_input/Media_archi.csv")


############ Aggrega archi comuni
###### Simile a simplify, ma non vengono accorpati link di type diverso
###### I pesi in questo caso vengono sommati
links <- aggregate(links[,3], links[,-3], sum)

###### Riordiniamo il dataframe prima per la colonna from, e poi per to
links <- links[order(links$from, links$to),]
###### Reset dei row names
rownames(links) <- NULL


############ Matrice di adiacenza

nodes2 <- read_csv("https://raw.githubusercontent.com/Luigi-Ferraro/Lezione_network/main/networks_input/Media_User_nodi.csv")
links2 <- read_csv("https://raw.githubusercontent.com/Luigi-Ferraro/Lezione_network/main/networks_input/Media_User_archi.csv")
links2 <- as.matrix(links2)
rownames(links2) = links2[,1]
links2 = links2[,-1]


############ Converitire i file in network
###### graph.data.frame prende in input 
###### d: gli archi
###### vertices: i nodi
###### qualsiasi altra colonna è considerata un attributo
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 

class(net)
net 

E(net)
V(net)
E(net)$type
V(net)$media

plot(net, edge.arrow.size=.4,vertex.label=NA)

###### Semplifichiamo il grafo
net <- simplify(net, remove.multiple = F, remove.loops = T) 

###### Ricavare la lista degli archi o la matrice di adiacenza
as_edgelist(net, names=T)
as_adjacency_matrix(net, attr="weight") # equivalente a net[]

###### Ricavare dataframe che descrivono i nodi o gli archi
as_data_frame(net, what="edges")
as_data_frame(net, what="vertices")


############ Rifacciamo con l'altro dataset 
############ a partire dalla matrice di adiacenza

head(nodes2)
head(links2)

net2 <- graph_from_incidence_matrix(links2)
net2
###### type dice se è uno user o un media
table(V(net2)$type)

plot(net2,vertex.label=NA)


##################################################################
############ Plot di network
##################################################################


############ Le opzioni di plotting possono essere inserite in due modi
############ Un modo è inserire le informazioni in plot()
###### Plot con gli archi curvati e grandezza ridotta
plot(net, edge.arrow.size=.4, edge.curved=.1)

###### Imposta il colore dei nodi in arangio e il colore dei bordi in hex #555555
###### Invece degli id dei nodi, possiamo usare i nomi in "media"
plot(net, edge.arrow.size=.2, edge.curved=0,
     vertex.color="orange", vertex.frame.color="#555555",
     vertex.label=V(net)$media, vertex.label.color="black",
     vertex.label.cex=.7) 

############ Il secondo modo per inserire le informazioni 
############ è metterle direttamente nell'oggeto igraph

###### Genera i colori a partire dal tipo di media
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

###### Imposta la grandezza del nodo in base all'audience size
V(net)$size <- V(net)$audience.size*0.7

###### Le labels sono attualmente gli id dei nomi
###### Mettendoli a NA non verranno mostrati label
V(net)$label.color <- "black"
V(net)$label <- NA

###### Imposta lo spessore degli archi in base a weight 
E(net)$width <- E(net)$weight/6

###### Imposta il colore degli archi e la grandezza delle frecce
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"

plot(net) 

###### Possiamo sempre usare plot() per risettare i parametri entro il plot
plot(net, edge.color="orange", vertex.color="gray50") 

###### Possiamo aggiungere una leggenda che spieghi i colori che usiamo
plot(net) 
legend(x=-1.1, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2.5, bty="n", ncol=1)



###### Possiamo colorare gli archi in base al nodo di partenza
###### Otteniamo i nodi di partenza per ogni archo con la funzione ends()
edge.start <- ends(net, es=E(net), names=F)[,1]
edge.col <- V(net)$color[edge.start]

plot(net, edge.color=edge.col, edge.curved=.1)


############ Plot di network con layout

####### Network layouts sono algoritmi che ritornano coordinate
###### per ogni nodo della network

net.bg <- sample_pa(80, 1.2) 
V(net.bg)$size <- 8
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- "" 
E(net.bg)$arrow.mode <- 0
plot(net.bg)

###### Puoi inserire i layout nella funzione di plot
plot(net.bg, layout=layout_randomly)

l <- layout_in_circle(net.bg)
plot(net.bg, layout=l)


###### Heatmap per visualizzare una network matrix:
netm <- get.adjacency(net, attr="weight", sparse=F)
colnames(netm) <- V(net)$media
rownames(netm) <- V(net)$media

heatmap(netm[,17:1], Rowv = NA, Colv = NA, 
        scale="none", margins=c(10,10) )


 

############ Plot di grafi bipartiti

head(nodes2)
head(links2)

net2
plot(net2)

###### Colorare i nodi in base al loro tipo
V(net2)$color <- c("steel blue", "orange")[V(net2)$type+1]
V(net2)$shape <- c("square", "circle")[V(net2)$type+1]
V(net2)$label <- ""
V(net2)$label[V(net2)$type==F] <- nodes2$media[V(net2)$type==F] 
V(net2)$label.cex=.6
V(net2)$label.font=2


plot(net2, vertex.label=NA, vertex.size=7, layout=layout_as_bipartite) 

 



##################################################################
############ Properietà dei nodi, archi e network
##################################################################


############  Densità
###### Percentuale di archi su tutti i possibili archi
edge_density(net, loops=F)
ecount(net)/(vcount(net)*(vcount(net)-1)) # per grafi diretti

############ Reciprocità
###### Percentuale di nodi con archi (a b) e (b a)
reciprocity(net)
dyad_census(net) # Mutual, asymmetric, e null 
2*dyad_census(net)$mut/ecount(net) 

############ Transitività o coefficiente di clustering 
###### Misura quanto il grafo tende a clusterizzare
###### Numero di triplette chiuse:
###### per tre nodi nel grafo, quanti di questi sono
###### racchiusi in una tripletta chiusa?
transitivity(net) 


############ Diametro
###### massima distanza possibile tra due nodi nel grafo
###### la distanza è definita come il percorso più breve tra due nodi
diameter(net, directed=F, weights=NA)
diameter(net, directed=F)
diameter(net, directed=T, weights=NA)
diam <- get_diameter(net, directed=T, weights=NA)
diam


###### Colorare i nodi lungo il diametro
vcol <- rep("gray40", vcount(net))
vcol[diam] <- "gold"
ecol <- rep("gray80", ecount(net))
ecol[E(net, path=diam)] <- "orange" 
###### E(net, path=diam) trova gli archi lungo il percorso
plot(net, vertex.color=vcol, edge.color=ecol)

############ Grado di un nodo (in, out, all)
###### in : entranti
###### out : uscenti
###### all : tutti
deg <- degree(net, mode="all")
plot(net, vertex.size=deg*3)
hist(deg, breaks=1:vcount(net)-1)



############ Misure di CENTRALITA' (importanza di un nodo)



############ Grado di centralità
###### Capacità di un nodo di comunicare con gli altri nodi
###### In un grafo diretto si divide in in e out
###### Corrispode al grado di un nodo

degree(net, mode="in")
centr_degree(net, mode="in", normalized=T)



############ Closeness centrality
###### misura quanto un nodo è vicino agli alti nodi 
closeness(net, mode="all", weights=NA) 
centr_clo(net, mode="all", normalized=T) 



############ Betweenness centrality
###### quanto un nodo è importante nella comunicazione
###### quante volte uno shortest path passa per quel nodo

betweenness(net, directed=T, weights=NA)
edge_betweenness(net, directed=T, weights=NA) # si può calcolare anche per un arco
centr_betw(net, directed=T, normalized=T)






##################################################################
############ Distanze
##################################################################

############ Lunghezza media di un percorso
mean_distance(net, directed=F)
mean_distance(net, directed=T)

############ Lunghezze del percorso più breve tra due nodi
distances(net) # con pesi
distances(net, weights=NA) # senza pesi


###### Shortest path tra due specifici nodi
news.path <- shortest_paths(net, 
                            from = V(net)[media=="MSNBC"], 
                             to  = V(net)[media=="New York Post"],
                             output = "both") 

###### Genera i colori di un arco seguendo il percorso
ecol <- rep("gray80", ecount(net))
ecol[unlist(news.path$epath)] <- "orange"
###### Genera lo spessore di un arco seguendo il percorso
ew <- rep(2, ecount(net))
ew[unlist(news.path$epath)] <- 4
###### Genera i colori di un nodo seguendo il percorso
vcol <- rep("gray40", vcount(net))
vcol[unlist(news.path$vpath)] <- "gold"

plot(net, vertex.color=vcol, edge.color=ecol, 
     edge.width=ew, edge.arrow.mode=0)



###### Possiamo colorare il vicinato di un nodo
neigh.nodes <- neighbors(net, V(net)[media=="Wall Street Journal"], mode="out")

vcol[neigh.nodes] <- "#ff9d00"
plot(net, vertex.color=vcol)


##################################################################
############ Communities
##################################################################

############ Convertire un grafo diretto in uno indiretto
net.sym <- as.undirected(net, mode="collapse", edge.attr.comb=list(weight="sum", "ignore"))


########### Cliques
###### Grafo che contiene nodi connessi con tutti gli altri
###### Trova cliques in un grafo
cliques(net.sym) # lista di cliques       
sapply(cliques(net.sym), length) # grandezza clique 
largest_cliques(net.sym) # cliques più grandi

vcol <- rep("grey80", vcount(net.sym))
vcol[unlist(largest_cliques(net.sym))] <- "gold"
plot(net.sym, vertex.label=V(net.sym)$name, vertex.color=vcol)




########### Community detection basato su edge betweenness (Newman-Girvan)
##### Archi con alta betweenness sono rimossi sequenzialmente e poi si ricalcola
##### e il miglior partizionamento è poi selezionato
ceb <- cluster_edge_betweenness(net) 
dendPlot(ceb, mode="hclust")
plot(ceb, net) 

###### Vediamo un oggetto communities
class(ceb)
length(ceb)     # numero di cluster
membership(ceb) # appartenenza ad un cluster
crossing(ceb, net)   # TRUE per archi che passano tra un cluster




############ Community detection basato sulla propagazione delle label
###### Assegna dei label ai nodi, li randomizza e lo sostituisce con il label
###### che appare più volte nel vicinato
clp <- cluster_label_prop(net)
plot(clp, net)





