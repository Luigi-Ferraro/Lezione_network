library(vroom)
library(igraph)
library(minet)



CCLE_expression <- vroom("C:/Users/luigi/Desktop/Lezioni/nets/CCLE_expression.csv")
#CCLE_expression <- read_csv("C:/Users/luigi/Desktop/Lezioni/nets/CCLE_expression.csv")
ccls = CCLE_expression[,1]
ccls = ccls[[1]]
CCLE_expression = CCLE_expression[,-1]

expr = CCLE_expression[1:1000,1:3000]
expr = as.matrix(expr)
rownames(expr) = ccls[1:1000]


###### Coespressione
coexpr = abs(cor(expr,use =  "pairwise.complete.obs"))

Ad = coexpr
threshold = 0.7

Ad[Ad >= threshold] = 1
Ad[Ad < threshold] = 0

diag(Ad)<-0

Gr=graph.adjacency(Ad,mode="undirected",add.colnames=NULL,diag=FALSE)

de <- degree(Gr,loops = FALSE)
Ad <- Ad[which(de > 0), which(de > 0)]
net_coexpr <- graph.adjacency(Ad,mode="undirected",add.colnames=NULL,diag=FALSE)




###### mutua informatione

#presimil <- build.mim(expr, estimator = "mi.shrink", disc = "globalequalwidth")
presimil <- build.mim(expr, estimator = "spearman", disc = "globalequalwidth")

simil<-sqrt(1-exp(-2*presimil))

simil[which(is.na(simil))]<-0

Ad = simil
threshold = 0.5

Ad[Ad >= threshold] = 1
Ad[Ad < threshold] = 0

diag(Ad)<-0

Gr=graph.adjacency(Ad,mode="undirected",add.colnames=NULL,diag=FALSE)

de <- degree(Gr,loops = FALSE)
Ad <- Ad[which(de > 0), which(de > 0)]
net_mutinf <- graph.adjacency(Ad,mode="undirected",add.colnames=NULL,diag=FALSE)



###### ARACNE
Ad <- aracne(simil)


threshold = 0.5

Ad[Ad >= threshold] = 1
Ad[Ad < threshold] = 0

diag(Ad)<-0

Gr=graph.adjacency(Ad,mode="undirected",add.colnames=NULL,diag=FALSE)

de <- degree(Gr,loops = FALSE)
Ad <- Ad[which(de > 0), which(de > 0)]
net_aracne <- graph.adjacency(Ad,mode="undirected",add.colnames=NULL,diag=FALSE)
net_aracne




inter = graph.intersection(net_coexpr, net_mutinf, keep.all.vertices = F)
