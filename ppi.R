library(R.matlab)
ppi <- readMat("/home/scala/CovidNet/GPSnet/Data_mat/Net_PPI.mat")
ppi_net = ppi$Net

gene_drug <- readMat("/home/scala/CovidNet/GPSnet/Data_mat/Gene_Drug.mat")
gene_drug_net = gene_drug$Gene.Drug

gene_distance <- readMat("/home/scala/CovidNet/GPSnet/Data_mat/Gene_Distance.mat")
gene_distance_net = gene_distance$Distance
gene_distance_genes = gene_distance$Genes

gene_length <- readMat("/home/scala/CovidNet/GPSnet/Data_mat/Gene_Length.mat")
gene_length = gene_length$Gene.Length

map_list <- readMat("/home/scala/CovidNet/GPSnet/Data_mat/Map_List.mat")



matrix = matrix(runif(12), nrow = 4, ncol = 3)
cor(matrix)
