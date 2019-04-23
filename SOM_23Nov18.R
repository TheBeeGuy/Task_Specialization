library(kohonen)
library(RCurl)
library(tidyr)

load("SamplesAndTraits_bpo_OutliersRemoved.RData")
load("Network_allSamples_signed_nomerge_RLDfiltered.RData")

##load in WGCNA data sets, spcifically bpo.info and variance stabilized gene counts
bpo.ExprOut<-bpo.ExprOut%>%t()
bpo.ExprOut[1:5,1:5] #these are genes with outliers removed
dim(bpo.ExprOut)

bpo.Expr <-read.table(file="varianceStabilized_counts.txt",header = T,row.names = 1)
bp_info<-read.csv(file="Bpo_info.csv", row.names=1, header=T)
bp_info<-bp_info[colnames(bpo.Expr),]
bp_info$PheroXFood<- factor(paste0(bp_info$Pheromone, bp_info$Food))

bpo.Expr%>%dim()

colors <- function(n, alpha = 1) {
  rev(heat.colors(n, alpha))
}

##read in data
#data has 18 columns and >10K columns


samples.som <- som(bpo.ExprOut, grid = somgrid(3, 2, "rectangular"))
plot(NBA.SOM1,)
plot(NBA.SOM1,type="counts",palette.name=colors)
plot(NBA.SOM1,type="mapping")

#genes
genes.som <- som(as.matrix(bpo.ExprOut%>%t()), grid = somgrid(30, 30, "hexagonal"))
#plot(genes.som,) #not useful. why lines?
plot(genes.som,type="counts",palette.name=colors, heatkey = T)
plot(genes.som,type="mapping")

#toroidal genes
genes.toroid.som <- som(as.matrix(bpo.ExprOut%>%t()), grid = somgrid(6, 6, "hexagonal",toroidal = T))
plot(genes.toroid.som, type = "dist.neighbours", palette.name = terrain.colors)


##supervised classification

training_indices <- sample(nrow(bpo.ExprOut%>%t()), 2000)
bpo_training<-bpo.ExprOut%>%t()
