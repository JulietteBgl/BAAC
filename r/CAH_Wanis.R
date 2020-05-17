install.packages("RColorBrewer")
install.packages("JLutils")

library(RColorBrewer)
library(JLutils)

#------------------------------------------CAH

###CAH via ACM et FactoMiner

#Partition
global_acc_f2<-global_acc_f[sample(nrow(global_acc_f), 5000), ]


#Clusterisation
res.mca_cah<-MCA(global_acc_f2[,2:ncol(global_acc_f2)],quali.sup=1,graph=F,na.method="average",ncp=13)
res.cah<-HCPC(res.mca_cah,consol = F,max = 10)


#Représentation
plot(res.cah, choice = "tree")

plot(res.cah, choice = "3D.map")

plot(res.cah, choice = "bar")

plot(res.cah, choice = "map")


#catégorisation des clusters

res.cah$hcpc$desc.var
res.cah$hcpc$desc.axes
res.cah$hcpc$desc.ind



#Récuupération des cluster dans les données
global_acc_f2$cluster <- res.cah$data.clust$clust


###CAH via ACM et ade4


library(ade4)

global_acc_f2<-global_acc_f[sample(nrow(global_acc_f), 5000), ]

res.mca_cah <- dudi.acm(global_acc_f2[,2:ncol(global_acc_f2)], scannf = FALSE, nf = 13)
md <- dist.dudi(res.mca_cah)

arbre <- hclust(md, method = "ward.D2")

plot(arbre, labels = FALSE, main = "Dendrogramme")

inertie <- sort(arbre$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")

typo <- cutree(arbre, 5)

par(mfrow = c(1, 2))
library(RColorBrewer)
s.class(res.mca_cah$li, as.factor(typo), col = brewer.pal(5, "Set1"), 
        sub = "Axes 1 et 2")
s.class(res.mca_cah$li, as.factor(typo), 3, 4, col = brewer.pal(5, "Set1"), 
        sub = "Axes 3 et 4")

A2Rplot(arbre, k = 5, boxes = FALSE, col.up = "gray50", col.down = brewer.pal(5, 
                                                                              "Dark2"), show.labels = FALSE)