#Libraries----------------------------------------------------------------------
library(corrplot)
library(VIM)
library(naniar)
library(FactoMineR)
library(factoextra)
library(cluster)
library(plmf)
library(anacor)
library(ggplot2)
library(grid)
library(ca)
library(FactoMineR)
library(dplyr)
library(ClustGeo)
library(dplyr)


#Data---------------------------------------------------------------------------
rm(list=ls())
d1=read.csv(choose.files(), header=T) #2016
d2=read.csv(choose.files(), header=T) #hfi_cc_2018

#d1
rownames(d1)=d1[,1]
d1.new=d1[,-c(1,2,3)]
which(is.na(d1.new)) #no missing values
sum(is.na(d1.new)) 

#d2
which(d2[,1]==2016) #from 1 to 162, data of 2016
new.d2=d2[1:162,]
rownames(new.d2)=new.d2[,3]

rownames(new.d2)=d2[1:162,3]
new.d2=new.d2[,-3]


#corrplot.mixed(cor(new.d2), order="hclust", tl.col="black")
#install.packages("DMwR")
#new.d2=new.d2[,-c(1,2,3,26,27,34,35,37,38,40,41)]

aggr(new.d2) 
any_na(new.d2) #TRUE there are missing values
miss_var_summary(new.d2)

nomissing=knnImputation(new.d2[, !names(new.d2) %in% "medv"])
#corrplot.mixed(cor(nomissing), order="hclust", tl.col="black")

#column to keep: 7, 14, 20 ,21, 25, 31, 43, 51, 60, 61, 69, 78, 83, 98, 117, 118
d2.new=new.d2[,c(7, 14, 20 ,21, 25, 31, 43, 51, 60, 61, 69, 80, 85, 98, 
                 117, 118,120)]
#sum(is.na(d2.new)) #32 missing values


d2.off=d2.new[,c(10,16,17)]
is.na(d2.off)
sum(is.na(d2.off))


#aggr(d2.new) #Missing values in only one variable
#library(naniar)
#any_na(d2.new) #TRUE there are missing values
#miss_var_summary(d2.new) 
#26 missing values in pf_association
#5 missing values in pf_ss_women
#1 missing value in pf_religion


#substitute the missing value in pf_religion with the mean of the column
#which(is.na(d2.new$pf_religion)) #160
#d2.new$pf_religion[160]=mean(d2.new$pf_religion[-160])
#miss_var_summary(d2.new) 

#substitute the missing values in pf_ss_women with the mean of the column
#which(is.na(d2.new$pf_ss_women)) #9 12 29 101 129
#d2.new$pf_ss_women[c(9,12,29,101,129)]=mean(d2.new$pf_ss_women[-c(9,12,29,101,129)])
#miss_var_summary(d2.new) 

#fill the missing values in pf_association
#library(DMwR)
#d2.nomissing=knnImputation(d2.new[, !names(d2.new) %in% "medv"]) 
#any_na(d2.nomissing) #no missing values left

a=merge(d1.new,d2.off, by = 0) #by=0 => by="row.names"
row.names(a)=a[,1]
a.new=a[,-1]

colnames(a.new)=c("HappinessScore", "LowerCI", "UpperCI", "Economy_GDP", 
                  "Family", "Health_LifeExpectancy", "Freedom",
                  "Trust_GovernmentCorruption", "Generosity", 
                  "Dystopya_residual", "pf_score", "ef_score", "hf_score")

summary(a.new)


#Descriptive analysis-----------------------------------------------------------

dim(a.new) #137 rows and 13 columns
str(a.new) #all numerical

sum(is.na(a.new)) #no missing data
pairs(a.new, col="mistyrose4", pch=19)

par(mfrow=c(1,2))
summary(a.new[,1])
colors()
hist(a.new[,1], main="", xlab="Happiness Score", col="mistyrose4")
quantile(a.new[,1])
boxplot(a.new[,1], col="mistyrose4")

summary(a.new[,2])
hist(a.new[,2], main="", xlab="Lower confidence interval", col="mistyrose4")
quantile(a.new[,2])
boxplot(a.new[,2], col="mistyrose4")

summary(a.new[,3])
hist(a.new[,3], main="", xlab="Upper confidence interval", col="mistyrose4")
quantile(a.new[,3])
boxplot(a.new[,3], col="mistyrose4")

summary(a.new[,4])
hist(a.new[,4], main="", xlab="Economy (GDP)", col="mistyrose4")
boxplot(a.new[,4], col="mistyrose4")

summary(a.new[,5]) 
hist(a.new[,5], main="", xlab="Family", col="mistyrose4") 
boxplot(a.new[,5], col="mistyrose4")

summary(a.new[,6])
hist(a.new[,6], main="", xlab="Health (Life expectancy)", col="mistyrose4")
boxplot(a.new[,6], col="mistyrose4")

summary(a.new[,7])
hist(a.new[,7], main="", xlab="Freedom", col="mistyrose4")
boxplot(a.new[,7], col="mistyrose4")

summary(a.new[,8])
hist(a.new[,8], main="", xlab="Trust (government corruption)", col="mistyrose4")
boxplot(a.new[,8], col="mistyrose4")

summary(a.new[,9])
hist(a.new[,9], main="", xlab="Generosity", col="mistyrose4")
boxplot(a.new[,9], col="mistyrose4")

summary(a.new[,10])
hist(a.new[,10], main="", xlab="Dystopia residual", col="mistyrose4")
boxplot(a.new[,10], col="mistyrose4")

summary(a.new[,11])
hist(a.new[,11], main="", xlab="pf_score", col="mistyrose4")
boxplot(a.new[,11], col="mistyrose4")

summary(a.new[,12])
hist(a.new[,12], main="", xlab="ef_score", col="mistyrose4")
boxplot(a.new[,12], col="mistyrose4")

summary(a.new[,13])
hist(a.new[,13], main="", xlab="hf_score", col="mistyrose4")
boxplot(a.new[,13], col="mistyrose4")

hist(a.new)
x11()
par(mfrow=c(1,1))
hist(scale(a.new), main="", col="mistyrose4" )

round(cor(a.new),2)

library(corrplot)
x11()
corPlot(a.new, cex = 0.4, xlas=2,show.legend=F)

#PCA----------------------------------------------------------------------------

pca=prcomp(a.new, scale=T)
round(pca$rotation,3)
screeplot(pca,type=c("lines")) 
summary(pca)
#the first 2 principal component explain the 68% of the total variability
#3 --> 78%
#4--> 85%
x11()
biplot(pca, cex=.9, col= c("black","darkmagenta"))
plot(pca, col="mistyrose4")


res.pca = PCA(a.new,graph=F) 

(bip1.2 = fviz_pca_biplot(res.pca, col.ind="cos2", repel = TRUE, axes = c(1,2))+
  scale_color_gradient2(low="green", mid="black",  high="red"))
(bip2.3 = fviz_pca_biplot(res.pca, col.ind="cos2", repel = TRUE, axes = c(2,3))+
  scale_color_gradient2(low="green", mid="black",  high="red"))

fviz_eig(res.pca, addlabels=TRUE, hjust = -0.3, barcolor = "mistyrose4", 
         barfill="mistyrose4") +
  ylim(0, 70)
fviz_pca_ind(prcomp(scale(a.new)),ggtheme=theme_classic(),legend="bottom") 
#score plot
fviz_pca_var(res.pca,col.var="cos2",repel=T) #Loading plot

fviz_contrib(res.pca,choice="var",axes=1,top=10)
fviz_contrib(res.pca,choice="var",axes=2,top=10)
fviz_contrib(res.pca,choice="var",axes=1:2,top=10)

#(var = get_pca_var(res.pca))
#res.km = kmeans(var$coord,centers = 4, nstart = 25)
#grp = as.factor(res.km$cluster)
#fviz_pca_var(res.pca,col.var=grp,legend.title="cluster")

#Cluster analysis---------------------------------------------------------------

##Hierarchical method##

d=dist(scale(a.new), method = "euclidean") #important to scale the data

#choose the right method (using cophenetic distance)
fit=hclust(d, method="ward.D2")
coph=cophenetic(fit)
cor(d,coph) #0.5646204

fit.single=hclust(d, method="single")
coph.s=cophenetic(fit.single)
cor(d,coph.s) #0.4090292

fit.c=hclust(d, method="complete")
coph.c=cophenetic(fit.c)
cor(d,coph.c) # 0.6206036

fit.a=hclust(d, method="average")
coph.a=cophenetic(fit.a)
cor(d,coph.a) #0.6572907

#use the method "average" to do the analysis
plot(fit.a, cex=.7) #not good because we have chaining

#so try with complete
plot(fit.c, cex= .7)
rect.hclust(fit.c, k=3)
#rect.hclust(fit.c, k=4)

#plot(fit, cex=.7)
#rect.hclust(fit, k=3)
#rect.hclust(fit, k=4)

(groups.c=cutree(fit.c, k=3))
a.newmap=cbind(a.new,groups.c)
names(groups.c)

rownames(a.newmap)=ifelse(rownames(a.newmap)=='United States', 'USA', 
                          rownames(a.newmap))
rownames(a.newmap)=ifelse(rownames(a.newmap)=='United Kingdom', 'UK', 
                          rownames(a.newmap))
map = map_data('world')
unique(map$region)

thismap=mutate(map, fill = ifelse(region %in% row.names(subset(a.newmap, subset = groups.c == 1)), '1st cluster ', 
                                     ifelse(region %in% row.names(subset(a.newmap, subset = groups.c == 2)), '2nd cluster', 
                                            ifelse(region %in% row.names(subset(a.newmap, subset = groups.c == 3)), '3rd cluster', 
                                                   'NA'))))
unique(thismap$region)

ggplot(thismap, aes(long, lat, fill = fill, group=group)) + 
  geom_polygon(colour="white") + 
  ggtitle("Map of World with K = 3")

hier=cbind(apply(a.new[a.newmap$groups.c==1,],2, mean),
           apply(a.new[a.newmap$groups.c==2,], 2, mean),
           apply(a.new[a.newmap$groups.c==3,], 2,mean))

h32=((hier[,3]-hier[,2])/hier[,3])*100
h31=((hier[,3]-hier[,1])/hier[,3])*100
h12=((hier[,1]-hier[,2])/hier[,1])*100

round(cbind("1 vs 2 (%)"=h12,"3 vs 1(%)"= h31,
      "3 vs 2(%)"= h32),1)


##K-means##

SSV=vector(mode = "numeric", length = 15)
SSV[1]=(n - 1) * sum(apply(scale(a.new),2,var)) 
for (i in 1:15)SSV[i]=sum(kmeans(scale(a.new),centers=i,nstart=200)$withinss)
plot(1:15, SSV, type="b", xlab="Number of Clusters",
     ylab="Sum of squares within groups",pch=19, col="black")

#1.chose 3 the ideal number of clusters (from elbow chart)
kclust=kmeans(scale(a.new), 3, nstart= 200)
#2. calculate centroids 
centr= aggregate(scale(a.new), by=list(kclust$cluster), FUN=mean)
#3. scatterplot of clusters
nk=3
pairs(scale(a.new), col=kclust$cluster, pch=19)
points(kclust$centers, col= 2:nk+1, pch=19, cex=2)

data.kclust=kclust$cluster

x11()
fviz_cluster(kclust, data = a.new,
             palette = "set2", 
             geom = c("point", "text"),
             ellipse.type = "convex", 
             ggtheme = theme_bw(), repel = TRUE
)

a.newmap[,15]=kclust$cluster

#map with clusters
map = map_data('world')
unique(map$region)

thismap <- mutate(map, fill = ifelse(region %in% row.names(subset(a.newmap, subset = V15 == 1)), '1st cluster ', 
                                     ifelse(region %in% row.names(subset(a.newmap, subset = V15== 2)), '2nd cluster', 
                                            ifelse(region %in% row.names(subset(a.newmap, subset = V15 == 3)), '3rd cluster', 
                                                   'NA'))))
unique(thismap$region)

ggplot(thismap, aes(long, lat, fill = fill, group=group)) + 
  geom_polygon(colour="white") + 
  ggtitle("Map of World with K = 3")

k=cbind(apply(a.new[a.newmap$V15==1,],2, mean),
      apply(a.new[a.newmap$V15==2,], 2, mean),
      apply(a.new[a.newmap$V15==3,], 2,mean))

k32=((k[,3]-k[,2])/k[,3])*100
k31=((k[,3]-k[,1])/k[,3])*100
k12=((k[,1]-k[,2])/k[,1])*100

round(cbind("1 vs 2 (%)"=k12,"3 vs 1(%)"= k31,
            "3 vs 2(%)"= k32),1)

##K-medoids##

fviz_nbclust(scale(a.new), pam, method = "silhouette")+ 
  theme_classic()


x11()
pam.res = pam(scale(a.new),3)
fviz_cluster(pam.res, data = a.new,
             palette = "set2", 
             geom = c("point", "text"),
             ellipse.type = "convex", 
             ggtheme = theme_bw(), repel = TRUE
)


c=pam.res$clustering

c = case_when(
  c == 1 ~ 2,
  c == 2 ~ 1,
  TRUE ~ 3
)

a.newmap[,16]=c

thismap <- mutate(map, fill = ifelse(region %in% row.names(subset(a.newmap, subset = V16 == 1)), '1st cluster ', 
                                     ifelse(region %in% row.names(subset(a.newmap, subset = V16== 2)), '2nd cluster', 
                                            ifelse(region %in% row.names(subset(a.newmap, subset = V16 == 3)), '3rd cluster', 
                                                   'NA'))))
unique(thismap$region)
x11()
ggplot(thismap, aes(long, lat, fill = fill, group=group)) + 
  geom_polygon(colour="white") + 
  ggtitle("Map of World with K = 3")

km=cbind(apply(a.new[a.newmap$V16==1,],2, mean),
      apply(a.new[a.newmap$V16==2,], 2, mean),
      apply(a.new[a.newmap$V16==3,], 2,mean))

km32=((km[,3]-km[,2])/km[,3])*100
km31=((km[,3]-km[,1])/km[,3])*100
km12=((k[,1]-km[,2])/km[,1])*100

round(cbind("1 vs 2 (%)"=km12,"3 vs 1(%)"= km31,
            "3 vs 2(%)"= km32),1)

a=cbind("Hierachical"=a.newmap[,14], "Kmean"=a.newmap[,15], "Kmedoid"=a.newmap[,16])
rownames(a)=row.names(a.new)
a
