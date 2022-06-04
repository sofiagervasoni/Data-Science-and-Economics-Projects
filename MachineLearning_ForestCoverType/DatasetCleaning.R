library("psych")
library("ggplot2")
library("DataExplorer")
library("gplots")
library("patchwork")
library("dplyr")
library("ggthemes")
library("tidyr")
library("data.table")
library("ggpubr")
library("scales")
library("corrplot")
library("ggplot2")
library("GGally")  
library("rstatix")
library("performanceEstimation")

trees=read.csv(file.choose(), header=T) 

head(trees)
tail(trees)
sapply(trees, class) #all integers

ncol(trees) #55
nrow(trees) #581012

for(i in 11:55){
  trees[,i]=as.factor(trees[,i])
} #transform in factor 

describe(trees[,1:10])
summary(trees)
describe(trees)

sapply(trees, class) #variabili dicotomiche trasformate in factor

sum(is.na(trees)) #no missing values

#outliers

ct1=subset(trees, subset=Cover_Type==1)
ct2=subset(trees, subset=Cover_Type==2)
ct3=subset(trees, subset=Cover_Type==3)
ct4=subset(trees, subset=Cover_Type==4)
ct5=subset(trees, subset=Cover_Type==5)
ct6=subset(trees, subset=Cover_Type==6)
ct7=subset(trees, subset=Cover_Type==7)

numero_righe=c(nrow(ct1),nrow(ct2),nrow(ct3),nrow(ct4),nrow(ct5),nrow(ct6),nrow(ct7))
x11()
barplot2(c(nrow(ct1),nrow(ct2),nrow(ct3),nrow(ct4),nrow(ct5),nrow(ct6),nrow(ct7)),
         names.arg=1:7, main="Number of observation for each class (Cover Type)")
(numero_righe/nrow(trees))*100

extreme1=vector()
for(i in 1:10){
  extreme1[i]=sum(is_extreme(ct1[,i]))
}

extreme2=vector()
for(i in 1:10){
  extreme2[i]=sum(is_extreme(ct2[,i]))
}

extreme3=vector()
for(i in 1:10){
  extreme3[i]=sum(is_extreme(ct3[,i]))
}

extreme4=vector()
for(i in 1:10){
  extreme4[i]=sum(is_extreme(ct4[,i]))
}

extreme5=vector()
for(i in 1:10){
  extreme5[i]=sum(is_extreme(ct5[,i]))
}

extreme6=vector()
for(i in 1:10){
  extreme6[i]=sum(is_extreme(ct6[,i]))
}

extreme7=vector()
for(i in 1:10){
  extreme7[i]=sum(is_extreme(ct7[,i]))
  
}

extr_out=cbind(extreme1, extreme2, extreme3, extreme4, extreme5, extreme6, extreme7)
extr_out #tabella con extr out per variabile e per classe
hist(extr_out)
sum(extr_out) #Th


nrow_type=c(nrow(ct1), nrow(ct2), nrow(ct3), nrow(ct4),nrow(ct5),nrow(ct6),nrow(ct7))
x11()
par(las=2)
barplot((colSums(extr_out)/nrow_type)*100, main="BarPlot proportion of outliers for each CoverType (%)",
        names.arg = c("class1","class2","class3","class4","class5","class6","class7")) #inserisci 
  
names(trees)
boxplot(Hillshade_9am~Cover_Type, data=trees)

#class1
class1=c(which(is_extreme(ct1[,3])==T),which(is_extreme(ct1[,5])==T),
  which(is_extreme(ct1[,7])==T),which(is_extreme(ct1[,8])==T))
(out1=unique(class1))
ct1=ct1[-out1,]

#class2
class2=c(which(is_extreme(ct2[,3])==T),which(is_extreme(ct2[,4])==T),
         which(is_extreme(ct2[,5])==T),which(is_extreme(ct2[,7])==T),
         which(is_extreme(ct2[,8])==T),which(is_extreme(ct2[,10])==T))
(out2=unique(class2))
ct2=ct2[-out2,]

#class5
class5=c(which(is_extreme(ct5[,4])==T),which(is_extreme(ct5[,8])==T),
        which(is_extreme(ct5[,10])==T))
(out5=unique(class5))
ct5=ct5[-out5,]

#class6
class6=c(which(is_extreme(ct6[,5])==T),which(is_extreme(ct6[,7])==T),
  which(is_extreme(ct6[,8])==T))
(out6=unique(class6))
ct6=ct6[-out6,]

#class7
class7=c(which(is_extreme(ct7[,1])==T),which(is_extreme(ct7[,3])==T),
         which(is_extreme(ct7[,5])==T),which(is_extreme(ct7[,7])==T),
         which(is_extreme(ct7[,8])==T))
(out7=unique(class7))
ct7=ct7[-out7,]

new_trees=rbind(ct1,ct2,ct3,ct4,ct5,ct6,ct7)
nrow(new_trees) #572627
(1-(nrow(new_trees)/nrow(trees)))*100 #we lose 1.44% of obs

#Distributions and scaling of continuous variables
#create_report(new_trees)
new_trees[,1:10]=sapply(new_trees[,1:10], scale)
nrow(new_trees)
table(new_trees$Cover_Type)




#write.csv(ct1, "C:\\Users\\Utente\\Desktop\\ct1.csv", row.names = F)
#write.csv(ct2, "C:\\Users\\Utente\\Desktop\\ct2.csv", row.names = F)
#write.csv(ct3, "C:\\Users\\Utente\\Desktop\\ct3.csv", row.names = F)
#write.csv(ct4, "C:\\Users\\Utente\\Desktop\\ct4.csv", row.names = F)
#write.csv(ct5, "C:\\Users\\Utente\\Desktop\\ct5.csv", row.names = F)
#write.csv(ct6, "C:\\Users\\Utente\\Desktop\\ct6.csv", row.names = F)
#write.csv(ct7, "C:\\Users\\Utente\\Desktop\\ct7.csv", row.names = F)


#Correlation

new_trees$Cover_Type=as.numeric(new_trees$Cover_Type)
colnames(new_trees)=c("Elevation","Aspect","Slope","Hor_DistHydr",
                   "Vert_DistHydr","Hor_DisRoad","Hillshade_9am",                      
                   "Hillshade_Noon", "Hillshade_3pm","Hor_DistFireP",
                   "Wilderness_Area1","Wilderness_Area2","Wilderness_Area3",                
                   "Wilderness_Area4","Soil_Type1","Soil_Type2","Soil_Type3",                         
                   "Soil_Type4","Soil_Type5","Soil_Type6","Soil_Type7",                       
                   "Soil_Type8","Soil_Type9", "Soil_Type10","Soil_Type11",                       
                   "Soil_Type12","Soil_Type13","Soil_Type14", "Soil_Type15",
                   "Soil_Type16","Soil_Type17","Soil_Type18","Soil_Type19",                        
                   "Soil_Type20","Soil_Type21","Soil_Type22", "Soil_Type23",
                   "Soil_Type24","Soil_Type25","Soil_Type26","Soil_Type27",                       
                   "Soil_Type28","Soil_Type29","Soil_Type30","Soil_Type31","Soil_Type32",                  
                   "Soil_Type33","Soil_Type34","Soil_Type35","Soil_Type36",
                   "Soil_Type37","Soil_Type38","Soil_Type39","Soil_Type40","Cover_Type")
x11()
corPlot(new_trees[,c(1:10,55)], cex = 0.4, xlas=2,show.legend=F)

heatmap(cor(new_trees[,c(1:10,55)]))
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = cor(new_trees[,c(1:10,55)]), col = col, symm = TRUE, Colv = NA, Rowv = NA)


