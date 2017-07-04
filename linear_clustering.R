library(TSdist)
library(TSclust)

source("make_eps_script_linear.R")


source("data/import_gmd.R")
import_data()

new_Freq <- Freq[,colnames(Freq)!="40749"]

#ts_freq2 <- ts(data = Freq[,2],start = c(0,),end = Freq[nsamples(),1],frequency = 30)

ts_freq1 <- as.ts(Freq[,2])
ts_freq2 <- as.ts(Freq[,3])

# dist_mat <- matrix(nrow=nsamples(),ncol = 2)
# colnames(dist_mat) <- c("Time","Freq.Dist")
# dist_mat[,1] <- Freq[1:nsamples(),1]
# dist_mat[,2] <- 0
fdissmat_CID <- list()
for (n in 2:50) {
  # dist_mat[n,][2] <- TSDistances(as.ts(Freq[(n-1),-1]),as.ts(Freq[n,-1]),"euclidean")
  fdissmat_CID <- lappend(fdissmat_CID,diss(SERIES =  Freq[(n-1):n,-1],METHOD = "CID"))
}


lappend <- function (lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}

fdissmat <- list()

fdissmat <- lappend(fdissmat,diss(SERIES =  Freq[1:2,-1],METHOD = "PER"))
fdissmat <- lappend(fdissmat,diss(SERIES =  Freq[2:3,-1],METHOD = "PER"))
fdissmat <- lappend(fdissmat,diss(SERIES =  Freq[(nsamples()-1):nsamples(),-1],METHOD = "CORT"))


fdissmat100_COR <- diss(SERIES =  Freq[99:100,-1],METHOD = "COR")
fdissmat100_CID <- diss(SERIES =  Freq[99:100,-1],METHOD = "CID")
fdissmat100_CDM <- diss(SERIES =  new_Freq[99:100,-1],METHOD = "CDM")

fmelt <- melt(data = Freq,id.vars = "Time",variable.name = "Bus",value.name = "Frequency")
fplot <- ggplot(data = fmelt,aes(x=Time,y=Frequency,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")

#################################################CDM

fdissmat_CDM <- diss(SERIES =  Freq[(nsamples()-1):nsamples(),-1],METHOD = "CDM")
hc_freq <- hclust(fdissmat_CDM,method = "complete")
cut_freq <- cutree(hc_freq,k = 5)

cut_freq_names <- data.frame(cut_freq,names(cut_freq))
colnames(cut_freq_names) <- c("group","Name")

f_1 <- cut_freq_names[cut_freq_names$group == 1,]
f_2 <- cut_freq_names[cut_freq_names$group == 2,]
f_3 <- cut_freq_names[cut_freq_names$group == 3,]
f_4 <- cut_freq_names[cut_freq_names$group == 4,]
f_5 <- cut_freq_names[cut_freq_names$group == 5,]


fgrouped_means <- data.frame(Freq[,1])
colnames(fgrouped_means) <- c("Time")
fgrouped_means$group1 <- 0
fgrouped_means$group2 <- 0
fgrouped_means$group3 <- 0
fgrouped_means$group4 <- 0
fgrouped_means$group5 <- 0

for (n in 1:nsamples()) {
  fgrouped_means$group1[n] <- ifelse(nrow(f_1)==1,Freq[n,f_1$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_1$Name)]))
  fgrouped_means$group2[n] <- ifelse(nrow(f_2)==1,Freq[n,f_2$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_2$Name)]))
  fgrouped_means$group3[n] <- ifelse(nrow(f_3)==1,Freq[n,f_3$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_3$Name)]))
  fgrouped_means$group4[n] <- ifelse(nrow(f_4)==1,Freq[n,f_4$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_4$Name)]))
  fgrouped_means$group5[n] <- ifelse(nrow(f_5)==1,Freq[n,f_5$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_5$Name)]))
}

fgroupsmelt_CDM <- melt(data = fgrouped_means,id.vars = "Time",variable.name = "Bus",value.name = "Frequency")
fplot_groups_CDM <- ggplot(data = fgroupsmelt_CDM,aes(x=Time,y=Frequency,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")

###################################### EUCL

fdissmat_EUCL <- diss(SERIES =  Freq[(nsamples()-1):nsamples(),-1],METHOD = "EUCL")
hc_freq <- hclust(fdissmat_EUCL,method = "complete")
cut_freq <- cutree(hc_freq,k = 5)

cut_freq_names <- data.frame(cut_freq,names(cut_freq))
colnames(cut_freq_names) <- c("group","Name")

f_1 <- cut_freq_names[cut_freq_names$group == 1,]
f_2 <- cut_freq_names[cut_freq_names$group == 2,]
f_3 <- cut_freq_names[cut_freq_names$group == 3,]
f_4 <- cut_freq_names[cut_freq_names$group == 4,]
f_5 <- cut_freq_names[cut_freq_names$group == 5,]


fgrouped_means <- data.frame(Freq[,1])
colnames(fgrouped_means) <- c("Time")
fgrouped_means$group1 <- 0
fgrouped_means$group2 <- 0
fgrouped_means$group3 <- 0
fgrouped_means$group4 <- 0
fgrouped_means$group5 <- 0

for (n in 1:nsamples()) {
  fgrouped_means$group1[n] <- ifelse(nrow(f_1)==1,Freq[n,f_1$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_1$Name)]))
  fgrouped_means$group2[n] <- ifelse(nrow(f_2)==1,Freq[n,f_2$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_2$Name)]))
  fgrouped_means$group3[n] <- ifelse(nrow(f_3)==1,Freq[n,f_3$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_3$Name)]))
  fgrouped_means$group4[n] <- ifelse(nrow(f_4)==1,Freq[n,f_4$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_4$Name)]))
  fgrouped_means$group5[n] <- ifelse(nrow(f_5)==1,Freq[n,f_5$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_5$Name)]))
}

fgroupsmelt_EUCL <- melt(data = fgrouped_means,id.vars = "Time",variable.name = "Bus",value.name = "Frequency")
fplot_groups_EUCL <- ggplot(data = fgroupsmelt_EUCL,aes(x=Time,y=Frequency,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")


############################################# CID


fdissmat_CID <- diss(SERIES =  Freq[,-1],METHOD = "CID")
hc_freq <- hclust(fdissmat_CID,method = "complete")
cut_freq <- cutree(hc_freq,k = 4)

cut_freq_names <- data.frame(cut_freq,names(cut_freq))
colnames(cut_freq_names) <- c("group","Name")

f_1 <- cut_freq_names[cut_freq_names$group == 1,]
f_2 <- cut_freq_names[cut_freq_names$group == 2,]
f_3 <- cut_freq_names[cut_freq_names$group == 3,]
f_4 <- cut_freq_names[cut_freq_names$group == 4,]
f_5 <- cut_freq_names[cut_freq_names$group == 5,]


fgrouped_means <- data.frame(Freq[,1])
colnames(fgrouped_means) <- c("Time")
fgrouped_means$group1 <- 0
fgrouped_means$group2 <- 0
fgrouped_means$group3 <- 0
fgrouped_means$group4 <- 0
fgrouped_means$group5 <- 0

for (n in 1:nsamples()) {
  fgrouped_means$group1[n] <- ifelse(nrow(f_1)==1,Freq[n,f_1$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_1$Name)]))
  fgrouped_means$group2[n] <- ifelse(nrow(f_2)==1,Freq[n,f_2$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_2$Name)]))
  fgrouped_means$group3[n] <- ifelse(nrow(f_3)==1,Freq[n,f_3$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_3$Name)]))
  fgrouped_means$group4[n] <- ifelse(nrow(f_4)==1,Freq[n,f_4$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_4$Name)]))
  fgrouped_means$group5[n] <- ifelse(nrow(f_5)==1,Freq[n,f_5$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_5$Name)]))
}

fgroupsmelt_CID <- melt(data = fgrouped_means,id.vars = "Time",variable.name = "Bus",value.name = "Frequency")
fplot_groups_CID <- ggplot(data = fgroupsmelt_CID,aes(x=Time,y=Frequency,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")

############################################# COR


fdissmat_COR <- diss(SERIES =  Freq[,-1],METHOD = "COR")
hc_freq <- hclust(fdissmat_COR,method = "complete")
cut_freq <- cutree(hc_freq,k = 5)

cut_freq_names <- data.frame(cut_freq,names(cut_freq))
colnames(cut_freq_names) <- c("group","Name")

f_1 <- cut_freq_names[cut_freq_names$group == 1,]
f_2 <- cut_freq_names[cut_freq_names$group == 2,]
f_3 <- cut_freq_names[cut_freq_names$group == 3,]
f_4 <- cut_freq_names[cut_freq_names$group == 4,]
f_5 <- cut_freq_names[cut_freq_names$group == 5,]


fgrouped_means <- data.frame(Freq[,1])
colnames(fgrouped_means) <- c("Time")
fgrouped_means$group1 <- 0
fgrouped_means$group2 <- 0
fgrouped_means$group3 <- 0
fgrouped_means$group4 <- 0
fgrouped_means$group5 <- 0

for (n in 1:nsamples()) {
  fgrouped_means$group1[n] <- ifelse(nrow(f_1)==1,Freq[n,f_1$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_1$Name)]))
  fgrouped_means$group2[n] <- ifelse(nrow(f_2)==1,Freq[n,f_2$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_2$Name)]))
  fgrouped_means$group3[n] <- ifelse(nrow(f_3)==1,Freq[n,f_3$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_3$Name)]))
  fgrouped_means$group4[n] <- ifelse(nrow(f_4)==1,Freq[n,f_4$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_4$Name)]))
  fgrouped_means$group5[n] <- ifelse(nrow(f_5)==1,Freq[n,f_5$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_5$Name)]))
}

fgroupsmelt_COR <- melt(data = fgrouped_means,id.vars = "Time",variable.name = "Bus",value.name = "Frequency")
fplot_groups_COR <- ggplot(data = fgroupsmelt_COR,aes(x=Time,y=Frequency,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")

############################################# CORT


fdissmat_CORT <- diss(SERIES =  Freq[,-1],METHOD = "CORT")
hc_freq <- hclust(fdissmat_CORT,method = "complete")
cut_freq <- cutree(hc_freq,k = 5)

cut_freq_names <- data.frame(cut_freq,names(cut_freq))
colnames(cut_freq_names) <- c("group","Name")

f_1 <- cut_freq_names[cut_freq_names$group == 1,]
f_2 <- cut_freq_names[cut_freq_names$group == 2,]
f_3 <- cut_freq_names[cut_freq_names$group == 3,]
f_4 <- cut_freq_names[cut_freq_names$group == 4,]
f_5 <- cut_freq_names[cut_freq_names$group == 5,]


fgrouped_means <- data.frame(Freq[,1])
colnames(fgrouped_means) <- c("Time")
fgrouped_means$group1 <- 0
fgrouped_means$group2 <- 0
fgrouped_means$group3 <- 0
fgrouped_means$group4 <- 0
fgrouped_means$group5 <- 0

for (n in 1:nsamples()) {
  fgrouped_means$group1[n] <- ifelse(nrow(f_1)==1,Freq[n,f_1$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_1$Name)]))
  fgrouped_means$group2[n] <- ifelse(nrow(f_2)==1,Freq[n,f_2$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_2$Name)]))
  fgrouped_means$group3[n] <- ifelse(nrow(f_3)==1,Freq[n,f_3$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_3$Name)]))
  fgrouped_means$group4[n] <- ifelse(nrow(f_4)==1,Freq[n,f_4$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_4$Name)]))
  fgrouped_means$group5[n] <- ifelse(nrow(f_5)==1,Freq[n,f_5$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_5$Name)]))
}

fgroupsmelt_CORT <- melt(data = fgrouped_means,id.vars = "Time",variable.name = "Bus",value.name = "Frequency")
fplot_groups_CORT <- ggplot(data = fgroupsmelt_CORT,aes(x=Time,y=Frequency,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")

############################################# DTWARP


fdissmat_DTWARP <- diss(SERIES =  Freq[(nsamples()-1):nsamples(),-1],METHOD = "DTWARP")
hc_freq <- hclust(fdissmat_DTWARP,method = "complete")
cut_freq <- cutree(hc_freq,k = 5)

cut_freq_names <- data.frame(cut_freq,names(cut_freq))
colnames(cut_freq_names) <- c("group","Name")

f_1 <- cut_freq_names[cut_freq_names$group == 1,]
f_2 <- cut_freq_names[cut_freq_names$group == 2,]
f_3 <- cut_freq_names[cut_freq_names$group == 3,]
f_4 <- cut_freq_names[cut_freq_names$group == 4,]
f_5 <- cut_freq_names[cut_freq_names$group == 5,]


fgrouped_means <- data.frame(Freq[,1])
colnames(fgrouped_means) <- c("Time")
fgrouped_means$group1 <- 0
fgrouped_means$group2 <- 0
fgrouped_means$group3 <- 0
fgrouped_means$group4 <- 0
fgrouped_means$group5 <- 0

for (n in 1:nsamples()) {
  fgrouped_means$group1[n] <- ifelse(nrow(f_1)==1,Freq[n,f_1$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_1$Name)]))
  fgrouped_means$group2[n] <- ifelse(nrow(f_2)==1,Freq[n,f_2$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_2$Name)]))
  fgrouped_means$group3[n] <- ifelse(nrow(f_3)==1,Freq[n,f_3$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_3$Name)]))
  fgrouped_means$group4[n] <- ifelse(nrow(f_4)==1,Freq[n,f_4$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_4$Name)]))
  fgrouped_means$group5[n] <- ifelse(nrow(f_5)==1,Freq[n,f_5$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_5$Name)]))
}

fgroupsmelt_DTWARP <- melt(data = fgrouped_means,id.vars = "Time",variable.name = "Bus",value.name = "Frequency")
fplot_groups_DTWARP <- ggplot(data = fgroupsmelt_DTWARP,aes(x=Time,y=Frequency,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")

############################################# DWT


fdissmat_DWT <- diss(SERIES =  Freq[,-1],METHOD = "DWT")
hc_freq <- hclust(fdissmat_DWT,method = "complete")
cut_freq <- cutree(hc_freq,k = 5)

cut_freq_names <- data.frame(cut_freq,names(cut_freq))
colnames(cut_freq_names) <- c("group","Name")

f_1 <- cut_freq_names[cut_freq_names$group == 1,]
f_2 <- cut_freq_names[cut_freq_names$group == 2,]
f_3 <- cut_freq_names[cut_freq_names$group == 3,]
f_4 <- cut_freq_names[cut_freq_names$group == 4,]
f_5 <- cut_freq_names[cut_freq_names$group == 5,]


fgrouped_means <- data.frame(Freq[,1])
colnames(fgrouped_means) <- c("Time")
fgrouped_means$group1 <- 0
fgrouped_means$group2 <- 0
fgrouped_means$group3 <- 0
fgrouped_means$group4 <- 0
fgrouped_means$group5 <- 0

for (n in 1:nsamples()) {
  fgrouped_means$group1[n] <- ifelse(nrow(f_1)==1,Freq[n,f_1$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_1$Name)]))
  fgrouped_means$group2[n] <- ifelse(nrow(f_2)==1,Freq[n,f_2$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_2$Name)]))
  fgrouped_means$group3[n] <- ifelse(nrow(f_3)==1,Freq[n,f_3$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_3$Name)]))
  fgrouped_means$group4[n] <- ifelse(nrow(f_4)==1,Freq[n,f_4$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_4$Name)]))
  fgrouped_means$group5[n] <- ifelse(nrow(f_5)==1,Freq[n,f_5$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_5$Name)]))
}

fgroupsmelt_DWT <- melt(data = fgrouped_means,id.vars = "Time",variable.name = "Bus",value.name = "Frequency")
fplot_groups_DWT <- ggplot(data = fgroupsmelt_DWT,aes(x=Time,y=Frequency,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")

############################################# FRECHET


fdissmat_FRECHET <- diss(SERIES =  Freq[(nsamples()-1):nsamples(),-1],METHOD = "FRECHET")
hc_freq <- hclust(fdissmat_FRECHET,method = "complete")
cut_freq <- cutree(hc_freq,k = 5)

cut_freq_names <- data.frame(cut_freq,names(cut_freq))
colnames(cut_freq_names) <- c("group","Name")

f_1 <- cut_freq_names[cut_freq_names$group == 1,]
f_2 <- cut_freq_names[cut_freq_names$group == 2,]
f_3 <- cut_freq_names[cut_freq_names$group == 3,]
f_4 <- cut_freq_names[cut_freq_names$group == 4,]
f_5 <- cut_freq_names[cut_freq_names$group == 5,]


fgrouped_means <- data.frame(Freq[,1])
colnames(fgrouped_means) <- c("Time")
fgrouped_means$group1 <- 0
fgrouped_means$group2 <- 0
fgrouped_means$group3 <- 0
fgrouped_means$group4 <- 0
fgrouped_means$group5 <- 0

for (n in 1:nsamples()) {
  fgrouped_means$group1[n] <- ifelse(nrow(f_1)==1,Freq[n,f_1$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_1$Name)]))
  fgrouped_means$group2[n] <- ifelse(nrow(f_2)==1,Freq[n,f_2$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_2$Name)]))
  fgrouped_means$group3[n] <- ifelse(nrow(f_3)==1,Freq[n,f_3$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_3$Name)]))
  fgrouped_means$group4[n] <- ifelse(nrow(f_4)==1,Freq[n,f_4$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_4$Name)]))
  fgrouped_means$group5[n] <- ifelse(nrow(f_5)==1,Freq[n,f_5$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_5$Name)]))
}

fgroupsmelt_FRECHET <- melt(data = fgrouped_means,id.vars = "Time",variable.name = "Bus",value.name = "Frequency")
fplot_groups_FRECHET <- ggplot(data = fgroupsmelt_FRECHET,aes(x=Time,y=Frequency,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")


############################################# NCD


fdissmat_NCD <- diss(SERIES =  Freq[(nsamples()-1):nsamples(),-1],METHOD = "NCD")
hc_freq <- hclust(fdissmat_NCD,method = "complete")
cut_freq <- cutree(hc_freq,k = 5)

cut_freq_names <- data.frame(cut_freq,names(cut_freq))
colnames(cut_freq_names) <- c("group","Name")

f_1 <- cut_freq_names[cut_freq_names$group == 1,]
f_2 <- cut_freq_names[cut_freq_names$group == 2,]
f_3 <- cut_freq_names[cut_freq_names$group == 3,]
f_4 <- cut_freq_names[cut_freq_names$group == 4,]
f_5 <- cut_freq_names[cut_freq_names$group == 5,]


fgrouped_means <- data.frame(Freq[,1])
colnames(fgrouped_means) <- c("Time")
fgrouped_means$group1 <- 0
fgrouped_means$group2 <- 0
fgrouped_means$group3 <- 0
fgrouped_means$group4 <- 0
fgrouped_means$group5 <- 0

for (n in 1:nsamples()) {
  fgrouped_means$group1[n] <- ifelse(nrow(f_1)==1,Freq[n,f_1$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_1$Name)]))
  fgrouped_means$group2[n] <- ifelse(nrow(f_2)==1,Freq[n,f_2$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_2$Name)]))
  fgrouped_means$group3[n] <- ifelse(nrow(f_3)==1,Freq[n,f_3$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_3$Name)]))
  fgrouped_means$group4[n] <- ifelse(nrow(f_4)==1,Freq[n,f_4$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_4$Name)]))
  fgrouped_means$group5[n] <- ifelse(nrow(f_5)==1,Freq[n,f_5$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_5$Name)]))
}

fgroupsmelt_NCD <- melt(data = fgrouped_means,id.vars = "Time",variable.name = "Bus",value.name = "Frequency")
fplot_groups_NCD <- ggplot(data = fgroupsmelt_NCD,aes(x=Time,y=Frequency,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")






# fdissmat <- diss(SERIES =  Freq[(nsamples()-1):nsamples(),-1],METHOD = clust_type)
fdissmat <- diss(SERIES =  Freq[,-1],METHOD = clust_type)
hc_freq <- hclust(fdissmat,method = "complete")
cut_freq <- cutree(hc_freq,k = 5)
cut_freq_names <- data.frame(cut_freq,names(cut_freq))
colnames(cut_freq_names) <- c("group","Name")
f_1 <- cut_freq_names[cut_freq_names$group == 1,]
f_2 <- cut_freq_names[cut_freq_names$group == 2,]
f_3 <- cut_freq_names[cut_freq_names$group == 3,]
f_4 <- cut_freq_names[cut_freq_names$group == 4,]
f_5 <- cut_freq_names[cut_freq_names$group == 5,]
fgrouped_means <- data.frame(Freq[,1])
colnames(fgrouped_means) <- c("Time")
fgrouped_means$group1 <- 0
fgrouped_means$group2 <- 0
fgrouped_means$group3 <- 0
fgrouped_means$group4 <- 0
fgrouped_means$group5 <- 0
for (n in 1:nsamples()) {
  fgrouped_means$group1[n] <- ifelse(nrow(f_1)==1,Freq[n,f_1$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_1$Name)]))
  fgrouped_means$group2[n] <- ifelse(nrow(f_2)==1,Freq[n,f_2$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_2$Name)]))
  fgrouped_means$group3[n] <- ifelse(nrow(f_3)==1,Freq[n,f_3$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_3$Name)]))
  fgrouped_means$group4[n] <- ifelse(nrow(f_4)==1,Freq[n,f_4$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_4$Name)]))
  fgrouped_means$group5[n] <- ifelse(nrow(f_5)==1,Freq[n,f_5$Name[1]],rowMeans(Freq[n,(colnames(Freq) %in% f_5$Name)]))
}
fgroupsmelt <- melt(data = fgrouped_means,id.vars = "Time",variable.name = "Bus",value.name = "Frequency")
fplot_clust<- ggplot(data = fgroupsmelt,aes(x=Time,y=Frequency,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                        axis.text.x=element_blank(),
                                                                                                        axis.ticks.x=element_blank())


# vdissmat <- diss(SERIES =  Volt[(nsamples()-1):nsamples(),-1],METHOD = clust_type)
vdissmat <- diss(SERIES =  Volt[,-1],METHOD = clust_type)
hc_volt <- hclust(vdissmat,method = "complete")
cut_volt <- cutree(hc_volt,k = 5)
cut_volt_names <- data.frame(cut_volt,names(cut_volt))
colnames(cut_volt_names) <- c("group","Name")
v_1 <- cut_volt_names[cut_volt_names$group == 1,]
v_2 <- cut_volt_names[cut_volt_names$group == 2,]
v_3 <- cut_volt_names[cut_volt_names$group == 3,]
v_4 <- cut_volt_names[cut_volt_names$group == 4,]
v_5 <- cut_volt_names[cut_volt_names$group == 5,]
vgrouped_means <- data.frame(Volt[,1])
colnames(vgrouped_means) <- c("Time")
vgrouped_means$group1 <- 0
vgrouped_means$group2 <- 0
vgrouped_means$group3 <- 0
vgrouped_means$group4 <- 0
vgrouped_means$group5 <- 0
for (n in 1:nsamples()) {
  vgrouped_means$group1[n] <- ifelse(nrow(v_1)==1,Volt[n,v_1$Name[1]],rowMeans(Volt[n,(colnames(Volt) %in% v_1$Name)]))
  vgrouped_means$group2[n] <- ifelse(nrow(v_2)==1,Volt[n,v_2$Name[1]],rowMeans(Volt[n,(colnames(Volt) %in% v_2$Name)]))
  vgrouped_means$group3[n] <- ifelse(nrow(v_3)==1,Volt[n,v_3$Name[1]],rowMeans(Volt[n,(colnames(Volt) %in% v_3$Name)]))
  vgrouped_means$group4[n] <- ifelse(nrow(v_4)==1,Volt[n,v_4$Name[1]],rowMeans(Volt[n,(colnames(Volt) %in% v_4$Name)]))
  vgrouped_means$group5[n] <- ifelse(nrow(v_5)==1,Volt[n,v_5$Name[1]],rowMeans(Volt[n,(colnames(Volt) %in% v_5$Name)]))
}
vgroupsmelt <- melt(data = vgrouped_means,id.vars = "Time",variable.name = "Bus",value.name = "Voltage")
vplot_clust<- ggplot(data = vgroupsmelt,aes(x=Time,y=Voltage,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                      axis.text.x=element_blank(),
                                                                                                      axis.ticks.x=element_blank())

# adissmat <- diss(SERIES =  Pangle[(nsamples()-1):nsamples(),-1],METHOD = clust_type)
adissmat <- diss(SERIES =  Pangle[,-1],METHOD = clust_type)
hc_angle <- hclust(adissmat,method = "complete")
cut_angle <- cutree(hc_angle,k = 5)
cut_angle_names <- data.frame(cut_angle,names(cut_angle))
colnames(cut_angle_names) <- c("group","Name")
a_1 <- cut_angle_names[cut_angle_names$group == 1,]
a_2 <- cut_angle_names[cut_angle_names$group == 2,]
a_3 <- cut_angle_names[cut_angle_names$group == 3,]
a_4 <- cut_angle_names[cut_angle_names$group == 4,]
a_5 <- cut_angle_names[cut_angle_names$group == 5,]
agrouped_means <- data.frame(Pangle[,1])
colnames(agrouped_means) <- c("Time")
agrouped_means$group1 <- 0
agrouped_means$group2 <- 0
agrouped_means$group3 <- 0
agrouped_means$group4 <- 0
agrouped_means$group5 <- 0
for (n in 1:nsamples()) {
  agrouped_means$group1[n] <- ifelse(nrow(a_1)==1,Pangle[n,a_1$Name[1]],rowMeans(Pangle[n,(colnames(Pangle) %in% a_1$Name)]))
  agrouped_means$group2[n] <- ifelse(nrow(a_2)==1,Pangle[n,a_2$Name[1]],rowMeans(Pangle[n,(colnames(Pangle) %in% a_2$Name)]))
  agrouped_means$group3[n] <- ifelse(nrow(a_3)==1,Pangle[n,a_3$Name[1]],rowMeans(Pangle[n,(colnames(Pangle) %in% a_3$Name)]))
  agrouped_means$group4[n] <- ifelse(nrow(a_4)==1,Pangle[n,a_4$Name[1]],rowMeans(Pangle[n,(colnames(Pangle) %in% a_4$Name)]))
  agrouped_means$group5[n] <- ifelse(nrow(a_5)==1,Pangle[n,a_5$Name[1]],rowMeans(Pangle[n,(colnames(Pangle) %in% a_4$Name)]))
}
agroupsmelt <- melt(data = agrouped_means,id.vars = "Time",variable.name = "Bus",value.name = "Angle")
aplot_clust<- ggplot(data = agroupsmelt,aes(x=Time,y=Angle,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")







