data.dir   <- '~/Desktop/7406/project/'
train.file <- paste0(data.dir, 'training.csv')
d.total<- read.csv(train.file, stringsAsFactors=F)

im.total<- d.total$Image
d.total$Image <- NULL

library(foreach)
library(iterators)
library(snow)
library(doSNOW)
library(doParallel) 
registerDoParallel()
library(matrixStats)

im.total<- foreach(im = im.total, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

n = dim(d.total)[1]; ### total number of observations
p = dim(im.total)[2]; ### total number of observations


##Histogram stretching
a <- rowMins(im.total)
b <- rowMaxs(im.total)
im.total = (255/(b-a))*(im.total  - a)

##Meanface
png(filename= paste0(data.dir, 'meanfacetotal.png'))
meanface = colMeans(im.total, na.rm=T)
im <- matrix(data=rev(meanface), nrow=96, ncol=96)
meanface.total = data.frame(t(meanface))
image(1:96, 1:96, im, col=gray((0:255)/255))
dev.off()

#Save first 20 faces
for (i in 1:20){
  png(filename= paste0(data.dir,100 + i,'face.png'))
  im <- matrix(data=rev(unlist(t(im.total[i,]))), nrow=96, ncol=96)
  image(1:96, 1:96, im, col=gray((0:255)/255))
  dev.off()
}

##Select training and testing data
n1 = round(n/5); 
set.seed(20160201);
flag = sort(sample(1:n, n1));
d.test= d.total[flag,]; 
im.test = im.total[flag,]; 
d.train= d.total[-flag,]; 
im.train = im.total[-flag,]
