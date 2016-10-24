##PCA
impca<- prcomp(im.total)
eigenfaces = data.frame(impca$rotation)
pcomp <-200
im.total2   <- as.matrix(im.total)%*%as.matrix(eigenfaces[ ,1:pcomp])%*%
  as.matrix(t(eigenfaces[ ,1:pcomp]))

a <- rowMins(im.total2)
b <- rowMaxs(im.total2)
im.total2 = (255/(b-a))*(im.total2  - a)


##Meanface2
png(filename= paste0(data.dir, 'meanfacetotal2.png'))
meanface = colMeans(im.total2, na.rm=T)
im <- matrix(data=rev(meanface), nrow=96, ncol=96)
meanface.total = data.frame(t(meanface))
image(1:96, 1:96, im, col=gray((0:255)/255))
dev.off()

#Save first transformed 20 faces
for (i in 1:20){
  png(filename= paste0(data.dir,100 + i,'face2.png'))
  im <- matrix(data=rev(unlist(t(im.total2[i,]))), nrow=96, ncol=96)
  image(1:96, 1:96, im, col=gray((0:255)/255))
  dev.off()
}

#Eigenfaces
for (i in 1:20){
  a = min(eigenfaces[,i])
  b = max(eigenfaces[,i])
  imeigenface = (255/(b-a))*(eigenfaces[,i] - a)
  png(filename= paste0(data.dir,100 + i,'eigenface.png'))
  im <- matrix(data=rev(unlist(t(imeigenface))), nrow=96, ncol=96)
  image(1:96, 1:96, im, col=gray((0:255)/255))
  dev.off()
}

##Select training and testing data
n1 = round(n/5); 
set.seed(20160201);
flag = sort(sample(1:n, n1));
d.test= d.total[flag,]; 
im.test = im.total2[flag,]; 
d.train= d.total[-flag,]; 
im.train = im.total2[-flag,]