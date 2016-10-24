##Laplacian Filter
library(adimpro)

im.total2 = im.total

for (i in 1:nrow(d.total)){
  img <-im.total[i,]
  img2<-img
  
  a = min(img)
  b = max(img)
  img2 = (1/(b-a))*(img - a)
  img3 <- matrix(data=rev(unlist(img2)), nrow=96, ncol=96)
  img4<-make.image(img3)
  img4edges <- edges(img4, type = "Laplacian", ltype=1, abs=FALSE)
  img5<-extract.image(img4edges)
  img6<-rev(array(img5))  

  a = min(img6)
  b = max(img6)
  im.total2[i,] = (255/(b-a))*(img6 - a)
}

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

##Select training and testing data
n1 = round(n/5); 
set.seed(20160201);
flag = sort(sample(1:n, n1));
d.test= d.total[flag,]; 
im.test = im.total2[flag,]; 
d.train= d.total[-flag,]; 
im.train = im.total2[-flag,]

