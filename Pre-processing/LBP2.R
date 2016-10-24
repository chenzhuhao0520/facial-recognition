##LBP
im.total2 <- im.total
for (i in 1:nrow(d.total)){
  im <- matrix(data=rev(unlist(t(im.total[i,]))), nrow=96, ncol=96)
  im2<- 0*im
  for(j in 2:95){
    for(k in 2:95){
      centerPixel <-im[j, k];
      pixel7<-im[j-1, k-1]> centerPixel;  
      pixel6<-im[j-1, k] > centerPixel;   
      pixel5<-im[j-1, k+1]> centerPixel;  
      pixel4<-im[j, k+1] > centerPixel;     
      pixel3<-im[j+1, k+1]> centerPixel;    
      pixel2<-im[j+1, k]> centerPixel;      
      pixel1<-im[j+1, k-1]> centerPixel;     
      pixel0<-im[j, k-1]> centerPixel;       
      im2[j, k] = pixel7 * 2^7 + pixel6 * 2^6 + pixel5 * 2^5 + 
        pixel4 * 2^4 + pixel3 * 2^3 + pixel2 * 2^2 + pixel1 * 2 + pixel0;
    }
  }
  im3<-rev(array(im2)) 
  a = min(im3)
  b = max(im3)
  im.total2[i,] = (255/(b-a))*(im3 - a)
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

