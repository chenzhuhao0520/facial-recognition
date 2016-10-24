##Patch regression
## knn
library(FNN)
library(reshape2)
patch_size  <- 10
search_size <- 2
kk<-3:10
TMSE<-matrix(0,1,15);
MSE<-matrix(0,15,15);

d.train.mean<-data.frame(t(colMeans(d.train, na.rm = TRUE)));

# list the coordinates we have to predict
coordinate.names <- gsub("_x", "", names(d.train)[grep("_x", names(d.train))])


for (m in kk) {
  cat(sprintf("k = %s\n", toString(m)))
  p<-0*d.test;

# for each one, compute the average patch
for(coord in coordinate.names) {
  cat(sprintf("computing regression for %s\n", coord))
  coord_x <- paste(coord, "x", sep="_")
  coord_y <- paste(coord, "y", sep="_")
  
  # compute regression
  xx<-NULL
  yy<-NULL
  idx<-NULL
  patches <- foreach (i = 1:nrow(d.train), .combine=rbind) %do% {
    im  <- matrix(data = im.train[i,], nrow=96, ncol=96)
    x   <- d.train.mean[1,coord_x]
    xi  <- d.train[i, coord_x]
    y   <- d.train.mean[1,coord_y]
    yi  <- d.train[i, coord_y]
    x1  <- (x-patch_size)
    x2  <- (x+patch_size)
    y1  <- (y-patch_size)
    y2  <- (y+patch_size)
    if ( (!is.na(xi)) && (!is.na(yi)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96) )
    {
      xx<-rbind(xx,xi)
      yy<-rbind(yy,yi)
      as.vector(im[x1:x2, y1:y2])
    }
    else
    {
      NULL
    }
  }

  train_x<-cbind(data.frame(xx),data.frame(patches))
  colnames(train_x)[1]<-"x"
  train_y<-cbind(data.frame(yy),data.frame(patches))
  colnames(train_y)[1]<-"y"

  patchtest <- foreach (i = 1:nrow(d.test), .combine=rbind) %do% {
    im  <- matrix(data = im.test[i,], nrow=96, ncol=96)
    x   <- d.train.mean[1,coord_x]
    xi  <- d.test[i, coord_x]
    y   <- d.train.mean[1,coord_y]
    yi  <- d.test[i, coord_y]
    x1  <- (x-patch_size)
    x2  <- (x+patch_size)
    y1  <- (y-patch_size)
    y2  <- (y+patch_size)
    if ( (!is.na(xi)) && (!is.na(yi)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96) )
    {
      idx<-rbind(idx,i)
      as.vector(im[x1:x2, y1:y2])
    }
    else
    {
      NULL
    }
  }
  test<-data.frame(patchtest)
  train <-train_x[-1]
  trainx <-train_x[1]
  trainy <-train_y[1]
  
  testx<- knn.reg(train, test, trainx, k=m);	
  testy<- knn.reg(train, test, trainy, k=m);	
  p[idx,coord_x] <- data.frame(testx$pred);
  p[idx,coord_y]  <- data.frame(testy$pred)
  
}  
  
  #Mean Squared Error
  TMSE[m]<-data.frame(sqrt(mean((d.test-p)^2, na.rm=T)))
  for (i in 1:15){
    MSE[m,i]<-sqrt(mean((d.test[,(2*i-1):(2*i)]-p[,(2*i-1):(2*i)])^2, na.rm=T));
  }
  
}

TMSE2<-data.frame(TMSE)








