d.train.mean<-data.frame(t(colMeans(d.train, na.rm = TRUE)));
p<-0*d.test;
  
##Patch Linear regression
library(reshape2)
patch_size  <- 10
search_size <- 2

# list the coordinates we have to predict
coordinate.names <- gsub("_x", "", names(d.train)[grep("_x", names(d.train))])

# for each one, compute the average patch
for(coord in coordinate.names) {
  cat(sprintf("computing regression for %s\n", coord))
  coord_x <- paste(coord, "x", sep="_")
  coord_y <- paste(coord, "y", sep="_")
  
  # compute regression
  patches <- foreach (i = 1:nrow(d.train), .combine=rbind) %do% {
    im  <- matrix(data = im.train[i,], nrow=96, ncol=96)
    x   <- d.train.mean[1,coord_x]
    y   <- d.train.mean[1,coord_y]
    x1  <- (x-patch_size)
    x2  <- (x+patch_size)
    y1  <- (y-patch_size)
    y2  <- (y+patch_size)
    if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96) )
    {
      as.vector(im[x1:x2, y1:y2])
    }
    else
    {
      NULL
    }
  }
  
  train_x<-cbind(d.train[,coord_x],data.frame(patches))
  colnames(train_x)[1]<-"x"
  train_y<-cbind(d.train[,coord_y],data.frame(patches))
  colnames(train_y)[1]<-"y"
  fitx <- lm( x ~ .,data=train_x);
  fity <- lm( y ~ .,data=train_y);
  
  
  patchtest <- foreach (i = 1:nrow(d.test), .combine=rbind) %do% {
    im  <- matrix(data = im.test[i,], nrow=96, ncol=96)
    x   <- d.train.mean[1,coord_x]
    y   <- d.train.mean[1,coord_y]
    x1  <- (x-patch_size)
    x2  <- (x+patch_size)
    y1  <- (y-patch_size)
    y2  <- (y+patch_size)
    if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96) )
    {
      as.vector(im[x1:x2, y1:y2])
    }
    else
    {
      NULL
    }
  }
  
  test<-data.frame(patchtest)
  p[,coord_x]<-predict(fitx,test)
  p[,coord_y]<-predict(fity,test)

}

#Mean Squared Error
TMSE<-data.frame(sqrt(mean((d.test-p)^2, na.rm=T)))
MSE<-data.frame(t(rep(0,15)))
for (i in 1:15){
  MSE[i]<-sqrt(mean((d.test[,(2*i-1):(2*i)]-p[,(2*i-1):(2*i)])^2, na.rm=T))
}
