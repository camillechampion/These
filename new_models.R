N=100
K=5
Nc <- matrix(0,ncol=K,nrow=1)
Nc=sample(50:100,K)


simulation1 <- function(Nc,K,N){
  rmin=0.5
  rmax=1

  X <- matrix(0,ncol=N,nrow=sum(Nc))
  nc=Nc[1]
  x1 <- matrix(0,ncol=N,nrow=nc)
  x1[1,] <- rnorm(N,0,1)
  r <- matrix(0,ncol=nc-1,nrow=1)
  epsilon <- matrix(0,ncol=N,nrow=nc)
  epsilon[1,] <- rnorm(N,0,1)
  r[1] <- rmin +(rmax-rmin)*(1-(1/nc))
  for (i in 2:nc){
    r[i] <- rmin +(rmax-rmin)*(1-(i/nc))
    epsilon[i,] <- rnorm(N,0,1)
    x1[i,] <- x1[1,]+ sqrt((1/(r[i])^2)-1)*epsilon[i,]
  }
  
  for(j in 2:K){
    nc=Nc[j]
    x <- matrix(0,ncol=N,nrow=nc)
    x[1,] <- rnorm(N,0,1)
    r <- matrix(0,ncol=nc-1,nrow=1)
    epsilon <- matrix(0,ncol=N,nrow=nc)
    epsilon[1,] <- rnorm(N,0,1)
    r[1] <- rmin +(rmax-rmin)*(1-(1/nc))
    for (i in 2:nc){
      r[i] <- rmin +(rmax-rmin)*(1-(i/nc))
      epsilon[i,] <- rnorm(N,0,1)
      x[i,] <- x[1,]+ sqrt((1/(r[i])^2)-1)*epsilon[i,]
    }
    X <- rbind(x1,x)
    x1 <- X
  }
  return(X)
}

simu1 <- simulation1(Nc,K,N)  
S1 <- cor(t(simu1))  

Nc <- matrix(0,ncol=K,nrow=1)
Nc=sample(50:100,K)

simulation2 <- function(Nc,K,N){
  armin <- c(0.4,0.5)
  rmin <- sample(armin,1)
  rmax=1
  if(rmin==0.4){
    rmax=0.7
  }
  X <- matrix(0,ncol=N,nrow=sum(Nc))
  nc=Nc[1]
  x1 <- matrix(0,ncol=N,nrow=nc)
  x1[1,] <- rnorm(N,0,1)
  r <- matrix(0,ncol=nc-1,nrow=1)
  epsilon <- matrix(0,ncol=N,nrow=nc)
  epsilon[1,] <- rnorm(N,0,1)
  r[1] <- rmin +(rmax-rmin)*(1-(1/nc))
  for (i in 2:nc){
    r[i] <- rmin +(rmax-rmin)*(1-(i/nc))
    epsilon[i,] <- rnorm(N,0,1)
    x1[i,] <- x1[1,]+ sqrt((1/(r[i])^2)-1)*epsilon[i,]
  }
  
  
  for(j in 2:K){
    rmin <- sample(armin,1)
    rmax=1
    if(rmin==0.4){
      rmax=0.7
    }
    nc=Nc[j]
    x <- matrix(0,ncol=N,nrow=nc)
    x[1,] <- rnorm(N,0,1)
    r <- matrix(0,ncol=nc-1,nrow=1)
    epsilon <- matrix(0,ncol=N,nrow=nc)
    epsilon[1,] <- rnorm(N,0,1)
    r[1] <- rmin +(rmax-rmin)*(1-(1/nc))
    for (i in 2:nc){
      r[i] <- rmin +(rmax-rmin)*(1-(i/nc))
      epsilon[i,] <- rnorm(N,0,1)
      x[i,] <- x[1,]+ sqrt((1/(r[i])^2)-1)*epsilon[i,]
    }
    X <- rbind(x1,x)
    x1 <- X
  }
  return(X)
}

simu2 <- simulation2(Nc,K,N)
s2 <- cor(t(simu2))

simulation3 <- function(Nc,K,N){
  rmin=0.5
  rmax=1
  cor<-matrix(c(1,.8,.8,1),byrow=TRUE,nrow=2)
  leader<-mvrnorm(N,mu=c(0,0),Sigma=cor)
  
  X <- matrix(0,ncol=N,nrow=sum(Nc))
  nc=Nc[1]
  x1 <- matrix(0,ncol=N,nrow=nc)
  x1[1,] <- t(leader[,1])
  r <- matrix(0,ncol=nc-1,nrow=1)
  epsilon <- matrix(0,ncol=N,nrow=nc)
  r[1] <- rmin +(rmax-rmin)*(1-(1/nc))
  
  for (i in 2:nc){
    r[i] <- rmin +(rmax-rmin)*(1-(i/nc))
    epsilon[i,] <- rnorm(N,0,1)
    x1[i,] <- x1[1,]+ sqrt((1/(r[i])^2)-1)*epsilon[i,]
  }
  
  
  nc=Nc[2]
  epsilon <- matrix(0,ncol=N,nrow=nc)
  x2 <- matrix(0,ncol=N,nrow=nc)
  
  x2[1,] <- t(leader[,2])
  
  r[1] <- rmin +(rmax-rmin)*(1-(1/nc))
  for (i in 2:nc){
    r[i] <- rmin +(rmax-rmin)*(1-(i/nc))
    epsilon[i,] <- rnorm(N,0,1)
    x2[i,] <- x2[1,]+ sqrt((1/(r[i])^2)-1)*epsilon[i,]
  }
  X <- rbind(x1,x2)
  
  
  
  for(j in 3:K){
    rmin=0.5
    rmax=1
    nc=Nc[j]
    x <- matrix(0,ncol=N,nrow=nc)
    x[1,] <- rnorm(N,0,1)
    r <- matrix(0,ncol=nc-1,nrow=1)
    epsilon <- matrix(0,ncol=N,nrow=nc)
    epsilon[1,] <- rnorm(N,0,1)
    r[1] <- rmin +(rmax-rmin)*(1-(1/nc))
    for (i in 2:nc){
      r[i] <- rmin +(rmax-rmin)*(1-(i/nc))
      epsilon[i,] <- rnorm(N,0,1)
      x[i,] <- x[1,]+ sqrt((1/(r[i])^2)-1)*epsilon[i,]
    }
    X <- rbind(X,x)
    
  }
  return(X)
}

simu3 <- simulation3(Nc,K,N)
s3 <- cor(t(simu3))

simulation4 <- function(Nc,K,N){
  rmin=0.5
  rmax=1
  set <- c(0.1,0.5,1)
  set_select <- sample(set,1)
  
  
  
  X <- matrix(0,ncol=N,nrow=sum(Nc))
  nc=Nc[1]
  x1 <- matrix(0,ncol=N,nrow=nc)
  x1[1,] <- rnorm(N,0,1)
  r <- matrix(0,ncol=nc-1,nrow=1)
  epsilon <- matrix(0,ncol=N,nrow=nc)
  epsilon[1,] <- rnorm(N,0,1)
  r[1] <- rmin +(rmax-rmin)*(1-(1/nc))
  for (i in 2:nc){
    r[i] <- rmin +(rmax-rmin)*(1-(i/nc))
    epsilon[i,] <- rnorm(N,0,1)
    x1[i,] <- x1[1,]+ sqrt((1/(r[i])^2)-1)*epsilon[i,]
  }
  noise <- rnorm(K,0,sd=set_select)
  x1 <- x1+noise
  
  for(j in 2:K){
    nc=Nc[j]
    x <- matrix(0,ncol=N,nrow=nc)
    x[1,] <- rnorm(N,0,1)
    r <- matrix(0,ncol=nc-1,nrow=1)
    epsilon <- matrix(0,ncol=N,nrow=nc)
    epsilon[1,] <- rnorm(N,0,1)
    r[1] <- rmin +(rmax-rmin)*(1-(1/nc))
    
    for (i in 2:nc){
      r[i] <- rmin +(rmax-rmin)*(1-(i/nc))
      epsilon[i,] <- rnorm(N,0,1)
      x[i,] <- x[1,]+ sqrt((1/(r[i])^2)-1)*epsilon[i,]
    }
    noise <- rnorm(K,0,sd=set_select)
    x <- x+noise
    X <- rbind(x1,x)
    x1 <- X
    
  }

  
  
  
  
  return(X)
  
  
}

simu4 <- simulation4(Nc,K,N)
s4 <- cor(t(simu4))


simulation5 <- function(Nc,K,N){
  rmin=0.5
  rmax=1
  
  X <- matrix(0,ncol=N,nrow=sum(Nc))
  Irrelevant <- matrix(0,ncol=N,nrow=sum(Nc))
  nc=Nc[1]
  x1 <- matrix(0,ncol=N,nrow=nc)
  irrelevant1 <- matrix(0,ncol=N,nrow=nc)
  x1[1,] <- rnorm(N,0,1)
  r <- matrix(0,ncol=nc-1,nrow=1)
  epsilon <- matrix(0,ncol=N,nrow=nc)
  epsilon[1,] <- rnorm(N,0,1)
  r[1] <- rmin +(rmax-rmin)*(1-(1/nc))
  irrelevant1[1,] <- rnorm(N,0,1)
  for (i in 2:nc){
    r[i] <- rmin +(rmax-rmin)*(1-(i/nc))
    epsilon[i,] <- rnorm(N,0,1)
    x1[i,] <- x1[1,]+ sqrt((1/(r[i])^2)-1)*epsilon[i,]
    irrelevant1[i,] <- rnorm(N,0,1)
  }
  
  
  
  for(j in 2:K){
    nc=Nc[j]
    x <- matrix(0,ncol=N,nrow=nc)
    irrelevant <- matrix(0,ncol=N,nrow=nc)
    x[1,] <- rnorm(N,0,1)
    r <- matrix(0,ncol=nc-1,nrow=1)
    epsilon <- matrix(0,ncol=N,nrow=nc)
    epsilon[1,] <- rnorm(N,0,1)
    r[1] <- rmin +(rmax-rmin)*(1-(1/nc))
    irrelevant[1,] <- rnorm(N,0,1)
    for (i in 2:nc){
      r[i] <- rmin +(rmax-rmin)*(1-(i/nc))
      epsilon[i,] <- rnorm(N,0,1)
      x[i,] <- x[1,]+ sqrt((1/(r[i])^2)-1)*epsilon[i,]
      irrelevant[i,] <- rnorm(N,0,1)
    }
    X <- rbind(x1,x)
    x1 <- X
    Irrelevant <- rbind(irrelevant1,irrelevant)
    irrelevant1 <- Irrelevant
  }
  Y <- rbind(X,Irrelevant)
  return(Y)
}
simu5 <- simulation5(Nc,K,N)
s5 <- cor(t(simu5))

## Adjacency graphical plot
# package Sparse M

#close the existing figure
plot.new() #open a new figure
frame()
#plotly



image(S1)
image(s2)
image(s3)
image(s4)
image(s5)

spect <- spectralClustering(S1,K=5)

Dunn_index=function(label,Data){
  distance <- as.matrix(dist(Data, method="euclidean"))
  unique_clusters <- length(unique(label))
  clust_diff <- matrix(NA,ncol=unique_clusters,nrow=unique_clusters)
  clust_sim <- matrix(NA,ncol=unique_clusters,nrow=1)
  for (i in 1:unique_clusters) {
    clust_ref <- which(label==i)
    for (j in i:unique_clusters) {
      if (j==i) clust_sim[i] <- max(distance[clust_ref,clust_ref])
      if (j>i) {
        clust_ref2 <- which(label==j)
        clust_diff[i,j] <- min(distance[clust_ref,clust_ref2])
      }
    }
  }
  dunn <- min(clust_diff,na.rm=TRUE)/max(clust_sim)
  return(dunn)
  
}
library(SNFtool)
classes <- c(2:10)
Dunn <- matrix(0,nrow=1,ncol=length(classes))
for (i in 1:length(classes)){
    for ( l in 1:50){
    Nc <- matrix(0,ncol=classes[i],nrow=1)
    Nc=sample(50:100,classes[i])
    simu1<- simulation1(Nc,classes[i],N)
    s1 <- abs(cor(t(simu1)))
    spectral <- spectralClustering(s1, K = classes[i])
    Dunn[i] <- Dunn_index(spectral,simu1)
  }
  
}
simu2 <- simulation2(Nc,K,N)
simu3 <- simulation3(Nc,K,N)
simu4 <- simulation4(Nc,K,N)
simu5 <- simulation5(Nc,K,N)

s2 <- cor(t(simu2))
s3 <- cor(t(simu3))
s4 <- cor(t(simu4))
s5 <- cor(t(simu5))


modularity_index=function(cluster,Data){
  max_cluster <- max(unique(cluster))
  Q=matrix(0,nrow=1,ncol=max_cluster)
  Data2 <- Data
  Data2[upper.tri(Data2, TRUE)] <- NA
  Data3 <- Data
  diag(Data3) <- 0
  m <- sum(Data2,na.rm=TRUE)
  for (i in 1:max_cluster){
    indiv_clust <-which(cluster==i)
    #lc <- dim(Data[indiv_clust,indiv_clust])[1]*dim(Data[indiv_clust,indiv_clust])[2]
    na <- length(which(is.na(Data)))
    dc <- length(which(Data3[,indiv_clust]!=0))
    #dc <- sum(Data[indiv_clust,])
    lc <- sum(Data[,indiv_clust])
    Q[i] <-(lc/m)-((dc/(2*m))^2)
  }
  return(sum(Q))
}
modularity_index(spectral,s1)
