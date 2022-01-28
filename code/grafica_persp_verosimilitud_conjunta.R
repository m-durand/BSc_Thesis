#verosimilitud conjunta
dat<-rnorm(100,4,2)
verosim<-function(mu,sig,dat){
  exp(-sum((dat-mu)^2)/(2*sig^2)+100)/(sig)^length(dat)
}

verosim(4,2,dat)

verosim.mu.v<-function(sigma,mu.v,dat){sapply(mu.v,FUN=verosim,sig=sigma,dat=dat)}

mu.v<-seq(-10,10,length=10)
verosim.mu.v(3,mu.v,dat)
sigma.v<-seq(0.1,10,length=5)
z<-sapply(sigma.v,FUN=verosim.mu.v,mu.v=mu.v,dat=dat)

mu.v<-seq(-10,10,length=20)
sigma.v<-seq(0.1,10,length=20)
z<-sapply(sigma.v,FUN=verosim.mu.v,mu.v=mu.v,dat=dat)
persp(x = mu.v, y = sigma.v, z = z)

mu.v<-seq(3,5, length=25)
sigma.v<-seq(1.4,2.2,length=25)
z<-sapply(sigma.v,FUN=verosim.mu.v,mu.v=mu.v,dat=dat)
persp(x = mu.v, y = sigma.v, z = z)
#theta and phi control the viewing angle.
#theta moves the viewing angle left and right
#phi moves it up and down.
persp(x = mu.v, y = sigma.v, z = z,theta=45)
persp(x = mu.v, y = sigma.v, z = z,theta=45,phi=20)
par(mar=c(0,0,0,0))
persp(x = mu.v, y = sigma.v, z = z,theta=45,phi=20,col=rgb(0,0,1,0.2))

persp(x = mu.v, y = sigma.v, z = z,theta=45,phi=20,col=rgb(0,0,1,0.2),xlab="mu",ylab="sigma",zlab="verosimilitud",border=grey(.5))
?persp

persp(x = mu.v, y = sigma.v, z = z,theta=45,phi=20,col=rgb(0,0,1,0.2),xlab="mu",ylab="sigma",zlab="verosimilitud",border=grey(.5),box=FALSE)
par(new=TRUE) 
persp(x = mu.v, y = sigma.v, z = matrix(1.5e-4,length(mu.v),length(sigma.v)),zlim=c(0,1e-3),theta=45,phi=20,col=rgb(1,0,0,0.2),xlab="mu",ylab="sigma",zlab="verosimilitud",border=grey(.5),box=FALSE,add=TRUE)


persp(x = mu.v, y = sigma.v, z = matrix(0,length(mu.v),length(sigma.v)),zlim=c(0,1e-3),theta=45,phi=20,col=rgb(1,0,0,0.2),xlab="mu",ylab="sigma",zlab="verosimilitud",border=grey(.5),box=FALSE,add=TRUE)
par(new=TRUE) 
persp(x = mu.v, y = sigma.v, z = z,theta=45,phi=20,col=rgb(0,0,1,0.2),xlab="mu",ylab="sigma",zlab="verosimilitud",border=grey(.5),box=FALSE)



library(plot3D)
persp3D(x = mu.v, y = sigma.v, z = z,theta=45,phi=20,xlab="mu",ylab="sigma",zlab="verosimilitud")

persp3D(x = mu.v, y = sigma.v, z = z,theta=45,phi=20,col=rgb(0,0,1,0),xlab="mu",ylab="sigma",zlab="verosimilitud",border=rgb(0,0,0,0))
persp3D(x = mu.v, y = sigma.v, z = matrix(0,length(mu.v),length(sigma.v)),theta=45,phi=20,col="white",xlab="mu",ylab="sigma",zlab="verosimilitud",border=grey(.3),add=TRUE)
persp3D(x = mu.v, y = sigma.v, z = z,theta=45,phi=20,col=rgb(0,0,1,0.2),xlab="mu",ylab="sigma",zlab="verosimilitud",border=grey(.3),add=TRUE)

?scatter3D
points3D(4,2, 0.002859406,add=TRUE,pch=19)
points3D(4.5,2, 0,add=TRUE,pch=19,cex=.9)

max.mu.v<-rep(mean(dat),length(sigma.v))
max.sigma.v<-
  

