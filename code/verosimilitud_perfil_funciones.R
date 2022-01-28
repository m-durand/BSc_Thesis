#verosimilitud perfil DVEG

path1 <- "..."

setwd(path1)


alpha=0.05
datos<-read.table(file="indicadores.txt",sep="\t", header=FALSE)
datos<-(as.numeric(datos))

#DIFERENTES MUS. 
#Weibull
#Mualpha<-Qalpha - sigma (-log(1-alpha)^(1/xi))
#Gumbel
#Mualpha<-Qalpha - sigma (-log(1-alpha))
#Frechet
#Mualpha<-Qalpha + sigma (-log(1-alpha)^(-1/xi))

#en el paso anterior se sugiere un modelo Weibull
#Tambien habria que checar con Frechet - el cero queda capturado

sigmita <- function(mu,ji){
  x <- mu
  y <- ji
  #all
  #p00 <- 116.8
  #p10 <- -0.9284
  #p01 <- -68.06
  #p20 <- 5.024e-15
  #p11 <- 0.5441
  #p02 <- 0.9925
  
 #after 1995 
  p00 <- -76.38  
  p10 <- 1.494  
  p01<- 16.28  
  p20<- -0.006184 
  p11<- -0.1605  
  p02<- -0.1861 
  sigmita <- p00 + p10*x + p01*y + p20*x**2 + p11*x*y + p02*y**2
  return(sigmita)
}

l<-function(x,mod="w",take=20){
  Qalpha<-x[1]
  sigma<-x[2]
  xi<-x[3]
  #una sola eleccion de modelo. los datos sugieren weibull
  if(mod=="g"){Mualpha<-Qalpha - sigma*(log(-log(1-alpha)))
               each<-(1/sigma)*((datos-Mualpha)/sigma)*exp(-exp((datos-Mualpha)/sigma))
  }else{
    if(mod=="w"){Mualpha<-Qalpha - sigma*((-log(1-alpha))^(-1/xi))
              each<-(1/sigma) * ((1+ xi*(datos-Mualpha)/sigma)^((-1/xi)-1)) * exp(-((1+xi*((datos-Mualpha)/sigma)^(-1/xi))))
    }else{ 
      if(mod=="f"){Mualpha<-Qalpha + sigma*((-log(1-alpha))^(1/xi))
              each<-(1/sigma) * ((1+ xi*(datos-Mualpha)/sigma)^((-1/xi)-1)) * exp(-((1+xi*((datos-Mualpha)/sigma)^(-1/xi))))
      }
    } 
   }
  #para deshacerme del cero máquina. ¿se vale?
  vector<-each
  maximos<-rep(0,19)
  for (i in 1:take)
  {
    maximos[i]=max(vector)
    vector[which.max(vector)]=0
  }
  #verosimilitud del vector
  veros<-prod(maximos)
  print(sum(each))
  list(veros=veros, each=each)
}

l.profile.s = function(s, y) {
    l.slice = function(alpha) l(c(alpha, s), y)
    opt.slice = optim(1, l.slice, method="BFGS")
    opt.slice$value
}

rr<-l(c(100,10.37,-0.13),mod="w",take=20)

verosl<-function(x,mod="w",take=20){l(x,mod,take)$veros}

LL <- function(mu,sigma,xi) {
  Qalpha <- mu
  logo <- log(1-alpha)
  nlogo <- -1*logo
  expo <- -1/xi
  siginv <- 1/sigma
  Mualpha <- Qalpha - sigma*nlogo**expo
  inexp <- ((x-Mualpha)/sigma)
  R = suppressWarnings(((1+xi*inexp)**(expo-1))*siginv*exp(-1*(1+xi*inexp)**expo))
  -sum(log(R), na.rm=TRUE) 
}

LLdedevis <- function(mu,xi) {
  mu <- mu
  xi <- xi
  sigma <- sigmita(mu,xi)
  Qalpha <- mu
  logo <- log(1-alpha)
  nlogo <- -1*logo
  expo <- -1/xi
  siginv <- 1/sigma
  Mualpha <- Qalpha - sigma*nlogo**expo
  inexp <- ((x-Mualpha)/sigma)
  R = suppressWarnings(((1+xi*inexp)**(expo-1))*siginv*exp(-1*(1+xi*inexp)**expo))
  suppressWarnings(-sum((R), na.rm=TRUE))
  
}

LL2 <- function(mu,sigma,xi) {
  mu <- mu
  xi <- xi
  sigma <- sigma
  Qalpha <- mu
  logo <- log(1-alpha)
  nlogo <- -1*logo
  expo <- -1/xi
  siginv <- 1/sigma
  Mualpha <- Qalpha - sigma*nlogo**expo
  inexp <- ((x-Mualpha)/sigma)
  R = suppressWarnings(((1+xi*inexp)**(expo-1))*siginv*exp(-1*(1+xi*inexp)**expo))
  suppressWarnings(-sum((R), na.rm=TRUE))
  
}


#fit
firstorder<-function(x){
  Qalpha<-x[1]
  sigma<-x[2]
  xi<-x[3]
  
  Mualpha<-Qalpha - sigma*(-log(1-alpha)^(1/xi))
   c<- -1/beta
   b<- -sigma/beta
   a<- Mualpha-sigma
 ynew<-(1/b)*((1+c*((z-a)/b))^(-(1+(1/c))))* exp (-(1+c*((z-a)/b))^(-1/c))
  
  
  RMSE<-prod(ynew)
  return(RMSE)
}

