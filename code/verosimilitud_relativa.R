path1 <- "..."

setwd(path1)

x<-c(4,65,23,12,54,23,31,28,49,27)
theta1<-1:1000
theta<-theta1/10
sum=sum(x)
n=length(x)
N=length(theta)
L<-1:length(theta)
pint1<-1:length(theta)
pint2<-1:length(theta)
int853<-1:2
int964<-1:2

#hace verosimilitud
for (i in 1:length(theta)){L[i]= theta[i]^(-n) * exp (-((1/theta[i])*sum))
}

#verosimilitud relativa
RL=L/max(L)

x11()

#para intervalos de verosimilitud
for (i in 1:length(theta)){if (RL[i] >= .853) {pint1[i] <- 1
                           } 
                           else {pint1[i]=0
                                }
                            }
k<-1
while(pint1[k]==0){
  k=k+1
}

j<-1
while(pint1[N-j]==0){
  j=j+1
}

int853<-c(theta[k],theta[N-j])


for (i in 1:length(theta)){if (RL[i] >= .964) {pint2[i] <- 1 
                            } 
                           else {pint2[i]=0
                                }
                            }


k<-1
while(pint2[k]==0){
  k=k+1
}

j<-1
while(pint2[N-j]==0){
  j=j+1
}

int964<-c(theta[k],theta[N-j])

#COMO HAGO QUE EL EJE DE ABAJO QUEDE DE 0 A 100
plot(RL,type="l",main="Verosimilitud Relativa",
     xlab=expression(theta),
     ylab= expression(paste("R ( ", theta, " )"))
     )
abline(h=.853)
abline(h=.964)

#intervalos con cada nivel de confianza
int853
int964

text(43, .823, "c=.853", cex = .8)
text(213, .823, expression(paste(theta,"=26.6")), cex = .6)
text(433, .823, expression(paste(theta,"=37.9")), cex = .6)

text(43, .934, "c=.964", cex = .8)
text(243, .934, expression(paste(theta,"=29.1")), cex = .6)
text(393, .934, expression(paste(theta,"=34.4")), cex = .6)

#EMV
m<-1
while(RL[m]<1){
  m=m+1
}

thetagorro<-theta[m]

text(400, 1, expression(paste(hat(theta),"=31.6")),cex = .8)
abline(v=316,untf = FALSE)