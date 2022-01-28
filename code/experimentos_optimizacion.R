
#Initial parameter values
params<-c(1,1)
#Optimize
optim(params, function(x) firstorder(x[1], x[2]))


fit1.1 <- optim(par=c(120,5,0.15), fn=l, method="BFGS", hessian=TRUE)
fit1.1   

#
fit1.1$se = 1/sqrt(fit1.1$hessian)
#fit1.1$se = sqrt(diag(solve(fit4.1$hessian))) 
fit1.1$par + 1.96 * fit1.1$se
fit1.1$par - 1.96 * fit1.1$se
#fit1.1$par+qnorm(.975)*fit4.1$se
fit1.1$wald = fit1.1$par + c(-1,1) * 1.96 * fit1.1$se
fit1.1$wald


fit4.s.1 = optim(c(1,15,-0.1), l.profile.s, method="BFGS", y=datos)

l.profile.s.vec = Vectorize(l.profile.s, "s")

curve(l.profile.s.vec(x, datos)-fit1.1$value,
        1, 1,
        xlab="s", ylab="Drop in profile log-likelihood for s")
abline(h=qnorm(.975)ˆ2/2, lty=2)
abline(v=fit1.1$wald["s",], lty=3)
fit4.s.1$lrci = lr.ci(
                fit4.s.1,
                l.profile.s,
                c(1, 1),
                y=datos)
fit4.s.1$lrci
abline(v=fit4.s.1$lrci, lty=4)
legend("top",
      c("Level for profile LR CI", "Wald CI",
      "Profile LR CI"),
      lty=2:4, bg="white")

#no está funcionando bien, no cambia los valores de los parámetros
optimx(c(100,10.37,-0.13), fn=verosl)
optim(c(150,10,-0.2), fn=l)



#qalpha es un cuantil, necesitamos un valor grande
#sigma=1 supongo que es razonable
#xi es un parámetro que naturalmente es cercano a cero

##########
#aqui seria mas razonable poner los estimadores que se obtuvieron en el inciso anterior
params<-c(1000,10.37,-0.13)
optim(params, fn=function(x){l(x,mod="w")})

###si esto sirve, falta hacer los intervalos
#fija el mayor valor de verosimilitud??


#########
########
######
#####
####
###
##
#ejemplo que encontré en internet
xdata<-read.table(file="indicadores.txt",sep="\t", header=FALSE)
ydata<-2*exp(-2*(xdata+rnorm(100)))




$\xi$ & -0.8394 & -0.4458 & -0.05231\\
$\sigma$ & 5.4139 & 10.0118 & 14.6099\\
$\mu$ & 92.9612 & 99.3014 & 105.6416\\

sigma <- 10
mu <- 99
xi <- -0.44
Qalpha <- mu
alpha=0.05
#seed<-c(100 , 15 , -0.15)
#mu <- seed[1]
#Qalpha<-seed[1]
#sigma<-seed[2]
#xi<-seed[3]

#mu <- Qalpha

datos<-read.table(file="indicadores1995.txt",sep="\t", header=FALSE)
datos<-(as.numeric(datos))

#n <- 1000
#stat = data.frame(matrix(vector(), n, 3, dimnames=list(c(), c("Qalpha", "sigma", "xi"))), stringsAsFactors=F)

b1<-vector()
n <- 1000

for (i in 1:n)
{
  print(i)
  pos <- sample(1:(length(datos)), 224 , replace=F)
  new <- c(1:(length(datos)))
  for (i in 1:(length(datos))){
    postotes<- pos[i]
    data_in <- datos[postotes]
    new[i] <- data_in
  }

  for (i in 1:15){
    start = (15*i)-14
    end = start +14
    assign(paste0("box_",i), new[start:end])       
  }
  
  x <- c(max(box_1),max(box_2),max(box_3),max(box_4),max(box_5),max(box_6),max(box_7),max(box_8),max(box_9),max(box_10),max(box_11),max(box_12),max(box_13),max(box_14),max(box_15))  
#  sigma = sigmita(mu,xi)  
fit <-suppressWarnings(mle(LLdedevis, start = list(mu = mu, xi= xi)))
  #fit <- mle(LL2, start = list(mu=mu, sigma=sigma, xi= xi))
  summary(fit)
  #fit <- optimx(par=c(mu,sigma,xi),LL2)
  
#fit <-mle(LLdedevis, start = list(mu = mu, xi= xi), hessian=FALSE)
  #print(fit)
  #fit <- optim(c(mu,xi),LLdedevis)
  a1 <- fit@coef[1]
  a2 <- fit@coef[2]
  a3 <- fit@coef[3]
  c <- c(a1,a2,a3)
  
  b1 <- rbind(b1,c)
remove(c)
}  

df <- as.data.frame(b1)
df<- subset(df,df$xi<0)
df <- subset(df, df$mulog05<200)


df$log05 <- (-log(1-0.05))^(xi)
df$log95 <- (-log(1-0.95))^(xi)

df$mulog05 <- df$mu + df$sigma*(-log(1-0.05))^(df$xi)
df$mulog95 <- df$mu - df$sigma*(-log(1-0.95))^(df$xi)

df$mulog95 <- df$mu - df$sigma*(-log(1-0.95))^(df$xi)
df$mulog05 <- df$mu + df$sigma*(-log(1-0.05))^(df$xi)

xi_prom <- mean(df$xi)
mu_prom <- mean(df$mu)
sigma_prom <- mean(df$sigma)

steps <- 100
mu_prom_min <- min(mu_info$x)
mu_prom_max <- max(mu_info$x)
mu_int <- (mu_prom_max - mu_prom_min)/steps

xi_prom_min <- min(xi_info$x)
xi_prom_max <- max(xi_info$x)
xi_int <- (xi_prom_max- xi_prom_min)/steps

n <- steps*steps
aprox <- matrix(0, ncol = 3, nrow = n)
aprox <- data.frame(aprox)
names(aprox)[1] <- "mu_eval"
names(aprox)[2] <- "xi_eval"
names(aprox)[3] <- "sig_eval"

h <-LL(mu_prom,sigma_prom,xi_prom)

c1 <-1
c2 <-1
for (i in 1:n){
  mu_eval <- mu_prom_min + (c2-1)*mu_int
  aprox$mu_eval[i] <- mu_eval 
  xi_eval <-xi_prom_min + (c1-1)*xi_int
  aprox$xi_eval[i] <- xi_eval
  sigma_aprox <- func(h,mu_eval,xi_eval)
  aprox$sig_eval[i] <- sigma_aprox
  if (c1<steps){
    c1 <- c1 + 1 
  } else { 
    c1 <- 1
    c2 <- c2 + 1
  }
}

write.csv(aprox,"results/aprox.csv")
