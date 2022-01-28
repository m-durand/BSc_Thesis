########### CREATED PLOTS - TEST

mulow<-92.9612;muhigh<-105.6416
xilow<- -0.8394;xihigh<- -0.05231

l05<- -log(1-0.05);l95<- -log(1-.95)

mu<-vector();xi<-vector()

for (i in 1:100){
xivector<-seq(xilow,xihigh,len=1000)
xi<-c(xi,xivector)}

for (i in 1:100){
muvector<-seq(mulow,muhigh,len=1000)
mu<-c(mu,muvector)}

mu<-sort(mu)

data<-as.data.frame(cbind(xi,mu))
data$sigma<-sigmita(data$mu,data$xi)
data$q05<-data$mu-data$sigma*(l05^(-data$xi))
data$q95<-data$mu+data$sigma*(l95^(1/data$xi))

titulo<-substitute("Densidad de " ~ Q[0.95])
cus<-substitute(Q[0.95])
fcus<-substitute("f(" ~ Q[0.95] ~")")

hist(data$q95,
     probability = TRUE, # In stead of frequency
     breaks = 10,# For more breaks than the default
     col = "darkslategray4", border = "seashell3",main=titulo,xlab=cus,ylab=fcus)
lines(density(data$q95),# Add the kernel density estimate (-.5 fix for the bins)
      col = "black", lwd = 3)

titulo<-substitute("Densidad de " ~ Q[0.05])
cus<-substitute(Q[0.05])
fcus<-substitute("f(" ~ Q[0.05] ~")")

hist(data$q05,
     probability = TRUE, # In stead of frequency
     breaks = 10,# For more breaks than the default
     col = "darkslategray4", border = "seashell3",main=titulo,xlab=cus,ylab=fcus)
lines(density(data$q05),# Add the kernel density estimate (-.5 fix for the bins)
      col = "black", lwd = 3)


############ OFFICIAL PLOTS

### Mu ############

hist(df$mu, 
     probability = TRUE, # Instead of frequency
     breaks = "FD",# For more breaks than the default
     col = "darkslategray4", border = "seashell3",main="Densidad de mu desde 1995",xlab="mu",ylab="f(mu)")
lines(density(df$mu),# Add the kernel density estimate (-.5 fix for the bins)
      col = "black", lwd = 3)

mu_info <- density(df$mu)

print(mu_info)

### Xi ############

hist(df$xi,
     probability = TRUE, # In stead of frequency
     breaks = "FD",# For more breaks than the default
     col = "darkslategray4", border = "seashell3",main="Densidad de xi desde 1995",xlab="xi",ylab="f(xi)")
lines(density(df1$xi),# Add the kernel density estimate (-.5 fix for the bins)
      col = "black", lwd = 3)

xi_info <- density(df$xi)

print(xi_info)

### Sigma ############
hist(df$sigma,
     probability = TRUE, # In stead of frequency
     breaks = "FD",# For more breaks than the default
     col = "darkslategray4", border = "seashell3",main="Densidad de sigma desde 1995",xlab="sigma",ylab="f(sigma)")

lines(density(df$sigma),# Add the kernel density estimate (-.5 fix for the bins)
      col = "black", lwd = 3)

sigma_info <- density(sigma)

print(sigma_info)

### Mulog 05 y 95 ############

df$mulog95b=df$mulog95-30

titulo<-substitute("Densidad de " ~ Q[0.05])
cus<-substitute(Q[0.05])
fcus<-substitute("f(" ~ Q[0.05] ~")")

hist(df$mulog95,
     probability = TRUE, # In stead of frequency
     breaks = "FD",# For more breaks than the default
     col = "darkslategray4", border = "seashell3",main=titulo,xlab=cus,ylab=fcus)
lines(density(df$mulog95b),# Add the kernel density estimate (-.5 fix for the bins)
      col = "black", lwd = 3)


titulo<-substitute("Densidad de " ~ Q[0.95])
cus<-substitute(Q[0.95])
fcus<-substitute("f(" ~ Q[0.95] ~")")

hist(df$mulog05,
     probability = TRUE, # In stead of frequency
     breaks = "FD",# For more breaks than the default
     col = "darkslategray4", border = "seashell3",main=titulo,xlab=cus,ylab=fcus)
lines(density(df$mulog05),# Add the kernel density estimate (-.5 fix for the bins)
      col = "black", lwd = 3)

xi_info <- density(df$mulog95)

new <- rbind(df$mulog05, df$mulog95)
hist(new,
     probability = TRUE, # In stead of frequency
     breaks = "FD",      # For more breaks than the default
     col = "darkslategray4", border = "seashell3")
lines(density(new ),   # Add the kernel density estimate (-.5 fix for the bins)
      col = "firebrick2", lwd = 3)

print(xi_info)