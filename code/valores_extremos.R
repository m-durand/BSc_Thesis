path1 <- "..."

setwd(path1)

# Choose which data
#data<-read.table(file="indicadores.txt",sep="\t", header=FALSE)
#data<-read.csv(file="indicadores19702.csv", header=FALSE)
data<-read.csv(file="indicadores1970.csv",header=FALSE)

data<-(as.numeric(data$V1))

#gr'afica del REER
x<-1:length(data)
x<-seq(1995,2015+(3/12),1/12)
x<-seq(1970,2015+(3/12),1/12)
myts <- ts(x, start=c(1995, 1), end=c(2013, 09), frequency=12) 
graf<-plot(x,data, xlab="",ylab="REER" ,pch=".",main="REER mensual en M?xico desde 1995")
#lines(x,data, xlab="", ylab=" " ,pch=".")

#empyrical plot
emplot(data, alog = "", labels = TRUE)
thresh<-findthresh(data, 50)

qplot(data, xi = 0, trim = NA, threshold = NA, line = TRUE,labels = TRUE)

meplot(data)

out1<-gev(data, block = 15)
out1$block
out2<-gpd(data, threshold= thresh)
out3<-pot(data, threshold= thresh)
%An object of class gev describing the fit and including parameter estimates and standard errors.%
%Fitting is carried out using maximum likelihood.%
  
  %The plot method plot.gev provides two different residual plots for assessing fitted %
%GEV model. The user selects the plot type from a menu.%
plot(out1)
plot(out2)
plot(out3)


#estimadores Max verosimilitud
#estimacion del VaR
#con viejos
#qgev(0.05, xi = -0.252, mu = 97.368, sigma = 14.216)
#con 1970
qgev(0.05, xi = -0.3154277, mu = 103.280563, sigma = 14.7365867)
qgev(0.95, xi = -0.3154277, mu = 103.280563, sigma = 14.7365867)
#con nuevos
qgev(0.05, xi = -0.4458598, mu = 99.3013936, sigma = 10.0118761)
qgev(0.95, xi = -0.4458598, mu = 99.3013936, sigma = 10.0118761)
plot.gev

-0.4458598+(2*0.1967726)
-0.4458598-(2*0.1967726)
10.0118761+(2*2.299006)
10.0118761-(2*2.299006)
99.3013936+(2*3.1700870)
99.3013936-(2*3.1700870)

a<-c(131.01, 133.64,  99.67, 105.28,  72.12,  82.56,  92.80, 105.18, 110.46,  79.61,  93.18, 100.95, 114.44 ,117.76,104.76, 104.57, 101.20 ,106.08,  97.94, 100.52, 100.77)
a<-out1$data


hist(datos,probability=TRUE,nclass=15,border = "seashell3", col = "darkslategray4",
     main = "Histograma de los Datos desde 1970", xlab = "REER",ylab="Densidad")

hist(a,probability=TRUE,nclass=15,border = "seashell3", col = "darkslategray4",
     main = "Distrib. Emp?rica y Estimada de los M?ximos 1995", xlab = "REER",ylab="Densidad")


x <- 1:200
mu<-103.146
sigma<-11.657
xi<- -0.309
siginv <- 1/sigma
inexp <- (x-mu)/sigma
expo <- -1/xi
eval <- 1*(siginv*(1+xi*inexp)**(expo-1))*exp(-1*(1+xi*inexp)**expo)
lines(eval)



hist(a,probability=TRUE,nclass=15,border = "seashell3", col = "darkslategray4",
     main = "Distrib. Emp?rica y Estimada de los M?ximos 1970", xlab = "REER",ylab="Densidad")


x <- 1:200
mu<-101.881
sigma<-14.884
xi<- -0.335
siginv <- 1/sigma
inexp <- (x-mu)/sigma
expo <- -1/xi
eval <- 1*(siginv*(1+xi*inexp)**(expo-1))*exp(-1*(1+xi*inexp)**expo)
lines(eval)





