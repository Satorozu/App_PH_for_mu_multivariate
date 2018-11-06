##con funcion

To<-function(base,alpha,M0){
  Mmues<-c(mean(base$x1), mean(base$x2), mean(base$x3)) #varia segun la base sumistrada
  n<- nrow(base)
  Scom<-solve(cov(base))
  #---obtencion del valor del EP-------
  T0<-n*t(Mmues-M0)%*%Scom%*%(Mmues-M0)
  p<-length(M0) # cambia segun el tamaño de variables escogidas para el vector
  n<-nrow(base)
  a<-((n-1)*p/(n-p))
  f<-(qf(alpha,p,n-p))*a
  res<-list(estaditico=T0,
            valor=f)
  return(res)
}
z<-To(dt,0.9,c(4,50,10))
z

p<-length(M0) # cambia segun el tamaño de variables escogidas para el vector
n<-nrow(base)
a<-((n-1)*p/(n-p))
f<-(qf(alpha,p,n-p))*a

res<- list(estadistico=T0,
           valorf=f)

class(res) <- "htest"
return(res)






str(To(dt,0.9,c(4,50,10)))
str(To)

distF<-function(base,alpha,M0){
  p<-length(M0) # cambia segun el tamaño de variables escogidas para el vector
  n<-nrow(base)
  a<-((n-1)*p/(n-p))
  f<-(qf(alpha,p,n-p))*a
  f
}
distF(dt,0.9,c(4,50,10))
distF

#decision de rechazo
T0-f
