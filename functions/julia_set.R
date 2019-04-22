library(data.table)
library(viridis)


julia_set<-function(a,b,iter,step){

  
x<-seq(-2,2,by=step)
y<-seq(-2,2,by=step)

complex.plane<-merge(x,y)
complex.plane<-data.table(complex.plane)

complex.plane[,'xn']<-complex.plane[,
                                    x^2-y^2+a]
complex.plane[,'yn']<-complex.plane[,
                                    2*x*y+b]


complex.plane[,'xn_1']<-complex.plane[,
                                    x^2-y^2+a]
complex.plane[,'yn_1']<-complex.plane[,
                                    2*x*y+b]

for(i in 2:iter){
  complex.plane[,'xn']<-complex.plane[,
                                      (xn_1<15 & yn_1<15)*(xn_1^2-yn_1^2+a)+
                                        (1-(xn_1<15 & yn_1<15))*15]
  complex.plane[,'yn']<-complex.plane[,(xn_1<15 & yn_1<15)*(2*xn_1*yn_1+b)+
                                        (1-(xn_1<15 & yn_1<15))*15]
  complex.plane[,'xn_1']<-complex.plane[,
                                        xn]
  
  complex.plane[,'yn_1']<- complex.plane[,yn]
  
  
}

complex.plane[,'norm']<-complex.plane[,
                                      xn^2+2*xn*yn+yn^2]

complex.plane[x>=0.
              & norm<900]

return(complex.plane)
}

