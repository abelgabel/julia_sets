library(data.table)
#library(viridis)
library(ggplot2)

julia_set<-function(a,b,iter,step){
  
  
  x<-seq(-1.2,1.2,by=step)
  y<-seq(-1.2,1.2,by=step)
  
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





# 0 
# -.077+0.22i
#  -1 +0i
# 0.365- .37i



a0<- -.77; b0<-0.22
a1<- -1; b1<-0
a2<-0; b2<-0
a3<-0.365; b3<-0.37



p_seq<-seq(0.06,1,0.01)

colfunc <-
  colorRampPalette(c('white','#5FEFE5',"#E27D60","#C38D9E",'black'))


k<-1
for(p in p_seq)
{
  an<-(p<=1)*((1-p)*a0+p*a1)+
    (p>1 & p<=2)*((2-p)*a1+(p-1)*a2)+
    (p>2)*((3-p)*a2+(p-2)*a3)
  
  bn<-(p<=1)*((1-p)*b0+p*b1)+
    (p>1 & p<=2)*((2-p)*b1+(p-1)*b2)+
    (p>2)*((3-p)*b2+(p-2)*b3)
  
  
  an<-((1-p)*a0+p*a1)
  bn<-((1-p)*b0+p*b1)
  
  
  complex.plane<-julia_set(a=an,b=bn,iter=60,step=0.001)
  complex.plane[norm>16,'norm']<-NA
  
  ggplot(complex.plane, aes(x, y, fill=norm)) + 
    geom_raster() + 
    coord_fixed(expand = FALSE) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background =element_rect(fill='#1F156B'),
          axis.text = element_text(size=rel(0))) +
    xlab('')+ylab('')+
    scale_fill_gradientn(
                         colours =colfunc(100)
                         ,na.value='#1F156B',guide=F)+
    ylim(-1.1,1.1)+xlim(-1.1,1.1)
  
  filename<-paste0('simulations2\\julia',k,'.png')
  ggsave(filename, dpi=400,
         heigh=3,width=3)
  k<-k+1
}



a1<- -1; b1<-0
if(1==2){
  d<-.02
a0<-0.365-1*d;
b0<-0.37

complex.plane<-julia_set(a=a0,b=b0,iter=60,step=0.001)
complex.plane[norm>5,'norm']<-NA

quant<-quantile(complex.plane,probs=c(.02,.05,.1,.2), na.rm=T)


complex.plane$norm_scale<-scale(complex.plane$norm, scale=F)

ggplot(complex.plane, aes(x, y, fill=norm_scale)) + 
  geom_raster() + 
  coord_fixed(expand = FALSE) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill='#1F156B'),
        axis.text = element_text(size=rel(0))) +
  xlab('')+ylab('')+
  scale_fill_gradientn(
                       colours =colfunc(10)
                       ,na.value='#1F156B')+
  ylim(-1.1,1.1)+xlim(-1.1,1.1)


filename<-paste('simulations\\julia_qualityhigh_5.jpg')
ggsave(filename, dpi=600,
       heigh=9,width=12)



 
}
