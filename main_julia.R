

setwd(getwd())
source('functions\\julia_set.R')

# 0 
# -.077+0.22i
#  -1 +0i
# 0.365- .37i


a0<- -.77; b0<-0.22
a1<- -1; b1<-0
a2<-0; b2<-0
a3<-0.365; b3<-0.37



p_seq<-seq(0,3,0.05)

k<-1
for(p in p_seq[6:61])
{
  an<-(p<=1)*((1-p)*a0+p*a1)+
    (p>1 & p<=2)*((2-p)*a1+(p-1)*a2)+
    (p>2)*((3-p)*a2+(p-2)*a3)
  
  bn<-(p<=1)*((1-p)*b0+p*b1)+
    (p>1 & p<=2)*((2-p)*b1+(p-1)*b2)+
    (p>2)*((3-p)*b2+(p-2)*b3)
  
  complex.plane<-julia_set(a=an,b=bn,iter=60,step=0.001)
  complex.plane[norm>1,'norm']<-NA
  
  ggplot(complex.plane, aes(x, y, fill=norm)) + 
    geom_raster() + 
    coord_fixed(expand = FALSE) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background =element_rect(fill='#1F156B'),
          axis.text = element_text(size=rel(0))) +
    xlab('')+ylab('')+
    scale_fill_gradientn(breaks=c(0,.5),colours =c('#F13C20','#D79922')
                         ,na.value='#1F156B',guide=F)+
    ylim(-1.1,1.1)+xlim(-1.1,1.1)
  
  filename<-paste('simulations\\julia',k,'.jpg')
  ggsave(filename, dpi=400,
         heigh=9,width=12)
  k<-k+1
}
