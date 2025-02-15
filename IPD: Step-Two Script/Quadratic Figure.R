## QUADRACTIC GRAPHS FOR OVERALL ESTIMATES - DEPRESSIVE SYMPTOMS ----
library(tidyverse)

# QUADRATIC GRAPH FOR INCOME
in2cdepmin3sd<-(0.0464357*((-3)^2))+((-0.0823295)*(-3))+0.7681326  
in2cdepmin2sd<-(0.0464357*((-2)^2))+((-0.0823295)*(-2))+0.7681326  
in2cdepmin1sd<-(0.0464357*((-1)^2))+((-0.0823295)*(-1))+0.7681326  
in2cdepmean<-(0.0464357*0)+((-0.0823295  )*0)+0.7681326  
in2cdeppls1sd<-(0.0464357*(1^2))+((-0.0823295)*(1))+0.7681326  
in2cdeppls2sd<-(0.0464357*(2^2))+((-0.0823295)*(2))+0.7681326  
in2cdeppls3sd<-(0.0464357*(3^2))+((-0.0823295)*(3))+0.7681326  
predictdepbyinc<-data.frame(predval=c(in2cdepmin3sd,in2cdepmin2sd,in2cdepmin1sd,
                   in2cdepmean,in2cdeppls1sd,in2cdeppls2sd,in2cdeppls3sd),
                   type=c("est","est","est","est","est","est","est"),
                   inc2values=c((-3),(-2),(-1),0,1,2,3))

in2cdepmin3sdul<-(0.0761422*((-3)^2))+((-0.0505265)*(-3))+0.7681326  
in2cdepmin2sdul<-(0.0761422*((-2)^2))+((-0.0505265)*(-2))+0.7681326  
in2cdepmin1sdul<-(0.0761422*((-1)^2))+((-0.0505265)*(-1))+0.7681326  
in2cdepmeanul<-(0.0761422*0)+((-0.0505265)*0)+0.7681326  
in2cdeppls1sdul<-(0.0761422*(1^2))+((-0.0505265)*(1))+0.7681326  
in2cdeppls2sdul<-(0.0761422*(2^2))+((-0.0505265)*(2))+0.7681326  
in2cdeppls3sdul<-(0.0761422*(3^2))+((-0.0505265)*(3))+0.7681326  
predictdepbyincul<-data.frame(predval=c(in2cdepmin3sdul,in2cdepmin2sdul,in2cdepmin1sdul,
                   in2cdepmeanul,in2cdeppls1sdul,in2cdeppls2sdul,in2cdeppls3sdul),
                   type=c("ul","ul","ul","ul","ul","ul","ul"),
                   inc2values=c((-3),(-2),(-1),0,1,2,3))

in2cdepmin3sdll<-(0.0167292*((-3)^2))+((-0.1141324)*(-3))+0.7681326  
in2cdepmin2sdll<-(0.0167292*((-2)^2))+((-0.1141324)*(-2))+0.7681326  
in2cdepmin1sdll<-(0.0167292*((-1)^2))+((-0.1141324)*(-1))+0.7681326  
in2cdepmeanll<-(0.0167292*0)+((-0.1141324)*0)+0.7681326  
in2cdeppls1sdll<-(0.0167292*(1^2))+((-0.1141324)*(1))+0.7681326  
in2cdeppls2sdll<-(0.0167292*(2^2))+((-0.1141324)*(2))+0.7681326  
in2cdeppls3sdll<-(0.0167292*(3^2))+((-0.1141324)*(3))+0.7681326  
predictdepbyincll<-data.frame(predval=c(in2cdepmin3sdll,in2cdepmin2sdll,in2cdepmin1sdll,
                   in2cdepmeanll,in2cdeppls1sdll,in2cdeppls2sdll,in2cdeppls3sdll),
                   type=c("ll","ll","ll","ll","ll","ll","ll"),
                   inc2values=c((-3),(-2),(-1),0,1,2,3))

incdepplotdata<-bind_rows(predictdepbyinc,predictdepbyincll,predictdepbyincul)

incdepplot<-incdepplotdata%>%ggplot(aes(x=inc2values,y= predval, group=type))+
  geom_line(data=filter(incdepplotdata,type=='est'))+
  geom_line(data=filter(incdepplotdata,type=='ll'),linetype = "dashed")+
  geom_line(data=filter(incdepplotdata,type=='ul'),linetype = "dashed")+
  labs(x="Income",y="Depressive Symptoms")+
  scale_x_continuous(limits=c(-3,3),breaks=c(-3,-2,-1,0,1,2,3))+
  scale_y_continuous(limits=c(0.5,2),breaks=c(0.5,1.0,1.5,2.0))+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(family='serif',colour='black',size=14),
        legend.text = element_text(family='serif',colour='black',size = 12),
        legend.title = element_text(family='serif',colour='black',size = 12),
        axis.text = element_text(family='serif',colour='black',size = 12))
  

