## !/user/bin/env RStudio 1.1.423
## -*- coding: utf-8 -*-
## polar coordinates system of ggplot2

## 《R语言商务图表与数据可视化》


rm(list = ls())
gc()

########第七章：ggplot极坐标系应用########


####7.1 极坐标系参数转换原理####

library("showtext")
library("ggplot2")
library("magrittr")
library("reshape2")
library("ggthemes")
library('dplyr')

mydata <- data.frame(
  Name = c("苹果","谷歌","脸书","亚马逊","腾讯"),
  Company = c("Apple","Google","Facebook","Amozon","Tencent"),
  Sale2013 = c(5000,3500,2300,2100,3100),
  Sale2014 = c(5050,3800,2900,2500,3300),
  Sale2015 = c(5050,3800,2900,2500,3300),
  Sale2016 = c(5050,3800,2900,2500,3300)
)  %>% melt(id.vars=c("Company","Name"),variable.name="Year",value.name="Sale")

#极坐标系转化函数：

coord_polar(
  theta = "x",        #极坐标化的中心轴，以哪个轴为转换依据
  start = 0,          #序列起始角度
  direction = 1       #序列排列方向
  )

#7.1 极坐标系参数转换原理

#7.1.1 单序列柱形图翻转

mydata <- diamonds[sample(nrow(diamonds),1000),] %>% filter(!color %in% c('J','I'))
str(mydata)
#单序列柱形图——以x轴为中心极坐标化
ggplot(mydata ,aes(x = '',y = depth,fill = cut))+
  geom_bar(width = 1,stat = 'identity') +
  #ylim(-50,450)+
  coord_polar(??? , start=0) + #Y轴退化为半径，X轴退化为圆周

  scale_fill_wsj()

#单序列柱形图——以y轴为中心极坐标化
ggplot(mydata ,aes(x = '',y = depth,fill = cut))+
  geom_bar(width = 1,stat = 'identity') +
  #ylim(-50,450)+
  coord_polar(??? , start=0) + #X轴退化为半径，y轴退化为圆周

  scale_fill_wsj()


#单序列柱形图——以y轴为中心极坐标化
ggplot(mydata,aes(x = cut,y = depth,fill = cut))+
  geom_bar(width=1,stat = 'identity') +
  scale_fill_wsj() +
  coord_polar(???,start=0) #X轴退化为半径，y轴退化为圆周



#单序列柱形图——以x轴为中心极坐标化
ggplot(mydata,aes(x =cut,y = depth,fill = cut))+
  geom_bar(width=1,stat = 'identity') +
  coord_polar(???,start=0,direction = -1) + #Y轴退化为半径，X轴退化为圆周
  scale_fill_wsj()



# 原理要点一：
# ggplot2中的极坐标系不是原生坐标系，
# 而是通过笛卡尔坐标系衍生而来——coord_polar()

# 原理要点二：
# 极坐标系的转化只依赖坐标系统，
# 不依赖任何当个图表（尽管我们平时仅仅使用它来制作饼图）

# 原理要点三：
# 极坐标系的转化核心思想是将笛卡尔坐标系中一个轴退化为极坐标系下的半径，
# 一个轴退化为圆周，原有坐标系下的第三维度仍然保留

# 原理要点四：
# 极坐标系的转化后仍然支持所有ggplot2原生函数控制，
# 比如标度调整系统、标签系统、图例系统、分面系统等

####7.2 极坐标系衍生图表类型（多序列）####

#簇状柱形图——沿着X轴旋转
ggplot(mydata,aes(x = color,y = depth ,fill=cut))+
  geom_bar(position = 'dodge',colour = 'transparent',stat = 'identity') +
  scale_fill_wsj() +
  ylim(-10,80) +
  coord_polar(???,start=0) #Y轴退化为半径，X轴退化为圆周



 #簇状柱形图——沿着y轴旋转
ggplot(mydata,aes(x = color,y = depth ,fill=cut))+
  geom_bar(position = 'dodge',colour = 'transparent',stat = 'identity') +
  ylim(0,80) +
  coord_polar(???,start=0) + #X轴退化为半径，y轴退化为圆周
  scale_fill_wsj()


#簇状柱形图——沿着x轴旋转——分面(x)
ggplot(mydata,aes(x = color,y = depth ,fill=cut))+
  geom_bar(position = 'dodge',colour = 'transparent',stat = 'identity') +
  ylim(-10,80) +
  #??? #先不分面，观察图形，按cut值不同按行分面
  scale_fill_wsj() +
  coord_polar(theta = "x",start=0) 


#簇状柱形图——沿着y轴旋转——分面(y)
ggplot(mydata,aes(x = color,y = depth ,fill=cut))+
  geom_bar(position = 'dodge',colour = 'transparent',stat = 'identity') +
  #???#先不分面，观察图形，再按cut值不同按行分面，观察图形
  scale_fill_wsj() +
  coord_polar(theta = "y",start=0) 


#堆积柱形图——沿着x轴旋转

ggplot(mydata,aes(x = color,y = depth ,fill=cut))+
  geom_bar(position = 'stack',colour = 'transparent',stat = 'identity') +
  coord_polar(???,start=0)+ #先不转换坐标系，观察图形，Y轴退化为半径，X轴退化为圆周
  scale_fill_wsj() #说明：scale_fill_wsj() 设置序列的填充颜色,有5种参数


#堆积柱形图——沿着y轴旋转
ggplot(mydata,aes(x = color,y = depth ,fill=cut))+
  geom_bar(position = 'stack',colour = 'transparent',stat = 'identity') +
  coord_polar(???,start=0)+#先不转换坐标系，观察图形，X轴退化为半径，y轴退化为圆周
  scale_fill_wsj()




#堆积柱形图——沿着x轴旋转——分面
ggplot(mydata,aes(x = color,y = depth ,fill=cut))+
  geom_bar(position = 'stack',colour = 'transparent',stat = 'identity') +
  #???#先不分面，观察图形，再按cut值不同按行分面，观察图形
  coord_polar(theta = "x",start=0)+
  scale_fill_wsj()


#堆积柱形图——沿着y轴旋转——分面
ggplot(mydata,aes(x = color,y = depth ,fill=cut))+
  geom_bar(position = 'stack',colour = 'transparent',stat = 'identity') +
  #??? +#先不分面，观察图形，再按cut值不同按行分面，观察图形
  coord_polar(theta = "y",start=0) +
  scale_fill_wsj() 


#### 7.3 极坐标下的标签设定技巧 ####

#单序列标签：
mydata <- diamonds[sample(nrow(diamonds),1000),] %>% 
  filter(!color %in% c('J','I'))

#单序列堆积柱形图——标签
library('dplyr')
mydata1 <- mydata   %>% 
  select(cut,depth) %>%
  group_by(cut)     %>% 
  summarize(depth = sum(depth))

ggplot(mydata1 ,aes(x = '' , y = depth ,fill = cut))+
  geom_bar(width = 1,stat = 'identity') +
  geom_text(aes(label = depth),position = position_stack(.5),stat = 'identity',vjust = .5 ,hjust = .5) +
  scale_fill_wsj() +
  coord_polar(theta = "y",start=0)


#单序列柱形图——标签
ggplot(mydata1 ,aes(x = cut,y = depth,fill = cut))+
  geom_bar(width = 1,stat = 'identity') +
  geom_text(aes(label = depth),stat = 'identity',vjust = .5 ,hjust = .5,???) +
  # #标签层的position必须也是position_stack(.5)，vjust=0.5保证标签正好处于垂直居中水平
  scale_fill_wsj()


ggplot(mydata1 ,aes(x = cut,y = depth,fill = cut))+
  geom_bar(width = 1,stat = 'identity') +
  geom_text(aes(label = depth),stat = 'identity',vjust = .5 ,hjust = .5,position = position_stack(.5)) +
  scale_fill_wsj() +
  ylim(-1000,28000) +
  coord_polar(???,start=0)#Y轴退化为半径，X轴退化为圆周


#多序列堆积柱形图——标签
mydata2 <- mydata  %>% 
  group_by(cut,color) %>% 
  summarize(depth = sum(depth))

#多序列簇状柱形图标签添加：
ggplot(mydata2,aes(x = cut,y = depth,fill = color))+
  geom_bar(width=1,stat = 'identity', ???) + #柱形图为簇状，position为dodge
  scale_fill_wsj() +
  geom_text(aes(label = depth ),position = ???,hjust = .5,vjust =.5)#标签层的position必须也是position_dodge(width = 1) 


ggplot(mydata2,aes(x = cut,y = depth,fill = color))+
  geom_bar(width=1,stat = 'identity', position = 'dodge') +
  geom_text(aes(label = depth ),position = position_dodge(width = 1),hjust = .5,vjust =.5) +
  #??? +  #将所有柱子按颜色按行分面
  scale_fill_wsj()


#多序列堆积柱形图标签添加：
ggplot(mydata2,aes(x = cut,y = depth,fill = color))+
  geom_bar(width=1,stat = 'identity') +
  scale_fill_wsj() +
  geom_text(aes(label = depth ),position = ???,hjust = .5,vjust =.5)#多序列堆积柱形图添加标签 


ggplot(mydata2,aes(x = cut,y = depth,fill = color))+
  geom_bar(width=1,stat = 'identity',position = 'stack') +
  geom_text(aes(label = depth ),position = position_stack(vjust = .5),hjust = .5,vjust =.5) +
  ???#将所有柱子按颜色按行分面
  scale_fill_wsj()

ggplot(mydata2,aes(x = cut,y = depth,fill = color))+
  geom_bar(width=1,stat = 'identity',position = 'stack') +
  geom_text(aes(label = depth ),position = position_stack(vjust = .5),hjust = .5,vjust =.5) +
  facet_grid(.~color) +
  ???#将分面后的柱形图转换成极坐标，coord_polar默认设置，观察旋转是按哪个轴为中心
  scale_fill_wsj()


#### 7.4 极坐标下分面操作规则 ####

#facet_grid:
#facet_wrap:
#https://github.com/tidyverse/ggplot2/issues/1152

scale  = 'fixed'    #固定x,y轴
scale  = 'free'     #释放x，y轴
scale  = 'free_x'   #x轴自适应
scale  = 'free_y'   #y轴自使用


mydata3 <- diamonds[sample(nrow(ggplot2::diamonds),5000),] %>% 
  filter(!color %in% c('J','I')) %>% 
  na.omit()
head(mydata3)
###针对连续性变量：

#facet_grid:
ggplot(mydata3,aes(x = depth,y = price))+
  geom_point(aes(fill = color),shape = 21) +
  facet_grid(.~color,scales = ???) +   #固定x,y轴
  scale_fill_wsj()

ggplot(mydata3,aes(x = depth,y = price))+
  geom_point(aes(fill = color),shape = 21) +
  facet_grid(.~color,scales = ???) +  #x轴自适应
  scale_fill_wsj()

ggplot(mydata3,aes(x = depth,y = price))+
  geom_point(aes(fill = color),shape = 21) +
  facet_grid(.~color,scales = ???) +  #y轴自使用
  scale_fill_wsj()

ggplot(mydata3,aes(x = depth,y = price))+
  geom_point(aes(fill = color),shape = 21) +
  facet_grid(.~color,scales = ???) +  #释放x，y轴
  scale_fill_wsj()


###针对离散性变量：

#facet_grid:

mydata4 <- mydata %>% 
  group_by(cut,color) %>% 
  summarize(depth = sum(depth),price = sum(price))

#scales = 'fixed'
ggplot(mydata4,aes(x = cut,y = depth,fill = color))+
  geom_bar(width=1,stat = 'identity',position = 'stack',colour = 'white') +
  geom_text(aes(label = depth ),position = position_stack(vjust = .5),hjust = .5,vjust =.5) +
  facet_grid(.~color,scales = ???) + #固定x,y轴
  scale_fill_wsj()

#scales = 'free_y'
ggplot(mydata4,aes(x = cut,y = depth,fill = color))+
  geom_bar(width=1,stat = 'identity',position = 'stack',colour = 'white') +
  geom_text(aes(label = depth ),position = position_stack(vjust = .5),hjust = .5,vjust =.5) +
  facet_grid(.~color,scales = ???) +#y轴自适应
  scale_fill_wsj()


#scales = 'free_x'
ggplot(mydata4,aes(x = cut,y = depth,fill = color))+
  geom_bar(width=1,stat = 'identity',position = 'stack',colour = 'white') +
  coord_flip() +
  geom_text(aes(label = depth ),position = position_stack(vjust = .5),hjust = .5,vjust =.5) +
  facet_grid(.~color,scales = ???) +#x轴自适应, 观察x轴自适应后与上面的图片有什么明显的差异
  scale_fill_wsj()

#scales = 'free'
ggplot(mydata4,aes(x = cut,y = depth,fill = color))+
  geom_bar(width=1,stat = 'identity',position = 'stack',colour = 'white') +
  coord_flip() +
  geom_text(aes(label = depth ),position = position_stack(vjust = .5),hjust = .5,vjust =.5) +
  facet_grid(.~color,scales = ???) +#释放x，y轴，观察图片有什么变化
  scale_fill_wsj()




#### 7.6 ggplot2辅助极坐标图——雷达图 ####

#ggplot2极坐标系转化的雷达图貌似不太对！！！
library("ggradar")

data <- data.frame(
  Name = c("苹果","谷歌","脸书","亚马逊","腾讯"),
  Company = c("Apple","Google","Facebook","Amozon","Tencent"),
  Sale2013 = c(5000,3500,2300,2100,3100),
  Sale2014 = c(5050,3800,2900,2500,3300),
  Sale2015 = c(5050,4000,3200,2800,3700),
  Sale2016 = c(6000,4800,4500,3500,4300)
  )

mydata<-melt(
  data,id.vars  = c("Name","Company"),
  variable.name = "Year",
  value.name    = "Sale"
  )

ggplot(mydata,aes(Company,Sale,group=Year,color=Year)) +
  geom_line() +
  scale_x_discrete(expand = c(0,0)) +
  coord_polar(theta = "x")



mydata <- matrix(runif(40,0,1),5,8)
rownames(mydata) <- LETTERS[1:5]
colnames(mydata) <- c("Apple","Google","Facebook","Amozon","Tencent","Alibaba","Baidu","Twitter")
mynewdata<-data.frame(mydata)

Name <- c("USA","CHN","UK","RUS","JP")
mynewdata <- data.frame(Name,mynewdata)

#单序列：
ggradar(mynewdata[1,])

#多序列：
ggradar(mynewdata)

 
#### 7.7 ggplot2辅助极坐标图——气泡饼图 ####

library("ggplot2")
library("scatterpie")
library("Cairo")

set.seed(123)
long <- rnorm(50, sd=100)
lat <- rnorm(50, sd=50)
d <- data.frame(long=long, lat=lat)
d <- with(d, d[abs(long) < 150 & abs(lat) < 70,])
n <- nrow(d)
d$region <- factor(1:n)
d$A <- abs(rnorm(n, sd=1))
d$B <- abs(rnorm(n, sd=2))
d$C <- abs(rnorm(n, sd=3))
d$D <- abs(rnorm(n, sd=4))
d[1, 4:7] <- d[1, 4:7] * 3

ggplot() + 
  geom_scatterpie(data=d,aes(x=long, y=lat),cols=LETTERS[1:4]) + 
  coord_equal() +
  scale_fill_wsj()

d$radius <- 6 * abs(rnorm(n))
ggplot() + 
  geom_scatterpie(data=d, aes(x=long, y=lat, r =radius), cols = LETTERS[1:4], color=NA) + 
  coord_equal() +
  geom_scatterpie_legend(d$radius, x=-140, y=-70) +
  scale_fill_wsj()


###案例练习：

mydata <- c(1,1,1,1,1,1,1,1,1,2,3,2,3,5,5,1,1,1,1,1,2,2,4,5,1,3,2,3,5,5,4,2,4,2,1,2,1,1,0.5,0.5)
Year<-2004:2011
Dummy<-5*seq(1:8)
Data<-c(5,6,18,5,14,18,13,5)
mynewdata<-matrix(mydata,nrow=8,ncol=5,byrow=T)
colnames(mynewdata)<-c("S1","S2","S3","S4","S5")
mynewdata <- as.data.frame(mynewdata)


mynewdata1 <- cbind(Year,Dummy,Data,mynewdata)
as.integer(mynewdata1$Dummy)
as.integer(mynewdata1$Year)

color1<-c("#FF2D2D","#F79646","#4BACC6","#FFC000","#92D050")
color2<-c("#17375E","#23538D","#558ED5","#8EB4E3","#C6D9F1")

CairoPNG(file="C:/Users/Administrator/Desktop/scatterpie1.png",width=800,height=528)
ggplot()+
  geom_line(aes(x=Dummy,y=Data,group=1),col="#085264",size=.8)+
  geom_scatterpie(data=mynewdata1,aes(x=Dummy,y=Data,r = 1.5),cols= colnames(mynewdata1)[4:8],color=NA)+
  ylim(0,25)+
  scale_fill_manual(values=color1)+
  scale_x_continuous(breaks=mynewdata1$Dummy,labels=c(2004:2011))+
  coord_equal() +
  guides( fill=guide_legend(label.position ="top"))+
  theme_grey() %+replace%
  theme(
    axis.text.x=element_text(size=12,face="italic",margin=margin(2,2,5,2,"pt")),
    axis.text.y=element_text(size=12,face="italic",margin=margin(2,2,2,5,"pt")),
    axis.ticks.length=unit(0.3,'cm'),
    legend.direction="horizontal",
    legend.position=c(0.15,0.9)
  )
dev.off()








