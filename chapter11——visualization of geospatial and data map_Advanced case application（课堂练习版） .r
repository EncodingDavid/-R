## !/user/bin/env RStudio 1.1.423
## -*- coding: utf-8 -*-
## visualization of geospatial and data map_case and model 

## 《R语言商务图表与数据可视化》


########第十一章：空间可视化与数据地图高阶案例扩展应用######

####11.1 地图上的mini条形图（柱形图）####
rm(list = ls())
gc()

library("ggplot2")
library("plyr")
library("rgdal")
library("magrittr")
library("dplyr")

china_shp <-readOGR("/Users/david/Downloads/吴晓玲/Rstudy/CHN_adm/bou2_4p.shp",stringsAsFactors = FALSE)    

#地图数据（多边形经纬度边界点数据）
china_map <- fortify(china_shp)  

#城市经纬度数据
province_city <- read.csv("/Users/david/Downloads/吴晓玲/Rstudy/chinaprovincecity.csv",stringsAsFactors = FALSE) 
province_city1 <- mutate(  #在读进来的省会数据中添加N15、N16即2015和2016年的业务数据，并设置Ratio为表示这两年业务指标上升还是下降
  province_city,
  N15=runif(34,min=500,max=1000),#生成34个随机均匀分布数，产生2015指标的数据，列名为N15
  N16=runif(34,600,1100),#生成34个随机均匀分布数，产生2016指标的数据，列名为N16
  Ratio=round((N16-N15)/N15,3)#求解表示2016与与2015年业务指标升降程度的变量Ratio，并进行四舍五入，保留3位小数
  )
province_data <- province_city1[top_n(nrow(province_city1),???),]#从所有省会数据中抽10个样本

#案例一：mini柱形图——竖向
windowsFonts(myFont = windowsFont("微软雅黑")) 
ggplot()+
  geom_polygon(data=china_map,aes(x=long,y=lat,group = group), fill="white", colour="grey60") +
  geom_linerange(data=province_data,aes(x=jd - 0.5,ymin = wd,ymax = wd + 0.7*N15/max(N15,N16)*5),size=3,color="#5B88A0",alpha=0.8)+#添加竖状条形图，根据图片的刻度范围设置柱子的高度
  geom_linerange(data=province_data,aes(x=jd + 0.5,ymin = wd,ymax = wd + 0.7*N16/max(N15,N16)*5),size=3,color="#FB882C",alpha=0.8)+#添加竖状条形图，根据图片的刻度范围设置柱子的高度
  geom_text(data=province_data,aes(x=jd,y=wd - 0.6,label=???(province_data$province,,Ratio*100,"%")), family="myFont",size=2.5)+#将省的名字+指标上升下降+%这三部分文字拼接成为geom_text层的文字标签
  # coord_map("polyconic") +
  annotate("text", x=105, y=52, label="● 2015", color= "#5B88A0", size=8)+  #增加文字注释，文字的位置为x=105, y=52
  annotate("text", x=105, y=49, label="● 216", color= "#FB882C", size=8)+  #增加文字注释，文字的位置为x=105, y=49
  theme_void()


#案例二：mini条形图——纵向条形图 horizon y值控制空白
ggplot()+
  geom_polygon(aes(x=long, y=lat,group=group),data=china_map, fill="white", colour="grey60")+
  geom_errorbarh(data=province_data,aes(y=wd,xmin=jd-3,xmax=jd + 2.5*N15/max(N15,N16)),size=3,color="#5B88A0",height=0,alpha=0.8)+#添加横向条形图，根据图片的刻度范围设置柱子的高度
  geom_errorbarh(data=province_data,aes(y=wd-0.8,xmin=jd-3,xmax=jd + 2.5*N16/max(N15,N16)),size=3,color="#FB882C",height=0,alpha=0.8)+#添加横向条形图，根据图片的刻度范围设置柱子的高度
  geom_text(data=province_data,aes(x=jd+0.2,y=wd + 1,label=paste0(province_data$province,ifelse(Ratio>0,"▲","▼"),Ratio*100,"%")), family="myFont",size=2.5)+
  annotate("text", x=105, y=52, label="● 2015", color= "#5B88A0", size=7)+ 
  annotate("text", x=105, y=50, label="● 2016", color= "#FB882C", size=7)+
  coord_equal()+
  theme_void()


####11.2 地图上的mini气泡饼图####
rm(list = ls())
gc()

library("ggplot2")
library("plyr")
library("rgdal")
library("xlsx")
library("ggthemes")
library("magrittr")
library("scatterpie")
library('scales')



#案例：
rm(list = ls())
gc()

china_map <- readOGR(
  "/Users/david/Downloads/吴晓玲/Rstudy/china.geojson",
  stringsAsFactors=FALSE,
  encoding = "utf-8",
  use_iconv = TRUE
  )
china_map <- fortify(china_map) 

province_city <- read.csv(
  "/Users/david/Downloads/吴晓玲/Rstudy/chinaprovincecity.csv",
  stringsAsFactors = FALSE,
  check.names=FALSE
  )

#构造气泡饼图的指标数据（行政区域-年份-业务指标，1:N:1的关系）6代表每列数据有6个
city_data<-data.frame(Name=rep(c("北京","上海","重庆","武汉","广州","西安")))
for (i in 2:7) city_data[,i]<-round(runif(6,0,250)) #对于city_data从第2-6列数据，每列数据产生一组均匀分布的随机数
names(city_data)[2:7]<-paste0("Year",2011:2016) #对第2-6列数据的列名设置为Year+对应数字
city_data$Full<-apply(city_data[,-1],1,sum)  #按每个地点对各年业务数据求和，并成为city_data的full列
city_data$Full_scale <- scale(city_data$Full,center=F,scale = T)* 2 #对Full列数据进行中心标准化处理（用于制作饼图时的半径长度）

#提取中心城市数据：
city_data <- city_data %>% merge(province_city[,c("city","wd","jd")],by.x="Name",by.y="city",all.x=TRUE)


ggplot() +    
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group),fill="white",color="grey") +
  geom_sca(data=city_data,aes(x=jd,y=wd,r = Full_scale),cols = names(city_data)[2:7],color="grey", alpha=.8) +#绘制气泡饼图,半径的长短由Full_scale控制，饼图饼块大小由各年的业务数据设置
  coord_equal() +
  scale_fill_brewer(guide=FALSE)+   
  theme_void()
#思考：绘制气泡饼图的关键是确定哪些数据？
#案例扩展——空间地图分面（行政区域—年份-业务指标 1:N:N）
#问：图片想按什么类别呈现数据？按年份还是行政指标还是行政区域？可以按行政区域吗？

city_data2 <- data.frame(Name=rep(city_data$Name,6))
for (i in 2:4) city_data2[,i]<-runif(nrow(city_data2),10,100)#构造三列业务指标数据
names(city_data2)[2:4]<-paste0("Value",1:3)
city_data2$Year<-rep(paste0("Year",2011:2016),each=6)#构造年份列数据,数据值形式类似为Year2012

city_data2 <- city_data2 %>% merge(city_data[,c("Name","jd","wd")],by="Name",all.x=T)#将地理位置数据与业务指标拼接
city_data2$Full <- apply(city_data2[,2:4],1,sum)#求每行数据第2-4列数据的和，结果作为新的列Full
city_data2$Full_scale<- as.numeric(city_data2$Full,center=F,scale=T)*2 #对Full列数据标准化并乘以2作为饼图的半径大小
city_data2 <- city_data2%>%arrange(Year,Name)#将city_data2按Year、Name列进行排序

ggplot()+
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group),fill="white",color="grey")+
  geom_scatterpie(data=city_data2,aes(x=jd,y=wd,r=Full_scale),cols = names(city_data2)[2:4],color="grey", alpha=.8) +#气泡饼图的饼块大小由每年3指标值来确定
  coord_equal()+#等坐标转换：使用这个函数后，x轴和y轴坐标会被转换成相等形式，此时图形会产生较大的缩放
  scale_fill_brewer(guide=FALSE)+   
  facet_wrap(~Year) +
  theme_void()

####11.3 地图+网络流向图案例用应（含多种流向类型）####
rm(list = ls())
gc()

library("magrittr")
library("plyr")
library("dplyr")
library("ggplot2")
library("ggmap")
library("rgdal")
library("maps")
library('devtools')
library("REmap")#关于REmap的安装方法，参考https://blog.csdn.net/BEYONDMA/article/details/85345831，(github搜索包可以进入https://github.com/YTLogos/REmap)
# 安装参考命令devtools::install_local("/Users/david/Downloads/吴晓玲//geo_data//REmap-master.zip")#把下载的包的路径写在这里
library("Cairo")
library("baidumap")
#baidumap安装方法
# library(devtools)
# install_github('badbye/baidumap')
#baidumap加载使用
library(baidumap)
options(baidumap.key ='???')#其中???为个人在百度地图平台申请的账号名（https://github.com/badbye/baidumap）
library("showtext")



#案例1.1 放射状路径图
china_map <-readOGR("/Users/david/Downloads/吴晓玲/Rstudy/CHN_adm/bou2_4p.shp",stringsAsFactors = FALSE)
x <- china_map@data  %>% mutate(id = row.names(.)) 
china_map1 <- fortify(china_map)  
china_map_data <- merge(china_map1,x, by = "id",all.x = TRUE) 
mydata1 <- read.csv("/Users/david/Downloads/吴晓玲/Rstudy/geshengzhibiao.csv",stringsAsFactors = FALSE,check.names=FALSE)
china_data <- join(china_map_data, mydata1, type="full") 

#下面百度地图API接口获取指定城市的位置，思考：为什么不用china_map_data里面的long与lat数据
city_list <- c("西安","西宁","郑州","重庆","成都","石家庄","兰州","济南","大同","咸阳","包头","长沙")
source("/Users/david/Downloads/吴晓玲/Rstudy/web_city_adress.r",encoding = "utf-8")#source是执行r脚本文件,web_city_adress.r是在百度Api接口抓取指定地市经纬度的脚本程序
address <- GetJD(city_list)#使用脚本程序里定义的函数GetJD获取city_list对应城市的经纬度数据


address$lonx <- address[address$address =="长沙","???"]#增加列：长沙的经度做为起点的x坐标
address$laty <- address[address$address =="长沙","???"]#增加列：长沙的纬度做为起点的y坐标
address <- address[address$address != "长沙",]#将address列中长沙的数据去除（长沙不能做为终点）
names(address)[2:3]<-c("lon","lat")
address$Num<- round(runif(11,50,100),2)#在address中产生Num列，用来控制散点的大小

ggplot()+
  geom_???(data=china_data,aes(x=long,y=lat,group=group),fill="white",size=0.2,colour="#D9D9D9")+#增加中国版图层
  geom_???(data=address,aes(x=lon,y=lat,xend=lonx,yend=laty),size=0.3,colour="#FF6833")+#增加直线段图层
  geom_point(data=address,aes(x=???,y=???,size=???),shape=21,fill="#ED7D31",col="#E02939",alpha=.6)+#增加散点图层
  geom_point(data=???,aes(x=112.97935,y=28.21347),shape=21,size=8,fill=NA,col="steelblue")+#增加长沙所在城市的散点
  guides(fill=FALSE)+
  coord_map("polyconic") +
  scale_size_area(max_size = 8)+ 
  theme_void() %+replace%
  theme(
    plot.background=element_rect(fill="#D0DEDE", color=NA),
    panel.spacing = unit(0,"lines"), 
    plot.margin=unit(rep(0.2,4),"cm"),
    legend.position="none"
  )

#案例2：迁徙路径气泡图

city_list <- c("海口","广州","长沙","武汉","郑州","石家庄","北京","沈阳","长春","哈尔滨")
source("/Users/david/Downloads/吴晓玲/Rstudy/web_city_adress.r",encoding = "utf-8")
address <- GetJD(city_list)
address$Num<-round(runif(10,50,100),2)

ggplot()+
  geom_polygon(data=china_data,aes(x=long,y=lat,group=group),fill="white",size=0.2,colour="#D9D9D9")+
  geom_???(data=address,aes(x=???,y=???),size=0.3,colour="#FF6833")+#增加路径图（按address城市先后顺序自动绘制路径）
  geom_point(data=address,aes(x=lng,y=lat,size=???),shape=21,fill="#ED7D31",col="#E02939",alpha=.6)+
  guides(fill=FALSE)+
  coord_map("polyconic")+
  scale_size_area(max_size=8)+ 
  theme_void() %+replace%
  theme(
    plot.background=element_rect(fill="#D0DEDE", color=NA),
    panel.spacing = unit(0,"lines"), 
    plot.margin=unit(rep(0.2,4),"cm"),
    legend.position="none"
  )

#案例1.3：闭环路径气泡图：思考一下，如何调整迁徙路径成为闭环路径

city_list <- c("兰州","成都","重庆","贵阳","昆明","南宁","海口","广州","福州","上海","青岛","石家庄","呼和浩特","银川")
city_list <- c(city_list,???)#城市列表里增加兰州作为路径终点城市

source("/Users/david/Downloads/吴晓玲/Rstudy/web_city_adress.r",encoding = "utf-8")
address <- GetJD(city_list)
address$Num<-round(runif(nrow(address),50,100),2)

ggplot()+
  geom_polygon(data=china_data,aes(x=long,y=lat,group=group),fill="white",size=0.2,colour="#D9D9D9")+#绘制中国版图
  geom_???(data=address,aes(x=lng,y=lat),size=0.3,colour="#FF6833")+#绘制路径图
  geom_???(data=address,aes(x=lng,y=lat,size=???),shape=21,fill="#ED7D31",col="#E02939",alpha=.6)+#绘制散点图
  guides(fill=FALSE)+
  coord_map("polyconic")+
  scale_size_area(max_size=8)+ 
  theme_void() %+replace%
  theme(
    plot.background=element_rect(fill="#D0DEDE", color=NA),
    panel.spacing = unit(0,"lines"), 
    plot.margin=unit(rep(0.2,4),"cm"),
    legend.position="none"
  )
#案例2：多维度放射状(多维度？显示地点多个，每个地点有多个指标数据)

library("ggplot2")
library("dplyr")
library("rgdal")
library("shiny")
library("shinythemes")
library("magrittr")
rm(list = ls())
gc()

##转换为数据框并合并城市数据：
china_map <- readOGR(
  "/Users/david/Downloads/吴晓玲/Rstudy/china.geojson",
  stringsAsFactors=FALSE,
  encoding = "utf-8",
  use_iconv = TRUE
)  %>% fortify() 

province_city <- read.csv(
  "/Users/david/Downloads/吴晓玲/Rstudy/chinaprovincecity.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
) 

###构造线条起始点数据：
city<-c("北京","上海","重庆","天津","武汉","南京","广州","沈阳","西安","郑州")

city_data <- merge(???,city) %>% rename(Start=x,End=y) %>% arrange(Start)#合并city，产生两列城市列，一列为Start,一列为End
city_data <- city_data %>% filter(Start ??? End)#筛选Start与End列不相等的行数据
#下面构造起点城市与终点城市的位置（经纬度）作为直线线的起点与终点坐标
city_data <- city_data %>% ???(province_city[,c("city","jd","wd")],by.x="???",by.y="city",all.x=TRUE) %>% rename(Start_long=jd,Start_lat=wd)
city_data <- city_data %>% merge(province_city[,c("city","jd","wd")],by.x="???",by.y="city",all.x=TRUE) %>% rename(End_long=jd,End_lat=wd)
city_data <- transform(   #增加三个指标数据
  city_data,
  ???=runif(nrow(city_data),0,100),
  ???=runif(nrow(city_data),0,100),
  ???=runif(nrow(city_data),0,100)
)

#思考：如何画所有城市的直线段图？直接增加一个geom_segment，将所有的起点与终点坐标进行一次性绘制，效果如果？
ggplot()+
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group),fill="white",colour="grey60") +
  geom_segment(data=city_data,aes(x=???,y=Start_lat,xend=???,yend=End_lat,size=???),colour="black")+
  coord_map("polyconic") + 
  scale_size_area(max_size=2) +
  theme_void()

###最合适的做法1：图形分面：

ggplot()+
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group),fill="white",colour="grey60")+
  geom_segment(data=city_data,aes(x=???,y=???,xend=End_long,yend=End_lat),colour="black")+
  #为所有终点城市增加指标数据，即散点图，散点大小按zhibiao1大小设置
  geom_point(data =city_data,aes(x=???,y=End_lat,size=zhibiao1),shape=21,fill="#8E0F2E",colour="black",alpha=0.4)+
  scale_size_area(max_size=6)+
  coord_map("polyconic") + 
  ???(~Start,nrow = ???)+ #按起点城市分面,所有图片分成2行
  theme_void()

#思考：上面图形只是显示了各个城市zhibiao1的区别，如何显示zhibiao2与zhibiao3?

###最合适的做法2——Shiny动态交互图：控制显示3个zhibiao的情况，供参考

city_list <- list("北京"="北京","上海"="上海","重庆"="重庆","天津"="天津","武汉"="武汉","南京"="南京","广州"="广州","沈阳"="沈阳","西安"="西安","郑州"="郑州")

ui <-shinyUI(fluidPage(  #设置界面布局
  theme=shinytheme("cerulean"),
  titlePanel("Population Structure Data"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("var1","City",city_list,inline=FALSE),
      selectInput("var2","Value",c("zhibiao1"="zhibiao1","zhibiao2"="zhibiao2","zhibiao3"="zhibiao3"),selected="zhibiao1")
    ),
    mainPanel(h2("Trade Stream"),plotOutput("distPlot"))
  )
))

server <- shinyServer(function(input,output){ #服务器端脚本程序设置
  output$distPlot <- renderPlot({
    mydata=filter(city_data%>%filter(Start==input$var1))
    argu<-switch(input$var2,zhibiao1=mydata$zhibiao1,zhibiao2=mydata$zhibiao2,zhibiao3=mydata$zhibiao3)
    ggplot(mydata)+
      geom_polygon(data=china_map,aes(x=long,y=lat,group=group),fill="white",colour="grey60")+
      geom_segment(aes(x=Start_long,y=Start_lat,xend=End_long,yend=End_lat),colour="black")+
      geom_point(aes(x=End_long,y=End_lat,size=argu),shape=21,fill="#8E0F2E",colour="black",alpha=0.4)+
      scale_size_area(max_size=6)+
      coord_map("polyconic") + 
      theme_void()
  })
})
shinyApp(ui=ui,server=server) #调用运行脚本代码，显示效果图


####11.4 ggmap背景+ggplot2图层混合应用####

library("dplyr")
library("dplyr")
library("ggplot2")
library("ggmap")
library("rgdal")
library("maps")
library("Cairo")
library("showtext")
library("baidumap")
library("grid")
rm(list = ls())
gc()

#devtools::install_github("dkahle/ggmap")
#devtools::install_github("hadley/ggplot2")
#如下代码若出错，请尝试请更新ggplot2、ggmap至最新开发板！
#问题详情请参见Stack Overflow 主页：
#https://stackoverflow.com/questions/40642850

bbox_everest <- c(left =60, bottom =10, right =150, top =60)
mapdata <- get_stamenmap(bbox_everest, zoom =5)
ggmap(mapdata)

#案例1：基于google地图的放射状路径图

city_list <- c("西安","西宁","郑州","重庆","成都","石家庄","兰州","济南","大同","咸阳","包头","长沙")
source("/Users/david/Downloads/吴晓玲/R/web_city_adress.r",encoding = "utf-8")
address <- GetJD(city_list)

address$lonx <- address[address$address =="长沙","lng"]
address$laty <- address[address$address =="长沙","lat"]
address <- address[address$address!= "长沙",]
names(address)[2:3]<-c("lon","lat")
address$Num<-round(runif(11,50,100),2)

ggmap(mapdata)+
  geom_segment(data=address,aes(x=lon,y=lat,xend=lonx,yend=laty),size=0.3,colour="#FF6833")+
  geom_point(data=address,aes(x=lon,y=lat,size=Num),shape=21,fill="#ED7D31",col="#E02939",alpha=.6)+
  geom_point(data=NULL,aes(x=112.97935,y=28.21347),shape=21,size=8,fill=NA,col="steelblue")+
  scale_size_area(max_size=8)+ 
  theme_nothing()

#案例2：基于google地图的路径流向图
city_list <- c("海口","广州","长沙","武汉","郑州","石家庄","北京","沈阳","长春","哈尔滨")
source("/Users/david/Downloads/吴晓玲/R/web_city_adress.r",encoding = "utf-8")
address <- GetJD(city_list)
address$Num<-round(runif(10,50,100),2)

ggmap(mapdata)+
  geom_path(data=address,aes(x=lng,y=lat),size=0.3,colour="#FF6833")+
  geom_point(data=address,aes(x=lng,y=lat,size=Num),shape=21,fill="#ED7D31",col="#E02939",alpha=.6)+
  guides(fill=FALSE)+
  scale_size_area(max_size=8)+ 
  theme_nothing()

#案例3：基于google地图的闭环路径图

city_list <- c("兰州","成都","重庆","贵阳","昆明","南宁","海口","广州","福州","上海","青岛","石家庄","呼和浩特","银川")
city_list <- c(city_list,city_list[1])

source("/Users/david/Downloads/吴晓玲/R/web_city_adress.r",encoding = "utf-8")
address <- GetJD(city_list)
address$Num<-round(runif(nrow(address),50,100),2)

ggmap(mapdata)+
  geom_path(data=address,aes(x=lng,y=lat),size=0.3,colour="#FF6833")+
  geom_point(data=address,aes(x=lng,y=lat,size=Num),shape=21,fill="#ED7D31",col="#E02939",alpha=.6)+
  guides(fill=FALSE)+
  scale_size_area(max_size=8)+ 
  theme_nothing()

####11.5 地图+mini字体地图应用####

#导入数据：
#生成一个虚拟指标，并分割为有序分段因子变量。
mymapdata <- read.csv("/Users/david/Downloads/吴晓玲/R/File/EyesAsia.csv",stringsAsFactors=FALSE,check.names=FALSE)
mymapdata<-transform(
  mymapdata,
  scale=5,
  peform=runif(43,20,50)
  )

mymapdata$group <- cut(
  mymapdata$peform,
  breaks=c(20,26,32,38,44,50),
  labels=c("20~26","26~32","32~38","38~44","44~50"),
  order=TRUE
  )

word<-c("日本","蒙古","朝鲜","韩国","青海湖","鄱阳湖","洞庭湖","太湖","洪泽湖")
mymapdata <- mymapdata %>% filter(!Cname %in% word) 

mymapdata<-arrange(mymapdata,-peform) 
mymapdata$order=1:nrow(mymapdata)

font_add("myfont","EyesAsia-Regular.otf")
font_add("myyh","msyhl.ttc")


p1<- ggplot(mymapdata,aes(order,scale,label = case))+
  ylim(-6,6)+
  coord_polar(theta="x",start=0)+
  geom_text(aes(colour = group),family="myfont",size=20)+
  scale_colour_brewer(palette="Greens",guide=FALSE)+
  theme_void()


china_map <- readOGR("/Users/david/Downloads/吴晓玲/R/rstudy/CHN_adm/bou2_4p.shp",stringsAsFactors = FALSE)

mydata1   <- china_map@data %>% mutate(id = row.names(.)) %>% select(id,NAME)   
china_map1 <- fortify(china_map)  %>% select(id,long,lat,group,order)         
china_map_data <- left_join(china_map1,mydata1,by = "id")                     

mydata2   <- read.csv("/Users/david/Downloads/吴晓玲/R/rstudy/Province/geshengzhibiao.csv",stringsAsFactors = FALSE)   
mydata2$zhibiao <- as.numeric(mydata2$zhibiao)
final_data <- left_join(china_map_data,mydata2,by = "NAME") %>% select(-province)         

#1、常见的热力填色地图：
p2<- ggplot() +
  geom_polygon(data = final_data,aes(x = long,y = lat, group = group,fill = zhibiao),colour="grey40") +
  scale_fill_distiller(palette = "BrBG",na.value = "white") +  
  guides(fill=FALSE)+
  coord_map("polyconic") + 
  theme_void()
  


CairoPNG("C:/Users/RAINDU/Desktop/chineserador.png",900,900)
showtext_begin()

vs <- viewport(width=0.95,height=0.95,x=0.5,y=0.5)    
print(p1,vp = vs)  
vs <- viewport(width=0.75,height=0.8,x=0.5,y=0.5)   
print(p2,vp=vs)

showtext_end()
dev.off()


CairoPNG("C:/Users/RAINDU/Desktop/chineserador2.png",900,900)
showtext_begin()
ggplot(mymapdata,aes(order,scale,label = case))+
  ylim(-6,6)+
  coord_polar(theta="x",start=0)+
  geom_text(aes(colour = group),family="myfont",size=20)+
  scale_colour_brewer(palette="Greens",guide=FALSE)+
  theme_void()
showtext_end()
dev.off()


CairoPNG("C:/Users/RAINDU/Desktop/chineserador3.png",900,900)
showtext_begin()
ggplot() +
  geom_polygon(data = final_data,aes(x = long,y = lat, group = group,fill = zhibiao),colour="grey40") +
  scale_fill_distiller(palette = "BrBG",na.value = "white") +  
  guides(fill=FALSE)+
  coord_map("polyconic") + 
  theme_void()
showtext_end()
dev.off()



####11.6 基于ggplot2的等值线密度图####
rm(list=ls())
gc()
library("dplyr")
library("dplyr")
library("ggplot2")
library("ggmap")
library("rgdal")
library("maps")
library("Cairo")
library("showtext")
library("baidumap")
library("grid")

china_map <- readOGR("/Users/david/Downloads/吴晓玲/R/rstudy/CHN_adm/bou2_4p.shp",stringsAsFactors = FALSE)

mydata1   <- china_map@data %>% mutate(id = row.names(.)) %>% select(id,NAME)   
china_map1 <- fortify(china_map)  %>% select(id,long,lat,group,order)         
china_map_data <- left_join(china_map1,mydata1,by = "id")                     

cityname <- c("北京", "天津", "哈尔滨", "乌鲁木齐", "西宁", "兰州", "银川", "呼和浩特", "石家庄", "太原", 
            "沈阳", "长春", "济南", "拉萨", "成都", "昆明", "西安", "郑州", "重庆", "武汉", "长沙", "贵阳", "南京", 
            "合肥", "上海", "杭州", "南昌", "福州", "广州", "南宁", "海口", "台北", "香港", "澳门", "厦门", "青岛", 
            "大连",  "无锡", "桂林")
source("/Users/david/Downloads/吴晓玲/R/web_city_adress.r",encoding = "utf-8")
address <- GetJD(cityname)

###纯ggplot2静态图

ggplot() +
  geom_polygon(data = china_map_data,aes(x = long,y = lat, group = group),colour="grey40",fill = "white") +
  geom_point(data = address,aes(x = lng,y = lat),colour = "red") +
  geom_polygon(data = address,aes(x=lng,y=lat,fill = ..level..), stat = "density_2d", alpha = .3, color = NA)+ 
  scale_fill_distiller(palette = "Reds",na.value = "white",direction = 1) +  
  guides(fill=FALSE)+
  coord_map("polyconic") + 
  theme_void()


###基于googlemap的密度热度图
bbox_everest <- c(left =60, bottom =10, right =150, top =60)
mapdata <- get_stamenmap(bbox_everest, zoom =5)


ggmap(mapdata) +
  geom_polygon(data = address,aes(x=lng,y=lat,fill = ..level..), stat="density_2d", alpha = .3, color = NA)+ 
  geom_point(data = address,aes(x = lng,y = lat),colour = "red") +
  scale_fill_distiller(palette = "Reds",na.value = "white",direction = 1) +  
  guides(fill=FALSE)+
  theme_nothing()


####END####
