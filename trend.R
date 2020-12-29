# 功能：绘制入血成分散点图、折线图
    # 输入：
        # dataTotal2.csv:含有成分和平均值的数据
    # 输出：
        # point.png:成分含量散点图
        # pointAndLine.png：成分含量散点折线图
# 使用函数
dataTransform = function(data,name){
    dataMelted = melt(data,id.vars = name,variable.name = "group",value.name = "value")
    dataMelted[,1] =factor(dataMelted[,1],levels = data[,1],ordered = T)
    dataMelted$value = log(dataMelted$value +1)
    dataMelted
}
myPointPlot = function(data,colNum,color,shape,fileName){
    # 数据变换
    dataMelted = dataTransform(data[,colNum],"compound")
    p = ggplot(dataMelted,aes(x = compound,y = value,fill = group,color = group,shape = group))
    p + geom_point(stat = "identity",size = 1.3) + 
        # geom_line(aes(group=group)) +
        #坐标轴刻度及标签
        scale_x_discrete(breaks = NULL) +
        # 自定义点的颜色和形状
        scale_colour_manual(values = color) +
        scale_shape_manual(values = shape) +
        xlab(label = "compound") + 
        ylab(label = "log(value+1)") +
        theme(panel.background = element_blank(),axis.line = element_line(colour = "black"), axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.position = c(0.85,0.9),legend.key = element_rect(fill = "white"))
    ggsave(fileName,width = 15,height = 8)
}
myPointAndLinePlot = function(data,colNum,color,shape,fileName){
    # 数据变换
    dataMelted = dataTransform(data[,colNum],"compound")
    p = ggplot(dataMelted,aes(x = compound,y = value,fill = group,color = group,shape = group))
    p + geom_point(stat = "identity",size = 1.3) + 
        geom_line(aes(group=group)) +
        #坐标轴刻度及标签
        scale_x_discrete(breaks = NULL) +
        # 自定义点的颜色和形状
        scale_colour_manual(values = color) +
        scale_shape_manual(values = shape) +
        xlab(label = "compound") + 
        ylab(label = "log(value+1)") +
        theme(panel.background = element_blank(),axis.line = element_line(colour = "black"), axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.position = c(0.85,0.9),legend.key = element_rect(fill = "white"))
    ggsave(fileName,width = 15,height = 8)
}
meanBlank = function(data){
    colNum = length(data)
    countZero = sum(as.numeric(data) == 0)
    if(countZero >= 2){
        return(0)
    }else{
        return(sum(data)/(colNum - countZero))
    }
}
meanExp = function(data){
    colNum = length(data)
    countZero = sum(as.numeric(data) == 0)
    if(countZero >= 3){
        return(0)
    }else{
        return(sum(data)/(colNum - countZero))
    }
}


# 1环境、目录
library(ggplot2)
library(reshape2)
# 获取当前脚本文件路径全称
wholeName = parent.frame(2)$filename
# 文件名和路径名称
pathName = dirname(wholeName)
scriptName = basename(wholeName)
# 设置路径
setwd(pathName)
# 文件夹名称
dirName = gsub(".R","",scriptName)
# 创建脚本文件同名文件夹
if(!dir.exists(dirName)){
    dir.create(dirName)
}
# 输出文件路径
outputDir = gsub(".R$","/",wholeName)


# 2绘图
data = read.csv("dataTotal2.csv")
data = data[,c(2,20:23)]
# 排序
data = data[order(data$入血前),]
# 正常剂量-入血前
myPointPlot(data,c(1,3,4,5),c(2,3,4),c(0,1,3),paste(outputDir,"point.png"))
myPointAndLinePlot(data,c(1,3,4,5),c(2,3,4),c(0,1,3),paste(outputDir,"pointAndLine.png"))