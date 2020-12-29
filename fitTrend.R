# 功能：绘制不同组成分含量的拟合线
    # 输入：
        # dataTotal.csv：属性含有compound,空白对照1,空白对照2,空白对照3,正常剂量1,正常剂量2,正常剂量3,高剂量1,高剂量2,高剂量3,入血前1,入血前2,入血前3
    # 输出：
        # dataTotal2.csv：额外添加不同组的平均值
        # fitPlot.png：拟合图
# 使用函数
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
dataTransform = function(data,name){
    dataMelted = melt(data,id.vars = name,variable.name = "group",value.name = "value")
    dataMelted[,1] =factor(dataMelted[,1],levels = data[,1],ordered = T)
    dataMelted$value = log(dataMelted$value +1)
    dataMelted
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


# 2数据整理
# 读取
dataTotal = read.csv("dataTotal.csv")
# 新列：空白对照；正常剂量；高剂量
dataTotal$空白对照 = apply(data.frame(dataTotal$空白对照1,dataTotal$空白对照2,dataTotal$空白对照3),1,meanBlank)
dataTotal$正常剂量 = apply(data.frame(dataTotal$正常剂量1,dataTotal$正常剂量2,dataTotal$正常剂量3),1,meanExp)
dataTotal$高剂量 = apply(data.frame(dataTotal$高剂量1,dataTotal$高剂量2,dataTotal$高剂量3),1,meanExp)
dataTotal$入血前 = apply(data.frame(dataTotal$入血前1,dataTotal$入血前2,dataTotal$入血前3),1,meanExp)
# 保存
write.csv(dataTotal,paste(outputDir,"dataTotal2.csv"),row.names = F)
# 提取平均值部分
data = data.frame(compound = dataTotal$compound,空白对照 = dataTotal$空白对照,正常剂量 = dataTotal$正常剂量,高剂量 = dataTotal$高剂量,入血前 = dataTotal$入血前)
# 排序
data = data[order(data$入血前),]
# 横坐标
rownames(data) = data$compound
data$compound = 1:dim(data)[1]-1
# log转换
data[,-1] = log(data[,-1] +1,2)
# 数据转换
dataMelted = dataTransform(data,"compound")


# 3图形展示
p = ggplot(dataMelted,aes(x = compound,y = value,fill = group,color = group,shape = group))
p + geom_point(stat = "identity",size = 1.3) + 
    geom_smooth(method = "lm",aes(group = group))+
    #坐标轴刻度及标签
    scale_x_discrete(breaks = NULL) +
    # 自定义点的颜色和形状
    scale_colour_manual(values = c("grey","blue","red","black")) +
    scale_shape_manual(values = c(0,1,2,3)) +
    xlab(label = "compound") + 
    ylab(label = "log(value+1)") +
    theme(panel.background = element_blank(),axis.line = element_line(colour = "black"), axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.key = element_rect(fill = "white"))
ggsave(paste(outputDir,"fitPlot.png"),width = 10,height = 8)
