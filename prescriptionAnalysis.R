#功能：分析药方的含量组成
    # 输入：
        # allCompound.csv:所有成分含量信息，成分在行，不同药材在列，每种成分每种药材含有多少。
    # 输出：
        # singleContent.png：药材根据成分总含量降序。
        # totalContent.png：该顺序下，每种成分含量的箱线图。
#函数
herbSort = function(data){
    herbTotal = apply(data,2,sum)
    herbOrder = order(herbTotal,decreasing = T)
    dataHerbSotred = data[,herbOrder]
    list(dataHerbSotred,herbTotal[herbOrder])
}
ratioCal = function(data){
    data = data/data[length(data)]
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


# 2导入数据
# 当前目录文件
fileNames = dir()[grep(".csv",dir())]
data = read.csv(fileNames,row.names = 1)


# 3数据计算
# 单味药材成分含量降序
sortRes = herbSort(data)
dataHerbSotred = sortRes[[1]]
herbTotal = sortRes[[2]]
#药材中单化合物含量累积、比例
singleCompoundAcc = t(apply(dataHerbSotred,1,cumsum))
singleCompoundAccRatio = t(apply(singleCompoundAcc,1,ratioCal))
#药材中所有化合物含量累积、比例
allCompoundAcc = cumsum(herbTotal)
allCompoundAccRatio = ratioCal(allCompoundAcc)


# 4绘图
# 药材所有化合物含量图
dataDraw = data.frame(name = names(allCompoundAccRatio),ratio = allCompoundAccRatio)
dataDraw$name = factor(dataDraw$name,levels =dataDraw$name, ordered = T)
# 数据和映射
p = ggplot(dataDraw,aes(x = name,y = ratio))
# 几何对象(geom)和统计变换(stat)
p + geom_point(size = 3) +  
    xlab("药材") +
    ylab("ratio") +
    geom_text(label = round(dataDraw$ratio,2),vjust= 1.5) +
    #标度：标签、自定义图形选项、坐标轴
    scale_y_continuous(limits = c(0,1)) +
    theme(panel.background = element_blank(),axis.line = element_line(colour = "black"), axis.text = element_text(size = 20),axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1),axis.title = element_text(size = 20))
ggsave(paste(outputDir,"totalContent.png",sep = ""))
# 药材中单化合物含量图
dataDraw = data.frame(singleCompoundAccRatio)
dataDraw = melt(dataDraw,measure.vars = names(dataDraw))
p = ggplot(dataDraw,aes(x = variable,y = value))
p + geom_boxplot(position ="dodge") +
    xlab("药材") +
    ylab("ratio") +
    annotate("text", label = "0.019", x = 11, y = .01, size = 4, colour = "red") +
    annotate("text", label = "0.342", x = 15, y = .32, size = 4, colour = "red") +
    annotate("text", label = "0.455", x = 16, y = .43, size = 4, colour = "red") +
    annotate("text", label = "0.899", x = 17, y = .87, size = 4, colour = "red") +
    theme(panel.background = element_blank(),axis.line = element_line(colour = "black"), axis.text = element_text(size = 20),axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1),axis.title = element_text(size = 20) )
ggsave(paste(outputDir,"singleContent.png",sep = ""))

