# 功能：不同方式提取的成分含量
    # 输入：
        # dataTotal.csv：属性含有compound,乙酸乙酯提取物,氯仿提取物,正丁醇提取物,水提取物
    # 输出：
        # 0.95Cut.csv：对应比例截取
        # 0.8Cut.csv：
# 函数
toRatio = function(data){
    sum = sum(data)
    if(sum != 0){
        data = data/sum
    }
    data
}
ratioCut = function(data,threShold){
    data = sort(data,decreasing = T)
    ratioSum = 0
    i = 1
    while(ratioSum < threShold & i < 5){
        if(data[i] != 0){
            ratioSum = ratioSum + data[i]
        }
        i = i +1
    }
    res = c()
    for(j in 1:i-1){
        res = c(res,paste(names(data)[j],data[j],sep = "|"))
    }
    res = c(ratioSum,toString(res))
    res
}


# 1环境、目录
library(ggplot2)
library(reshape2)
threShold =0.95
herbNum = 4
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


# 2数据导入
dataTotal = read.csv("dataTotal.csv")
# 提取数据
data  = data.frame(compound = dataTotal$compound,乙酸乙酯提取物 = dataTotal$乙酸乙酯提取物,氯仿提取物 = dataTotal$氯仿提取物,正丁醇提取物 = dataTotal$正丁醇提取物,水提取物 = dataTotal$水提取物)
#转换为比例
dataProportional = data.frame(t(apply(data[,2:5],1,toRatio)))


# 3截取操作
# 截取，整理数据
dataCuted = data.frame((t(apply(dataProportional,1,ratioCut,threShold))),row.names = data$compound)
names(dataCuted) = c("ratio","herb")
# 逗号展开
expandCol = lapply(dataCuted$herb,strsplit,",")
expandData = data.frame(matrix(NA,dim(dataCuted)[1],herbNum))
for(i in 1:dim(expandData)[1]){
    line = unlist(expandCol[[i]][[1]])
    expandData[i,1:length(line)] = line
}


# 4数据整理、输出
res = data.frame(ratio = dataCuted$ratio,expandData,row.names = rownames(dataCuted))
write.csv(res,paste(outputDir,"dataCuted",threShold*100,".csv",sep = ""))
