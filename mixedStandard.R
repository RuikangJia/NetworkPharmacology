#功能：使用混合标准整理成分药材归属(95%以上为一种，使用这种,否则使用80%以上的所有)
    # 输入：
        # 填表.csv:所有成分额外信息
        # dataCuted95.csv:
        # dataCuted80.csv:
    # 输出：
        # 95And80Res.csv:混合比例
# 函数
pickStandard = function(data,data80,data95){
    line = data95[match(data[3],data95$X),]
    if(is.na(line$X2)){
        data[6] = line[3]
        data[2] = 95
    }else{
        line = data80[match(data[3],data80$X),]
        if(strsplit(line$X1,split = "|",fixed = T)[[1]][2] == "0"){
            data[6:9] = ""
            data[2] = 0
        }else{
            data[6:9] = line[3:6]
            data[2] = 80
        }
        
    }
    unlist(data)
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
dirName = gsub(".R$","",scriptName)
# 创建脚本文件同名文件夹
if(!dir.exists(dirName)){
    dir.create(dirName)
}
# 输出文件路径
outputDir = gsub(".R","/",wholeName)


# 2数据导入
dataTotal = read.csv("填表.csv")
data95 = read.csv("dataCuted95.csv")
data80 = read.csv("dataCuted80.csv")
# 比例混合
res = data.frame(t(apply(dataTotal,1,pickStandard,data80,data95)))
res[is.na(res)] = ""
# 保存
write.csv(res,paste(outputDir,"95And80Res.csv"),row.names = F)
