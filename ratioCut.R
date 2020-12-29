# ���ܣ���ͬ��ʽ��ȡ�ĳɷֺ���
    # ���룺
        # dataTotal.csv�����Ժ���compound,����������ȡ��,�ȷ���ȡ��,��������ȡ��,ˮ��ȡ��
    # �����
        # 0.95Cut.csv����Ӧ������ȡ
        # 0.8Cut.csv��
# ����
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


# 1������Ŀ¼
library(ggplot2)
library(reshape2)
threShold =0.95
herbNum = 4
# ��ȡ��ǰ�ű��ļ�·��ȫ��
wholeName = parent.frame(2)$filename
# �ļ�����·������
pathName = dirname(wholeName)
scriptName = basename(wholeName)
# ����·��
setwd(pathName)
# �ļ�������
dirName = gsub(".R","",scriptName)
# �����ű��ļ�ͬ���ļ���
if(!dir.exists(dirName)){
    dir.create(dirName)
}
# ����ļ�·��
outputDir = gsub(".R$","/",wholeName)


# 2���ݵ���
dataTotal = read.csv("dataTotal.csv")
# ��ȡ����
data  = data.frame(compound = dataTotal$compound,����������ȡ�� = dataTotal$����������ȡ��,�ȷ���ȡ�� = dataTotal$�ȷ���ȡ��,��������ȡ�� = dataTotal$��������ȡ��,ˮ��ȡ�� = dataTotal$ˮ��ȡ��)
#ת��Ϊ����
dataProportional = data.frame(t(apply(data[,2:5],1,toRatio)))


# 3��ȡ����
# ��ȡ����������
dataCuted = data.frame((t(apply(dataProportional,1,ratioCut,threShold))),row.names = data$compound)
names(dataCuted) = c("ratio","herb")
# ����չ��
expandCol = lapply(dataCuted$herb,strsplit,",")
expandData = data.frame(matrix(NA,dim(dataCuted)[1],herbNum))
for(i in 1:dim(expandData)[1]){
    line = unlist(expandCol[[i]][[1]])
    expandData[i,1:length(line)] = line
}


# 4�������������
res = data.frame(ratio = dataCuted$ratio,expandData,row.names = rownames(dataCuted))
write.csv(res,paste(outputDir,"dataCuted",threShold*100,".csv",sep = ""))