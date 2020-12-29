#���ܣ�ʹ�û�ϱ�׼�����ɷ�ҩ�Ĺ���(95%����Ϊһ�֣�ʹ������,����ʹ��80%���ϵ�����)
    # ���룺
        # ���.csv:���гɷֶ�����Ϣ
        # dataCuted95.csv:
        # dataCuted80.csv:
    # �����
        # 95And80Res.csv:��ϱ���
# ����
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


# 1������Ŀ¼
library(ggplot2)
library(reshape2)
# ��ȡ��ǰ�ű��ļ�·��ȫ��
wholeName = parent.frame(2)$filename
# �ļ�����·������
pathName = dirname(wholeName)
scriptName = basename(wholeName)
# ����·��
setwd(pathName)
# �ļ�������
dirName = gsub(".R$","",scriptName)
# �����ű��ļ�ͬ���ļ���
if(!dir.exists(dirName)){
    dir.create(dirName)
}
# ����ļ�·��
outputDir = gsub(".R","/",wholeName)


# 2���ݵ���
dataTotal = read.csv("���.csv")
data95 = read.csv("dataCuted95.csv")
data80 = read.csv("dataCuted80.csv")
# �������
res = data.frame(t(apply(dataTotal,1,pickStandard,data80,data95)))
res[is.na(res)] = ""
# ����
write.csv(res,paste(outputDir,"95And80Res.csv"),row.names = F)