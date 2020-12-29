# ���ܣ����Ʋ�ͬ��ɷֺ����������
    # ���룺
        # dataTotal.csv�����Ժ���compound,�հ׶���1,�հ׶���2,�հ׶���3,��������1,��������2,��������3,�߼���1,�߼���2,�߼���3,��Ѫǰ1,��Ѫǰ2,��Ѫǰ3
    # �����
        # dataTotal2.csv���������Ӳ�ͬ���ƽ��ֵ
        # fitPlot.png�����ͼ
# ʹ�ú���
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
dirName = gsub(".R","",scriptName)
# �����ű��ļ�ͬ���ļ���
if(!dir.exists(dirName)){
    dir.create(dirName)
}
# ����ļ�·��
outputDir = gsub(".R$","/",wholeName)


# 2��������
# ��ȡ
dataTotal = read.csv("dataTotal.csv")
# ���У��հ׶��գ������������߼���
dataTotal$�հ׶��� = apply(data.frame(dataTotal$�հ׶���1,dataTotal$�հ׶���2,dataTotal$�հ׶���3),1,meanBlank)
dataTotal$�������� = apply(data.frame(dataTotal$��������1,dataTotal$��������2,dataTotal$��������3),1,meanExp)
dataTotal$�߼��� = apply(data.frame(dataTotal$�߼���1,dataTotal$�߼���2,dataTotal$�߼���3),1,meanExp)
dataTotal$��Ѫǰ = apply(data.frame(dataTotal$��Ѫǰ1,dataTotal$��Ѫǰ2,dataTotal$��Ѫǰ3),1,meanExp)
# ����
write.csv(dataTotal,paste(outputDir,"dataTotal2.csv"),row.names = F)
# ��ȡƽ��ֵ����
data = data.frame(compound = dataTotal$compound,�հ׶��� = dataTotal$�հ׶���,�������� = dataTotal$��������,�߼��� = dataTotal$�߼���,��Ѫǰ = dataTotal$��Ѫǰ)
# ����
data = data[order(data$��Ѫǰ),]
# ������
rownames(data) = data$compound
data$compound = 1:dim(data)[1]-1
# logת��
data[,-1] = log(data[,-1] +1,2)
# ����ת��
dataMelted = dataTransform(data,"compound")


# 3ͼ��չʾ
p = ggplot(dataMelted,aes(x = compound,y = value,fill = group,color = group,shape = group))
p + geom_point(stat = "identity",size = 1.3) + 
    geom_smooth(method = "lm",aes(group = group))+
    #������̶ȼ���ǩ
    scale_x_discrete(breaks = NULL) +
    # �Զ�������ɫ����״
    scale_colour_manual(values = c("grey","blue","red","black")) +
    scale_shape_manual(values = c(0,1,2,3)) +
    xlab(label = "compound") + 
    ylab(label = "log(value+1)") +
    theme(panel.background = element_blank(),axis.line = element_line(colour = "black"), axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.key = element_rect(fill = "white"))
ggsave(paste(outputDir,"fitPlot.png"),width = 10,height = 8)