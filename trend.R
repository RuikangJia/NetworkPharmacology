# ���ܣ�������Ѫ�ɷ�ɢ��ͼ������ͼ
    # ���룺
        # dataTotal2.csv:���гɷֺ�ƽ��ֵ������
    # �����
        # point.png:�ɷֺ���ɢ��ͼ
        # pointAndLine.png���ɷֺ���ɢ������ͼ
# ʹ�ú���
dataTransform = function(data,name){
    dataMelted = melt(data,id.vars = name,variable.name = "group",value.name = "value")
    dataMelted[,1] =factor(dataMelted[,1],levels = data[,1],ordered = T)
    dataMelted$value = log(dataMelted$value +1)
    dataMelted
}
myPointPlot = function(data,colNum,color,shape,fileName){
    # ���ݱ任
    dataMelted = dataTransform(data[,colNum],"compound")
    p = ggplot(dataMelted,aes(x = compound,y = value,fill = group,color = group,shape = group))
    p + geom_point(stat = "identity",size = 1.3) + 
        # geom_line(aes(group=group)) +
        #������̶ȼ���ǩ
        scale_x_discrete(breaks = NULL) +
        # �Զ�������ɫ����״
        scale_colour_manual(values = color) +
        scale_shape_manual(values = shape) +
        xlab(label = "compound") + 
        ylab(label = "log(value+1)") +
        theme(panel.background = element_blank(),axis.line = element_line(colour = "black"), axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.position = c(0.85,0.9),legend.key = element_rect(fill = "white"))
    ggsave(fileName,width = 15,height = 8)
}
myPointAndLinePlot = function(data,colNum,color,shape,fileName){
    # ���ݱ任
    dataMelted = dataTransform(data[,colNum],"compound")
    p = ggplot(dataMelted,aes(x = compound,y = value,fill = group,color = group,shape = group))
    p + geom_point(stat = "identity",size = 1.3) + 
        geom_line(aes(group=group)) +
        #������̶ȼ���ǩ
        scale_x_discrete(breaks = NULL) +
        # �Զ�������ɫ����״
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


# 2��ͼ
data = read.csv("dataTotal2.csv")
data = data[,c(2,20:23)]
# ����
data = data[order(data$��Ѫǰ),]
# ��������-��Ѫǰ
myPointPlot(data,c(1,3,4,5),c(2,3,4),c(0,1,3),paste(outputDir,"point.png"))
myPointAndLinePlot(data,c(1,3,4,5),c(2,3,4),c(0,1,3),paste(outputDir,"pointAndLine.png"))