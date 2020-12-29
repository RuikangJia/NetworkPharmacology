#���ܣ�����ҩ���ĺ������
    # ���룺
        # allCompound.csv:���гɷֺ�����Ϣ���ɷ����У���ͬҩ�����У�ÿ�ֳɷ�ÿ��ҩ�ĺ��ж��١�
    # �����
        # singleContent.png��ҩ�ĸ��ݳɷ��ܺ�������
        # totalContent.png����˳���£�ÿ�ֳɷֺ���������ͼ��
#����
herbSort = function(data){
    herbTotal = apply(data,2,sum)
    herbOrder = order(herbTotal,decreasing = T)
    dataHerbSotred = data[,herbOrder]
    list(dataHerbSotred,herbTotal[herbOrder])
}
ratioCal = function(data){
    data = data/data[length(data)]
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
# ��ǰĿ¼�ļ�
fileNames = dir()[grep(".csv",dir())]
data = read.csv(fileNames,row.names = 1)


# 3���ݼ���
# ��ζҩ�ĳɷֺ�������
sortRes = herbSort(data)
dataHerbSotred = sortRes[[1]]
herbTotal = sortRes[[2]]
#ҩ���е������ﺬ���ۻ�������
singleCompoundAcc = t(apply(dataHerbSotred,1,cumsum))
singleCompoundAccRatio = t(apply(singleCompoundAcc,1,ratioCal))
#ҩ�������л����ﺬ���ۻ�������
allCompoundAcc = cumsum(herbTotal)
allCompoundAccRatio = ratioCal(allCompoundAcc)


# 4��ͼ
# ҩ�����л����ﺬ��ͼ
dataDraw = data.frame(name = names(allCompoundAccRatio),ratio = allCompoundAccRatio)
dataDraw$name = factor(dataDraw$name,levels =dataDraw$name, ordered = T)
# ���ݺ�ӳ��
p = ggplot(dataDraw,aes(x = name,y = ratio))
# ���ζ���(geom)��ͳ�Ʊ任(stat)
p + geom_point(size = 3) +  
    xlab("ҩ��") +
    ylab("ratio") +
    geom_text(label = round(dataDraw$ratio,2),vjust= 1.5) +
    #��ȣ���ǩ���Զ���ͼ��ѡ�������
    scale_y_continuous(limits = c(0,1)) +
    theme(panel.background = element_blank(),axis.line = element_line(colour = "black"), axis.text = element_text(size = 20),axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1),axis.title = element_text(size = 20))
ggsave(paste(outputDir,"totalContent.png",sep = ""))
# ҩ���е������ﺬ��ͼ
dataDraw = data.frame(singleCompoundAccRatio)
dataDraw = melt(dataDraw,measure.vars = names(dataDraw))
p = ggplot(dataDraw,aes(x = variable,y = value))
p + geom_boxplot(position ="dodge") +
    xlab("ҩ��") +
    ylab("ratio") +
    annotate("text", label = "0.019", x = 11, y = .01, size = 4, colour = "red") +
    annotate("text", label = "0.342", x = 15, y = .32, size = 4, colour = "red") +
    annotate("text", label = "0.455", x = 16, y = .43, size = 4, colour = "red") +
    annotate("text", label = "0.899", x = 17, y = .87, size = 4, colour = "red") +
    theme(panel.background = element_blank(),axis.line = element_line(colour = "black"), axis.text = element_text(size = 20),axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1),axis.title = element_text(size = 20) )
ggsave(paste(outputDir,"singleContent.png",sep = ""))
