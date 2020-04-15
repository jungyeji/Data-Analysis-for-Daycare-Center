setwd("C:/Users/yeji/Documents/R/project")

library(readxl)
#서울시 5구 2018년 동별 0-4세 인구통계
data1 = read_excel("서울시 5구 2018년 동별 0-4세 인구통계.xls")

dobong1 = data1[data1$자치구=="도봉구",][c(-1,-2,-3),c(-1,-3)]
nowon1 = data1[data1$자치구=="노원구",][c(-1,-2,-3),c(-1,-3)]
yangchun1 = data1[data1$자치구=="양천구",][c(-1,-2,-3),c(-1,-3)]
seocho1 = data1[data1$자치구=="서초구",][c(-1,-2,-3),c(-1,-3)]
gangnam1 = data1[data1$자치구=="강남구",][c(-1,-2,-3),c(-1,-3)]

dobong1 = dobong1[seq(1,42,3),]
nowon1 = nowon1[seq(1,57,3),]
yangchun1 = yangchun1[seq(1,54,3),]
seocho1 = seocho1[seq(1,54,3),]
gangnam1 = gangnam1[seq(1,66,3),]

#서울시 5구 2017년 동별 혼인 통계
data2 = read_excel("서울시 5구 2017년 동별 혼인 통계.xls")

dobong2 = data2[data2$자치구=="도봉구",][-1,-1]
nowon2 = data2[data2$자치구=="노원구",][-1,-1]
yangchun2 = data2[data2$자치구=="양천구",][-1,-1]
seocho2 = data2[data2$자치구=="서초구",][-1,-1]
gangnam2 = data2[data2$자치구=="강남구",][-1,-1]

#서울시 5구 2017년 동별 출생 통계
data3 = read_excel("서울시 5구 2017년 동별 출생 통계.xls")

dobong3 = data3[data3$자치구=="도봉구",][-1,-1]
nowon3 = data3[data3$자치구=="노원구",][-1,-1]
yangchun3 = data3[data3$자치구=="양천구",][-1,-1]
seocho3 = data3[data3$자치구=="서초구",][-1,-1]
gangnam3 = data3[data3$자치구=="강남구",][-1,-1]

#서울시 5구 2017년 동별 아파트가구수 통계
data4 = read_excel("서울시 5구 2017년 동별 아파트가구수 통계.xls")

dobong4 = data4[data4$자치구=="도봉구",][-1,-1]
nowon4 = data4[data4$자치구=="노원구",][-1,-1]
yangchun4 = data4[data4$자치구=="양천구",][-1,-1]
seocho4 = data4[data4$자치구=="서초구",][-1,-1]
gangnam4 = data4[data4$자치구=="강남구",][-1,-1]

dobong = cbind(dobong1,dobong2,dobong3)[c(1,2,4,6)]
nowon = cbind(nowon1,nowon2,nowon3)[c(1,2,4,6)]
yangchun = cbind(yangchun1,yangchun2,yangchun3)[c(1,2,4,6)]
seocho = cbind(seocho1,seocho2,seocho3)[c(1,2,4,6)]
gangnam = cbind(gangnam1,gangnam2,gangnam3)[c(1,2,4,6)]

dobong$`0~4세` = as.numeric(dobong$`0~4세`)
nowon$`0~4세` = as.numeric(nowon$`0~4세`)
yangchun$`0~4세` = as.numeric(yangchun$`0~4세`)

#3구 데이터 kids,marr,birth
data = rbind(dobong,nowon,yangchun,seocho,gangnam)
colnames(data) = c("dong","kids","marr","birth")
data$kids = as.numeric(data$kids)
str(data)

#minmax normalizaiton

normalize <- function(x){
  return(100*(x-min(x))/(max(x)-min(x)))
}

#3구 데이터 정규화 점수
data_norm = data.frame(cbind(data[1],apply(data[2:4],2,normalize)))
data_norm_score = cbind(data[1],apply(data_norm[2:4],1,sum))
colnames(data_norm_score) = c("동","score")
data_norm_score_sort = data_norm_score[order(-data_norm_score$score),]

data_norm_score_sort

#kindergarden
kg = as.data.frame(read_excel("5구 동별 보육시설.xls"))

#도봉
dobong_kg = kg[kg[1] == "도봉구",][-1,]

#노원
nowon_kg = kg[kg[1] == "노원구",][-1,] 

#양천
yangchun_kg = kg[kg[1] == "양천구",][-1,] 

#서초
seocho_kg = kg[kg[1] == "서초구",][-1,] 

#강남
gangnam_kg = kg[kg[1] == "강남구",][-1,] 

kg = data.frame(rbind(dobong_kg,nowon_kg,yangchun_kg,seocho_kg,gangnam_kg))


#최종데이터
finaldata = cbind(data,kg)[c(1,2,3,4,7)]
colnames(finaldata) = c("dong","kids","marr","birth","kg")
str(finaldata)
finaldata$kids = as.numeric(finaldata$kids)
finaldata$sum = finaldata$kids+finaldata$marr+finaldata$birth
finaldata$kidskg = finaldata$kids/finaldata$kg
finaldata$sumkg = finaldata$sum/finaldata$kg

#최종데이터 3구와 2구로 분할
finaldata_3gu = finaldata[1:51,]
finaldata_2gu = finaldata[52:91,]

########################################################################################
#상관관계
library(corrgram)
finaldata_3gu_tmp = finaldata_3gu
finaldata_3gu_tmp$dong= NULL
finaldata_3gu_tmp$kidskg = NULL
finaldata_3gu_tmp$sumkg = NULL
finaldata_3gu_tmp$sum = NULL
cor(finaldata_3gu_tmp)
corrgram(cor(finaldata_3gu_tmp),type="corr",upper.panel = panel.conf)
finaldata_tmp = finaldata
finaldata_tmp$dong= NULL
finaldata_tmp$kidskg = NULL
finaldata_tmp$sumkg = NULL
finaldata_tmp$sum = NULL
cor(finaldata_tmp)
corrgram(cor(finaldata_tmp),type="corr",upper.panel = panel.conf)

finaldata_2gu[order(finaldata_2gu$sumkg)]
#회귀 
#혼인수와 출산수로 0-4세 예측
model.lm = lm(kids~marr+birth,finaldata)
summary(model.lm)

str(model.lm)
model.lm$fitted.values
finaldata$kids

fitted = cbind(finaldata$dong, finaldata$kids-model.lm$fitted.values)
write.csv(fitted, "kids.csv")

#군집
library(cluster)
kmeans1 = kmeans(finaldata_3gu[2:5],3)

finaldata_3gu[kmeans1$cluster == 1,][1]
finaldata_3gu[kmeans1$cluster == 2,][1]
finaldata_3gu[kmeans1$cluster == 3,][1]
plot(finaldata_3gu[2:5], pch=kmeans1$cluster, col=kmeans1$cluster)


kmeans2 = kmeans(finaldata_2gu[2:5],3)
finaldata_2gu[kmeans2$cluster == 1,][1]
finaldata_2gu[kmeans2$cluster == 2,][1]
finaldata_2gu[kmeans2$cluster == 3,][1]
plot(finaldata_2gu[2:5], pch=kmeans2$cluster, col=kmeans2$cluster)


#회귀1
#marr와 birth로 kids 예측
model1.lm = lm(kids~marr+birth,finaldata)
summary(model1.lm) 
#모든 변수가 유의함

model1.lm$fitted.values
finaldata$kids

fitted = cbind(finaldata$dong, finaldata$kids-model.lm$fitted.values)
write.csv(fitted, "kids.csv")

#회귀2
#kids, marr, birth로 kg 예측
model2.lm = lm(kg~kids+marr+birth,finaldata)
summary(model2.lm) 
#kids 변수만 유의함

#회귀3
#kids 변수로만 kg 예측
model3.lm = lm(kg~kids,finaldata)
summary(model3.lm) 
#adj r square가 0.5 근방으로 높지 않은 것으로 볼 때 파악하지 못한 변수들이 있음을 알 수 있다.

#지도학습과 비지도학습을 혼합하여 분석
#3구에 대해 kg변수를 제외하고 kids, marr, birth 세변수만을 이용해 정규화 점수를 이용한 군집분석
#1.정규화 점수 계산
finaldata_3gu_norm = data.frame(cbind(finaldata_3gu[1],apply(finaldata_3gu[2:4],2,normalize)))
finaldata_norm_score = cbind(finaldata_3gu[1],apply(finaldata_3gu_norm[2:4],1,sum))
colnames(finaldata_norm_score) = c("동","score")
finaldata_norm_score_sort = finaldata_norm_score[order(-finaldata_norm_score$score),]
head(finaldata_norm_score_sort,5)

#2.정규화 점수로 군집분석finaldata_norm_score_sort
kmeans3 = kmeans(finaldata_norm_score[2],3)
kmeans3$size

finaldata_3gu$class = 0

#3.군집분석 결과로 상,중,하 labeling
#상
finaldata_3gu[kmeans3$cluster == 1,]$class = "high"
#중 
finaldata_3gu[kmeans3$cluster == 2,]$class = "middle" 
#하
finaldata_3gu[kmeans3$cluster == 3,]$class = "low"

finaldata_3gu$class = as.factor(finaldata_3gu$class)

table(finaldata_3gu$class)

#4.DT 적합
library("party")
tree1 = ctree(class~kids+marr+birth, data=finaldata_3gu)
print(tree1)
plot(tree1)
plot(tree1, type = "simple")

#5.결과
library(caret)
library(e1071)
confusionMatrix(finaldata_3gu$class,predict(tree1))
