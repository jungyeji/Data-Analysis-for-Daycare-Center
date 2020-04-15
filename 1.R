setwd("C:/Users/yeji/Documents/R/project")

library("readxl")

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


#서울시 5구 동별 보육시설 통계
data5 = read_excel("5구 동별 보육시설.xls")

dobong5 = data5[data5$자치구=="도봉구",][-1,-1]
nowon5 = data5[data5$자치구=="노원구",][-1,-1]
yangchun5 = data5[data5$자치구=="양천구",][-1,-1]
seocho5 = data5[data5$자치구=="서초구",][-1,-1]
gangnam5 = data5[data5$자치구=="강남구",][-1,-1]


#scaling
normalize <- function(x){
  return(100*(x-min(x))/(max(x)-min(x)))
}

##
seocho = cbind(seocho1, seocho2[2],seocho3[2],seocho4[3],seocho5[2])
seochotmp = seocho
seochotmp$동=NULL
seochotmp$`0~4세`=as.numeric(seochotmp$`0~4세`)
cor(seochotmp)

library(corrgram)
corrgram(cor(seochotmp),type="corr",upper.panel = panel.conf)

seochotmp$kid=seochotmp$`0~4세`/seochotmp$보육시설

seochokid = lm(kid~., data=seochotmp)
summary(seochokid)
plot(kid~., data=seochotmp)

seochokidsimple = lm(kid~`0~4세`, data=seochotmp)
anova(seochokid, seochokidsimple)

seochokinder = lm(보육시설~., data=seochotmp)
summary(seochokinder)
