setwd("C:/Users/yeji/Documents/R/project")
#scaling
normalize <- function(x){
  return(100*(x-min(x))/(max(x)-min(x)))
}

#0~4세
kids = c(4167,4308,8007,12587,12046,11409,13874,15238,9401,10841,18484,16307,10742,14297,15723,24927,17188,8318,15356,14296,14911,16930,17866,26350,15846)

library("readxl")

data1 = read_excel("kindergarden.xls")[c(1,2)]
data1 = data.frame(data1)[-1,1:2]
data1$sum = kids/data1$sum

data2 = read_excel("서울시 구별 공동주택 통계.xls")[-1,2]

data3 = read_excel("서울시 구별 석유판매소 통계.xls")[-1,2]

data = cbind(data1,data2,data3)
colnames(data) = c("gu","kindergarden.kids", "houses","gasstation")
data = data.frame(cbind(data[1],apply(data[2:4],2,normalize)))

summary(data)
#kindergarden.kids
data11 = data[1:2]
data11 = data11[c(order(data11$kindergarden.kids)),]
head(data11,3)
tail(data11,2)
barplot(data11$kindergarden.kids, names.arg = data11$gu)

#houses
data22 = data[c(1,3)]
data22 = data22[c(order(-data22$houses)),]
head(data22)
tail(data22)

#gasstation
data33 = data[c(1,4)]
data33 = data33[c(order(-data33$gasstation)),]
head(data33)
tail(data33)

#clustering
library("cluster")
kmeans1 <- kmeans(data[2:4], 3) 
str(kmeans1)
kmeans1$cluster == 1

data[kmeans1$cluster == 1,][1]
data[kmeans1$cluster == 2,][1]
data[kmeans1$cluster == 3,][1]

plot(data[c("kindergarden.kids","houses")], pch=kmeans1$cluster, col=kmeans1$cluster)
plot(data[c("kindergarden.kids","gasstation")], pch=kmeans1$cluster, col=kmeans1$cluster)

#col==1 black, 2 red, 3 green 
#cluster==1 종로구, 중구, 용산구, 성동구, 광진구, 중랑구, 강북구, 서대문구, 마포구, 금청구, 동작구, 관악구
#cluster==2 강서구, 영등포구, 서초구, 강남구, 송파구
#cluster==3 동대문구, 성북구, 도봉구, 노원구, 은평구, 양천구, 구로구, 강동구

