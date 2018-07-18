### data_integration.R , data_trim.R 실행후 시작

# 1인가구 비율과 주거 형태에 대한 군집 분석

options(encoding='UTF-8')
rawdata <- read.csv('rawdata.csv', header=T, sep=',', stringsAsFactors = F)
rawdata[1] <- NULL
colnames(rawdata)[c(4:6)] <- c('1인가구', '2인이상.가구', '1인가구.비율')
str(rawdata)

### Hierachical clustering
## 전체 데이터 군집화
str(trimdata)
dist(trimdata[c(1:5),c(10, 11)], method='euclidean')
dist(normdata[c(1:5),c(10, 11)], method='euclidean')

hc_raw <- hclust(dist(trimdata[,c(-1, -2)]), method='ave')
hc_norm <- hclust(dist(normdata[,c(-1, -2)]), method='ave')
plot(hc_raw, hang=-1) # cluster 수는 5개로 결정
rect.hclust(hc_raw, k=5)
plot(hc_norm, hang=-1) # 정규화한 데이터의 hclust 결과는 부적절해 보임

## 1인가구 비율 ~ 주택 종류
hc_raw <- hclust(dist(trimdata[,c(4:8, 10:16, 37:39)]), method='ave')
hc_norm <- hclust(dist(normdata[,c(4:8, 10:16, 37:39)]), method='ave')
plot(hc_raw, hang=-1) # cluster 수는 5개로 결정
rect.hclust(hc_raw, k=5)
plot(hc_norm, hang=-1) # 정규화한 데이터의 hclust 결과는 부적절해 보임


### Kmeans clustering
## 전체 데이터로 군집화
str(trimdata)
summary(trimdata$`1인가구.비율`)
kmeans.model <- kmeans(trimdata[c(-1,-2,-3,-9)], 5) # 자치구, 동, 총.가구수, 주택.수.합계를 제외하고 k 군집화 진행
str(kmeans.model)
kmeans.model$centers # 1인가구.비율이 그룹화에 기여하는 바가 적어보임
table(kmeans.model$cluster)

kmeans.model <- kmeans(trimdata[c(-1,-2,-3,-5,-6,-9,-37,-38)], 5) # 인구, 면적, 독거노인, 여성가구비율 추가 제외
str(kmeans.model)
kmeans.model$centers # 별다른 변화 없음
table(kmeans.model$cluster)

library(caret) # trimdata 분석결과의 시각화
clusterdata <- trimdata
clusterdata$cluster <- kmeans.model$cluster
names(clusterdata)
qplot(`1인가구.비율`, `단독주택`, colour=cluster, data=clusterdata)
qplot(`1인가구.비율`, `다가구주택`, colour=cluster, data=clusterdata)
qplot(`1인가구.비율`, `연립주택`, colour=cluster, data=clusterdata)
qplot(`1인가구.비율`, `아파트`, colour=cluster, data=clusterdata)

str(normdata)
summary(normdata$`1인가구.비율`)
kmeans.model <- kmeans(normdata[c(-1,-2,-3,-9)], 5) # 정규화된 데이터로 k 군집화 진행
str(kmeans.model)
kmeans.model$centers # 1인가구.비율이 그룹화에 기여하는 바가 적어보임
table(kmeans.model$cluster)


library(caret) # normdata 분석결과의 시각화
clusterdata <- normdata
clusterdata$cluster <- kmeans.model$cluster
names(clusterdata)
par(mfrow=c(2,2))
plot(clusterdata$`1인가구.비율`, clusterdata$`단독주택`, col=clusterdata$cluster)
plot(clusterdata$`1인가구.비율`, clusterdata$`다가구주택`, col=clusterdata$cluster)
plot(clusterdata$`1인가구.비율`, clusterdata$`연립주택`, col=clusterdata$cluster)
plot(clusterdata$`1인가구.비율`, clusterdata$`아파트`, col=clusterdata$cluster)

qplot(`1인가구.비율`, `단독주택`, colour=cluster, data=clusterdata)
qplot(`1인가구.비율`, `다가구주택`, colour=cluster, data=clusterdata)
qplot(`1인가구.비율`, `연립주택`, colour=cluster, data=clusterdata)
qplot(`1인가구.비율`, `아파트`, colour=cluster, data=clusterdata)

## 1인가구 비율 ~ 주택 종류(독거노인, 여성가구 포함)
set.seed(100)
str(trimdata)  # trimdata 분석결과
summary(trimdata$`1인가구.비율`)
hometype <- trimdata[c(4,7,8, 10:16, 37:39)]
str(hometype)
kmeans.model <- kmeans(hometype, 5)
str(kmeans.model)
kmeans.model$centers # 1인가구.비율이 그룹화에 기여하는 바가 적어보임
table(kmeans.model$cluster)


str(normdata) # normdata 분석결과
summary(normdata$`1인가구.비율`)
hometype <- normdata[c(4,7,8, 10:16, 37:39)]
str(hometype)
kmeans.model <- kmeans(hometype, 5)
str(kmeans.model)
kmeans.model$centers
table(kmeans.model$cluster)

library(caret)
library(dplyr)
clusterdata <- hometype
clusterdata$cluster <- as.factor(kmeans.model$cluster)
names(clusterdata)
clusterdata$original.apt <- trimdata$`아파트`
clusterdata$`자치구` <- trimdata$`자치구`
clusterdata$recipient <- trimdata$`총.수급자`
clusterdata$population <- trimdata$`인구밀도`

str(clusterdata)
table(clusterdata$cluster)

(centers <- arrange(as.data.frame(kmeans.model$centers), `1인가구.비율`)) # 1인가구수와 여성가구수는 무관.
cov(centers)

aggregate(data=clusterdata, `아파트` ~ cluster, mean)
single.vs.apt <- aggregate(data=clusterdata, original.apt ~ cluster, mean)
single.vs.apt$recipient <- aggregate(data=clusterdata, recipient ~ cluster, mean)$recipient
single.vs.apt$population <- aggregate(data=clusterdata, population ~ cluster, mean)$population
single.vs.apt$single.ratio <- aggregate(data=clusterdata, `1인가구.비율` ~ cluster, mean)$`1인가구.비율`
arrange(single.vs.apt, single.ratio) # 1인가구 비율이 높으면 아파트에 사는 가구수도 적음


arrange(as.data.frame(kmeans.model$centers), `독거노인.수`)
old.vs.recipient <- aggregate(data=clusterdata, `독거노인.수` ~ cluster, mean)
old.vs.recipient$recipient <- aggregate(data=clusterdata, recipient ~ cluster, mean)$recipient
old.vs.recipient$recipient.ratio <- aggregate(data=clusterdata, `총.수급자` ~ cluster, mean)$총.수급자
arrange(old.vs.recipient, `독거노인.수`)


# normdata 분석결과의 시각화
png('hometype.png')
par(mfrow=c(3,2))
plot(clusterdata$`1인가구.비율`, clusterdata$`아파트`,
     col=clusterdata$cluster, xlab='1인가구 비율', ylab='아파트')
points(kmeans.model$centers[, c('1인가구.비율', '아파트')], col=c(1,2,3,4,5), pch=8, cex=5)

plot(clusterdata$`1인가구.비율`, clusterdata$`단독주택`,
     col=clusterdata$cluster, xlab='1인가구 비율', ylab='단독주택')
points(kmeans.model$centers[, c('1인가구.비율', '단독주택')], col=c(1,2,3,4,5), pch=8, cex=5)

plot(clusterdata$`1인가구.비율`, clusterdata$`다가구주택`,
     col=clusterdata$cluster, xlab='1인가구 비율', ylab='다가구주택')
points(kmeans.model$centers[, c('1인가구.비율', '다가구주택')], col=c(1,2,3,4,5), pch=8, cex=5)

plot(clusterdata$`1인가구.비율`, clusterdata$`연립주택`,
     col=clusterdata$cluster, xlab='1인가구 비율', ylab='연립주택')
points(kmeans.model$centers[, c('1인가구.비율', '연립주택')], col=c(1,2,3,4,5), pch=8, cex=5)

plot(clusterdata$`1인가구.비율`, clusterdata$`영업겸용`,
     col=clusterdata$cluster, xlab='1인가구 비율', ylab='영업겸용')
points(kmeans.model$centers[, c('1인가구.비율', '영업겸용')], col=c(1,2,3,4,5), pch=8, cex=5)

plot(clusterdata$`1인가구.비율`, clusterdata$`비거주용건물내주택`,
     col=clusterdata$cluster, xlab='1인가구 비율', ylab='비거주용건물내주택')
points(kmeans.model$centers[, c('1인가구.비율', '비거주용건물내주택')], col=c(1,2,3,4,5), pch=8, cex=5)
dev.off()

par(mfrow=c(1,1))
plot(clusterdata$`1인가구.비율`, clusterdata$`아파트`,
     col=clusterdata$cluster, xlab='1인가구 비율', ylab='아파트')
points(kmeans.model$centers[, c('1인가구.비율', '아파트')], col=c(1,2,3,4,5), pch=8, cex=5)
legend('topright', c('1', '2', '3', '4', '5'), fill=c('black', 'red', 'green', 'blue', 'cyan'))

ggplot(clusterdata, aes(`1인가구.비율`, `아파트`)) + geom_point(aes(colour=cluster))
ggplot(clusterdata, aes(`1인가구.비율`, `단독주택`)) + geom_point(aes(colour=cluster))
ggplot(clusterdata, aes(`1인가구.비율`, `다가구주택`)) + geom_point(aes(colour=cluster))
ggplot(clusterdata, aes(`1인가구.비율`, `연립주택`)) + geom_point(aes(colour=cluster))
ggplot(clusterdata, aes(`1인가구.비율`, `영업겸용`)) + geom_point(aes(colour=cluster))
ggplot(clusterdata, aes(`1인가구.비율`, `비거주용건물내주택`)) + geom_point(aes(colour=cluster))


## 1인가구 비율 ~ 주변 업종 분포
str(trimdata)  # trimdata 분석결과
summary(trimdata$`1인가구.비율`)
nearmarket <- trimdata[c(4,7,8, 17:39)]
str(hometype)
kmeans.model2 <- kmeans(nearmarket, 5)
str(kmeans.model2)
kmeans.model2$centers # 1인가구.비율이 그룹화에 기여하는 바가 적어보임
table(kmeans.model2$cluster)


str(normdata) # normdata 분석결과
summary(normdata$`1인가구.비율`)
nearmarket <- normdata[c(4,7,8, 17:39)]
str(hometype)
kmeans.model2 <- kmeans(nearmarket, 5)
str(kmeans.model)
kmeans.model2$centers 
table(kmeans.model2$cluster)

library(caret)  # normdata 분석결과의 시각화
clusterdata2 <- nearmarket
clusterdata2$cluster <- as.factor(kmeans.model2$cluster)
names(clusterdata2)
clusterdata2$`자치구` <- trimdata$`자치구`
clusterdata2$recipient <- trimdata$`총.수급자`
clusterdata2$population <- trimdata$`인구밀도`
str(clusterdata2)

(centers2 <- arrange(as.data.frame(kmeans.model2$centers), `1인가구.비율`))
library(psych)
pairs.panels(clusterdata2[c(1:13)])
pairs.panels(clusterdata2[c(1:3,14:26)])

png('nearmarket.png')
par(mfrow=c(3,2))
plot(clusterdata2$`1인가구.비율`, clusterdata2$`한식음식점업`,
     col=clusterdata2$cluster, xlab='1인가구 비율', ylab='한식음식점업')
points(kmeans.model2$centers[, c('1인가구.비율', '한식음식점업')], col=c(1,2,3,4,5), pch=8, cex=5)

plot(clusterdata2$`1인가구.비율`, clusterdata2$`비알콜.음료점`,
     col=clusterdata2$cluster, xlab='1인가구 비율', ylab='비알콜.음료점')
points(kmeans.model2$centers[, c('1인가구.비율', '비알콜.음료점')], col=c(1,2,3,4,5), pch=8, cex=5)

dev.off()

par(mfrow=c(1,1))
plot(clusterdata2$`1인가구.비율`, clusterdata2$`아파트`,
     col=clusterdata2$cluster, xlab='1인가구 비율', ylab='아파트')
points(kmeans.model2$centers[, c('1인가구.비율', '아파트')], col=c(1,2,3,4,5), pch=8, cex=5)
legend('topright', c('1', '2', '3', '4', '5'), fill=c('black', 'red', 'green', 'blue', 'cyan'))


ggplot(clusterdata2, aes(`1인가구.비율`, `한식음식점업`)) + geom_point(aes(colour=cluster))
ggplot(clusterdata2, aes(`1인가구.비율`, `비알콜.음료점`)) + geom_point(aes(colour=cluster))


## 주택 종류 ~ 주변 업종 분포
hc_raw <- hclust(dist(trimdata[,c(10:36)]), method='ave')
hc_norm <- hclust(dist(normdata[,c(10:36)]), method='ave')
plot(hc_raw, hang=-1) # cluster 수는 5개로 결정
rect.hclust(hc_raw, k=5)
plot(hc_norm, hang=-1) # 정규화한 데이터의 hclust 결과는 부적절해 보임


str(trimdata)  # trimdata 분석결과
summary(trimdata$`1인가구.비율`)
hometype.market <- trimdata[c(10:36)]
str(hometype.market)
kmeans.model3 <- kmeans(hometype.market, 5)
str(kmeans.model3)
kmeans.model3$centers
table(kmeans.model3$cluster)
(centers3 <- arrange(as.data.frame(kmeans.model3$centers), `아파트`))
# 아파트 수가 주변 업종 변화에 기여하는 바가 적어 보임

str(normdata) # normdata 분석결과
summary(normdata$`1인가구.비율`)
hometype.market <- normdata[c(10:36)]
str(hometype.market)
kmeans.model3 <- kmeans(hometype.market, 5)
str(kmeans.model3)
kmeans.model3$centers
table(kmeans.model3$cluster)
(centers3 <- arrange(as.data.frame(kmeans.model3$centers), `아파트`))
# 아파트 수와 요식업종이 정비례하지 않음(직접적인 관계없고, 1인가구비율과 관련이 있는가?)

library(caret)  # tirmdata 분석결과의 시각화
clusterdata3 <- hometype.market
clusterdata3$cluster <- as.factor(kmeans.model3$cluster)
names(clusterdata3)
str(clusterdata3)

par(mfrow=c(1,2))
plot(clusterdata3$`아파트`, clusterdata3$`한식음식점업`,
     col=clusterdata3$cluster, xlab='아파트 비율', ylab='한식음식점업')
points(kmeans.model3$centers[, c('아파트', '한식음식점업')], col=c(1,2,3,4,5), pch=8, cex=5)

plot(clusterdata3$`아파트`, clusterdata3$`비알콜.음료점`,
     col=clusterdata3$cluster, xlab='아파트 비율', ylab='비알콜.음료점')
points(kmeans.model3$centers[, c('아파트', '비알콜.음료점')], col=c(1,2,3,4,5), pch=8, cex=5)


par(mfrow=c(1,1))
plot(clusterdata3$`아파트`, clusterdata3$`한식음식점업`,
     col=clusterdata3$cluster, xlab='아파트 비율', ylab='한식음식점업')
points(kmeans.model3$centers[, c('아파트', '한식음식점업')], col=c(1,2,3,4,5), pch=8, cex=5)
legend('topright', c('1', '2', '3', '4', '5'), fill=c('black', 'green', 'red', 'blue', 'cyan'))


ggplot(clusterdata3, aes(`아파트`, `한식음식점업`)) + geom_point(aes(colour=cluster))
ggplot(clusterdata3, aes(`아파트`, `비알콜.음료점`)) + geom_point(aes(colour=cluster))
