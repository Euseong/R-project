# data_trim.R 실행 후 진행
# 1인가구 비율이 아파트 실거래가에 얼마나 영향 미치는지 분석

options(encoding='UTF-8')
pricedata <- read.csv('아파트 매매 실거래가.csv', header=T, sep=',', stringsAsFactors = F)

pricedong <- gsub('서울특별시 \\w+구 ', '', pricedata$시군구)
colnames(pricedata)[1] <- '동'
pricedata$동 <- pricedong
str(pricedata)
which(is.na(pricedata)==T)

# trimdata의 '동'에서 1가, 2가 등 삭제
dong <- gsub('\\d+가?', '', trimdata$동)
dong <- gsub('\\W+', '', dong)
str(trimdata)
newdongdata <- trimdata[c(1:8)]
newdongdata$동 <- dong
str(newdongdata)

aggregate(data=newdongdata, `1인가구.비율`~`동`, var)
which(aggregate(data=newdongdata, `1인가구.비율`~`동`, var)[2] > 0.1)
# 동이 여러개인 경우, 1인가구 비율 분산이 0.1 이하임 -> 평균내서 실거래가와 1:1 merge 실행
# 동별 1인가구 비율 평균치 테이블 생성
mean.single.ratio <- aggregate(data=newdongdata, `1인가구.비율`~`동`, mean)
names(table(newdongdata$동))
mean.single.ratio$`1인가구.비율` <- round(mean.single.ratio$`1인가구.비율`, 4)

Gu <- newdongdata[c(1,2)]
str(Gu)
samedong <- NULL
for (i in 2:nrow(Gu)){
  if (Gu[i,2] == Gu[i-1,2]) samedong <- append(samedong, i)
}
samedong
Gu <- Gu[-samedong,]
str(Gu)
library(dplyr)
mean.single.ratio <- inner_join(mean.single.ratio, Gu)
str(mean.single.ratio)

# mean.single.ratio을 pricedata와 통합

price_single <- merge(mean.single.ratio, pricedata)

str(price_single)

library(psych)
library(rpart)

pairs.panels(price_single[c(2, 5:8)])
round(cor(price_single[c(2, 5:8)]), 3) # 전용면적 외에 거래금액에 큰 영향을 미치는 요인은 없음
# decision tree로 실거래가에 대한 영향을 간단하게 분석
summary(price_single$거래금액.만원.)
price.lv.def <- function(x) {ifelse(x<40000, 'low', 
                                    ifelse(x<100000, 'middle', 'high'))}
price_single$price.lv <- sapply(price_single$거래금액.만원., price.lv.def)
str(price_single)
table(price_single$price.lv)

names(price_single)
nrow(price_sample)
price_model <- rpart(price.lv ~ `자치구` + `1인가구.비율` + `층` + `전용면적...` + `건축년도`, data=price_single)
plot(price_model)
text(price_model, use.n=T, cex=0.6)

library(rpart.plot)
library('rattle')
prp(price_model)
rpart.plot(price_model, cex=0.7)
fancyRpartPlot(price_model)

library(ggplot2)
ggplot(price_single, aes(`1인가구.비율`, 거래금액.만원.)) + geom_point(aes(colour=자치구))
ggplot(price_single[which(price_single$자치구=='관악구' | price_single$자치구=='강남구'),],
       aes(`1인가구.비율`, 거래금액.만원.)) +
    geom_point(aes(colour=자치구))
# min-max 정규화 (안해도 될듯)
pricenorm <- price_single

for (i in c(3:6)){
  pricenorm[i] <- as.data.frame(
    (price_single[[i]] - min(price_single[[i]])) / (max(price_single[[i]]) - min(price_single[[i]]))
  )
}

str(pricenorm)
pairs.panels(pricenorm[-1])
