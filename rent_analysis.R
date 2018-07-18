# 1인가구 비율과 전월세 가격, 주택면적과의 상관관계 분석

options(encoding='UTF-8')
rawdata <- read.csv('rawdata.csv', header=T, sep=',', stringsAsFactors = F)
str(rawdata)

rent <- read.csv('서울시 동별 월세.csv', header=T, stringsAsFactors=F)
jeonse <- read.csv('서울시 동별 전세.csv', header=T, stringsAsFactors=F)

library(dplyr)

# single_household와 single_population의 '동'에서 1가, 2가 등 삭제
dong <- gsub('\\d+가?', '', rawdata$동)
dong <- gsub('\\W+', '', dong)
single_household2 <- rawdata[2:7]
single_household2$동 <- dong
single_population2 <- rawdata[c(2,3,8,9,10)]
single_population2$동 <- dong
str(single_household2)
str(single_population2)

# rent를 single_household 및 single_population와 통합
library(plyr)
single_house_pop <- single_household2
single_house_pop <- as.data.frame(append(single_house_pop, single_population2[3:length(single_population2)]),
                                  stringsAsFactors = F)
colnames(single_house_pop)[4:6] <- c('1인가구', '2인이상.가구', '1인가구.비율')
str(single_house_pop)

# rent에서 single_house_pop의 '동'과 같은 지역의 데이터를 통합(1,2,3가 등이 포함된 동은 여러 행으로 통합됨)
colnames(rent)[1] <- '자치구'
rent$동 <- gsub('\\d+가?', '', rent$동)
rent_single <- merge(single_house_pop, rent)
str(rent_single)
head(single_house_pop)
head(rent_single)
single_house_pop[which(single_house_pop$동 == '상도동'),]
rent_single[which(rent_single$동 == '상도동'),] # single_house_pop에 4개, rent_single의 2개 있는 상도동은 통합결과 8행

# jeonse에서 single_house_pop의 '동'과 같은 지역의 데이터를 통합
colnames(jeonse)[1] <- '자치구'
jeonse$동 <- gsub('\\d+가?', '', jeonse$동)
jeonse_single <- merge(single_house_pop, jeonse)
str(jeonse_single)
head(single_house_pop)
head(jeonse_single)
single_house_pop[which(single_house_pop$동 == '상도동'),]
jeonse_single[which(jeonse_single$동 == '상도동'),]

# 1인가구, 2인가구와 전,월세 데이터 간 상관관계 시각화
summary(single_house_pop$`1인가구.비율`)
plot(rent_single[c(-1, -2, -3, -8)])
plot(jeonse_single[c(-1, -2, -3, -8)])

par(mfrow=c(1,2))
plot(rent_single$월세..만원., rent_single$`1인가구`)
plot(rent_single$월세..만원., rent_single$`2인이상.가구`)

cor(rent_single[4:length(rent_single)]) # 1인가구와 월세의 상관관계는 적음(0.1 이하)
cor(jeonse_single[4:length(jeonse_single)]) # 1인가구와 전세 보증금의 상관관계는 적음(0.05 이하)

library(psych)
pairs.panels(rent_single[c(-1, -2, -3, -8)])
pairs.panels(jeonse_single[c(-1, -2, -3, -8)])
# 1인가구와 주택 면적의 상관계수는 약 -0.3으로 1인가구 비율이 높을 수록 거래되는 주택의 면적에 약간만 영향줌

# 월세 데이터(rent_single) 결정트리 분석
library(rpart)
str(rent_single)
summary(rent_single$`1인가구.비율`)
summary(rent_single$월세..만원.)
summary(rent_single$면적..m..)

rent.lv.def <- function(x) {ifelse(x<35, 'low', 
                                   ifelse(x<55, 'middle', 'high'))}
area.lv.def <- function(x) {ifelse(x<25.9, 'low', 
                                   ifelse(x<33.3, 'middle', 'high'))}

rent_single$rent.lv <- sapply(rent_single$월세..만원., rent.lv.def)
rent_single$area.lv <- sapply(rent_single$면적..m.., area.lv.def)

table(rent_single$rent.lv)

head(rent_single)

set.seed(100)
index <- sample(1:nrow(rent_single), 0.7*nrow(rent_single))
rent_train <- rent_single[index,]
rent_test <- rent_single[-index,]

# rent_model <- rpart(rent.lv ~ `1인가구`+`1인가구.비율`+`2인이상.가구`+`인구`+`인구밀도`, data=rent_train)
# 위에서 
rent_model1 <- rpart(rent.lv ~ `1인가구.비율`+`2인이상.가구`+`인구밀도`+`자치구`,  # 동 까지 고려하면 너무 오래 걸림
                    data=rent_train, na.action=na.omit)
rent_model1 <- rpart(rent.lv ~ `인구밀도`+`자치구`, data=rent_train, na.action=na.omit)
# 자치구만 고려해도 정분류율 0.7872 -> 1인가구 비율은 월세에 큰 영향 안줌
par(mfrow=c(1,1))
plot(rent_model1)
text(rent_model1, use.n=T, cex=0.6)

pred <- predict(rent_model1, rent_test)
head(pred)
cpred <- ifelse(pred[,1] > 0.5, 'high',
                ifelse(pred[,2] > 0.5, 'low', 'middle'))
head(cpred)
head(rent_test$rent.lv)
(pred_table <- table(cpred, rent_test$rent.lv))
sum(diag(pred_table)) / sum(pred_table) # 월세 예측 정분류율 : 0.7978

# 월세 데이터 주택 면적 예측
rent_model2 <- rpart(area.lv ~ `월세..만원.`+`1인가구.비율`+`2인이상.가구`+`인구밀도`+`자치구`,
                     data=rent_train, na.action=na.omit) # 정분류율 : 0.6283
rent_model2 <- rpart(area.lv ~ `1인가구.비율`+`2인이상.가구`+`인구밀도`+`자치구`,
                     data=rent_train, na.action=na.omit) # 정분류율 : 0.5384
rent_model2 <- rpart(area.lv ~ `월세..만원.`+`인구밀도`+`자치구`,
                     data=rent_train, na.action=na.omit) # 정분류율 : 0.5714
# 1인가구 비율은 월세 데이터에서 주택 면적에 큰 영향 주지 않음

par(mfrow=c(1,1))
plot(rent_model2)
text(rent_model2, use.n=T, cex=0.6)

pred <- predict(rent_model2, rent_test)
head(pred)
cpred <- ifelse(pred[,1] > 0.5, 'high',
                ifelse(pred[,2] > 0.5, 'low', 'middle'))
head(cpred)
head(rent_test$area.lv)
(pred_table <- table(cpred, rent_test$area.lv))
sum(diag(pred_table)) / sum(pred_table) # 정분류율 : 0.6283


# 전세 데이터(jeonse_single) 결정트리 분석
str(jeonse_single)
summary(jeonse_single$`1인가구.비율`)
summary(jeonse_single$보증금..만원.)
set.seed(100)
index <- sample(1:nrow(jeonse_single), 0.7*nrow(jeonse_single))

jeonse.lv.def <- function(x) {ifelse(x<5000, 'low', 
                                   ifelse(x<7000, 'middle', 'high'))}

jeonse_single$jeonse.lv <- sapply(jeonse_single$보증금..만원., jeonse.lv.def)
jeonse_single$area.lv <- sapply(jeonse_single$면적..m.., area.lv.def)
table(jeonse_single$jeonse.lv)

jeonse_train <- jeonse_single[index,]
jeonse_test <- jeonse_single[-index,]

jeonse_model1 <- rpart(jeonse.lv ~ `1인가구.비율`+`2인이상.가구`+`인구밀도`+`자치구`, 
                    data=jeonse_train, na.action=na.omit) # 정분류율 : 0.8484
jeonse_model1 <- rpart(jeonse.lv ~ `인구밀도`+`자치구`, 
                    data=jeonse_train, na.action=na.omit) # 정분류율 : 0.8257 -> 1인가구보다 자치구가 더 큰 영향
par(mfrow=c(1,1))
plot(jeonse_model1)
text(jeonse_model1, use.n=T, cex=0.6)

pred <- predict(jeonse_model1, jeonse_test)
head(pred)
cpred <- ifelse(pred[,1] > 0.5, 'high',
                ifelse(pred[,2] > 0.5, 'low', 'middle'))
head(cpred)
head(jeonse_test$rent.lv)
(pred_table <- table(cpred, jeonse_test$rent.lv))
sum(pred_table[1,1], pred_table[2,3]) / sum(pred_table)

# 전세 데이터 주택 면적 예측
jeonse_model2 <- rpart(area.lv ~ `보증금..만원.`+`1인가구.비율`+`2인이상.가구`+`인구밀도`+`자치구`,
                     data=jeonse_train, na.action=na.omit) # 정분류율 : 0.5833
jeonse_model2 <- rpart(area.lv ~ `1인가구.비율`+`2인이상.가구`+`인구밀도`+`자치구`,
                     data=jeonse_train, na.action=na.omit) # 정분류율 : 0.6590
jeonse_model2 <- rpart(area.lv ~ `보증금..만원.`+`인구밀도`+`자치구`,
                     data=jeonse_train, na.action=na.omit) # 정분류율 : 0.6818
# 1인가구 비율은 전세 데이터에서 주택 면적에 큰 영향 주지 않음
par(mfrow=c(1,1))
plot(rent_model2)
text(rent_model2, use.n=T, cex=0.6)

pred <- predict(jeonse_model2, jeonse_test)
head(pred)
cpred <- ifelse(pred[,1] > 0.5, 'high',
                ifelse(pred[,2] > 0.5, 'low', 'middle'))
head(cpred)
head(jeonse_test$area.lv)
(pred_table <- table(cpred, jeonse_test$area.lv))
sum(diag(pred_table)) / sum(pred_table)


