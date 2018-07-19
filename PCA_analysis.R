### data_integration.R , data_trim.R 실행후 시작

# PCA(주성분 분석)
# 1인가구.비율이 제외된 데이터를 prcomp로 분석 -> 주성분 요소값 확인
# biplot을 통해 데이터의 주성분에 대한 기여도 확인
# 주성분을 회귀분석 결과, k-clustering 결과와 비교하여 1인가구.비율의 영향과 일치하는지 확인
# 즉, 주성분과 방향이 비슷한 요소의 변화에 따라 1인가구.비율이 비슷한 양상으로 변하는지를 확인

str(trimdata)
trimdata2 <- trimdata[c(-1,-2)]

trimdata2_cor <- cor(trimdata2)      # 대각성분을 제외한 하삼각행렬에서 상관계수가 0.8보다 큰 인덱스를 구함
high_cor <- which(trimdata2_cor>0.8 & lower.tri(trimdata2_cor)) - 1 # 37에 대한 몫이 각 열마다 일정하도록 1을 빼줌
length(trimdata2_cor[,1])
high_cor_vars <- data.frame(rowname = rownames(trimdata2_cor)[high_cor%%37 + 1])
high_cor_vars$colname <- colnames(trimdata2_cor)[(high_cor%/%37) + 1]
high_cor_vars
# 총.가구수, 인구, 주택.수.합계 간의 상관계수가 높음
# 한식음식점업, 분식점, 음료좀, 편의점 간의 상관계수가 높음
# 총.가구수만을 사용하고, 업종은 합쳐서 차원 축소
trimdata2$인구 <- NULL
trimdata2$주택.수.합계 <- NULL

trimdata2$`식음료판매.편의점업` <- with(trimdata2,
                               (`한식음식점업` + `비알콜.음료점` + `체인화.편의점` + `분식.및.김밥전문점`))
trimdata2$한식음식점업 <- NULL
trimdata2$비알콜.음료점 <- NULL
trimdata2$체인화.편의점 <- NULL
trimdata2$분식.및.김밥전문점 <- NULL
trimdata2$`1인가구.비율` <- NULL
trimdata2$`2인이상.가구.비율` <- NULL # 주성분이 1인가구, 2인이상가구를 설명하는지 보기 위해 두 컬럼 삭제
str(trimdata2)
# preProcess를 이용한 정규화
library(caret)
trimdata2_norm_model <- preProcess(x=trimdata2, method=c('BoxCox', 'center', 'scale'))
trimdata2_norm <- predict(trimdata2_norm_model, newdata=trimdata2)
str(trimdata2_norm)


### 전체 데이터 주성분 분석
pcadata <- princomp(trimdata2_norm)
pcadata

screeplot(pcadata)

summary(pcadata)
str(pcadata)
pcadata$loadings[,1:9]
head(pcadata$scores[,1:9], 10)

biplot(pcadata) # 변수가 너무 많아 분석하기 어려움
ggbiplot(pcadata) + labs(x='PC1', y='PC2')

### 주택 종류 데이터 주성분 분석
names(trimdata2_norm)
hometype <- trimdata2_norm[c(-(12:27), -30)]
str(hometype)

hometype.pr <- princomp(hometype)

screeplot(hometype.pr) # 주성분 3개가 적당

summary(hometype.pr)
str(hometype.pr)
hometype.pr$loadings[,1:3]
head(hometype.pr$scores[,1:3], 10)
biplot(hometype.pr)

library(ggbiplot)
ggbiplot(hometype.pr) + labs(x='PC1', y='PC2')


# 주성분 1에 대해 score가 높은 지역과 낮은 지역의 데이터를 비교하여 주성분에 기여하는 특징 분석
summary(hometype.pr$scores[,1])
PC1_highscore_p <- which(hometype.pr$scores[,1] > 4) # 주성분 1에 +로 영향 많은 지역들
PC1_highdata_p <- hometype[PC1_highscore_p,]
PC1_highscore_n <- which(hometype.pr$scores[,1] < -4) # 주성분 1에 -로 영향 많은 지역들
PC1_highdata_n <- hometype[PC1_highscore_n,]


PC1_highdata_p
PC1_highdata_n
sapply(PC1_highdata_p, mean) # 평균이 0, 분산이 1인 분포에 있음을 인식하고 분석
sapply(PC1_highdata_n, mean)
summary(hometype)
summary(hometype$아파트)
trimdata[PC1_highscore_n, c(1,2)]
# PC1은 아파트를 제외한 주택 종류 수와 총.수급자, 인구밀도가 증가함에 따라 감소하는 경향(가구의 소득, 집값과 관련?)

# 주성분 2에 대해 분석
summary(hometype.pr$scores[,2])
PC2_highscore_p <- which(hometype.pr$scores[,2] > 3) # 주성분 1에 +로 영향 많은 지역들
PC2_highdata_p <- hometype[PC2_highscore_p,]
PC2_highscore_n <- which(hometype.pr$scores[,2] < -3) # 주성분 1에 -로 영향 많은 지역들
PC2_highdata_n <- hometype[PC2_highscore_n,]

PC2_highdata_p
PC2_highdata_n
sapply(PC2_highdata_p, mean)
sapply(PC2_highdata_n, mean)
summary(hometype)
summary(hometype$아파트)
trimdata[PC2_highscore_n, c(1,2)]
# 면적, 총.가구수, 아파트 수에 영향을 많이 받음(주택용지 면적과 관련?) 좁은 지역에 아파트를 못 지으니까

# 주성분 3에 대해 분석
summary(hometype.pr$scores[,3])
PC3_highscore_p <- which(hometype.pr$scores[,3] > 3) # 주성분 1에 +로 영향 많은 지역들
PC3_highdata_p <- hometype[PC3_highscore_p,]
PC3_highscore_n <- which(hometype.pr$scores[,3] < -2) # 주성분 1에 -로 영향 많은 지역들
PC3_highdata_n <- hometype[PC3_highscore_n,]

PC3_highdata_p
PC3_highdata_n
sapply(PC3_highdata_p, mean)
sapply(PC3_highdata_n, mean)
summary(hometype)
summary(hometype$아파트)
# 별다른 특징 없어 보임

### 주택 주변 업종 주성분 분석
names(trimdata2_norm)
nearmarket <- trimdata2_norm[-(5:11)]
str(nearmarket)

nearmarket.pr <- princomp(nearmarket)

screeplot(nearmarket.pr) # 주성분 2~3개가 적당

summary(nearmarket.pr)
str(nearmarket.pr)
nearmarket.pr$loadings[,1:3]

ggbiplot(nearmarket.pr)+ labs(x='PC1', y='PC2') + 
  scale_x_continuous(limits = c(-4, 2.5)) + scale_y_continuous(limits = c(-3, 3))


# 주성분 1에 대해 score가 높은 지역과 낮은 지역의 데이터를 비교하여 주성분에 기여하는 특징 분석
summary(nearmarket.pr$scores[,1])
PC1_highscore_p <- which(nearmarket.pr$scores[,1] > 4) # 주성분 1에 +로 영향 많은 지역들
PC1_highdata_p <- nearmarket[PC1_highscore_p,]
PC1_highscore_n <- which(nearmarket.pr$scores[,1] < -5) # 주성분 1에 -로 영향 많은 지역들
PC1_highdata_n <- nearmarket[PC1_highscore_n,]



PC1_highdata_p
PC1_highdata_n
round(sapply(PC1_highdata_p, mean), 3)
round(sapply(PC1_highdata_n, mean), 3)

round(sapply(PC1_highdata_p, mean), 3) - round(sapply(PC1_highdata_n, mean), 3)
trimdata[PC1_highscore_p, c(1,2)]
trimdata[PC1_highscore_n, c(1,2)]
# 인구밀도와 운송업을 제외한 모든 요소가 증가함에 따라 주성분 1은 감소 -> 경기침체, 구도심 여부? 사무실 많은 지역 여부?

# 주성분 2에 대해 score가 높은 지역과 낮은 지역의 데이터를 비교하여 주성분에 기여하는 특징 분석
summary(nearmarket.pr$scores[,2])
PC2_highscore_p <- which(nearmarket.pr$scores[,2] > 3) # 주성분 1에 +로 영향 많은 지역들
PC2_highdata_p <- nearmarket[PC2_highscore_p,]
PC2_highscore_n <- which(nearmarket.pr$scores[,2] < -2.5) # 주성분 1에 -로 영향 많은 지역들
PC2_highdata_n <- nearmarket[PC2_highscore_n,]

PC2_highdata_p
PC2_highdata_n
round(sapply(PC2_highdata_p, mean), 3)
round(sapply(PC2_highdata_n, mean), 3)
round(sapply(PC2_highdata_p, mean), 3) - round(sapply(PC2_highdata_n, mean), 3)
trimdata[PC2_highscore_p, c(1,2)]
trimdata[PC2_highscore_n, c(1,2)]
# 주택 주변에 주로 분포되는 세탁업, 과일/채소소매업, 육류소매업 등과 음의 상관관계,
# 주점, 의류점, 제과점, 음식점, 당구장 등과 양의 상관관계 -> 상가 밀집 지역 여부?
# 치킨집, 노래방, 슈퍼마켓 등과는 무관(상가 지역이든, 주택 지역이든 이들 업종은 비슷하게 분포?)


# 주성분 3에 대해 score가 높은 지역과 낮은 지역의 데이터를 비교하여 주성분에 기여하는 특징 분석
summary(nearmarket.pr$scores[,3])
PC3_highscore_p <- which(nearmarket.pr$scores[,3] > 2) # 주성분 1에 +로 영향 많은 지역들
PC3_highdata_p <- nearmarket[PC3_highscore_p,]
PC3_highscore_n <- which(nearmarket.pr$scores[,2] < -2.5) # 주성분 1에 -로 영향 많은 지역들
PC3_highdata_n <- nearmarket[PC3_highscore_n,]

PC3_highdata_p
PC3_highdata_n
round(sapply(PC3_highdata_p, mean), 3)
round(sapply(PC3_highdata_n, mean), 3)
round(sapply(PC3_highdata_p, mean), 3) - round(sapply(PC3_highdata_n, mean), 3)
trimdata[PC3_highscore_p, c(1,2)]
trimdata[PC3_highscore_n, c(1,2)]
# 별다른 특징 없어 보임
