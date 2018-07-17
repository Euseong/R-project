# 1인가구 비율과 부동산, 주택 관련 데이터들을 하나의 자료구조로 통합

# 각 데이터 불러오기
options(encoding='UTF-8')
getwd()
setwd("C:\\work\\R")
single_household <- read.csv('1인가구 동별 데이터.csv', header=T, stringsAsFactors=F)
single_population <- read.csv('1인가구 인구밀도 데이터.csv', header=T, stringsAsFactors=F)
basic_aid <- read.csv('기초생활수급자(가구).csv', header=T, stringsAsFactors=F)
house_type <- read.csv('주택 유형별 분류.csv', header=T, stringsAsFactors=F)
market <- read.csv('생계형사업 분포현황.csv', header=T, stringsAsFactors=F)
rent <- read.csv('서울시 동별 월세.csv', header=T, stringsAsFactors=F)
jeonse <- read.csv('서울시 동별 전세.csv', header=T, stringsAsFactors=F)
single_sliverman <- read.csv('독거노인 현황(연령별,동별).csv', header=T, stringsAsFactors=F)
masterwoman_data <- read.csv('여성가구주 데이터.csv', header=T, stringsAsFactors=F)


str(single_household)
length(single_household[is.na(single_household)])
str(single_population)
str(basic_aid)
str(house_type)
str(market)
str(rent)
str(jeonse)
str(single_sliverman)
str(masterwoman_data)

# 1인가구 동별 데이터에서 불필요한 컬럼(2인 ~ 7인 가구 수) 삭제
single_household[, c(5:10)] <- NULL
str(single_household)

# 각 데이터에서 '소계', '기타'가 있는 행 삭제, 콤마가 있음
single_household <- single_household[-which(single_household$동 == '소계'),]
str(single_household)

single_population <- single_population[-which(single_population$동 == '소계'),]
str(single_population)

basic_aid <- basic_aid[-which(basic_aid$동 == '소계'),]
basic_aid <- basic_aid[-which(basic_aid$동 == '기타'),]
str(basic_aid)
basic_aid$수급자.비율 <- round(basic_aid$총.수급자 / single_household$계, 4)
summary(basic_aid$수급자.비율)
basic_aid[which(basic_aid$수급자.비율 > 0.1),]

house_type <- house_type[-which(house_type$동 == '소계'),]
str(house_type)

market <- market[-which(market$동 == '소계'),]
str(market)

single_sliverman <- single_sliverman[-which(single_sliverman$동 == '소계'),]
single_sliverman <- single_sliverman[-which(single_sliverman$동 == '합계'),]
str(single_sliverman)

masterwoman_data <- masterwoman_data[-which(masterwoman_data$동 == '소계'),]
masterwoman_data <- masterwoman_data[-which(masterwoman_data$동 == '합계'),]
str(masterwoman_data)

# 숫자를 제외한 동 이름의 개수 구해보기
head(single_household$동)
dong <- gsub('\\d+', '', single_household$동)
dong <- gsub('\\W+', '', dong)
length(table(dong))


# 데이터 통합, rawdata - 필요에 따라서 가공
library(dplyr)
rawdata <- single_household
rawdata <- append(rawdata, single_population[3:length(single_population)])
rawdata <- append(rawdata, basic_aid[3:length(basic_aid)])
rawdata <- append(rawdata, house_type[3:length(house_type)])
rawdata <- append(rawdata, market[3:length(market)])
rawdata <- append(rawdata, single_sliverman[3:length(single_sliverman)])
rawdata <- append(rawdata, masterwoman_data[3:length(masterwoman_data)])

str(rawdata)
length(is.na(rawdata)) 
