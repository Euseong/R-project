### data_integration.R 실행 후 진행
# rawdata의 각 컬럼 데이터가 정규성의 띄는지 확인
xnorm_vec <- NULL
for (i in 4:length(rawdata)) {
  if (shapiro.test(na.omit(rawdata[[i]]))$p.value < 0.0500000){
    xnorm_vec <- append(xnorm_vec, i)
  }
}
length(xnorm_vec) # 38개가 비정규성 -> min-max 정규화 진행

# NA 값이 있는 열 확인
xnorm_vec <- NULL
for (i in 4:length(rawdata)) {
  if (is.na(rawdata[i])){
    xnorm_vec <- append(xnorm_vec, i)
  }
}
xnorm_vec # 주택 종류 데이터, 업종 데이터에만 NA 있음 -> 0으로 판단됨

# rawdata 수정(2인 이상 가구비율 산출 후, 1인가구와 2인이상.가구 삭제)
which(is.na(rawdata$`2인이상.가구`)==T)
which(is.na(rawdata$총.가구수)==T)
trimdata <- rawdata
trimdata$`2인이상.가구.비율` <- round(rawdata$`2인이상.가구` / rawdata$총.가구수, 4)
trimdata$여성가구주가구비율 <- round(trimdata$여성가구주.가구 / trimdata$일반가구, 4)

trimdata$`1인가구` <- NULL
trimdata$`2인이상.가구` <- NULL
trimdata$일반가구 <- NULL
trimdata$여성가구주.가구 <- NULL

for (i in c(3:(length(trimdata)) )){
  trimdata[i] <- as.data.frame(
    ifelse(is.na(trimdata[i]), 0, trimdata[[i]]) # NA가 아닌 값은 그대로, NA인 값은 0으로
  )
}

str(trimdata)

# min-max 정규화
normdata <- trimdata

for (i in c(3, 5:(length(trimdata)-2) )){
  normdata[i] <- as.data.frame(
    (trimdata[[i]] - min(trimdata[[i]])) / (max(trimdata[[i]]) - min(trimdata[[i]]))
  )
}

str(normdata)
