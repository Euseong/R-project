str(trimdata)
hometype <- trimdata[c(4:16)]
nearmarket <- trimdata[c(15, 16, 17, 18, 19, 20, 21, 22, 23,
                         24, 25, 26, 27, 28, 29, 30, 31, 32,
                         33, 34)]
str(hometype)
str(nearmarket)

##
library(rpart)
summary(hometype$`1인가구.비율`)
summary(hometype)
summary(nearmarket)
plot(hometype$`1인가구.비율`)
jutec.lv.def <- function(x) {ifelse(x < 0.1, 'low',
                                    ifelse(x < 0.4, 'middle', 'high'))}

hometype$jutec.lv <- sapply(trimdata$`1인가구.비율`, jutec.lv.def)

table(hometype$jutec.lv)

set.seed(100)
index <- sample(1:nrow(hometype), 0.7*nrow(hometype))
hometype_train <- hometype[index,]
hometype_test <- hometype[-index,]

hometype_model <- rpart(jutec.lv ~ 아파트 + 단독주택 + 다가구주택 + 영업겸용 + 연립주택 +
                          다세대주택 + 비거주용건물내주택, data = hometype, na.action = na.omit)
par(mfrow=c(1,1))
plot(hometype_model)
text(hometype_model, use.n = T, cex = 0.6)

##
summary(nearmarket)

nearmarket$jutec.lv <- sapply(trimdata$`1인가구.비율`, jutec.lv.def)

table(nearmarket$jutec.lv)

set.seed(100)
index <- sample(1:nrow(nearmarket), 0.7*nrow(nearmarket))
nearmarket_train <- nearmarket[index,]
nearmarket_test <- nearmarket[-index,]

nearmarket_model <- rpart(jutec.lv ~ 한식음식점업 + 부동산자문.및.중개업 + 기타주점업 + 두발미용업 +
                            슈퍼마켓 + 분식.및.김밥전문점 + 비알콜.음료점 + 체인화.편의점 + 육류.소매업 + 컴퓨터게임방.운영업 +
                            의류점 + 용달.및.개별화물.자동차운송업 + 당구장.운영업 + 중국.음식점업 +
                            가정용.세탁업 + 노래연습장.운영업 + 치킨.전문점 + 자동차.전문.수리업 + 과실.및.채소소매업 + 제과점업,
                          data = nearmarket, na.action = na.omit)
nearmarket_model

par(mfrow=c(1,1))
plot(nearmarket_model)
text(nearmarket_model, use.n = T, cex = 0.6)

#########################################################################




























