### 다중선형회귀(Multiple Linear Regression)

## 1인가구와 생계형 업종 - 1 

pairs.panels(normdata[c("1인가구.비율", "한식음식점업", "부동산자문.및.중개업", "기타주점업", "두발미용업",
                        "슈퍼마켓", "분식.및.김밥전문점", "비알콜.음료점", "체인화.편의점",
                        "육류.소매업", "컴퓨터게임방.운영업")])

form1 <- normdata$`1인가구.비율` ~ 한식음식점업 + 부동산자문.및.중개업 + 기타주점업 + 두발미용업 +
         슈퍼마켓 + 분식.및.김밥전문점 + 비알콜.음료점 + 체인화.편의점 + 육류.소매업 + 컴퓨터게임방.운영업 

lifebusiness_model <- lm(formula = form1, data = normdata)
lifebusiness_model
summary(lifebusiness_model)

## 1인가구와 생계형 업종 - 2

pairs.panels(normdata[c("1인가구.비율", "의류점","용달.및.개별화물.자동차운송업", "당구장.운영업", "중국.음식점업",
                        "가정용.세탁업", "노래연습장.운영업","치킨.전문점","자동차.전문.수리업","과실.및.채소소매업","제과점업")])


form2 <- normdata$`1인가구.비율` ~  의류점 + 용달.및.개별화물.자동차운송업 + 당구장.운영업 + 중국.음식점업 +
  가정용.세탁업 + 노래연습장.운영업 + 치킨.전문점 + 자동차.전문.수리업 + 과실.및.채소소매업 + 제과점업
lifebusiness_model2 <- lm(formula = form2, data = normdata)
lifebusiness_model2
summary(lifebusiness_model2)

## 2인가구와 생계형 업종 - 1

pairs.panels(normdata[c("2인이상.가구.비율", "한식음식점업", "부동산자문.및.중개업", "기타주점업", "두발미용업",
                        "슈퍼마켓", "분식.및.김밥전문점", "비알콜.음료점", "체인화.편의점",
                        "육류.소매업", "컴퓨터게임방.운영업")])

form3 <- normdata$`2인이상.가구.비율` ~ 한식음식점업 + 부동산자문.및.중개업 + 기타주점업 + 두발미용업 +
  슈퍼마켓 + 분식.및.김밥전문점 + 비알콜.음료점 + 체인화.편의점 + 육류.소매업 + 컴퓨터게임방.운영업 

lifebusiness_model3 <- lm(formula = form3, data = normdata)
lifebusiness_model3
summary(lifebusiness_model3)

## 2인가구와 생계형 업종 - 2

pairs.panels(normdata[c("2인이상.가구.비율", "의류점","용달.및.개별화물.자동차운송업", "당구장.운영업", "중국.음식점업",
                        "가정용.세탁업", "노래연습장.운영업","치킨.전문점","자동차.전문.수리업","과실.및.채소소매업","제과점업")])

form4 <- normdata$`2인이상.가구.비율` ~  의류점 + 용달.및.개별화물.자동차운송업 + 당구장.운영업 + 중국.음식점업 +
  가정용.세탁업 + 노래연습장.운영업 + 치킨.전문점 + 자동차.전문.수리업 + 과실.및.채소소매업 + 제과점업
lifebusiness_model4 <- lm(formula = form4, data = normdata)
lifebusiness_model4
summary(lifebusiness_model4)

## 인구와 생계형 업종 - 1

pairs.panels(normdata[c("인구", "한식음식점업", "부동산자문.및.중개업", "기타주점업", "두발미용업",
                        "슈퍼마켓", "분식.및.김밥전문점", "비알콜.음료점", "체인화.편의점",
                        "육류.소매업", "컴퓨터게임방.운영업")])
form5 <- normdata$`인구` ~ 한식음식점업 + 부동산자문.및.중개업 + 기타주점업 + 두발미용업 +
  슈퍼마켓 + 분식.및.김밥전문점 + 비알콜.음료점 + 체인화.편의점 + 육류.소매업 + 컴퓨터게임방.운영업 

lifebusiness_model5 <- lm(formula = form5, data = normdata)
lifebusiness_model5
summary(lifebusiness_model5)

## 인구와 생계형 업종 - 2

pairs.panels(normdata[c("인구", "의류점","용달.및.개별화물.자동차운송업", "당구장.운영업", "중국.음식점업",
                        "가정용.세탁업", "노래연습장.운영업","치킨.전문점","자동차.전문.수리업","과실.및.채소소매업","제과점업")])

form6 <- normdata$`인구` ~  의류점 + 용달.및.개별화물.자동차운송업 + 당구장.운영업 + 중국.음식점업 +
  가정용.세탁업 + 노래연습장.운영업 + 치킨.전문점 + 자동차.전문.수리업 + 과실.및.채소소매업 + 제과점업
lifebusiness_model6 <- lm(formula = form6, data = normdata)
lifebusiness_model6
summary(lifebusiness_model6)

## 인구밀도와 생계형 업종 - 1

pairs.panels(normdata[c("인구밀도", "한식음식점업", "부동산자문.및.중개업", "기타주점업", "두발미용업",
                        "슈퍼마켓", "분식.및.김밥전문점", "비알콜.음료점", "체인화.편의점",
                        "육류.소매업", "컴퓨터게임방.운영업")])
form7 <- normdata$`인구밀도` ~ 한식음식점업 + 부동산자문.및.중개업 + 기타주점업 + 두발미용업 +
  슈퍼마켓 + 분식.및.김밥전문점 + 비알콜.음료점 + 체인화.편의점 + 육류.소매업 + 컴퓨터게임방.운영업 

lifebusiness_model7 <- lm(formula = form7, data = normdata)
lifebusiness_model7
summary(lifebusiness_model7)

## 인구밀도와 생계형 업종 - 2

pairs.panels(normdata[c("인구밀도", "의류점","용달.및.개별화물.자동차운송업", "당구장.운영업", "중국.음식점업",
                        "가정용.세탁업", "노래연습장.운영업","치킨.전문점","자동차.전문.수리업","과실.및.채소소매업","제과점업")])

form8 <- normdata$`인구밀도` ~  의류점 + 용달.및.개별화물.자동차운송업 + 당구장.운영업 + 중국.음식점업 +
  가정용.세탁업 + 노래연습장.운영업 + 치킨.전문점 + 자동차.전문.수리업 + 과실.및.채소소매업 + 제과점업
lifebusiness_model8 <- lm(formula = form8, data = normdata)
lifebusiness_model8
summary(lifebusiness_model8)

## 여성가구주비율과 생계형 업종 - 1

pairs.panels(normdata[c("여성가구주가구비율", "한식음식점업", "부동산자문.및.중개업", "기타주점업", "두발미용업",
                        "슈퍼마켓", "분식.및.김밥전문점", "비알콜.음료점", "체인화.편의점",
                        "육류.소매업", "컴퓨터게임방.운영업")])
form9 <- normdata$여성가구주가구비율 ~ 한식음식점업 + 부동산자문.및.중개업 + 기타주점업 + 두발미용업 +
  슈퍼마켓 + 분식.및.김밥전문점 + 비알콜.음료점 + 체인화.편의점 + 육류.소매업 + 컴퓨터게임방.운영업 

lifebusiness_model9 <- lm(formula = form9, data = normdata)
lifebusiness_model9
summary(lifebusiness_model9)

## 여성가구주비율과 생계형 업종 - 2

pairs.panels(normdata[c("여성가구주가구비율", "의류점","용달.및.개별화물.자동차운송업", "당구장.운영업", "중국.음식점업",
                        "가정용.세탁업", "노래연습장.운영업","치킨.전문점","자동차.전문.수리업","과실.및.채소소매업","제과점업")])

form10 <- normdata$`인구밀도` ~  의류점 + 용달.및.개별화물.자동차운송업 + 당구장.운영업 + 중국.음식점업 +
  가정용.세탁업 + 노래연습장.운영업 + 치킨.전문점 + 자동차.전문.수리업 + 과실.및.채소소매업 + 제과점업
lifebusiness_model10 <- lm(formula = form10, data = normdata)
lifebusiness_model10
summary(lifebusiness_model10)

##########################################################################

# "인구"                          "면적"                         
# "인구밀도"                      "총.수급자"                    
# "주택.수.합계"                  "단독주택"                     
# "다가구주택"                    "영업겸용"                     
# "아파트"                        "연립주택"                     
# "다세대주택"                    "비거주용건물내주택"           
# "한식음식점업"                  "용달.및.개별화물.자동차운송업"
# "부동산자문.및.중개업"          "의류점"                       
# "기타주점업"                    "두발미용업"                   
# "슈퍼마켓"                      "분식.및.김밥전문점"           
# "비알콜.음료점"                 "가정용.세탁업"                
# "노래연습장.운영업"             "체인화.편의점"                
# "치킨.전문점"                   "자동차.전문.수리업"         
# "과실.및.채소소매업"            "육류.소매업"                  
# "중국.음식점업"                 "당구장.운영업"                
# "제과점업"                      "컴퓨터게임방.운영업"          
# "독거노인.수"                   "여성가구주가구비율"           
# "2인이상.가구.비율"

############################################################################


# 1인가구와 주택유형

library(psych)
pairs.panels(normdata[c("1인가구.비율", "단독주택","다가구주택","아파트","다세대주택","연립주택","비거주용건물내주택")])

jutec <- normdata$`1인가구.비율` ~ 단독주택 + 다가구주택 + 아파트 + 
               다세대주택 + 연립주택 + 비거주용건물내주택
jutec_model <- lm(formula = jutec, data = normdata)
jutec_model
summary(jutec_model)

### 1인가구와 아파트 ###
head(normdata) # 모델 생성

m <- lm(normdata$`1인가구.비율` ~ normdata$아파트)
m

# 모형 평가#
m
summary(lm(normdata$`1인가구.비율` ~ normdata$아파트, data = normdata))

# 모델 평가 플롯 #
par(mfrow = c(2,2))
plot(m)

# 회귀 직선의 시각화 #
par(mfrow = c(1,1))
with(normdata, plot(normdata$`1인가구.비율` ~ normdata$아파트))
abline(coef(lm(normdata$`1인가구.비율` ~ normdata$아파트)))

par(mfrow = c(1, 2))
plot(m, which = c(4, 6))

###1인 가구와 다세대주택###

m2 <- lm(normdata$`1인가구.비율` ~ normdata$다세대주택)
m2

# 모형 평가#
m2
summary(lm(normdata$`1인가구.비율` ~ normdata$다세대주택, data = normdata))

# 모델 평가 플롯 #
par(mfrow = c(2,2))
plot(m2)

# 회귀 직선의 시각화 #
par(mfrow = c(1,1))
with(normdata, plot(normdata$`1인가구.비율` ~ normdata$다세대주택))
abline(coef(lm(normdata$`1인가구.비율` ~ normdata$다세대주택)))

par(mfrow = c(1, 2))
plot(m2, which = c(4, 6))


###1인 가구와 연립주택###

m3 <- lm(normdata$`1인가구.비율` ~ normdata$연립주택)
m3

# 모형 평가#
m3
summary(lm(normdata$`1인가구.비율` ~ normdata$연립주택, data = normdata))

# 모델 평가 플롯 #
par(mfrow = c(2,2))
plot(m3)

# 회귀 직선의 시각화 #
par(mfrow = c(1,1))
with(normdata, plot(normdata$`1인가구.비율` ~ normdata$연립주택))
abline(coef(lm(normdata$`1인가구.비율` ~ normdata$연립주택)))

par(mfrow = c(1, 2))
plot(m3, which = c(4, 6))


###1인 가구와 비거주용건물내주택###
m4 <- lm(normdata$`1인가구.비율` ~ normdata$비거주용건물내주택)
m4

# 모형 평가#
m4
summary(lm(normdata$`1인가구.비율` ~ normdata$비거주용건물내주택, data = normdata))

# 모델 평가 플롯 #
par(mfrow = c(2,2))
plot(m4)

# 회귀 직선의 시각화 #
par(mfrow = c(1,1))
with(normdata, plot(normdata$`1인가구.비율` ~ normdata$비거주용건물내주택))
abline(coef(lm(normdata$`1인가구.비율` ~ normdata$비거주용건물내주택)))

par(mfrow = c(1, 2))
plot(m4, which = c(4, 6))

###1인 가구와 한식음식점업### - special
c <- lm(normdata$`1인가구.비율` ~ normdata$한식음식점업)
c

# 모형 평가#
c
summary(lm(normdata$`1인가구.비율` ~ normdata$한식음식점업, data = normdata))

# 모델 평가 플롯 #
par(mfrow = c(2,2))
plot(c)

# 회귀 직선의 시각화 #
par(mfrow = c(1,1))
with(normdata, plot(normdata$`1인가구.비율` ~ normdata$한식음식점업))
abline(coef(lm(normdata$`1인가구.비율` ~ normdata$한식음식점업)))

par(mfrow = c(1, 2))
plot(c, which = c(4, 6))

#################################################################

# 2인이상가구와 주택유형

library(psych)
pairs.panels(normdata[c("2인이상.가구.비율", "단독주택","다가구주택","아파트","다세대주택","연립주택","비거주용건물내주택")])

jutec2 <- normdata$`2인이상.가구.비율` ~ 단독주택 + 다가구주택 + 아파트 + 
  다세대주택 + 연립주택 + 비거주용건물내주택
jutec_model2 <- lm(formula = jutec2, data = normdata)
jutec_model2
summary(jutec_model2)

###2인이상가구와 아파트###

t <- lm(normdata$`2인이상.가구.비율` ~ normdata$아파트)
t

# 모형 평가#
t
summary(lm(normdata$`2인이상.가구.비율` ~ normdata$아파트, data = normdata))

# 모델 평가 플롯 #
par(mfrow = c(2,2))
plot(t)

# 회귀 직선의 시각화 #
par(mfrow = c(1,1))
with(normdata, plot(normdata$`1인가구.비율` ~ normdata$연립주택))
abline(coef(lm(normdata$`1인가구.비율` ~ normdata$연립주택)))

par(mfrow = c(1, 2))
plot(t, which = c(4, 6))


# 인구와 주택유형
pairs.panels(normdata[c("인구", "단독주택","다가구주택","아파트","다세대주택","연립주택","비거주용건물내주택")])

jutec3 <- normdata$`인구` ~ 단독주택 + 다가구주택 + 아파트 + 
  다세대주택 + 연립주택 + 비거주용건물내주택
jutec_model3 <- lm(formula = jutec3, data = normdata)
jutec_model3
summary(jutec_model3)

# 인구밀도와 주택유형
pairs.panels(normdata[c("인구밀도", "단독주택","다가구주택","아파트","다세대주택","연립주택","비거주용건물내주택")])

jutec4 <- normdata$`인구밀도` ~ 단독주택 + 다가구주택 + 아파트 + 
  다세대주택 + 연립주택 + 비거주용건물내주택
jutec_model4 <- lm(formula = jutec4, data = normdata)
jutec_model4
summary(jutec_model4)

# 여성가구주가구비율과 주택유형
pairs.panels(normdata[c("여성가구주가구비율", "단독주택","다가구주택","아파트","다세대주택","연립주택","비거주용건물내주택")])

jutec5 <- normdata$여성가구주가구비율 ~ 단독주택 + 다가구주택 + 아파트 + 
  다세대주택 + 연립주택 + 비거주용건물내주택
jutec_model5 <- lm(formula = jutec5, data = normdata)
jutec_model5
summary(jutec_model5)

# 총.수급자와 주택유형
pairs.panels(normdata[c("총.수급자", "단독주택","다가구주택","아파트","다세대주택","연립주택","비거주용건물내주택")])

jutec6 <- normdata$총.수급자 ~ 단독주택 + 다가구주택 + 아파트 + 
  다세대주택 + 연립주택 + 비거주용건물내주택
jutec_model6 <- lm(formula = jutec6, data = normdata)
jutec_model6
summary(jutec_model6)

# 독거노인과 주택유형
pairs.panels(normdata[c("독거노인.수", "단독주택","다가구주택","아파트","다세대주택","연립주택","비거주용건물내주택")])
jutec7 <- normdata$독거노인.수 ~ 단독주택 + 다가구주택 + 아파트 + 
  다세대주택 + 연립주택 + 비거주용건물내주택
jutec_model7 <- lm(formula = jutec7, data = normdata)
jutec_model7
summary(jutec_model7)


###독거노인과 아파트###

oldman <- lm(normdata$`독거노인` ~ normdata$아파트)
oldman

# 모형 평가#
summary(lm(normdata$`독거노인` ~ normdata$아파트, data = normdata))

# 모델 평가 플롯 #
par(mfrow = c(2,2))
plot(oldman)

# 회귀 직선의 시각화 #
par(mfrow = c(1,1))
with(normdata, plot(normdata$`독거노인` ~ normdata$아파트))
abline(coef(lm(normdata$`독거노인` ~ normdata$아파트)))

par(mfrow = c(1, 2))
plot(oldman, which = c(4, 6))


###독거노인과 다가구주택###

oldman2 <- lm(normdata$`독거노인` ~ normdata$다가구주택)
oldman2

# 모형 평가#
summary(lm(normdata$`독거노인` ~ normdata$다가구주택 , data = normdata))

# 모델 평가 플롯 #
par(mfrow = c(2,2))
plot(oldman2)

# 회귀 직선의 시각화 #
par(mfrow = c(1,1))
with(normdata, plot(normdata$`독거노인` ~ normdata$다가구주택))
abline(coef(lm(normdata$`독거노인` ~ normdata$다가구주택)))

par(mfrow = c(1, 2))
plot(oldman2, which = c(4, 6))


###독거노인과 다세대주택###

oldman3 <- lm(normdata$`독거노인` ~ normdata$다세대주택)
oldman3

# 모형 평가#
summary(lm(normdata$`독거노인` ~ normdata$다세대주택 , data = normdata))

# 모델 평가 플롯 #
par(mfrow = c(2,2))
plot(oldman3)

# 회귀 직선의 시각화 #
par(mfrow = c(1,1))
with(normdata, plot(normdata$`독거노인` ~ normdata$다세대주택))
abline(coef(lm(normdata$`독거노인` ~ normdata$다세대주택)))

par(mfrow = c(1, 2))
plot(t, which = c(4, 6))

###독거노인과 연립주택### 

oldman4 <- lm(normdata$`독거노인` ~ normdata$연립주택)
oldman4

# 모형 평가#
summary(lm(normdata$`독거노인` ~ normdata$연립주택 , data = normdata))

# 모델 평가 플롯 #
par(mfrow = c(2,2))
plot(oldman4)

# 회귀 직선의 시각화 #
par(mfrow = c(1,1))
with(normdata, plot(normdata$`독거노인` ~ normdata$연립주택))
abline(coef(lm(normdata$`독거노인` ~ normdata$연립주택)))

par(mfrow = c(1, 2))
plot(t, which = c(4, 6))























