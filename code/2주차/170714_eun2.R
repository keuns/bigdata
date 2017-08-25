setwd("C:/r_exam/mlr-ko/chapter 5")
credit = read.csv('credit.csv')
str(credit)
ind = order(runif(1000)) # 데이터 섞기

train = credit[ind[1:900], ]
test = credit[ind[901:1000], ] # train / test 로 나누기 (섞은거 또 섞기)

# install.packages('C50')
library(C50)

# C50 함수를 통해 decision tree 만들기, 정분류, 오분류 된 것 확인 가능
model1 = C5.0(x=train[, -17], y=train[,17])
model2 = C5.0(x = train[ , c(1,2,5,6)], y = train$default)

##########
## party 패키지 써보기, 결과는 같음
library(party)
model3 = ctree(default~., train)
summary(model3)
plot(model3)

pred1=predict(model1, test)
table(test$default,pred1)

pred2=predict(model2, test)
table(test$default,pred2)

cc1 = predict(model3, train)
table(train$default, cc1)

cc2 = predict(model3, test)
table(test$default, cc2)

library(e1071)
model4 = naiveBayes(x = train[, -17], y = train[, 17])
pred4 = predict(model4, newdata = test)
table(test$default, pred4)

library(randomForest)
model5 = randomForest(x = train[, -17], y = train[, 17])
pred5 = predict(model5, test)
table(test$default, pred5)

#####################################################
## 분류문제
## 어떤 알고리즘이 나은 지 확인해보기 (최적의 알고리즘 찾기)
## 랜덤포레스트 vs 디씨전 트리 vs 나이브베이즈
library(mlbench)
data(Vowel)

nn = nrow(Vowel)
ind = sample(nn, nn*0.7, replace = F)
train = Vowel[ind, ]
test = Vowel[-ind, ]

v1 = C5.0(x=train[, -11], y=train[, 11])
v2 = naiveBayes(x=train[, -11], y=train[, 11])
v3 = randomForest(Class~., train)

pred1 = predict(v1, test)
pred2 = predict(v2, test)
pred3 = predict(v3, test)

t1 = table(test$Class, pred1)
   correct1 = sum(diag(t1))/sum(t1)
t2 = table(test$Class, pred2)
   correct2 = sum(diag(t2))/sum(t2)
t3 = table(test$Class, pred3)
   correct3 = sum(diag(t3))/sum(t3)
   
c(correct1, correct2, correct3)
## -> 데이터가 어떻냐에 따라 성능 차이남
## 결론
## 다지(=Y 레이블이 여러개=다항 분류)에 보통 randomforest (이항/다항 두 곳에서 모두 많이 쓰임)
## 디씨전 트리는 randomforest의 하위호환버젼이라고 보면 됨
## 나이브베이즈는 이항분류에서 성능이 좋음 (특히 텍스트마이닝을 통한 문서분류, 스팸메일필터링 등)

# 
# co2 = data.frame()
# pred = c("pred1", "pred2", "pred3")
# for(i in 1:3) {
#   t1 = table(test$Class, pred[i])
#   correct1 = sum(diag(t1))/sum(t1)
#   co1 = cbind(i, correct1)
#   co2 = rbind(co2, col)
# }
#

################################################################################################################
## Y값을(사전정보) 모르는 상태에서 집단을 나눔 vs Y값을 아는 상태에서 집단을 나눔
# 모르는 상태 -> 군집 vs 아는 상태 -> 분류(1. 확률(=빈도)기반, 2. 회귀기반)

