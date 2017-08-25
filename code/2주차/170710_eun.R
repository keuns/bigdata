## 인과관계와 상관관계

## 인과관계(어느정도의 영향을 미치느냐 -> 따라서 x, y 에 대한 식(모형)이 나옴)
## 상관관계(관계가 있다/없다 만을 파악 -> 상관관계는 인과관계를 볼 수 없음!)

## 독립변수가 하나 : 단순선형회귀/여러개 : 다중선형회귀

## 입실론 : 오차, 베타0 : 상수, 베타1~p : 기울기
## 오차항은 평균이 0이며 분산은 σ2인 정규 분포를 따름, 오차항 간 독립

## x(i)에서 세로로 선을 그었을 때, 중심점에서 떨어진 점들은 오차이고, 정규분포를 따름
## 각 x(i)에서의 오차들은 '등분산'이라고 가정

## 독립변수(x)는 고정된 값(관측된 값), 독립변수 간 독립임
#############################################################

cars
plot(cars) # -> 경향성 파악
cor.test(cars$speed, cars$dist) # -> pvalue < 0.05, 따라서 귀무가설 기각 (=상관관계 존재)

# y=wx, y=bx (독립변수가 하나이므로, 단순선형회귀)

model1 = lm(dist ~ speed, cars)
model1$coefficients # 가중치 값 -> y = -17.579 + 3.932x1 식을 도출 가능 (가설임)
summary(model1) # pvalue를 통해 식이 타당한 지 점검 (1.49e-12는 0.05보다 작으므로 타당!)

residuals(model1) # 각 잔차(오차) 보기
plot(model1, which = 1, xlim = c(20,60)) # which를 이용해서 그래프를 선택할 수 있음
# xlim를 이용해서 특정 x값에 대한 결과 표를 볼 수 있음
# 각 그래프 중 residual vs leverage 그래프에서
# 점선(오른쪽상단)에 있는 포인트는 이상치임 -> 제거 대상

## 오차의 독립성 검정 -> 잔차에 대한 더빈 왓슨 통계
## (하한, 상한) 상한보다 크면 오차는 서로 독립적, 하한보다 작으면 서로 양의 상관관계
## 독립변수 1개(1.65, 1.69), 2개(1.63, 1.72), 3개(1.61, 1.74)....
library(car)
durbinWatsonTest(model1$residuals) # redidual = y - fitted value (실제값 - 예측값)
# 상한보다 작더라도 상한에 가까울수록 독립에 근접한다고 볼 수 있음

predict(model1, newdata = data.frame(speed=c(3,7)))
# 도출한 식을 통해 speed 3, 7에 따른 dist 예측

####################################################################################
# 다중선형회귀

n = nrow(iris)
set.seed(200)
ind = sample(1:n, n*0.7, replace = F)
train = iris[ind, ]
test = iris[-ind, ]

model2 = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, train)
# 물결 뒤에 .(마침표) 찍으면 좌변에 해당하는 변수 제외한 모든 변수를 넣는데,
# 그러면 Species(범주형 데이터)도 들어가므로 일일이 적음
summary(model2)
plot(model2) # summary와 plot을 통해 가정이 맞는 지 확인
durbinWatsonTest(model2$residuals) # 오차간 독립성 검정
vif(model2) # 다중공선성 문제 파악
# Petal.Length와 Petal.Width에서 문제 발생!


model3 = lm(Sepal.Length~ Sepal.Width, train)
summary(model3)
plot(train$Sepal.Length, train$Sepal.Width)
# 결과, r-squared가 작아지는 문제 발생!


cor(train[, 2:4]) # cor를 통해 상관계수가 높은 Peta.Width만 제거
model4 = lm(Sepal.Length ~ Sepal.Width + Petal.Length, train)
summary(model4)

#################################################################################
# 많은 변수 중 변수 선택 문제

library ( mlbench )
data ( BostonHousing )
str(BostonHousing)
m <- lm(medv ~., data = BostonHousing)

summary(m) # 여러 변수들이 나오는데, * 붙은 변수를 제외한 모형을 찾고 싶음

m2 <- step (m, direction = "both") # full모형에 대한 AIC가 나옴
# AIC : 1589.64보다 작은 것 중 가장 작은 변수 age 빼기
# age 뺀 AIC : 1587.65보다 작은 것 중 가장 작증 변수 indus 빼기
# age+indus 뺀 AIC보다 작은 변수 없음
# 이것이 최적의 모형!

summary(m2)

m3 = step(lm(medv~1, BostonHousing), direction = 'forward', 
          scope = "~ crim + zn + chas + nox + rm + dis + rad + 
          tax + ptratio + b + lstat") #forward 방법 사용

###############################################################
outlierTest(model1) # 49번이 outlier로 의심이 됨,
#본페로니 값이 0.05보다 크기 때문에 outlier는 아님
################

o1 <- outlierTest(m)
o1$rstudent
o2 <- as.numeric(names(o1$rstudent))

B2 = BostonHousing[-o2, ]
# 여기까지 outlier 제거 작업
# 그리고 위쪽 모형 찾는 단계 시작

######################################################

install.packages('mixlm')
library(mixlm)

pred1 = predict(model2, newdata = test)
pred2 = predict(model4, newdata = test)

mse1 = mean((test$Sepal.Length - pred1) ^2)
rmse1 = sqrt(mean((test$Sepal.Length - pred1) ^2))

mse2 = mean((test$Sepal.Length - pred2) ^2)
rmse2 = sqrt(mean((test$Sepal.Length - pred2) ^2))

# 평균 제곱근 오차(Root Mean Square Error; RMSE)
# 추정 값 또는 모델이 예측한 값과 실제 환경에서 관찰되는 값의 차이를 다룰 때 흔히 사용하는 측도
c(mse1, mse2, rmse1, rmse2) # -> 결과가 작은 쪽으로 선택 (최대한 0에 가까운 것)



###########################################################
#오후수업

# 범주형데이터가 들어가는 경우,
# 변수 전환을 해줘야함 ('더미변수'로 자동변환, 더미변수는 0 or 1의 값을 가짐)
# 범주가 성별(남자OR여자)인 경우,
# y = a+ b1x1 + b2x2 + (b3x3) -> a+ b1x1 + b2x2 + (b3D1 + b4D2)

#  D1  D2
#1  0   0
#2  0   1
#3  1   0

# 범주형 데이터를 넣는 순간 , R은 자동변환함 (더미변수로 변환)
# 레벨1은 기본형, 레벨2는 남(여)자가 들어가는 경우, 레벨3은 여(남)자가 들어가는 경우,
# 둘다 선택되는 경우는 없으므로 총 3가지가 나오는 것임
# 집단이 n개라면 (level = n) 더미변수가 n-1개 생성
nlevels(train$Species)
levels(train$Species)

d1 = stats::lm(Sepal.Length~., train)
summary(d1)
#species1, species2가 나오는데 기준을 무엇으로 잡는지에 따라 나머지 안나오는 것이 다름
#집단이 2개이고 더미변수가 0 0라면 3가지 집단 중 하나의 경우이고, 
# 그 집단에 대한 변동성이 intercept에 적용되어서 출력됨

cbind(test$Species, as.numeric(test$Species))
# -> 데이터 구조는 cbind는 매트릭스일 때 쓰이므로 as.numeric에 맞춰서 전부 숫자로 출력됨


# 여기까지 선형
####################################################################
#선형의 경우,
# y = a+b1x1+b2x2
# [a b1 b2] X [1 x1 x2] [1, 3] X [3, 1], 행렬의 곱으로 표현 가능
# [a b1 b2] = w 로 표현 가능, 따라서 y = wx로 표현 가능

# 비선형의 경우, 예를들어 sin(x)
# 비선형이지만 선형화하여 계산 : '커널'함수를 이용

x = sample(0:(2*pi), 1000, replace = T)
y = sin(x)
e = rnorm(1000, 0, 1) 
y_e = y+e # sin(X)와 오차가 나게 생성
plot(x, y_e) # 찍어보기

##########################################################

x1=seq(0, (2*pi), 0.01) # 0~2pi까지 x를 0.01 차이마다 출력
data = data.frame()
## y=sin(x) 에서 x마다 50개의 정규분포를 따르는 오차들을 생성
## 등분산의 경우,
for(i in x1) {
e=rnorm(10, 0, 1)
y=sin(i) + e
x2 = rep(i, 10)
data1 = cbind(x2, y)
data = rbind(data, data1)
}
plot(data, cex = 0.1) # cex 는 점 사이즈

non1 = lm(y~x2, data)
# line(data$x2, non1$fitted.values, col = 'red') # 비선형이므로 에러뜸!
abline(non1, col = 'blue')
###########################################
## 이분산의 경우,
for(i in x1) {
  e=rnorm(10, 0, sample(1:3, 1, replace = T))
  y=sin(i) + e
  x2 = rep(i, 10)
  data1 = cbind(x2, y)
  data = rbind(data, data1)
}
plot(data, cex = 0.1) # cex 는 점 사이즈
############################################

## 해결방안
# install.packages('e1071')
library(e1071)

svm1 = svm(y~x2, data)
pred1 = predict(svm1, newdata = data)
# fitted.value와 predict값 같은 개념, svm에는 fitted value가 없어서 predict를 사용

points(data$x2, pred1, col = 'red', pch = '*', cex = 0.3) # 비선형데이터의 예측값 찍어보기
points(data$x2, sin(data$x2), col = "blue", pch = '#', cex = 0.5) # 실제 sin값을 찍어서 비교

non_svm1 = lm(y~x2, data)
pred2 = predict(non_svm1, newdata = data)
rmse_s = sqrt(mean((data$y-pred1)^2))
rmse_n = sqrt(mean((data$y-pred2)^2))

c(rmse_s, rmse_n)

#########################################################

svm2 = svm(Species~., train)
pred3 = predict(svm2, newdata = test)
t1 = table(test$Species, pred3) # 대각선에 있는 값들이 제대로 예측한 것

sum(diag(t1))/sum(t1)
#######################################################

y2 = data$y-pred1
data3 = cbind(data, y2)

svm_e = svm(y2~x2, data3)
pred4 = predict(svm_e, data3)
hat_y = pred1+pred2

points(data3$x2, hat_y, col = 'blue', pch  = '$', cex = 0.6)
