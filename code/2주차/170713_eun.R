## 분류기법 (train으로 학습, test로 테스트 -> 적중률 계산)

# 데이터가 있을 때, 그것을 train과 test데이터로 나눈다
# train데이터로부터 모형(y = w*f(X))을 찾는다(학습한다)
# for문으로 반복하여 최적의 모형을 구한다
# test데이터(새로운 데이터)로부터 만들어진 모형을 평가한다 
# test데이터의 x를 모형에 넣어서 y를 구한다(y.hat)
# y-(y.hat)가 최소가 되도록 한다
# 여기서 y는 실수가 아닌 'factor변수'(ex. 남자or여자)

## 로지스틱 회귀모형

# y에 '확률값'이 나오도록 회귀모형을 변형
# 기존의 회귀모형 y = (베타0) + (베타1)xi
# f( (베타0) + (베타1)xi ) -> 함수를 취하여 확률값을 만들어냄
# 확률(0<y<1)이 기준치(ex. 0.5) 이상이면 A그룹, 아니면 B그룹으로 나눔

###########################################################################################
## 이항인 경우 (두 그룹으로 분류해야 하는 경우)

rm(list = ls())
data = iris[1:100, ]

levels(data$Species)
table(data$Species) # virginica 그룹 껍질이 아직 남아있음

data$Species = factor(data$Species) # 결과, virginica 껍질 사라짐

set.seed(1000) # 일정한 랜덤값이 나오게 하는 코드
ind = sample(1:nrow(data), nrow(data)*0.7, replace = F)
train = data[ind, ]
test = data[-ind, ]

#glm 함수를 이용해서 분류 (회귀도 가능한 함수임)
(m <- glm( Species~., data = train, family = 'binomial' )) # family에 binomial을 해줘야 로지스틱 회귀모형 실행됨

m$fitted.values # 각 데이터에 대해 확률을 계산
train_y = ifelse(m$fitted.values >=0.5, 2, 1) # 확률이 0.5이상이면 2그룹, 아니면 1그룹
table(train_y)
table(train$Species, train_y)

pred1 = predict.glm(m, newdata = test, type = 'response')
# type이 response면 확률값, class면 class로 돌려받음, 여기서의 predict는 class를 지원안함
pred2 = ifelse(pred1 >= 0.5, 2, 1)
table(test$Species, pred2) # -> 원래는 결과가 제대로 안나와야되는데 iris가 최적화된 데이터라서 결과가 잘 나오는 것임 

#####################################################################
## 다항인 경우 (3개 이상의 그룹으로 분류해야 하는 경우)
# A그룹이 나올 확률, B그룹이 나올 확률, C그룹이 나올 확률...을 구해서
# 특정그룹이 나올 확률/전체확률이 큰 그룹으로 할당

library(nnet)

set.seed(1000) # 일정한 랜덤값이 나오게 하는 코드
ind = sample(1:nrow(iris), nrow(iris)*0.7, replace = F)
train = iris[ind, ]
test = iris[-ind, ]

#glm 함수를 이용해서 분류 (회귀도 가능한 함수임)
(m <- multinom(Species~., data = train, family = 'binomial' )) # family에 binomial을 해줘야 로지스틱 회귀모형 실행됨
m$fitted.values # 각 데이터에 대해 확률을 계산
# m$softmax # 전체 확률로 나누는 코드
m_class = max.col(m$fitted.values) # 각 데이터가 어느 그룹에 들어가는 지 확인 가능한 코드

table(m_class)
table(train$Species, m_class)

pred1 = predict(m, newdata = test, type = 'class') # 여기서의 predict는 class 지원함

table(pred1)
table(test$Species, pred1) # 결과, 적중률이 조금씩 떨어짐

######################################################################################
## 연습문제
library(mlbench)
data(Sonar)
View(Sonar)

set.seed(1000)
ind = sample(1:nrow(Sonar), nrow(Sonar)*0.7, replace = F)
train = Sonar[ind, ]
test = Sonar[-ind, ]

(m <- glm(Class~., data = train, family = 'binomial' ))

m$fitted.values
train_y = ifelse(m$fitted.values >=0.5, 2, 1)
table(train_y)
table(train$Class, train_y)

pred1 = predict.glm(m, newdata = test, type = 'response')
pred2 = ifelse(pred1 >= 0.5, 2, 1)
table(test$Class, pred2)

######################################################################################
## decision tree -> 분류를 트리형태로 나타내는 방법
## 지니지수, 엔트로피 지수는 낮을수록 좋음 (불순도가 낮아지는 것임)
## 불순도는 그룹 안에 한가지 종류만 있는 것이 아니라 여러 종류가 섞여 있는 경우임)

set.seed(1000) # 일정한 랜덤값이 나오게 하는 코드
ind = sample(1:nrow(iris), nrow(iris)*0.7, replace = F)
train = iris[ind, ]
test = iris[-ind, ]

install.packages('party')
library(party)
c1 = ctree(Species ~ ., train, controls = ctree_control(maxdepth = 2))
# max_depth는 트리의 depth를 최대 어디까지 할 것인지 결정
plot(c1)
# decision tree가 생성됨, 노드의 평균을 기준으로 이상/미만으로 나눔
# n>=10인 경우에만 그룹으로 인정

pred2 = predict(c1, newdata = test)
sum(pred2==test$Species)/nrow(test) # 적중률 계산
table(test$Species, pred2)

####################################################################################

View(Vowel)
data(Vowel)

nn = nrow(Vowel)
ind2 = sample(1:nn, nn*0.7, replace = F)

set.seed(100)
train1 = Vowel[ind2, -1]
test1 = Vowel[-ind2, -1]

str(Vowel)
c2 = ctree(Class ~ ., train1)
pred3 = predict(c2, newdata = test1)
table(test1$Class, pred3)
sum(test1$Class==pred3)/nrow(test1) # 변수가 많을수록 적중률 떨어짐
    