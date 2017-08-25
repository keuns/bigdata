library('xlsx')
## 안에 7/5, 7/6 양일의 데이터가 들어가 있어서 에러가 났었음
## 둘의 데이터 타입이 달랐기 때문에 앞에 지역 앞에 날짜를 추가해서 타입을 동일하게 하면 됨!

g3 = read.xlsx('g3.xls', sheetIndex = 1, start = 2, header =T, encoding = 'UTF-8')
g4 = g3[-30, ] # 서청주 빼고 출력


View(g4)
g4$q1 = apply(g4[, 2:7], 1, FUN = mean)
g4$q2 = apply(g4[, 8:13], 1, FUN = mean)
g4$q3 = apply(g4[, 14:19], 1, FUN = mean)
g4$q4 = apply(g4[, 20:25], 1, FUN = mean)


# 행방향으로 반복처리
#1행에 대해서 2열~7열까지 평균 구하기, 행방향으로 2행, 3행...반복처리
# 위 apply문장과 밑의 for 코드가 같음
m2 = c()
for ( i in 1: nrow(g4)) {
  m1 = mean(g4[i, 2:7])
  m2 = c(m2, m1)
}
g4$q1 = m2


var.test(g4$q2, g4$q3)
t.test(x=g4$q3, y=g4$q2, var.equal = T, paired = T) # 검정 결과, 시간 간의 차이가 나지 않음

## 이표본인 경우 사전 점검 항목
## var.equal = T/F, paired = T/F

var.test # var.equal = T/F (공분산인 지 점검)
t.test() # paired = T/F(before, after로 볼 건지 조절 가능), conf.level = 0.9(신뢰구간 조절 가능)

sleep
# 1번 group 필드를 약1, 약2으로 보는 방법,
# 1번의 경우, 약에 안맞는 사람이 먹었을 가능성이 있기 때문에 분석 결과가 의미없음
# 원래 값에서 얼마나 '변화'했는 지가 중요! (원래 체질을 안 상태에서 약에 대한 수면효과 파악)
# 따라서, 2번 before, after로 보는 방법이 더욱 효과적

## 내가 한 것
var.test(extra~group, sleep)
t.test(extra~group, sleep, var.equal = T, conf.level = 0.99)

## 1번 경우,
var.test(x=sleep[1:10, 1], y=sleep[11:20, 1])
t.test(x=sleep[1:10, 1], y=sleep[11:20, 1], var.equal = T, paired = F, conf.level = 0.99)
## 2번 경우,
t.test(x=sleep[1:10, 1], y=sleep[11:20, 1], var.equal = T, paired = T, conf.level = 0.99)

## dataframe 만드는 방법
a3=data.frame(1:7, 11:17, letters[1:7])

###############################
# 분산분석

libarary(MASS)
data(survey)
View(survey)

model1 = aov(Pulse ~ Exer, data = survey) # 운동량에 따라 맥박수 분산 분석
summary(model1)

model2 = aov(Sepal.Length ~ Species, data = iris)
summary(model2)

model3 = aov(Pulse~Exer+Smoke+Exer:Smoke, data = survey)
summary(model3)

## *는 모든 경우 출력, 불편하게 굳이 + 쓸 필요 없음
model4 = aov(Pulse~Exer*Smoke, data = survey)

t1 = TukeyHSD(model1, "Exer") # 결과, some-freq에서 p값이 0.05보다 작으므로
# 운동을 some하는 집단과 freq하는 집단 간 심박수 평균이 차이가 난다는 결론을 낼 수 있음
# 보통 영향을 받는 변수 ~ 영향을 주는 변수(그룹변수) 로 쓰임

View(g4)
install.packages('reshape2')
library(reshape2)

g5 = melt(g4, id = 1:25) # id는 고정시키는 부분,
# 고정된 부분을 제외한 나머지는 variable의  factor로 들어감, 모든 경우를 출력
View(g5)
model21 = aov(value~variable, data = g5)
summary(model21)


## 황사 시간대 별로 차이나는지 확인
g6 = melt(g4[ ,1:25], id = 1)
View(g6)
model22 = aov(value~variable, data = g6)
summary(model22)
t2 = TukeyHSD(model22, 'variable')
str(t2)


t3=as.matrix(t2$variable)
str(t3)
table(t3[,4]<0.05)

rownames(t3[(which(t3[,4]<0.5)),])

#############################################
# 오후수업

# 값 하나만 가지고 있는 변수 : 스칼라
a=5
a2= "a"
# 한 행에 여러 열이 있는 구조 : 벡터, []괄호를 이용해서 값을 받아올 수 있음
a3=1:3
a3[2]
# 매트릭스 구조 vs dataframe 구조
# 매트릭스 : 행과 열이 있는구조, 동일한 데이터타입
# []괄호를 이용해서 값을 받아올 수 있음
# dataframe : 다른 데이터타입 가능

a4=matrix(1:15, nrow=3)
a6=array(dim=c(3,2,3)) # 3차원 배열 생성
a5=a4[2,1]
str(a4)
str(a6)
str(a5)

a7=data.frame(1:3, rep(1:3), LETTERS[5:7]) # dataframe 생성, '변수'별로 다른 데이터타입 입력 가능
str(a7)
#상하위 구조 개념이 있음 (상위 $ 하위 변수로 표현)

a7$X1.3
a9=a7[2] # 숫자를 하나만 적으면 행은 생략한 걸로 간주, 2열만 보여줌
a9=as.matrix(a7[2]) # a7[2] 구조를 dataframe에서 매트릭스로 변환
a9=as.matrix(a7) # a7를 매트릭스로 변환, 단 자료형이 문자로 변환

str(a9)

list # list구조는 변수 안에 변수 입력 가능
a8=list(a=a, a2=a2, a3=a3, a4=a4, a6=a6)

a10 = iris[iris$Sepal.Length >=6.5, ]
# 조건에 만족되는 값을 찾고 싶을 때 []괄호안에 조건을 쓰면 됨
# 조건에 대한 결과는 T,F로 나옴
iris[iris$Sepal.Length >=6.5, ]

rownames()
colnames()
names()

rownames(iris[iris$Sepal.Length >=6.5, ])
rownames(iris[which(iris$Sepal.Length >=6.5), ])

table(iris$Sepal.Length >= 6.5) # 결과 출력

################################################

install.packages('mlbench')
library(mlbench)

data(Vowel) # 데이터 로딩
View(Vowel)
grep('i', Vowel$Class)
v1 = Vowel[grep('i', Vowel$Class), ]
# 위치번호를 돌려주는 함수
# Class열에서 i가 포함된 행의 위치번호를 받아서 출력하는 예
View(v1)

##############################
# 분산분석 -> 등분산이라는 가정 하에 실시 
# 따라서, 등분산인지 아닌지 확인해줘야 함!

install.packages('car')
library(car)

leveneTest(Sepal.Length~Species, data = iris)
# 등분산인지 확인하는 함수 -> 결과, 등분산X
# Species는 3개의 '그룹', 따라서 분산분석을 해야하는데, 그 전에 등분산인 지 확인하는 작업임

## http://archive.ics.uci.edu/ml/index.php : 데이터 얻을 수 있는 사이트

##################### 온라인 데이터 이용해서 집단간의 차이 검정

library(xlsx)
go2 = read.table('clipboard', header = T)

View(go2)
go2$rating_bus = factor(go2$rating_bus)
go2$rating = factor(go2$rating)

good = aov(time~rating*rating_bus, data = go2)
summary(good)

tu = TukeyHSD(good, c("rating", "rating_bus"))
plot(TukeyHSD(good, c("rating", "rating_bus")))

library(car)
leveneTest(time~rating*rating_bus, data = go2)

# 시간과 평점에 관한 차이

############################################

go2$rating = as.factor(go2$rating) 

car1 = go2[go2$car_or_bus==1, ] # !!!!!!!!!!!!!!!!!!!주의!!!!!!!
bus1 = go2[go2$car_or_bus==2, ]

w1 = go2[go2$rating_weather == 1, ]
w2 = go2[go2$rating_weather == 2, ]

mm1 = aov(time~rating, car1)

# 차와 버스 각각 시간에 대한 평점

##########################################

go3 = go2[go2$rating_weather != 0, ]
str(go3)

go3$rating_weather = factor(go3$rating_weather)

var.test(as.numeric(rating)~rating_weather, go3)

tt1=t.test(as.numeric(rating)~rating_weather, data = go3, var.equal = T, paired = F)
table(go3$rating, go3$rating_weather)

############################################
