write.csv(iris, 'data.xlsx')
methods(plot)

rm(list = ls())

a1 = iris
a2 = cars

## 저장 및 불러오기 함수
save (a1, a2, file = 'a.rdata')
load('a.rdata')

x1 = matrix(1:15, nrow = 5)
x2 = matrix(1:15, nrow = 5, byrow = T)

## '강제로' 각 매트릭스를 세로로 합치는 함수
y1 = rbind(x1, x2)
## '강제로' 좌우로 합치는 함수
y2 = cbind(x1, x2)

## 기존 값에 1~10 값이 연속적으로 추가됨
x3 = c(1,3)
for(i in 1:10) {
  
x3 = c(x3, i)
}

x4 = c(1,2,3); x5=c(5,6,7)
rbind(x4, x5)
cbind(x4, x5)

## 동일한 항목을 기준으로 합치고 싶을 때 쓰는 함수
xx <- data.frame ( name =c("a", "b", "c"), math =c(1, 2, 3))
yy <- data.frame ( name =c("d", "b", "a"), english =c(4, 5, 6))
## yy에는 있는 데 xx에는 없는 것들을 제외하고 출력, all = T 추가하면 모든 경우 출력
merge(xx, yy, all = T)

## name이 다를 때(항목의 이름이 다를 때) 합치는 경우
xx <- data.frame ( name1 =c("a", "b", "c"), math =c(1, 2, 3))
yy <- data.frame ( name2 =c("d", "b", "a"), english =c(4, 5, 6))
merge(xx, yy, all = T, by.x = 'name1', by.y = 'name2')

## 부분적으로 선택하는 함수
s1 = subset(iris, Species == "setosa")
str(s1)
s1$Species = factor(s1$Species)

## 데이터를 속성별로 분리하는 데 사용되는 함수
s2 = split(iris, iris$Species)
str(s2)

s3 = c(1,5,3,7,9,2)
sort(s3, decreasing = F)
## 소팅은 값을 정렬하는 것
## 위치 번호로 돌려주는 것이 order
## 예를 들어 9는 소팅 전에 5번째에 있으므로 소팅한 후 9를 적는 것이 아니라 5를 적음
order(s3, decreasing = F)


## with, within

## attach, detach


## 위치 번호를 찾는 함수 : which
k <- c(2, 4, 6, 7, 10)
k %% 2
which (k %% 2 == 0)

## 반복 처리 : apply (중요!!!!!!!!!!!!!!)
d <- matrix (1:9 , ncol = 3)
mean(d)

## 행 단위로 구하고 싶을 때
for( i in 1:nrow(d)) {
print(mean(d[i,]))
}

## 이 작업을 편하게 하고 싶을 때, apply(매트릭스, (1은 행 단위, 2는 열 단위), 원하는계산)
apply(d, 1, mean)

## l은 결과를 list로, s는 결과를 벡터로 반환

## 그룹 별 처리 함수 (species 그룹 별 sepal length 최소 값 출력)
tapply(iris$Sepal.Length, iris$Species, min)

## mapply

## By 이하의 조건에 따라서 수행(do)하는 함수 :doBy

# install.packages('doBy')
library(doBy)

## '필드'별 요약
summary(iris)

## '그룹'별 요약
summaryBy(.~Species, iris, FUN = median)
summaryBy(Sepal.Length~Species, iris, FUN = median)

## sampling 시 뽑히는 것에 대한 가중치를 둘 수 있는 경우
table(sample(1:20, 200, replace = T, prob=c(1, 3, 3, rep(1, 17))))

## 그룹 별 10%(0.1) : 5개씩 sampling 한 경우
sampleBy(Sepal.Length~Species, data = iris, frac = 0.1)
## 또는
sampleBy(~Species, data = iris, frac = 0.1)
# 기준(Species)에 따라 뽑는 것이기 때문에 기준을 정확하게 적어주는 것이 중요!

## 전체 조각에 대한 10% : 15개씩 sampling 한 경우
sampleBy(Sepal.Length~Species, data = iris, frac = 0.1, systematic = T)

## aggregate와 summaryBy가 같은 역할을 함
aggregate(Sepal.Width~Species, iris, mean )

## stack, unstack, melt

## DB와 연동하는 패키지 : RMySQL

## SQL문을 이용해서 데이터 조작 : sqldf
# install.packages("sqldf")
library(sqldf)

sqldf("select distinct Species from iris")

## iris의 필드 이름 바꾸기, 왜냐하면 DB에서 마침표(.)와 헷갈리기 때문
# names(iris) = c('SL', 'SW', 'PL', 'PW', 'SP')
## 되돌리기
# names(iris) = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width', 'Species')

sqldf("select avg(SL) from iris where SP = 'setosa'")
sqldf("select SP, max(SW) from iris group by SP")

library('MASS')
data("survey")
View(survey)

## 가설검정, 경영진에서 의사결정하도록 자세한 결론을 내려줘야 함!
model1 = t.test(Height ~ Sex, survey)

## 표본평균 180ml, 표본표준편차 10인 100개의 우유팩 표본
data1 = rnorm(100, mean = 180, sd = 10)
## 우유팩에 적혀있는대로 200ml가 맞는 지 검정! -> 결과 : 귀무가설 기각
t.test(x = data1, mu = 200)

data2 = rnorm(100, mean = 160, sd = 5)
## 두 집단 간의 차이가 나는지 아닌지 검정!
## 먼저 등분산인지 아닌지 확인 후 검정해야 함!
var.test(x = data1, y=data2)
## pvalue값을 통해 등분산인지 확인 (여기선 0.05보다 큰지 작은지!)
t.test(x = data1, y = data2, var.equal = F)
