rm(list=ls())

options(digit = 3)
set.seed(100)
x1 = rnorm(100, mean = 0, sd = 1)
hist(x1)
plot(density(rnorm(1000000, 0, 10)))
mean(x1)
var(x1)

median(x1)

quantile(x1, c(0.25, 0.5, 0.75)) # 분위수 구하는 함수

dpois(3,1) # 푸아송 확률밀도함수

q1 = quantile(1:10, c(1/4, 3/4))
str(q1)
q1[2]-q1[1]

## 표본 추출
sample(1:10, 5) #1:10을 1: 생략하고 10으로 적어도 1~10까지 알아서 판단해줌
sample(1:10, 5, replace = T, prob = 1:10) # 가중치 : prob

##섞어서 뽑는 방법1
ind1 = sample(nrow(iris), nrow(iris), replace = F) # 숫자가 뒤섞임
A1 = iris[ind1, ] # iris 데이터의 행이 뒤섞임
n1 = nrow(iris)
train = A1[1:(n1*0.7), ]
test0 = A1[((n1*0.7)+1):n1, ]
test1 = A1[-(1:(n1*0.7)), ]

## 방법2, 섞어서 뽑는 것 중에 제일 편한 방법
ind2 = sample(n1, n1*0.7, replace = F)
train1 = iris[ind2, ]
test1 = iris[-ind2, ]

## 방법3
ind3 = sample(2, n1, replace = T, prob=c(0.7, 0.3))
train2 = iris[ind3 == 1, ]
test2 = iris[ind3 ==2, ]

## 그룹 별 표본 a개 추출
# install.packages('sampling')
library(sampling)

x <- strata (c("Species"), size =c(3, 4, 5) , method = "srswor", data = iris )
getdata(iris, x)

x <- strata (c("Species"), size =c(1, 2, 3) , method = "srswr", data = iris )
getdata(iris, x)

## 반복 함수 (반복하고싶은 숫자, 반복 횟수)
rep(c(3,7),3) # (3 7) 3개 출력
rep(c(3,7),c(3,2)) # 3 3개 7 2개 출력
rep(c(3,7), each=3) # 각 숫자 3개씩 출력
rep(1:10, length.out=15) # 15개 출력 (10보다 넘어가니깐 처음부터 시작해서 15개 채움)
#length.out와 each를 함께 쓸수 없음

## 층화 임의 추출
iris$Species2 <- rep (1:2 , 75)
strata (c("Species", "Species2"), size =c(1, 1, 1, 1, 1, 1), method="srswr", data = iris)

A1 = iris
A1$Species2 <- rep (1:2 , 75)
strata (c("Species", "Species2"), size =c(1, 1, 1, 1, 1, 1), method="srswr", data = A1)

library(doBy)
sampleBy(~Species+Species2, frac = 0.3, data = A1)

## 계통 추출 ex) 3->13->23->.... 번째 데이터 추출
d1 = data.frame()
for(i in seq(sample(1:10, 1), 150, 10)) {
  d2 = iris[i, ]
  d1 = rbind(d1, d2)
}

## 분할표
d <- data.frame (x=c("1", "2", "2", "1"), y=c("A", "B", "A", "B"), num=c(3, 5, 8, 7))
d1 = rbind(d, d) # 위아래로 붙이기

table(d$x, d$y) # 빈도수 출력
xt = xtabs(num ~ x+y, data = d1) # 분할표

sum(xt)
margin.table(xt, 2)
prop.table(xt, 1)

tot = sum(xt)
p_xt = prop.table(xt)

str(p_xt)

tot*p_xt[[1]][1]

## 독립성 검정 : chisq.test, 결과에 나온 p-value를 통해 가설검정
library("MASS")
data(survey)
View(survey)

xt = xtabs(~Sex+Exer, data = survey)
chi1 = chisq.test(xt) # 결과 값이 0.05보다 크므로 귀무가설 채택
chi1
str(chi1)
chi1$statistic

## 피셔.테스트
fisher.test(xtabs(~W.Hnd+Clap, data = survey))

## 맥니마 검정(캠페인 후 감정 변화 있는 지 파악할 때 쓰임)
Performance <- matrix (c(794 , 86, 150 , 570) , nrow = 2,
                       dimnames = list ( "1st Survey " = c(" Approve ", " Disapprove "),
                                         "2nd Survey " = c(" Approve ", " Disapprove ")))
mcnemar.test (Performance)



## 적합도 검정 : 3:7인지 검정하는 예, 계산 결과 3:7이 아니므로 귀무가설 기각
t1 = table(survey$W.Hnd)
chisq.test(table(survey$W.Hnd), p=c(.3, .7)) 
str(t1)
t1[1]
t1/sum(t1)

## 표본이 정규분포로 추출된 것인지 테스트하는 함수
shapiro.test(rnorm(1000))

## 주어진 두 데이터가 동일한 분포인지 테스트하는 함수
ks.test(rnorm(100), rnorm(100, 5, 3))

x <- rnorm (1000 , mean =10 , sd =1) # 정규분포를 따르는 난수생성
qqnorm(x) # 분위수를 좌표평면에 표시
qqline(x, lty = 2) # 데이터들이 선형인지 확인하기 위해 선을 긋는 함수

x <- rcauchy (1000) # 정규분포가 아닌 데이터 1000개 생성
qqnorm (x)
qqline (x, lty =2)
# 대각선일 때 정규분포를 따른다고 할 수 있는데, 결과 대각선이 아니므로 정규분포 아님
# 데이터 점검 용도 (신뢰도 높이기 위함)

## 켄달의 순위 상관계수 : 데이터에 대한 순위로 바꾼 다음 순위 값 비교
## 오름차순으로 정렬하는 지 내림차순으로 정렬하는 지에 따라 계산 결과가 다름

## 추정 및 검정
ss1 = survey[survey$Sex == 'Male',"Height"]
m1 = survey[survey$Sex == 'Male', ]
f1 = survey[survey$Sex == 'Female', ]


ss2 = na.omit(ss1) # NA 들어가 있는 데이터 제거!
ss3 = na.omit(survey[survey$Sex == 'Female',"Height"])
mean(ss1); mean(ss2)

## 남자 평균 키가 173.5가 맞는 지 가설 검정 -> 결과, 귀무가설 기각!
t.test(x=ss2, mu = 178) 

length(ss2); length(ss3) # 두 집단의 크기가 다름

## 두 집단 간 차이 존재하는 지 가설 검정, 따라서 mu는 쓸 필요 없음, 대신 공분산 점검해야 함
var.test(x=ss2, y=ss3) # pvalue가 0.05보다 작으므로 등분산 아님
#before -> after 확인하려면 paried = T 해야 함, AND 두 데이터 크기도 같아야 함)
t.test(x=ss2, y=ss3, var.equal = F, paired = F)

## 하나의 열 안에 한 사람의 정보가 여러 개 있을 때 ~(물결)을 이용해서 t.test할수 있음
var.test(extra~group, data = sleep)
t.test(extra~group, data = sleep, var.equal = T, paired = T)
# BEFORE, AFTER 알기 위한 것이기 때문에 paired = T로 설정,
# 1그룹에 i번째와 2그룹에 i번째 비교, i+1...

sleep1 = sleep[1:10, ]
sleep$group2 = sleep[11:20, 1]
t.test(x=sleep1$extra, y= sleep$group2)
