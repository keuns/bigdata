summary(iris)
x=table(iris$Species)

x1<-1
2->x2
<<-
x3 = 1:10
(x4 = c(1,5,3,9))
x4
x4[2]
X5=matrix(1:15, ncol=3, byrow=T)
X5[-c(2,4) , 3]
str(iris)


x1=c(1,2,3,4,5,4,3,6,7, 1000)
sum(x1)/length(x1)
mean(x1)
((n/2) + ((n/2)+1))/2
median(x1)

sample(1:45, 6, replace = F)
ind = sample(1:nrow(iris), 150, replace = F)
A1 = iris[ind, ]
View(iris)
View(A1)

ind1 = sample(1:nrow(iris), nrow(iris)*0.7, replace = F)

train=iris[ind1,]
test=iris[-ind1,]

summary(iris)

hist(iris$Petal.Length)
boxplot(iris[ ,1:4])

View(iris)
A2=iris
A2[, 1:4] = scale(iris[, 1:4])
View(A2)

str(iris)
nlevels(iris$Species) # 레벨 수를 볼 수 있음
levels(iris$Species) # 각 레벨을 볼 수 있음

library(MASS) # MASS 사용
data("survey") # survey 데이터 로딩
View(survey) # survey 보여줌
t1 = table(survey$Sex, survey$Smoke, margin)
prop.table(t1, 2)

barplot(t1)

plot(iris$Petal.Length, iris$Sepal.Length)
n = round((length(iris$Sepal.Length)*0.1)/2,0)
# 이상치를 제거하는 방법!! 10% 제거(상위 5%, 하위 5% 제거)
x1=sort(iris$Sepal.Length) # 정렬하는 함수
x1[(n+1):(length(iris$Sepal.Length)-n)] # 9번째 값부터 142번째 값까지 출력
str(x1)

hist(rnorm(1000000))
# 평균 100 표준편차 3인 정규분포 10000의 데이터를 생성, 시각화하는 함수

cor(iris[, 1:4])
# 상관계수를 볼 수 있는 함수
cor.test(iris$Sepal.Length, iris$Petal.Length)
# 상관계수 유무를 검증하는 함수