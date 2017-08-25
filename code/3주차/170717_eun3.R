# svm
# y : 수치형 as.numeric -> 회귀
# y : 범주형 as.factor -> 분류

library(mlbench)
data(Vowel)

str(Vowel)

data11 = Vowel
data11$Class = as.numeric(data11$Class)


model21 = svm(Class~., data = data11[, -1], type = 'C-classification', kernel = 'radial')
model21$fitted -> # Class 변수의 데이터타입이 factor가 아니더라도 type을 분류로 설정해서 분류문제로 바꿀 수 있음
levels(Vowel$Class)

t1 = table(data11$Class, model21$fitted)
sum(diag(t1))/sum(t1)

model22 = svm(Class~., data = Vowel)
t2 = table(Vowel$Class, model22$fitted)
sum(diag(t2))/sum(t2)
           
# www.web-r.org 
# https://www.rstudio.com/
