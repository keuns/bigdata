# 시계열 데이터 svm에 적용
# Y : 6일차
# X : Y 기준 과거 5일간의 데이터

data1 = seq(1, 500, 2)

d1 = array(dim=c((500/6), 6))

for ( i in 1:(500/6)) {
  d1[i, ] = data1[i:(i+5)] 
}

X = d1[, 1:5]
Y = d1[, 6]
library(e1071)
model3 = svm(x = X[1:60, ], y = Y[1:60], kernel = 'linear')
pred2 = predict(model3, newdata = X[61:83, ])
cbind(Y[61:83], pred2)

##################################################################################################

library(mlbench)
data("Ozone")
View(Ozone)
str(Ozone)
o2 = na.omit(Ozone)
# factor 변수를 numeric으로 변환
o2$V1 = as.numeric(o2$V1)
o2$V2 = as.numeric(o2$V2)
o2$V3 = as.numeric(o2$V3)

nn = nrow(o2)
train = o2[1:(nn*0.7), ]
test = o2[-(1:(nn*0.7)), ]

model4 = svm(V4~., train, cost = 1000, gamma = 0.1)

rmse(train$V4-model4$fitted)

pred4 = predict(model4, test)
cbind(test$V4, pred4)
rmse(test$V4-pred4)

#######################################
data01 = read.csv('data/data01.csv', header = T)

data02 = na.omit(data01)
data03 = matrix(as.matrix(data02), nrow = 1, byrow = F)

str(data03)

nn1 = length(data03)
dd1 = array(dim = c((nn1-6), 6))

for(i in 1:(nn1-6)) {
  dd1[i, ] = data03[i:(i+5)]
}

colnames(dd1) = c('V1', 'V2', 'V3', 'V4', 'V5', 'Y')
plot(1:length(data03), data03)

model5 = svm(x = dd1[,1:5], y = dd1[ ,6])
test_ind = sample(1:nrow(dd1), 10, replace = F)
pred6 = predict(model5, newdata = dd1[test_ind,1:5])

cbind(dd1[test_ind, 6], pred6)

#####################################################
## 신경망 (SVM과 비교)

library(nnet) # lineout을 F로 두면 팩터변수로 반환
# 예를 들어 3개의 그룹이 있다면 001, 010, 100으로 인코딩됨, T로 두면 실수로 반환

model6 = nnet(x = dd1[,1:5], y = dd1[ ,6], size = 2, linout = T)

# install.packages('neuralnet')
# install.packages('shiny')


