data <- read.csv('svm.csv', header = T)
data = data[1:20, ]

plot(data, pch = 16) # plot결과 비선형임

# Create a linear regression
model <- lm(Y~X, data)

# Add the fitted line
abline(model)

library(e1071)

model1 <- svm(Y~X, data)
# model2 <- svm(x = data$X, y = data$Y) # model1과 같은 결과

model1$kernel # 커널함수 지정할 수 있음
model1$cost # cost는 margin(c)임
model1$gamma
model1$SV
model1$coefs # 각 weight 값 (y = w*f(x)에서 w), svm의 개수에 비례함

rmse <- function(error) {
  sqrt(mean(error^2))
}

rmse(data$Y-model1$fitted) # Y-Y.hat

points(data$X, model1$fitted, col = "red", pch = '+')

# 커널함수를 linear로 바꿨을 때 결과 보기, 직선형으로 출력됨
# 따라서 비선형일 때는 polynomial를 사용해야 함
model1_1 = svm(Y~X, data = data, kernel = 'linear')
points(data$X, model1_1$fitted, col = "blue", pch = '*')  


# svm을 튜닝하기 -> best parameter 찾기
# 데이터를 10등분해서 1개의 데이터를 제외하고 9개를 가지고 svm 돌림, 그것을 10번 반복
# 총 700개의 svm모형이 만들어짐(10*10*7) 10번반복*(0에서1까지0.1간격이니깐 10번)*(2^2에서 2^9까지 7가지)
tuneResult <- tune(svm, Y ~ X, data = data, ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
pred1 = predict(tuneResult$best.model, newdata = data)

plot(data, pch='+')
points(data$X, model1$fitted, pch = '*', col = 'blue')
points(data$X, pred1, pch = '*', col = 'red')
