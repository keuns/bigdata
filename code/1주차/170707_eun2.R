cars
plot(cars)
# 회귀분석 -> 상관계수가 존재할 때 인과관계를 파악하기 위해 쓰임
model01 = lm(dist ~ speed ,cars) # 영향을 받는 변수 ~ 영향을 주는 변수
summary(model01) # y = a +b*x 에서 H0 : b=0 -> pvalue값으로 판단 -> 결과 귀무가설 기각 (b!=0)
# y = a+b*x+ () 괄호안에 입실론이 들어감
# 입실론에 변수가 들어갈 수 있음, 따라서 변수안에 변수가 들어가는 구조
# 변수 안에 선형, 비선형 모두 들어갈 수 있음

write(model01$fitted.values, file = 'eff1.txt')
write(model01$coefficients, file = 'eff2.txt')
str(model01$coefficients)
abline(model01, col = 'red') # 모형 찾기
pred1 = predict(model01, newdata = data.frame(speed = c(10, 5, 21, 22))) # 예측하기
# 이 다음 작업 : 회귀분석 모형 개발


