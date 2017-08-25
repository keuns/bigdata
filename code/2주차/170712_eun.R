# kNN 알고리즘 (k 근접이웃 알고리즘)
# 분류가 되어 있지 않은 데이터(Test data)들을 분류된 데이터(training data)들 중 
# 가장 비슷한 속성을 가진 그룹으로 분류해주는 방식
# ex) 토마토는 야채와 과일 중 어느 그룹에 속할까?
# 이런 질문에 속할 때,  어디에 더 가깝나에 따라서 어느 기준을 정해서 분류를 한다.
# 그 방법에는 여러가지가 있는데, 흔히 유클리드안 방법
# -> 학교에서 배운 두점 사이의 거리를 구하는 방법임

# knn -> Y(레이블 또는 군집,집단,그룹)값을 사전에 알고 있을 때 사용

wbcd = read.csv('mlr-ko/chapter 3/wisc_bc_data.csv', header = T, stringsAsFactors = F, sep = ',')
View(wbcd)
str(wbcd) # 엑셀파일 받아오면 데이터프레임 구조

wbcd = wbcd[ , -1] # id 필요없어서 1열 지움
wbcd$diagnosis = factor(wbcd$diagnosis) # y에 들어갈 변수를 factor로 변환해야 함
levels(wbcd$diagnosis) # factor 잘 되었는지 확인

wbcd_n = wbcd
wbcd_n[ , -1] = scale(wbcd_n[, -1]) # factor 변수 제외한 데이터를 표준화함

nn = nrow(wbcd_n)
train = wbcd_n[1:(nn*0.7), ]
test = wbcd_n[((nn*0.7)+1):nn, ]

install.packages('class')
library(class) # knn함수 쓰기 위함

out1 = data.frame()
for (i in 1:15) {
  pred1 = knn(train[ ,-1], test[ ,-1], train[ ,1], k = i)
  # train이 모의고사 개념으로 모의고사를 풀어서 터득한 지식
  # test는 실제 수능 개념으로 터득한 지식을 바탕으로 문제를 푸는 것이라고 보면 
  # 점 하나가 찍혔을 때 (데이터가 추가되었을 때) 가장 가까이 있는 점 i개를 구함
  # 그 i개의 점을 통해 어느 그룹에 속하는 지 예측
  t1 = table(test[ ,1], pred1)
  cor1 = sum(diag(t1))/sum(t1)
  # 정확도 = 대각선 값(M인데M으로 예측, B인데B로 예측) / 전체값
  out2 = cbind(i, cor1)
  out1 = rbind(out1, out2)
}
out3 = which.max(out1[ ,2])
out1[out3, ]  # 15번 한 것 중 5번째(5개 점을 통한 예측)의 정확도가 약 98%로 가장 높음
###########################################################################################################

iris_e = iris
iris_e[ ,-5] = scale(iris_e[ ,-5])
n = nrow(iris_e)
train_e = iris_e[1:(n*0.7), ]
test_e = iris_e[(n*(0.7)+1):n, ]

out_e = data.frame()
for (i in 1:15) {
  pred_e = knn(train[ ,-5], test[ ,-5], train[ ,5], k = i)
  t = table(test[ ,5], pred_e)
  cor_e = sum(diag(t))/sum(t)
  out3 = cbind(i, cor_e)
  out4 = rbind(out4, out3)
}
out5 = which.max(out4[ ,2])
out4[out5, ]
#############################################################################################################
# Y(레이블, 또는 군집,집단,그룹)값이 사전에 없는 상태 -> 계층적 군집분석 사용
# 거리를 측정해서 거리가 작은 것부터 하나씩 묶어나가는 방식

# kmeans
# 3개의 집단이 있고, 새로운 포인트가 들어왔을 때,
# 3개의 난수에 의해 생성한 중심점과의 거리를 구한 뒤 최소값을 구함
# 최소값에 해당하는 집단으로 할당 -> 다 모은 후 중심점(ex.평균) 구해봄
# 첫번째 시행 결과, 난수를 통해 생성한 중심점과 당연히 다를 것임
# 난수를 통해 새로이 중심점 3개 생성 -> 그 중심점을 기준으로 거리, 최소값 구함
# -> 변동 폭이 설정 범위안에 들어갈때까지 또는 중심점이 더이상 움직이지 않을때까지
# 반복하면서 중심점을 이동시킴

##다시 설명

# 그룹(군집)을 형성할 수 있는 기준점들이 하나씩 있음 -> 난수에 의해 발생시킴
# 데이터가 들어왔을 때 각 기준점과의 거리를 구해 -> 가까운 쪽으로 보내
# 먼저 들어가는 순서대로 1,2,3....으로 레이블이 붙여짐
# 각 레이블의 데이터들이 모아지면 모인데이터들의 중심점을 구해봄 
# 설정했던 중심점과 다르면 중심점을 이동시킴
# 다시 난수 발생에 의해 중심점 재설정하고 새로 집단 모은 후, 
# 중심점이 더이상 이동하지 않을 때까지 반복 (iter.max)


a<-c(1,6)
b<-c(2,4)
c<-c(5,7)
d<-c(3,5)
e<-c(5,2)
f<-c(5,1)

c_data <- data.frame(a,b,c,d,e,f)
c_data <- t(c_data) # 행과 열을 트랜스시킴

d = dist(c_data, method = 'euclidean')
h1 = hclust(d, method = 'single')
###############################################################################################

k1 = kmeans(iris[, 1:4], 3, iter.max = 200) # -> 3은 군집의 개수
k1$cluster
k1$withinss # 그룹 내 오차
k1$tot.withinss # 그룹 내 오차들의 합
table(iris$Species, k1$cluster)
clust1 = ifelse(k1$cluster ==2, 3, ifelse(k1$cluster ==3, 4, k1$cluster))

table(iris$Species, clust1)
################################################################################################

k0 = data.frame()
for (i in 1:6) {
  k2 = kmeans(iris[ ,1:4], i, iter.max = 30) # kmeans을 최대 30번 돌리겠다! -> 반복수를 늘릴수록 더 잘 찾아감
  k3 = cbind(i, k2$tot.withinss)
  k0 = rbind(k0, k3)
}

plot(k0, type='b') # line과 point둘다 보이기
k0[which.min(k0[, 2]), ]

# plot 결과가 완만해지기 시작하는 수를 선택하면 됨 -> 그것이 최적의 군집 수

##################################################################################################