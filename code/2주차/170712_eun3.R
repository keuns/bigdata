pig.region <- read.csv("data/pig.region.csv", header=T, fileEncoding="UTF-8")[,-1]
head(pig.region, n=10)

pig.region.monthly.mean <- read.csv("data/pig.region.monthly.mean.csv", header=T, fileEncoding="UTF-8")[,-1]
head(pig.region.monthly.mean, n=10)

date.item.mean <- read.csv("data/date.item.mean.csv", header=T, fileEncoding="UTF-8")[,-1]
head(date.item.mean, n=10)

month.item.mean <- read.csv("data/month.item.mean.csv", header=T, fileEncoding="UTF-8")[,-1]
head(month.item.mean)

library(plyr)
temp <- dlply(date.item.mean, .(name), summarise, mean.price)
str(temp)
farm.product <- data.frame(쌀=unlist(temp$쌀), 배추=unlist(temp$배추), 상추=unlist(temp$상추), 호박=unlist(temp$호박), 양파=unlist(temp$양파), 파프리카=unlist(temp$파프리카), 참깨=unlist(temp$참깨), 사과=unlist(temp$사과))
# 일별 농산물 평균 소매가격 자료와 지역별 돼지고기 평균 소매가격 자료를 이용하여 '군집분석' 시행하기 위해
# 일별 식품 평균 소매가격 중에 농산물 데이터만 추출
# 16줄 코드는 list 구조 -> 매트릭스 구조로 바꾼 것 -> cor함수 쓸 수 있음
# cor(temp) 실행안됨 -> 왜? -> list구조라서
cor(farm.product)
head(farm.product, n=10)

install.packages('TSclust')
library(TSclust)

plot(hclust(diss(farm.product,"COR")), axes = F, ann = F)
# diss(farm.product,"COR") 코드를 통해 상관계수 구하고 plot.hclust를 이용하여
# 연관성이 높은 농산물끼리 시각화, 여기서는 상관계수가 낮은 것부터 묶음(plot의 종류마다 묶는 순서가 다름)
# methods(plot)를 이용하여 plot의 종류를 볼 수 있음, 여기서는 plot.hclust 사용

# 시계열 -> 시간에 따른 변화를 보여줌
# plot.hclust 결과, 상추와 호박이 가장 먼저 묶였으므로, 두 농산물에 대한 시계열을 그려보기
month.item.mean$month <- as.Date(as.yearmon(month.item.mean$month, "%Y-%m"))
ggplot(month.item.mean[month.item.mean$name %in% c("상추", "호박"),], aes(x=month, y=mean.price, colour=name, group=name)) + geom_line() + theme_bw() + geom_point(size=6, shape=20, alpha=0.5) + ylab("가격") + xlab("")

