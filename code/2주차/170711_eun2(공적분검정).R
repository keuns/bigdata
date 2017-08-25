product <- read.csv("data/product.csv", header=T, fileEncoding="UTF-8")
code <- read.csv("data/code.csv", header=T, fileEncoding="UTF-8")

colnames(product) = c('date','category','item','region','mart','price')
colnames(code) = 

library(plyr)
temp <- ddply(product, .(item, date), summarise, mean.price=mean(price))
head(temp, n=10)


category <- subset(code, code$구분코드설명=="품목코드")
category
# ,(쉼표) 우측 조건에 맞는 데이터들을
#좌측 code 데이터 타입과 동일하도록 category 변수 생성

colnames(category) <- c('code', 'exp', 'item', 'name')
date.item.mean <- merge(temp, category, by="item")
View(date.item.mean)

library(stringr)
month.item.mean <- ddply(date.item.mean, .(name, item, month=str_sub(date,1,7)),
                         summarise, mean.price=mean(mean.price))
head(month.item.mean, n=10)

temp <- dlply(date.item.mean, .(name), summarise, mean.price) #data.frame -> list구조로 변환
daily.product <- data.frame(쌀=unlist(temp$쌀), 배추=unlist(temp$배추), 상추=unlist(temp$상추),
                             호박=unlist(temp$호박), 양파=unlist(temp$양파), 파프리카=unlist(temp$파프리카),
                             참깨=unlist(temp$참깨), 사과=unlist(temp$사과), 돼지고기=unlist(temp$돼지고기),
                             닭고기=unlist(temp$닭고기)) # 데이터 개수가 동일한 품목들만 뽑음
head(daily.product, n=10)

temp <- dlply(month.item.mean, .(name), summarise, mean.price)
monthly.product <- data.frame(쌀=unlist(temp$쌀), 배추=unlist(temp$배추), 상추=unlist(temp$상추),
                              호박=unlist(temp$호박), 양파=unlist(temp$양파), 파프리카=unlist(temp$파프리카),
                              참깨=unlist(temp$참깨), 사과=unlist(temp$사과), 돼지고기=unlist(temp$돼지고기),
                              닭고기=unlist(temp$닭고기))

## 공적분 검정
# aXt1+bXt2∼0가 성립하면 두 시계열이 공적분관계
# 두 상수 a,b의 부호가 '같은' 경우 (음의 공적분)에는 두 시계열은 서로 다른 방향으로 움직임을 의미
# '다른' 경우 (양의 공적분)에는 서로 같은 방향으로 움직임을 의미

# install.packages('urca')
library(urca)

for (i in 1:9){ 
  for (j in 1:9){ 
    if ((i+j) < 11){ 
      jc <- ca.jo(data.frame(daily.product[,i], daily.product[,i+j]), type="trace", K=2, ecdet="const") 
      if (jc@teststat[1] > jc@cval[1]) { 
        if(jc@V[1,1]*jc@V[2,1]>0){ 
          cat( colnames(monthly.product)[i],"와" , colnames(monthly.product)[i+j], ": 음의 공적분 관계가 있다.", "\n") } 
        else { 
          cat( colnames(monthly.product)[i],"와" , colnames(monthly.product)[i+j], ": 양의 공적분 관계가 있다.","\n") }
      }}}}

cor(daily.product)
library(corrplot)
corrplot(cor(daily.product))

###############################################################################
## 연습

a = data.frame(m = c(1,2,3), n = c(5,6,7))
str(a)

b = data.frame(m = c(1, 2, 4), p = c('d', 'e', 'f'))
merge(a,b)
