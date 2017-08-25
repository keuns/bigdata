install.packages

library1 <- c("plyr", "ggplot2", "stringr", "zoo", "corrplot", "RColorBrewer")
unlist(lapply(library1, require, character.only = TRUE ))

## 깨지는 데이터들 UTF-8로 encoding 해서 안깨지게 하기
product = read.csv('data/product.csv', header = T, fileEncoding = 'UTF-8')
code = read.csv('data/code.csv', header = T, fileEncoding = 'UTF-8')
weather = read.csv('data/weather.csv', header = T, fileEncoding = 'UTF-8')

summary(product) # summary 결과, 정상적으로 값이 다 나오면 NA값이 없는 것임
str(product)

summary(weather)
mean(weather$측정값) # NA값이 있어서 에러 뜸

colnames(product) <- c('date','category','item','region','mart','price')

category <- subset(code, code$구분코드설명=="품목코드")
colnames(category) <- c('code', 'exp', 'item', 'name')

total.pig <-product[which(product$item==514),]
head(total.pig, n=10)

region <- subset(code, code$구분코드설명=="지역코드")
colnames(region) <- c('code', 'exp', 'region', 'name')

day.pig <- merge(total.pig, region, by="region", all=T)
# x(total.pig)먼저 정리하고, y(region)정리, region이 주키이므로 제일 먼저 등장
head(day.pig,n=10)

