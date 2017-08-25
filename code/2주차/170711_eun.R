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

######################################################################################################
## product파일에서 total.pig (돼지고기 관련 데이터) 추출,
## code파일에서 category (돼지고기가 포함된 식품 품목코드) 추출, region (각 지역에 대한 데이터) 추출

colnames(product) <- c('date','category','item','region','mart','price')

category <- subset(code, code$구분코드설명=="품목코드")
colnames(category) <- c('code', 'exp', 'item', 'name')

total.pig <-product[which(product$item==514),]
head(total.pig, n=30)

region <- subset(code, code$구분코드설명=="지역코드")
colnames(region) <- c('code', 'exp', 'region', 'name')

day.pig <- merge(total.pig, region, by="region", all=T)
# x(total.pig)먼저 정리하고, y(region)정리, region이 주키이므로 제일 먼저 등장
head(day.pig, n=30)
View(day.pig)

# install.packages('dplyr')
library(dplyr)
library(plyr)

total.pig.mean <- dlply(ddply(ddply(day.pig, .(date), summarise, name=name, region=region, price=price),.(date, name), summarise, mean.price=mean(price)), .(name))
str(total.pig.mean)

## 위의 긴 코드를 간단히 작성하는 방법. 결과가 같은 코드임
library(doBy)
a1 = summaryBy(price~date+name, day.pig, FUN = mean) # SQL에서 group by 하는 것처럼 ~(물결표) 오른쪽에 grouping할 변수를 놓음
str(a1)

a2 = splitBy(~name, a1) # 데이터타입 : data.frame을 list구조로 만드는 코드
str(a2)
a2$대구

#################################################################################################
sort(day.pig$date, decreasing = T)
aa1 = arrange(day.pig, name, desc(date))
head(aa1)
#################################################################################################

day.pig <- day.pig [! day.pig$name %in% c("의정부","용인","창원","안동","포항","순천","춘천" ),]

pig.region.daily.mean <- ddply(day.pig, .(name, region, date), summarise, mean.price=mean(price))
head(pig.region.daily.mean, n=30)

pig.region.monthly.mean <- ddply(pig.region.daily.mean, .(name, region, month=str_sub(pig.region.daily.mean$date,1,7)), summarise, mean.price=mean(mean.price))
head(pig.region.monthly.mean, n=30)

pig.region.yearly.mean <- ddply(pig.region.daily.mean, .(name, region, year=str_sub(pig.region.daily.mean$date,1,4)), summarise, mean.price=mean(mean.price))
head(pig.region.yearly.mean, n=30)

# 여기까지 데이터 핸들링 (데이터 가공 완료)
#####################################################################################################

library(zoo)

pig.region.monthly.mean$month <- as.Date(as.yearmon(pig.region.monthly.mean$month, "%Y-%m"))

dd6 = as.yearmon(pig.region.monthly.mean$month, "%Y-%m")
dd7 <- as.Date(format(dd2, "%Y-%m-01"))

# install.packages('ggplot2')
library(ggplot2)

ggplot(pig.region.monthly.mean, aes(x=month, y=mean.price, colour=name, group=name)) + geom_line() + theme_bw() + geom_point(size=6, shape=20, alpha=0.5) + ylab("돼지고기 가격") + xlab("")

############################################################################################################################

temp <- dlply(pig.region.daily.mean, .(name), summarise, mean.price) 
str(temp)
cor(temp) # temp의 구조가 list인데 제일 마지막에 cor함수를 써야하므로 매트릭스 구조로 바꿔줘야함, data.frame도 안됨, only 매트릭스만!

## 위와 결과가 같은 코드???
temp1 <- pig.region.daily.mean %>%
  group_by(name) %>%
  summarise(mean.price)
str(temp1)

################################################################################################################
## product.pdf 30페이지, 일일이 unlistk하는 방법과 같은 코드1
c_data = data.frame()
for (i in 1:length(temp)) {
  if (nrow(temp[[i]]) ==745) {
    a1 = rep(names(temp)[i], nrow(temp[[i]]))
    #a1 = names(temp)[i]
    a2 = temp[[i]]
    c_data1 = data.frame(a2, a1) # 각 지역(a1)의 일평균가격(a2, 745개)을 data.frame으로 출력
    c_data = rbind(c_data, c_data1) # c_data1 변수를 rbind로 묶고 변수 안에 넣어서 data.frame -> list로 만들기 
  }
}
str(c_data1)
str(a1)

names(c_data) = c('price', 'ind')
c_data$ind = factor(c_data$ind)
c_data2 = unstack(c_data)
# list or data.frame 구조로는 cor함수(상관관계 파악)를 사용할 수 없음
# 열 간에 상관관계를 파악하도록 구조를 바꿔야하므로 unstack함
################################################################################################################
## 같은 코드2
temp = unlist(temp[[1]])
names2 = names(temp)[1]
for(i in 2:length(temp)) {
  if(nrow(temp[[i]])==745) {
    names2 = c(names2, names(temp[i]))
    temp = cbind(temp5, temp[[i]])
  }
}

colnames(temp5) = names2
str(temp5)
###########################################################################################################################
# install.packages('corrplot')
library(corrplot)
library(RColorBrewer)
pig.region <- c_data2
cor_pig <- cor(pig.region)
corrplot(cor_pig, method="color", type="upper", order="hclust", addCoef.col = "white", tl.srt=0, tl.col="black", tl.cex=0.7, col=brewer.pal(n=8, name="PuOr"))
##########################################################################################################
## unstack 개념 공부
a1 = as.data.frame(matrix(1:15, nrow = 3))
a2 = stack(a1) # 스택 사용하려면 data.frame 구조로 되어 있어야 함
a3 = unstack(a2)
################################################################################################################
ind3 = c(5,7,8,5,NA,3,NA,7,NA)
length(ind3)
mean(ind3, na.rm = T) # NA값 들어가기 때문에, na.rm = T 넣어야 평균값 계산됨

ind3 = ifelse(is.na(ind3), round(mean(ind3, na.rm = T), 0), ind3)
###############################################################################################################

write.csv(pig.region, "data/pig.region.csv", fileEncoding="UTF-8")
write.csv(pig.region.monthly.mean, "data/pig.region.monthly.mean.csv", fileEncoding="UTF-8")
