## 오후수업
TS <- read.csv("example_ts.csv")

ggplot(TS, aes(x=Date, y = Sales)) + geom_line()

str(TS)
## TS$Date 를 factor변수로 변환 후 ggplot
ggplot(TS, aes(x=factor(Date), y = Sales, group = 1)) + geom_line() + geom_point()
ggplot(TS, aes(x=factor(Date), y = Sales, group = 1)) + geom_line() + geom_point() + theme_light()
ggplot(TS, aes(x=factor(Date), y = Sales, group = 1)) + geom_line(color = "orange1", size = 1) + geom_point(color = "orange2", size = 3) + theme_light()
ggplot(TS, aes(x=factor(Date), y = Sales, group = 1)) + geom_line(color = "orange1", size = 1) + geom_point(color = "orange2", size = 3) + xlab("년도") + ylab("매출") + ggtitle("키득")+theme_light()

#####################################################################################################################
require(dplyr)
require(ggplot2)
# install.packages("ggthemes")
require(ggthemes)

DF <- read.csv("example_population_f.csv", fileEncoding = "euc-kr")
str(DF)
DF <- DF[, -1] #의미없는 데이터 제거

## 데이터프레임 객체로 변환
DF <- tbl_df(DF) # dplyr 패키지에 있는 함수를 쓰려면 tbl_df 함수를 써서 변환해줘야 함???????????????????? -> mutate, summarize 함수 쓸수 있는데........
str(DF)

g = group_by(DF, Provinces)
summarise(g, sum(Population))
mutate(DF, EUNS = ifelse (Households > 150000, "굿", "노굿"))

## subset과 비슷한 filter함수
DF2 <- filter(DF, Provinces == "충청북도" | Provinces == "충청남도")

Graph <- ggplot(DF2, aes(x = City, y = Population, fill = Provinces)) + geom_bar(stat = "identity") + theme_wsj()
Graph
## 오름차순으로 정렬 (reorder)
GraphReorder <- ggplot(DF2, aes(x = reorder(City, Population), y = Population, fill = Provinces)) + geom_bar(stat = "identity") + theme_wsj()
GraphReorder

## filter 한번 더!
DF3 <- filter(DF, SexRatio > 1, PersInHou < 2)
Graph <- ggplot(DF3, aes(x = City, y = SexRatio, fill = Provinces)) + geom_bar(stat = "identity") + geom_point(color  = "red")  + theme_wsj()
Graph

##################################################################################
library("reshape2")

DF <- read.csv("example_population_f.csv")
DF <- DF[, -1]

## 새로운 열 생성 (mutate)
DF <- mutate(DF, SexF = ifelse(SexRatio < 1, "여자비율높음", ifelse(SexRatio > 1, "남자비율높음", "남여비율같음")))
str(DF)
DF$SexF <- factor(DF$SexF)
DF$SexF <- ordered(DF$SexF, c("여자비율높음", "남여비율같음", "남자비율높음"))

DF2 <- filter(DF, Provinces == "경기도")
DF2

Graph <- ggplot(DF2, aes(x = City, y = (SexRatio-1), fill = SexF)) + geom_bar(stat = "identity", position = "identity")
Graph

DF4 <- filter(DF, Provinces == "서울특별시")
Graph <- ggplot(DF4, aes(x = City, y = (SexRatio-1), fill = SexF)) + geom_bar(stat = "identity", position = "identity") + theme_wsj()
Graph

####################################################################

DF <- read.csv("example_population_f.csv")
DF <- DF[, -1]
DF <- tbl_df(DF)
DF2 <- mutate(DF, SexF = ifelse(SexRatio < 1, "여자비율높음", ifelse(SexRatio > 1, "남자비율높음", "남여비율같음")))
DF3 <- filter(DF2, Provinces == "경기도")

# City를 SexRatio별로 reorder시킴, xend는 기준점 설정
Graph <- ggplot(DF3, aes(x=(SexRatio-1), y=reorder(City, SexRatio))) + geom_segment(aes(yend = City), xend = 0, colour = "grey50") + geom_point(size = 4, aes(color = SexF)) + theme_minimal()
Graph

#####################################################################
require("dplyr")
require("ggplot2")
require("ggthemes")
require("reshape2")
require("scales")

DF <- read.csv("example_population2.csv")
DF <- tbl_df(DF)
DF
str(DF)
group <- group_by(DF, Time)
group
DF2 <- summarise(group, s0 = sum(age0to4, age5to9), s10 = sum(age10to14, age15to19), s20 = sum(age20to24, age25to29), s30 = sum(age30to34, age35to39), s40 = sum(age40to44, age45to49), s50 = sum(age50to54, age55to59), s60 = sum(age60to64, age65to69), s70 = sum(age70to74, age75to79), s80 = sum(age80to84, age85to89), s90= sum(age90to94, age95to99), s100 = sum(age100to104, age105to109))
head(DF2, 5)

DF3 <- melt(DF2, id.vars = "Time", measure.vars = c("s0","s10","s20","s30","s40","s50","s60","s70","s80","s90","s100"))
colnames(DF3) <- c("Time", "Generation", "Population")
G1 <- ggplot(DF3, aes(x = Time, y = Population, colour = Generation , fill = Generation)) + geom_area(alpha = .6) + theme_wsj() # + scale_y_continuous()
G1

#########################################################################

group <- group_by(iris, Species)
d <- summarise(group, sum(Petal.Length), sum(Petal.Width))

