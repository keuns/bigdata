library(ggplot2)
library(ggmap)
getwd()
subway=read.csv("subway.csv", header = TRUE, fileEncoding = "UTF-8")
str(subway)
head(subway)

class(subway[, "income_date"]) <-"character"
subway[, "income_date"] <- as.Date(subway[, "income_date"], format = "%Y%m%d")
#변수 income_date의 자료형을 character로 변환. 함수 as.Date()을 이용하여 날짜를 표준서식으로 변환
unique(format(subway[, "income_date"], "%Y"))

#subway에서 2014년도에 해당하는 행 인덱스를 저장
idx= format(subway[, "income_date"], "%Y") == "2014"
unique(format(subway[idx, "income_date"], "%m"))
#2014년 자료는 7월까지의 정보만 포함

#2014년 자료를 분석에서 제외하고, 나머지 연도의 자료를 subway2 이름의 데이터프레임으로 저장
subway2 <- subset(subway, subset = format(income_date, "%Y") != "2014")

#자료의 역명이 실제 역이름과 동일한가? 본 데이터는 역명(노선)형식 
sort(unique(subway[ , 'stat_name']))

#역명과 노선번호를 분리
idx= grep("\\(" ,subway2[,"stat_name"])
unique(subway2[idx, "stat_name"])

######꼭 분리를 해야하나?
stat_name=subway2[, "stat_name"]
tmp=sapply(subway2[idx, "stat_name"], strsplit, "\\(", USE.NAMES = FALSE)
stat_renamed <- sapply(tmp, function(x) x[1])
###########

year <- format(subway2[,"income_date"], "%Y") 
month <- format(subway2[,"income_date"], "%m") 
subway2 <- cbind(subway2, year, month)


#지하철 노선별 역이름 및 위치정보(위도, 경도) 자료
subname= read.csv("subway_latlong.csv", header = TRUE, stringsAsFactors = FALSE, skip = 1, fileEncoding = "UTF-8")
str(subname)
head(sort(unique(subname[ , "STATION_NM"])), 10)

#지하철노선별 역명을 Line 이름의 리스트 객체에 저장
Line <- tapply(subname[,"STATION_NM"], subname[,"LINE_NUM"], unique) 
sapply(Line, head)

#노선별 역당 평균 탑승객수 계산
subway3= merge(subway2[, c("stat_name", "income_date", "on_tot", "year", "month")], subname[, c("STATION_NM","LINE_NUM","XPOINT_WGS","YPOINT_WGS")], by.x="stat_name", by.y="STATION_NM")

#2012년 5월 8일 하루동안 탑승한 인원
dat1= subset(subway3, income_date=="2012-05-08", select = c("XPOINT_WGS", "YPOINT_WGS", "on_tot", "stat_name", "LINE_NUM"))
Map_Seoul= get_map(location = c(lat=37.55, lon=126.97), zoom=11, maptype = "roadmap")
MM <- ggmap(Map_Seoul)
MM2= MM+geom_point(aes(x=YPOINT_WGS, y=XPOINT_WGS, size=on_tot, colour=as.factor(LINE_NUM)), data=dat1)
MM2 + scale_size_area(name=c("탑승객수")) + scale_colour_discrete(name=c("노선")) + labs(x="경도", y="위도")
