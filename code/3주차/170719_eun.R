df1 <- read.csv("kang.csv", header = T)
df1$address <- as.character(df1$address)

# gc1 <- geocode(enc2utf8(df1$address))
cen <- c(mean(df1$longitude), mean(df1$latitude))
map <- get_googlemap(cener = cen, maptype = "roadmap", zoom = 9)
ggmap(map)
ggmap(map) + geom_text(data = df1, aes(x = longitude, y = latitude), size = 3, label = df1$names)

##########################################
## 학교 위치를 point 찍기 + 범례 추가 + 텍스트 추가
u <- read.csv("university.csv", header = T)
kor <- get_map("seoul", zoom = 11, maptype = "watercolor")
gmap <- ggmap(kor, fullpage = TRUE) + geom_point(data = u, aes(x= LON, y = LAT, color = 학교명), size = 3)
gmap + geom_text(data = u, aes(x = LON, y = LAT+0.01, label = 학교명), size = 2.5)

################################################
## 지자체별 세대수 사이즈만큼 원을 보여주는 코드
pop <- read.csv("population.csv", header = T)
region <- pop$지역명
lon <- pop$LON
lat <- pop$LAT
house <- pop$세대수

df <- data.frame(region, lon, lat, house)
map1 <- get_map(enc2utf8("대구"), zoom = 7, maptype = "roadmap") # 센터잡는 코드
map2 <- ggmap(map1, fullpage = T)
map2+geom_point(data = df, aes(x = lon, y = lat, color = house, size = house))

###################################################################
library(ggplot2)
data(iris)
a1 <- iris
g1 <- ggplot(a1, aes(x=Petal.Length, y = Petal.Width)) + geom_point()
