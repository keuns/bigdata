vx = 5
if ( x > 6 ) {
  print ('TRUE')
  print ('hello')
} else {
  print ('FALSE')
  print ('world')
}

x %in% c(1,4,7,5)
# for (i in c(1,3,5)) {} # i 안에 차례대로 1,3,5가 들어간다

sum = 0
for (i in seq(1, 100, 3)) { 
     sum = sum + i
     print(sum)
} # i 안에 1,4,7,10,..... 100 이하까지 들어간다


s = array(dim=c(1,50)) # 1행, 50열의 array를 생성하고 변수 s에 대입하라
str(s)
for(i in 1:50) {
if(iris[i, 1] > 7) {
  s[1, i] = 1
} else {
  s[1, i] = 0
}
}

s1 = ifelse(iris$Sepal.Length > 7, 1, 0)
# ifelse는 자체적으로 matrix 단위로 계산하기 때문에
# for문을 사용할 필요없음

m2=c()
for(i in 1:100) {
ind1 = sample(1:150, 100, replace = T)
m1 = mean(ind1)
m2 = c(m2, m1)
}

hist(m2) # 히스토그램으로 시각화
mean(m2) # 100개의 표본평균들의 평균

mean(1:150) # 표본평균들의 평균은 평균에 근접함!

add <- function(a, b){
  add1 = a+b
  return (add1)
} # 함수 생성

add(5, 3)


add2 <- function(a, b) {
if(is.numeric(a) == T) {
  add=a+b
  return (add)
} else {
  add = paste(a, b, sep = '-')
  return (add)
}
}

add0 = add2("abc", "sss")

5/3 # 나누기
5%%3 # 나머지만 출력
5%/%3 # 몫만 출력

pi
1:(2*pi)
x = seq(0, 2*pi, 0.01)
y = sin(x) + rnorm(length(x))

plot(x, y)
length(y)

data1 = read.table('clipboard', header = T)
# 엑셀파일에서 블록잡은 상태(복사한 상태)에서 데이터를 read하는 방식

data2 = read.csv('data1.csv')
str(data2)
names(data2) = c("x1", "x2") # 복수값을 넣을 수 있는 c를 이용하여 이름 바꾸기

library(xlsx)

data3 = read.xlsx('data1.xlsx', sheetIndex = 1)

# 행, 렬, 면이 있는 이미지데이터 불러오기

str(iris3)
iris3[ , , 1]
iris3[1,1, ]

# jpg 이미지, bmp 이미지, png 이미지 차이점 확인
library(readbitmap)

bmp1 = read.bitmap('img7.bmp')
dim(bmp1) # bmp는 차수가 3임

jpg1 = read.bitmap('img7.jpg')
dim(jpg1) # jpg는 차수가 3임

png1 = read.bitmap('img7.png')
dim(png1) # png는 차수가 4임

## 이미지데이터를 matrix에 저장
jpg1_m = matrix(jpg1, nrow=1, byrow = T)
png1_m = matrix(png1, nrow=1, byrow = T) 

jpg1_m[1:10]

dim(jpg1_m)
dim(png1_m)

list1 = list.dirs('.', full.names = T) # 파일 경로들을 다 보여줘!
str(list1)

# list1에 저장된 각 폴더에서 폴더 안에 있는 jpg 파일명 다 읽어오기
# 현재 폴더에 있는 파일명 불러오려면 'list1[i]' 대신 '.' 쓰기
for(i in 2:length(list1)){
list2 = list.files('list1[i]', full.names = T, pattern = '.jpg', include.dirs = T)
}

