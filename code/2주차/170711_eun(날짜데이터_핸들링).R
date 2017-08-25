## %>% : 파이프 개념, 앞의 결과를 뒤로 넘겨서 뒤에 있는 문장 처리
Sys.Date()
year(Sys.Date())

# timeDate 설치
# lubridate 설치
library(timeDate)
library(lubridate)

dd1 = day.pig$date[1:10]
str(dd1)  
dd2 = as.Date(dd1) # 데이터타입 변경
str(dd2)

dd3 = gsub('[-]', '.', dd1) # -(빼기)을 찾아서 .(마침표)로 바꾸기, 왜? -> 데이터 핸들링 하기 위함
dd3
dd3 = as.Date(dd3) # 문자열이 표준서식을 따르지 않다고 에러 뜸

## 아래 3가지 코드 모두 같은 결과
## 중요 개념 : 데이터 핸들링할 때 날짜데이터는 데이터타입 Date로 바꿔줘야 함! 

dd4 = ymd(dd3) # gsub를 이용하지 않고 마침표로 바꾸기, 데이터타입 Date로 자동변환
dd5 <- as.Date(gsub('[.]', '-', dd3))
dd5 <- gsub('[.]', '-', dd3) %>% as.Date # 앞에 처리된 것을 뒤로 넘기기 (파이프 개념)

year(dd4)
months(dd4)

