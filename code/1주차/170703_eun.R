rm(list = ls())
# 변수 다 지우기
txt = readLines('big.txt', encoding = 'UTF-8')
# 엔터를 기준으로 문장 읽는 함수
str(txt)
#데이터 구조를 확인하는 함수
txt[10]
#데이터 잘 들어갔는 지 확인
list1 = list(sp=1, sp2=c(1,3,5), y=c("A", "B"))
list1[1]
list1$sp2
list1[[2]]

list2 = list(x=list1, y=list1)
# 리스트 안에 리스트 만듦
str(list2)

list2[[1]][[2]]
# 리스트2의 첫번째 리스트안의 두번째 리스트 값 출력
# dataframe 구조는 행과 렬이 같아야 함
# matrix vs dataframe (다른 데이타 타입이 들어가도 되는 지 여부)
# dataframe vs list (변수 안에 변수가 들어가도 되는 지 여부)

txt0=txt[nchar(txt) > 1]
nchar(txt0)
# 글자수가 1 이상인 문장들의 글자수를 세는 함수
#[]괄호 안에 인덱스를 써도 되고 조건을 써도 되는데 이 경우는 조건을 쓰는 경우이고,
#조건에 해당하는 값만 변수에 저장하고 있음
# install.packages('KoNLP', dependencies = T) 필요한 패키지 설치
library(KoNLP)
# 라이브러리 사용

useSejongDic()
#사전 추가

txt1 = gsub("bigdata", "빅데이터", txt0)
# 찾아바꾸기 기능
txt1 = gsub('[A-z]', '', txt1)
# 영어가 나오는 건 다 지우겠다!
txt1 = gsub("[[:digit:]]", "", txt1)
txt1 = gsub("[[:punct:]]", "", txt1)
txt1 = gsub(" ", " ", txt1)


txt_n = extractNoun(txt1);
# 명사 추출하는 함수
str(txt_n)

txt_t = table(unlist(txt_n))
# txt_n이 변수안에 변수가 있는 list 구조이기 때문에
# 하나의 변수에 대한 table을 만들기 위해 unlist를 함

# install.packages('wordcloud')
library(wordcloud)
# 라이브러리 설치 및 호출

wordcloud(names(txt_t), txt_t)
# 워드클라우드 함수 사용해서 비정형데이터 빈도화
# https://cran.r-project.org/ 패키지 정보를 얻을 수 있는 있는 사이트
# NLP : 자연어 처리, TM : 텍스트마이닝
