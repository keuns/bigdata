setwd('C:\r_exam') # 워킹디렉토리(경로) 설정
rm(list=ls()) # 이전에 사용했던 변수값 지우기

## 라이브러리 호출
library(KoNLP)
library(wordcloud)
library(stringi)
library(stringr)

txt = readLines('big.txt', encoding = 'UTF-8') # 데이터 읽어오기
length(txt)
# length(iris) # 열 숫자 확인
# ncol(iris) # 열 숫자 확인, length 함수와 같은 기능
nrow(iris) # 행 숫자 확인

## 사전(딕셔너리) 작업 : 어떤 사전을 쓸 것인지!
## 패키지 KoNLP에 사전과 관련된 함수들이 있음
## 신조어 등록
## 분야별 전문용어 등록
buildDictionary(user_dic = data.frame(c("빅데이터", "ncn")), replace_usr_dic = F)
mergeUserDic(data.frame(c("빅데이터", "ncn")))

## 텍스트 핸들링 : gsub, grep, str_length(stringi, stringr 패키지에 있음)
gsub
grep
length

txt0 = str_to_lower(txt) # 대->소문자로 바꾸기
## 유사 단어를 한 단어로 통일
txt1 = gsub("빅데이타", "빅데이터", txt0)
txt1 = gsub("bigdata", "빅데이터", txt1)
txt1 = gsub("big data", "빅데이터", txt1)
# 불필요한 단어를 제거(특수문자 등)
txt1 = gsub("[[:digit:]]", "", txt1)
txt1 = gsub("[A-z]", "", txt1)
txt1 = gsub("[[:punct:]]", "", txt1)
# txt1 = gsub("[((a(\\d)+)+|())|()]")
txt1 = gsub("  ", " ", txt) # 스페이스 제거

txt2 = txt1[str_length(txt1) > 1] # 단어 수 확인
# nchar(txt) > 1 # 단어 수 확인, str_length 함수와 같은 기능
# iris[iris$Sepal.Length >= 7, 1:2] # Sepal 길이가 7이상인 데이터에서 1~2열의 데이터를 찾아라
# 괄호 안에 인덱스가 아니라 조건이 들어갈 수도 있음!

# ifelse

txt_e = extractNoun(txt2) # 명사 추출, output -> list

txt_t = table(unlist(txt_e))

txt_s = sort(txt_t, decreasing = T)
txt_s1 = txt_s[str_length(names(txt_s)) > 1]
 
txt_h = head(txt_s1, 5)

## 시각화 작업
barplot(txt_h)

pal=brewer.pal(7, "Set1")
wordcloud(names(txt_s1), txt_s1, scale=c(3,0.2), min.freq = 1, random.order = F, rot.per = 0.2, col=pal)
