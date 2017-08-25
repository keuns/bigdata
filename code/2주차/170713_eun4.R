## 크롤링!

#rvest, httr

install.packages('rvest') # 통신과 관련된 패키지
install.packages('httr')

library(httr) 
library(rvest)

text1 = GET("http://terms.naver.com/entry.nhn?docId=1691554&cid=42171&categoryId=42183") # 다 끌어오는 코드
text2 = read_html(text1) # 본문영역(html부분)만 가져오는 코드
text3 = html_nodes(text2, "h3")
# 원하는 태그 가져오는 코드
#사이트의 요소검사를 실행해서 html_nodes(text2, "h3")
# 여기서 ,(쉼표) 뒷부분에 태그이름 또는 태그이름.class이름 또는 태그이름.id이름을 적으면
# 그 부분에 대한 태그만을 가져옴

text4 = html_text(text3) # 원하는 부분에 대한 text를 볼 수 있는 코드


###########################################################################################################
## 사이트 검색결과에서 원하는 부분 찾아오기

# 경기도 빅파이 검색결과 2페이지
# https://search.naver.com/search.naver?ie=utf8&where=news&query=%EA%B2%BD%EA%B8%B0%EB%8F%84%20%EB%B9%85%ED%8C%8C%EC%9D%B4&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2015.01.01&de=2017.07.13&docid=&nso=so:r,p:from20150101to20170713,a:all&mynews=0&cluster_rank=24&start=11&refresh_start=0
# 경기도 빅파이 검색결과 3페이지
# https://search.naver.com/search.naver?ie=utf8&where=news&query=%EA%B2%BD%EA%B8%B0%EB%8F%84%20%EB%B9%85%ED%8C%8C%EC%9D%B4&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2015.01.01&de=2017.07.13&docid=&nso=so:r,p:from20150101to20170713,a:all&mynews=0&cluster_rank=41&start=21&refresh_start=0

url = "https://search.naver.com/search.naver?ie=utf8&where=news&query=%EA%B2%BD%EA%B8%B0%EB%8F%84%20%EB%B9%85%ED%8C%8C%EC%9D%B4&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2015.01.01&de=2017.07.13&docid=&nso=so:r,p:from20150101to20170713,a:all&mynews=0&cluster_rank=41&start="
# i로 처리하기 위해 url에 넣을 때 바뀌는 숫자부분까지만 두고  i 뒷부분은 지움

tot = c()
for(i in seq(1, 449, 10)) {
  url1 = paste(url, i, sep = "refresh_start=0") # url뒤에 i 붙이기, sep = "원래 url에서 지웠던 뒷부분"
  
  text1 = GET(url1)
  text2 = read_html(text1)
  text3 = html_nodes(text2, "dd") ## dd 태그는 기사 제목 밑에 '요약된 부분'
  text4 = html_text(text3)
  tot = c(tot, text4)  
}

## https://github.com/forkonlp -> 네이버, 다음 기사 크롤링하기 좋게 코딩한 것 볼 수 있는 사이트
## sns 데이터를 크롤링하려면 facebook 개발자 등록해야함
## Rfacebook 패키지를 이용해서 연동!

## 데이터를 가져왔으면 핸들링해야지! 그전에 텍스트데이터를 수치화하는 방법
## [문서, 단어] 매트릭스를 만들어서 각 문서에서 각 단어가 있는 지 없는지로 수치화
## 있으면 단어변수에 +1카운트
## 또는 단어와 단어 사이의 연관관계를 파악하고 싶을 때 [단어, 단어] 매트릭스로 바꾸는 방법이 있음
