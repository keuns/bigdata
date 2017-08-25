## 베이즈 정리
# -> 조건부 확률 개념이 들어감
# 사전확률로부터 사후확률을 구하는 개념
# 수치 개념이 아니라 빈도 개념이 적용됨 (있거나 없거나 둘 중 하나)

# tm 패키지 사용
# tm map과 gsub가 같은 역할을 함(ex. 대문자를 소문자로 바꾸기, 특수문자 제거 등)

# DocumentTermMatrix (행에 Document, 열에 Term가 오는 함수), TermDocumentMatrix 함수도 존재
# 이 함수를 통해 매트릭스 만든 후 분석작업 시행

sms_raw <- read.csv("sms_spam.csv", sep = ',', stringsAsFactors = F)

str(sms_raw)

library(stringr)
sms_raw$text = str_to_lower(sms_raw$text) # 전부 소문자로 바뀜
sms_raw$type <- factor(sms_raw$type) # 그룹변수로 바꾸기

str(sms_raw$type)
table(sms_raw$type) # 햄과 스팸 개수 확인

# '말뭉치'가 만들어지고 각 document를 생성하는 과정
install.packages('tm')
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))

print(sms_corpus)
inspect(sms_corpus[1:3])
str(sms_corpus)

corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation) # 특수문자 제거 함수
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

inspect(corpus_clean[1:3])

# 문서-용어 희소 매트릭스 생성
sms_dtm <- DocumentTermMatrix((corpus_clean))
sms_dtm

#훈련과 테스트 데이터셋 생성
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test <- sms_raw[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5559]
corpus_clean[1]

# 스팸 비율 확인
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# 단어 클라우드 시각화
library(wordcloud)

wordcloud(sms_corpus_train, min.freq = 30, random.order = FALSE)

#훈련 데이터를 스팸과 햄으로 구분
spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_test, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

# ind = which(sms_raw$type == 'spam') #스팸인 데이터들의 번호 출력

# # 빈번한 단어에 대한 속성 지시자
# train_spam = corpus_clean[(ind <= 4169)]
# train1_spam = sms_dtm_train[(ind <= 4169), ]
# 
# findFreqTerms(sms_dtm_train, 5)
# findFreqTerms(train1_spam, 5)

# 단어 개수가 5개인 documentmatrix 만들기
sms_train1 <- DocumentTermMatrix(sms_corpus_train, list(dictionary = findFreqTerms(sms_dtm_train, 5)))
sms_test1 <- DocumentTermMatrix(sms_corpus_test, list(dictionary = findFreqTerms(sms_dtm_train, 5))) 


# 개수를 팩터로 변환
# 데이터를 1 or 0로 바꾸기 위함
convert_counts<- function(x) {
  x<-ifelse(x > 0, 1, 0)
#  x<-factor(x,levels=c(0,1), labels=c("NO", "YES")) # -> randomforest를 쓸때는 숫자 0,1로 들어가야 하는데 이 코드 쓰면 '문자형식'으로 들어가기 때문에 쓰면 안됨
} 

# apply() convert_counts()를 사용한 훈련/테스트
sms_train1 <- apply(sms_train1, MARGIN = 2, convert_counts)
sms_test1 <- apply(sms_test1, MARGIN = 2, convert_counts)


## 3단계 : 데이터로 모델 훈련
library(e1071)
start_time = Sys.time()
sms_classifier <- naiveBayes(sms_train1, sms_raw_train$type)
end_time = Sys.time()-start_time

sms_classifier$apriori

## 4단계 : 모델 성능 평가
sms_test_pred <- predict(sms_classifier, newdata = sms_test1)

#########################################################################################
## 위의 예제 randomforest로 해보기
as.integer(as.factor(sms_train1[1,1])) # -> yes/no를 1,0으로 바꾸기

library(randomForest)
rand1 = randomForest(sms_train1, sms_raw_train$type)
str(sms_train)
