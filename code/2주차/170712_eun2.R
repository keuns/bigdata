## 오후수업

# 결측치, 극단치나 이상치가 발생하고 그 수가 많을 때 이것들을 어떻게 처리해야 할까?
# NA값을 없애지 않도록 코딩하면 됨, 예를 들어 여자일 경우, 1 아니면 0이라고 하는 ifelse문을 추가하면
# 여자가 아닌 데이터들은 0이 되므로 NA도 카운팅 됨

# ave함수는 aggregate와 같은 기능, ave는 해당되는 데이터 개수만큼 결과가 나옴, aggragate는 그룹병 통계량이 나옴

rm(list = ls())
teens = read.csv('mlr-ko/chapter 9/snsdata.csv')
str(teens)

table(teens$gender, useNA = "ifany")
# 결측치 유무 확인, 왜? 전체데이터 양 대비 NA 양의 비율이 높을 경우, NA값을 사용하기 위함
summary(teens$age)

teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                    teens$age, NA)
# 오타 가능성 때문에 13~20세 데이터가 아니면 NA로 넣기 (NA는 대문자로 쓰기)

teens$female <- ifelse(teens$gender == 'F', 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm = T))

teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)
table(teens$age, useNA = "ifany")
length(which(teens$age == NA))
interests <- teens[ ,5:40]
interests_z <- scale(interests)

teen_clusters <- kmeans(interests_z, 5)
table(teen_clusters$cluster)

teen_clusters$centers
