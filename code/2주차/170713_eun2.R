## 오후 수업
## prune(가지치기 개념) -> 오분류한 것들을 가지치면서 오분류율을 감소시키는 개념
install.packages('tree')

library(MASS)
library(tree)

ir.tr = tree(Class~., train1) # 기존 모형 만들기 (ir.tr 검색하면 각 '룰'이 나옴)
plot(ir.tr)
text(ir.tr, all = T) # 트리와 각 text를 보여주는 코드, 트리가 나오고 각 룰들이 나옴

ir.tr1 = snip.tree(ir.tr) # 기존 모형에 대한 가지치기 -> 여기서는 결과가 같음 -> 실행 오래걸림...ㅠㅠ

ir.tr1 = plot(prune.misclass(ir.tr, best = 14))
plot(ir.tr1)
text(ir.tr1, all = T)

##############################################################################
## 랜덤 포레스트
# 데이터의 일부를 추출하여 decision tree를 만드는 작업을 반복하고
# 이렇게 만들어진 '다수'의 decision tree들의 투표(voting)로 최종 결과를 출력
install.packages('randomForest')
library(randomForest)

r1 = randomForest(Class~., train1)
r1$predicted
t1 = table(train1$Class, r1$predicted)
sum(diag(t1))/sum(t1)
pred4 = predict(r1, newdata = test1)
t2 = table(test1$Class, pred4)
sum(diag(t2))/sum(t2) 
# 랜덤 포레스트 방법 -> 성능 좋은 것으로 알려져있음
##############################################################################