## 이미지데이터를 randomForest로 train 및 test하기

test <- read.csv('https://raw.githubusercontent.com/ozt-ca/tjo.hatenablog.samples/master/r_samples/public_lib/jp/mnist_reproduced/short_prac_test.csv')
train <- read.csv('https://raw.githubusercontent.com/ozt-ca/tjo.hatenablog.samples/master/r_samples/public_lib/jp/mnist_reproduced/short_prac_train.csv')

str(train) # label 데이터형식 int 뜸 -> factor(그룹변수)로 바꿔줘야 함

train1 = train
test1 = test
train1$label = factor(train$label)
test1$label = factor(test$label)

r2 = randomForest(label~., train1)
pred2 = predict(r2, newdata = test1)
t2 = table(test1$label, pred2)
sum(diag(t2))/sum(t2)
###########################################################################################

train2 = train1
test2= test1
train2[,-1] = round(train2[, -1]/255, 0)
test2[,-1] = round(test2[, -1]/255, 0)

start1 = Sys.time()
r3 = randomForest(label~., train2)
interval1 = Sys.time()-start1

pred3 = predict(r3, newdata = test2)
t3 = table(test2$label, pred3)
sum(diag(t3))/sum(t3)
##########################################################################################
data(DNA)
View(DNA)
summary(DNA)

str(DNA)
n = nrow(DNA)
ok = sample(1:n, n*0.7, replace = F)

train3 = DNA[ok, ]
test3 =  DNA[-ok, ]

start2 = Sys.time()
r4 = randomForest(Class~., train3)
interval2 = Sys.time()-start2

pred4 = predict(r4, newdata = test3)
t4 = table(test3$Class, pred4)
sum(diag(t4))/sum(t4)

############################################################################################
## 