library(xlsx)

s1 = read.xlsx('s1.xlsx', sheetIndex = 1, startRow = 2, header = T, encoding = 'UTF-8')

gg1 = read.table('clipboard', header= T)

var.test(gg1$X06시, gg1$X18시)
t.test(x=gg1$X06시, y=gg1$X18시, var.equal = F)
t.test(x=gg1$X06시, y=gg1$X18시, var.equal = F , paired = T)
# 영향을 줄 수 있는 다른 요인들을  제거하기 위해 before, after로 보는 방법을 사용할 수 있음


##
View(survey)
pu = survey$Pulse
ag = survey$Age
eun = survey[survey$Pulse >= 100, ]

var.test(pu, ag)
