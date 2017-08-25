x <- c(100, 50, 10, 17)
company = c("a", "b", "c", "d", "e")
pie(x, labels = company, clockwise = T, init.angle = 90, main = "회사별 매출액")

p <- round((x/sum(x))*100, 1)
company2 <- paste(company, "\n\n", p, "%")
pie(x, labels = company2, clockwise = T, init.angle = 90, main = "회사별 매출액")

# 5가지 색 표현 방법
pie(rep(1,12), labels = seq(1,12), col=rainbow(12))
pie(rep(1,12), labels = seq(1,12), col=heat.colors(12))
pie(rep(1,12), labels = seq(1,12), col=terrain.colors(12))
pie(rep(1,12), labels = seq(1,12), col=topo.colors(12))
pie(rep(1,12), labels = seq(1,12), col=cm.colors(12))

################################################################################################

h<-c(83, 56, 77, 64)
team<-c("a팀", "b팀", "c팀", "d팀")
bp <- barplot(h, names.arg = team, co = rainbow(length(h)), main = "팀별 성적", horiz = F, xlab = "부서", ylab = "성적", ylim = c(0, 100))
text(x = bp, y= h, labels = round(h, 0), pos = 3)

cnt <- table(mtcars$gear)
barplot(cnt, main = "자동차 유통", xlab = "기어의 수")
barplot(cnt, main = "GOOD", col = rainbow(length(cnt)), names.arg = c("3기어", "4기어", "5기어"), horiz = T, xlim = c(0, 20))

#######################################################

h1 <-c(4,12,10,6)
h2<-c(9,5,20,8)
h<-rbind(h1, h2)
team <-c('1팀', '2팀', '3팀', '4팀')
barplot(h, names.arg = team, col = c("darkblue", "red"), main = "팀별 실적", xlab = "팀명", ylab = "실적", beside = T)

cnt<-table(mtcars$vs, mtcars$gear)
barplot(cnt, main = "zz", xlab = "gear의 수", col = c("darkblue", "red"), legend=rownames(cnt), ylim=c(0,20), beside = T)
##############################################################
h<- c(170,171,175,177,179)
w<- c(56,57,58,59,60)

plot(h, w, xlab = "키", ylab = "몸무게", main  = "키와 몸무게", type = "o")
# p는 점, l은 선, b는 둘다, o는 점위의 선, s는 계단형, h는 수직선

par(bg = "transparent")
## pch : 플로팅 캐릭터

p<-c(9,1)
points(p, pch=1, cex=2)

text(65, 150, "글자", col = "red")
abline(h = 100, lty = 3)
par(fg = "blue")
