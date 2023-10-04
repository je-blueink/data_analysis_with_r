# ---- 1장 (3강) ----

# 5

library(datasets)
dat0 <- trees
dat0$Height.grp <- as.factor(ifelse(dat0$Height>75, 1, 0))
dat1 <- dat0[dat0$Volume>=20, ]
summary(dat0)
summary(dat1)

library(dplyr)
dat00 <- trees
summary(dat00)

dat01 <- dat00 %>%
  mutate(Height.grp = as.factor(ifelse(Height>75, 1, 0))) %>%
  filter(Volume>=20)

# dat01 <- dat00 %>%
#  mutate(Height.grp = as.factor(ifelse(Height>75, 1, 0))) %>%
#  rowid_to_column(var = "id") %>%
#  filter(Volume>=20) %>%
#  column_to_rownames(var = "id")

summary(dat01)

# ---- 2장 (4-5강) ----

# 3

# 기하분포의 기댓값과 분산을 알 때 음이항분포 기댓값과 분산 예측

# 주어진 함수
nbinom.dist <-  function(p=0.1, r = 3, n=1000) {
  x <- rnbinom(n, r, p)
  meanx <- mean(x)
  varx <- var(x)
  list(meanx = meanx, varx= varx)
}
nbinom.dist()

# 주어진 조건
p <- 0.1
r <- 3

# 예측한 음이항분포의 기댓값과 분산
ex_mu = r/p
ex_var = r*(1-p)/p^2

list(ex_mu=ex_mu, ex_var=ex_var)
# 1000개 난수 발생하여 예측한 기댓값, 분산과 비교

# ---- 3장 (6-7강) ----

# 6

rr <- 0.4; nn <- 10
t0 <- rr*sqrt(nn-2) / sqrt(1-rr^2)
t0
1-pt(t0, nn-2)
# 유의확률이  0.126036 로 얻어지므로 귀무가설을 기각하지 못함.
# 즉, 양의 상관이 있다고 할 수 없음.

# t통계량 계산
sr <- 0.4
sn <- 10
t0 <- sr*sqrt(sn-2) / sqrt(1-sr^2)

# 기각역과 비교
t1 <- qt(0.95, sn-2)
t0-t1 > 0
list(t0=t0, t1=t1)

# 유의확률 계산
1-pt(t0, sn-2)
