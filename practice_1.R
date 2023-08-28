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