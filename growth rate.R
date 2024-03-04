load("reg_20240304.RData")
####计算growthrate
reg$gr_rate <- (log(reg$DBH2)-log(reg$DBH1))
reg_sc <- cbind(reg_sc,reg$gr_rate)
colnames(reg_sc)[which(names(reg_sc)== "reg$gr_rate")] <- "gr_rate"

#scale
scale_to_01 <- function(x) {
  # 使用na.rm参数忽略NA值
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
reg_sc$gr_rate <- scale_to_01(reg_sc$gr_rate)

###回归回归
#hist一下
hist(reg_sc$gr_rate)
#好像不用glmmTMB了，但试试
library(glmmTMB)
reg_sc$gr_rate_0.001 <- ifelse(reg_sc$reg_rate < 0.001, reg_sc$reg_rate + 0.001,
                               ifelse(reg_sc$reg_rate > 0.999, reg_sc$reg_rate - 0.0001,
                                      reg_sc$reg_rate))
glm_10 <- glmmTMB(gr_rate_0.001 ~ minpd_10 + avepd_10 + totpd_10 + SRA + DBH2 + CBD_10 + shannon_div_10 * RDi, reg_sc, family = beta_family)

#走不通就不试了，离开前搞一下lm
library(lme4)
model <- lmer(gr_rate ~ qr_AM + (1|history), data = reg_sc)
summary(model)
glm <- lm(gr_rate~  qr_AM + history, reg_sc)
summary(glm)
glm <- lm(gr_rate ~  em + DBH2, reg_sc)
summary(glm)
