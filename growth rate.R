load("reg_20240304.RData")
####计算growthrate
reg$gr_rate <- (log(reg$DBH2)-log(reg$DBH1))
reg_sc <- cbind(reg_sc,reg$gr_rate)
colnames(reg_sc)[which(names(reg_sc)== "reg$gr_rate")] <- "gr_rate"
reg_sc$history <- as.factor(reg_sc$history)
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

#走不通就不试了，离开前搞一下lm
library(lme4)
model <- lmer(gr_rate ~ qr_AM + (1|history) + (1|Latin), data = reg_sc)
summary(model)
residuals <- resid(model)
qqnorm(residuals)
hist(residuals, breaks = "FD")
shapiro.test(residuals)
glm <- lm(gr_rate~  qr_AM + history, reg_sc)
summary(glm)
glm <- lm(gr_rate ~  em + DBH2, reg_sc)
summary(glm)

###试一下不同history的区别
lm_models <- lapply(levels(reg_sc$history), function(level) {
  subset_data <- subset(reg_sc, history == level)
  lm_model <- lm(gr_rate ~ qr_AM, data = subset_data)
  return(list(level = level, lm_model = lm_model))
})
# 输出每个分类的 lm 模型摘要
for (i in 1:length(lm_models)) {
  cat("History level:", lm_models[[i]]$level, "\n")
  print(summary(lm_models[[i]]$lm_model))
  cat("\n")
}
##试一下sptype
lm_models <- lapply(levels(reg_sc$sptype1), function(level) {
  subset_data <- subset(reg_sc, sptype1 == level)
  lm_model <- lm(gr_rate ~ qr_AM, data = subset_data)
  return(list(level = level, lm_model = lm_model))
})
# 输出每个分类的 lm 模型摘要
for (i in 1:length(lm_models)) {
  cat("sptype1 level:", lm_models[[i]]$level, "\n")
  print(summary(lm_models[[i]]$lm_model))
  cat("\n")
}
#sptype2
reg_sc$sptype2 <- as.factor(reg_sc$sptype2)
lm_models <- lapply(levels(reg_sc$sptype2), function(level) {
  subset_data <- subset(reg_sc, sptype2 == level)
  lm_model <- lm(gr_rate ~ qr_AM, data = subset_data)
  return(list(level = level, lm_model = lm_model))
})
# 输出每个分类的 lm 模型摘要
for (i in 1:length(lm_models)) {
  cat("sptype2 level:", lm_models[[i]]$level, "\n")
  print(summary(lm_models[[i]]$lm_model))
  cat("\n")
}
