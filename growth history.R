#重新整理径级的分级
hsd <- read.csv("HSD.csv", fileEncoding = "GBK")
hsd <- subset(hsd, Branch == "0" & Status2 %in% c("Alive", "P", "Log", "Snag", "Lean"))

# 根据 Latin 分类对数据进行分组
grouped_data <- split(hsd$DBH2, hsd$Latin)

# 对每个分组计算分位数等级，并命名为 1、2、3、4
grouped_cut <- lapply(grouped_data, function(x) {
  cut_levels <- cut(x, breaks = unique(quantile(x, probs = seq(0, 1, 0.25), na.rm = TRUE)), labels = FALSE)
  cut_levels <- as.factor(cut_levels)
  return(cut_levels)
})

# 将结果合并为一个数据框
cut_data <- do.call(rbind, grouped_cut)

# 将结果添加到原始数据框中
hsd$DBH2_group <- unlist(cut_data)
