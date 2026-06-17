rm(list = ls())
# 加载包
library(dplyr)
library(ggplot2)
library(scales)   # 方便把 y 轴显示为百分数
# 设置工作目录
setwd('G:/返修/KXF2025040303')
if (!dir.exists("./08_Boruta分析")) {
  dir.create("./08_Boruta分析")
}
setwd("./08_Boruta分析")

# 取消科学计数法
options(scipen = 100000000)

# 读取数据 -----------------
rt <- read.csv('G:/返修/KXF2025040303/优化//06_亚组分析/modeldata.csv')
colnames(rt)
# 检查数据
table(rt$OP )
summary(rt$MDS)
# 加载必要的包
library(caret)
set.seed(123)  # 确保结果可重复
repeat {
  set.seed(sample(1:10000, 1))
  trainIndex <- createDataPartition(rt$OP, p = 0.7, list = FALSE)
  train_data <- rt[trainIndex, ]
  test_data  <- rt[-trainIndex, ]
  
  class_dist <- table(test_data$OP)
  if (length(class_dist) == 2 && all(class_dist > 10)) break
}
table(train_data$OP)
table(test_data$OP)

write.csv(train_data, 'train_data.csv')
write.csv(test_data, 'test_data.csv')


library(Boruta)
library(mlbench)
library(caret)
library(randomForest)
library(survey)
rt <- train_data

# 提取目标变量和自变量到新数据集
features <- c( 'MDS','Age_group', 'Gender', 'Race', 'PIR', 
               'Marital_status', 'BMI', 'Stroke', 'Hypertension', 
               'Diabetes', 'TG', 'TC', 'HbA1C', 'MAGN', 'CALC', 'PHOS')

# 确保结局是数值型

table(rt$OP,useNA = 'alway')
# 移除缺失值
rt_clean <- na.omit(rt[, c("OP", features)])
rt_clean$OP <- as.numeric(rt_clean$OP)
table(rt_clean$OP,useNA = 'alway')

str(rt_clean)

# # 将分类变量转换为因子
# factor_cols <- c("Gender", "Race", "Marriage", "smoking",
#                  "Hypertension", "Diabetes", "Coronary_Heart_Disease", "Heart_Attack")
# rt_clean[factor_cols] <- lapply(rt_clean[factor_cols], as.factor)

# 运行Boruta算法
set.seed(123)  # 设置随机种子保证结果可重复
boruta_result <- Boruta(OP ~ ., 
                        data = rt_clean, 
                        doTrace = 2,  # 显示详细过程
                        maxRuns = 100, # 最大运行次数
                        getImp = getImpRfZ) # 使用随机森林重要性

# 查看结果摘要
print(boruta_result)

# 获取确认的重要特征
confirmed_attributes <- getSelectedAttributes(boruta_result, withTentative = FALSE)

# 获取所有候选的特征，包括暂时的（Tentative）
all_selected_attributes <- getSelectedAttributes(boruta_result, withTentative = TRUE)

# 打印输出已确认的重要特征
print("Confirmed attributes:")
print(confirmed_attributes)

# [1] "MDS"            "Age_group"      "Gender"         "Race"           "Marital_status" "BMI"           
# [7] "Stroke"         "Hypertension"   "Diabetes"       "TG"             "TC"             "HbA1C"         
# [13] "MAGN"           "CALC"           "PHOS"   

# 打印输出所有选中的特征（包括暂时的）
print("All selected attributes (including tentative):")
print(all_selected_attributes)

# 处理暂时性特征
boruta_fixed <- TentativeRoughFix(boruta_result)
final_attributes <- getSelectedAttributes(boruta_fixed, withTentative = TRUE)

# 打印最终选定的特征
print("Final selected attributes after tentative rough fix:")
print(final_attributes)
# [1] "MDS"            "Age_group"      "Gender"         "Race"           "Marital_status" "BMI"           
# [7] "Stroke"         "Hypertension"   "Diabetes"       "TG"             "TC"             "HbA1C"         
# [13] "MAGN"           "CALC"           "PHOS" 
print(boruta_fixed)


pdf("boruta_plot_raw.pdf", width = 10, height = 6)

# 设置图形边距：下边距增加到8（默认约5），左/上/右边距保持不变
par(mar = c(8, 4, 2, 2))  # c(bottom, left, top, right)

# 绘制Boruta图（关闭默认X轴）
plot(boruta_result, 
     xlab = "", 
     xaxt = "n", 
     main = "Boruta Feature Importance")

# 准备自定义X轴标签
lz <- lapply(1:ncol(boruta_result$ImpHistory), function(i) {
  boruta_result$ImpHistory[is.finite(boruta_result$ImpHistory[,i]), i]
})
names(lz) <- colnames(boruta_result$ImpHistory)
Labels <- sort(sapply(lz, median))  # 按中位数排序

# 添加X轴标签（45度倾斜，调整位置）
axis(side = 1, 
     las = 2,          # 标签垂直于X轴
     at = 1:ncol(boruta_result$ImpHistory), 
     labels = names(Labels), 
     cex.axis = 0.7,   # 缩小字体
     padj = 0.5,       # 标签与轴线距离
     hadj = 1)         # 标签水平对齐方式

# # 可选：添加颜色图例
# legend("topright", 
#        legend = c("重要", "暂定", "不重要", "阴影"),
#        fill = c("green", "yellow", "red", "blue"),
#        cex = 0.8)

dev.off()

