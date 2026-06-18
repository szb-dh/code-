rm(list = ls())
setwd('G:/返修/KXF2025040303')
if (! dir.exists("./02_暴露因素和结局相关性分析")){
  dir.create("./02_暴露因素和结局相关性分析")
}
setwd("./02_暴露因素和结局相关性分析")


# 指定文件路径并读取
nhance_data <- read.csv(file = '../01_clean/nhanes_mat_choose.csv')

# 检查数据结构和前几行
str(nhance_data)  # 查看数据结构
head(nhance_data) # 显示前6行数据
colnames(nhance_data)

expoVar <- c("MDS",'MDS1')  # 暴露变量（自变量）"HPP",
var <- c("SEQN","SDMVPSU","SDMVSTRA","WTMEC11YR",
         'Age','Gender',"Race",'BMI','Marital_status','PIR',
         'Stroke','Hypertension','Diabetes','TG','TC','HbA1C',
         'MAGN','CALC','PHOS',"MDS",'MDS1',"OP")

table(nhance_data$MDS1)
table(nhance_data$MDS)
# 提取指定变量到新数据集
rt <- nhance_data[, var]




colnames(rt)

# 检查新数据集
str(rt)
head(rt)

# 加载必要的包
library(dplyr)

# 1. 检查每个变量的缺失值数量和比例
missing_summary <- sapply(rt[, var], function(x) {
  c(
    "缺失数量" = sum(is.na(x)),
    "缺失比例" = mean(is.na(x)) * 100
  )
})

# 转置为更易读的格式
missing_summary <- as.data.frame(t(missing_summary))

# 按缺失比例降序排列
missing_summary <- missing_summary[order(-missing_summary$缺失比例), ]

# 查看结果
print(missing_summary)

library(survey)
str(rt)

table(rt$OP, useNA = "always")
table(rt$MDS, useNA = "always")

rt <- rt %>%
  mutate(Age_group = ifelse(Age < 65, "< 65", 
                            ifelse(Age >= 65, ">= 65", NA)))


table(rt$Age_group, useNA = "always")
table(rt$OP, useNA = "always")

#获取所有变量
expoVar <- c("MDS","MDS1")  # 暴露变量（自变量）"HPP",
outcomeVar <- "OP"  # 结局变量（因变量）
allVars=setdiff(c(expoVar, colnames(rt)),c("WTMEC11YR","SDMVPSU","SDMVSTRA", outcomeVar))



# 明确定义bioFamily
bioFamily <- "binomial"

# 增强版bioOR函数
bioOR <- function(summData) {
  orData <- as.data.frame(summData)[-1, , drop = FALSE]
  
  # 兼容不同列名
  beta_col <- grep("Estimate|Coefficient", colnames(orData))
  se_col <- grep("Std. Error|SE", colnames(orData))
  
  if (bioFamily == "binomial" && length(beta_col) > 0 && length(se_col) > 0) {
    beta <- orData[, beta_col]
    se <- orData[, se_col]
    
    orData$OR <- exp(beta)
    orData$OR_lci95 <- exp(beta - 1.96 * se)
    orData$OR_uci95 <- exp(beta + 1.96 * se)
  } else {
    warning("OR calculation skipped. Check: 1) bioFamily='binomial', 2) Column names")
  }
  return(orData)
}

library(dplyr)
table(rt$OP, useNA = "always")
rt <- rt %>%
  mutate(OP = ifelse(OP == "Yes", 1,
                     ifelse(OP == "No", 0, NA)))
table(rt$OP, useNA = "always")

table(rt$MDS, useNA = "always")
rt$MDS <- factor(rt$MDS,levels = c('Low','Medium','High'))
# rt$BMI_category <- cut(rt$BMI,
#                        breaks = c(-Inf, 18.5, 25, 30, Inf),
#                        labels = c("Underweight", "Normal", "Overweight", "Obese"),
#                        right = FALSE,  # 区间左闭右开：[ , )
#                        ordered_result = TRUE)
# 
# # 查看结果
# table(rt$BMI_category, useNA = "always")




#对数据进行加权处理
weightData=svydesign(id=~SDMVPSU, 
                     strata=~SDMVSTRA, 
                     weights=~WTMEC11YR, 
                     nest=TRUE, 
                     data=rt, 
                     survey.lonely.psu="adjust")

colnames(weightData)
str(weightData)


# 模型1 -------------------
setwd("G:/返修/KXF2025040303/02_暴露因素和结局相关性分析")
if (! dir.exists("./01_模型1")){
  dir.create("./01_模型1")
}
setwd("./01_模型1")

# 循环每个暴露变量跑模型并保存结果
for (var in expoVar) {
  formula <- as.formula(paste("OP ~", var))  # 构造公式
  model <- svyglm(formula, design = weightData, family = bioFamily)  # 拟合模型
  summ <- summary(model)  # 获取摘要
  or_result <- bioOR(summData = summ$coefficients)  # 计算 OR 值
  filename <- paste0("model1_", var, ".csv")  # 设置输出文件名
  print(or_result)
  write.csv(or_result, file = filename, row.names = TRUE)  # 保存结果
}


# 模型2 ------------------------------------
setwd("G:/返修/KXF2025040303/02_暴露因素和结局相关性分析")
if (! dir.exists("./02_模型2")){
  dir.create("./02_模型2")
}
setwd("./02_模型2")

# var <- c("SEQN","SDMVPSU","SDMVSTRA","WTMEC11YR",
#          'Age','Gender',"Race",'BMI','Marital_status','PIR',
#          'Stroke','Hypertension','Diabetes','TG','TC','HbA1C',
#          'MAGN','CALC','PHOS',"MDS",'MDS1',"OP")


# 循环每个暴露变量跑模型并保存结果
for (var in expoVar) {
  formula <- as.formula(paste("OP ~", var,'+ Age_group + Gender + Race + PIR + Marital_status + BMI'))  # 构造公式
  model <- svyglm(formula, design = weightData, family = bioFamily)  # 拟合模型
  summ <- summary(model)  # 获取摘要
  or_result <- bioOR(summData = summ$coefficients)  # 计算 OR 值
  filename <- paste0("model2_", var, ".csv")  # 设置输出文件名
  # 只保留变量本身的 OR 行（根据变量名前缀匹配）
  or_result_filtered <- or_result[grepl(paste0("^", var), rownames(or_result)), , drop = FALSE]
  print(or_result_filtered)
  write.csv(or_result, file = filename, row.names = TRUE)  # 保存结果
}



# 模型3-------------
setwd("G:/返修/KXF2025040303/02_暴露因素和结局相关性分析")
if (! dir.exists("./03_模型3")){
  dir.create("./03_模型3")
}
setwd("./03_模型3")

# 循环每个暴露变量跑模型并保存结果
for (var in expoVar) {
  # 使用 reformulate 安全构造公式
  formula <- reformulate(
    termlabels = c(var, 'Age_group', 'Gender', 'Race', 'PIR', 
                   'Marital_status', 'BMI', 'Stroke', 'Hypertension', 
                   'Diabetes', 'TG', 'TC', 'HbA1C', 'MAGN', 'CALC', 'PHOS'),
    response = "OP"
  )
  
  tryCatch({
    model <- svyglm(formula, design = weightData, family = bioFamily)  # 拟合模型
    summ <- summary(model)  # 获取摘要
    or_result <- bioOR(summData = summ$coefficients)  # 计算 OR 值
    filename <- paste0("model3_", var, ".csv")  # 设置输出文件名
    
    # 只保留变量本身的 OR 行（使用更安全的匹配方式）
    var_pattern <- paste0("^", var)
    or_result_filtered <- or_result[grepl(var_pattern, rownames(or_result)), , drop = FALSE]
    print(or_result_filtered)
    write.csv(or_result, file = filename, row.names = TRUE)  # 保存结果
    
  }, error = function(e) {
    message("处理变量 ", var, " 时出错: ", e$message)
  })
}




# VIF --------------
setwd("G:/返修/KXF2025040303/")
if (! dir.exists("./03_VIF")){
  dir.create("./03_VIF")
}
setwd("./03_VIF")
# 加载必要的包
library(car)  # 用于VIF计算
library(dplyr)

# 提取模型3的所有变量（包括对数转换后的暴露变量和协变量）
model3_vars <- c('MDS1','Age_group', 'Gender', 'Race', 'PIR', 
                 'Marital_status', 'BMI', 'Stroke', 'Hypertension', 
                 'Diabetes', 'TG', 'TC', 'HbA1C', 'MAGN', 'CALC', 'PHOS'
)

# 检查数据是否存在缺失值（VIF计算需要完整数据）
complete_data <- na.omit(rt[, c("OP", model3_vars)])

# 构建线性模型（仅用于VIF检验）
vif_model <- lm(
  paste("OP ~", paste(model3_vars, collapse = " + ")),
  data = complete_data
)

# 计算VIF
vif_results <- vif(vif_model)

# 输出VIF结果
print(vif_results)

# 保存VIF结果
write.csv(vif_results, "VIF_results_model3.csv")

# 检查高VIF变量（VIF > 5 或 10 表示强共线性）
high_vif <- vif_results[vif_results > 5]
if (length(high_vif) > 0) {
  cat("以下变量可能存在强共线性（VIF > 5）：\n")
  print(high_vif)
} else {
  cat("所有变量的VIF均 <= 5，共线性问题较小。\n")
}




# ROC --------------------------
# 创建输出文件夹
setwd("G:/返修/KXF2025040303/")
if (!dir.exists("./04_ROC曲线")) {
  dir.create("./04_ROC曲线")
}
setwd("./04_ROC曲线")
library(pROC)
library(ggplot2)

# 创建模型3的协变量字符串
covariates_model3 <- "MDS1"



# 循环每个暴露变量并绘制ROC曲线
roc_results <- list()

for (var in expoVar) {
  
  # 构建完整公式
  formula_str <- paste("OP ~", var)
  formula <- as.formula(formula_str)
  
  # 注意：svyglm 不能直接用于 ROC，因此我们临时使用 glm
  glm_model <- glm(formula, data = rt, family = binomial)
  
  # 预测概率
  pred_probs <- predict(glm_model, type = "response")
  
  # 真实标签
  true_labels <- rt$OP
  
  # 计算 ROC
  roc_obj <- roc(true_labels, pred_probs)
  
  # 存储 AUC
  auc_val <- auc(roc_obj)
  roc_results[[var]] <- list(roc = roc_obj, auc = auc_val)
  
  # 绘图并保存
  pdf_filename <- paste0("ROC_", var, ".pdf")
  pdf(pdf_filename, width = 6, height = 6)
  plot(roc_obj, 
       main = paste("ROC Curve -", var, "(AUC =", round(auc_val, 3), ")"),
       xlab = "1 - Specificity (False Positive Rate)",  # 明确标注x轴
       ylab = "Sensitivity (True Positive Rate)",
       col = "blue", 
       lwd = 2,
       legacy.axes = TRUE)  # 关键参数：强制x轴为1-特异性  abline(a=0, b=1, lty=2, col="gray")
  dev.off()
}

# AUC汇总表
auc_summary <- data.frame(
  Variable = names(roc_results),
  AUC = sapply(roc_results, function(x) x$auc)
)

# 保存
write.csv(auc_summary, "MDS_ROC_AUC_summary.csv", row.names = FALSE)
print(auc_summary)


library(pROC)

# 初始化 plot
pdf("All_ROC_Curves.pdf", width = 7, height = 7)

# 用于控制颜色
colors <- rainbow(length(expoVar))
legend_text <- c()

for (i in seq_along(expoVar)) {
  var <- expoVar[i]
  
  # 构建模型
  formula_str <- paste("OP ~", var)
  glm_model <- glm(as.formula(formula_str), data = rt, family = binomial)
  pred_probs <- predict(glm_model, type = "response")
  true_labels <- rt$OP
  
  roc_obj <- roc(true_labels, pred_probs)
  auc_val <- auc(roc_obj)
  
  roc_results[[var]] <- list(roc = roc_obj, auc = auc_val)
  
  # 绘图（第一条曲线初始化，之后叠加）
  if (i == 1) {
    plot(roc_obj, col = colors[i], lwd = 2,
         main = "ROC Curves for Different Exposures",
         xlab = "1 - Specificity (False Positive Rate)",  # 明确标注x轴
         ylab = "Sensitivity (True Positive Rate)", legacy.axes = TRUE)
  } else {
    plot(roc_obj, xlab = "1 - Specificity (False Positive Rate)",  # 明确标注x轴
         ylab = "Sensitivity (True Positive Rate)",
         col = colors[i], lwd = 2, add = TRUE, legacy.axes = TRUE)
  }
  
  legend_text[i] <- paste0(var, " (AUC = ", round(auc_val, 3), ")")
}

abline(a=0, b=1, lty=2, col="gray")
legend("bottomright", legend = legend_text, col = colors, lwd = 2)

dev.off()


# RCS --------------
setwd("G:/返修/KXF2025040303/")

if (! dir.exists("./05_RCS")){
  dir.create("./05_RCS")
}
setwd("./05_RCS")

#library(smoothHR)
library(survival)
library(Hmisc)
library(ggrcs)
library(rms)
library(ggplot2)
library(scales)
library(cowplot)

#对数据进行加权处理
weightData=svydesign(id=~SDMVPSU, 
                     strata=~SDMVSTRA, 
                     weights=~WTMEC11YR, 
                     nest=TRUE, 
                     data=rt, 
                     survey.lonely.psu="adjust")

#在数据中添加权重变量
ori.weight <- 1/(weightData$prob)
rt$weights <- ori.weight/mean(ori.weight)

#对数据进行打包
dd=rms::datadist(rt)
options(datadist = "dd")
expoVar <- c('MDS1')
for (var in expoVar) {
  # 确保变量为数值型
  rt[[var]] <- as.numeric(rt[[var]])
  
  formula_obj <- as.formula(paste0("OP ~ rcs(", var, ", 3) + Age_group + Gender + Race + PIR + Marital_status + 
    BMI + Stroke + Hypertension + Diabetes + TG + TC + HbA1C + 
    MAGN + CALC + PHOS"))
  
  fit <- rms::Glm(formula_obj, data = rt, weights = weights, family = binomial)
  
  pred <- rms::Predict(fit, name = var, fun = exp, type = "predictions", ref.zero = TRUE)
  # 提取P值
  anova_fit <- anova(fit)
  p_nonlin <- anova_fit[" Nonlinear", "P"]
  p_nonlin <- ifelse(p_nonlin < 0.001, "<0.001", sprintf("%.03f", p_nonlin))
  p_overall <- anova_fit[which(row.names(anova_fit) == " Nonlinear") - 1, "P"]
  p_overall <- ifelse(p_overall < 0.001, "<0.001", sprintf("%.03f", p_overall))
  
  # 生成图形
  plot_file <- paste0("RCS_", var, ".pdf")
  pdf(file = plot_file, width = 6, height = 5)
  print(
    ggplot() +
      geom_line(data = pred, aes_string(x = var, y = "yhat"), color = "red", size = 1) +
      geom_ribbon(data = pred, aes_string(x = var, ymin = "lower", ymax = "upper"), alpha = 0.1, fill = "red") +
      geom_hline(yintercept = 1, linetype = 2, color = "gray") +
      scale_y_continuous("OR (95% CI)") +
      scale_x_continuous(var) +
      geom_text(aes(x = quantile(pred[[var]], 0.2),
                    y = quantile(pred$yhat, 0.9),
                    label = paste0("P-overall: ", p_overall, "\nP-non-linear: ", p_nonlin)), hjust = 0) +
      theme_test() +
      theme(axis.line = element_line(), panel.grid = element_blank(), panel.border = element_blank())
  )
  dev.off()
}


# 亚组分析 ------------
setwd("G:/返修/KXF2025040303/")

if (! dir.exists("./06_亚组分析")){
  dir.create("./06_亚组分析")
}
setwd("./06_亚组分析")


unique(rt$PIR)

expoVar <- c('MDS')
allVars <- c('Age_group',"Gender","Race","Marital_status",'PIR',"Stroke","Hypertension","Diabetes")

# 循环处理每个暴露变量
for(expo in expoVar) {
  outTab <- data.frame()
  
  # 对每个分组变量进行循环
  for(Variable in allVars) {
    varTab <- data.frame()
    rt2 <- rt
    
    # 对数据进行加权处理
    colnames(rt2) <- gsub(Variable, "Variable", colnames(rt2))
    weightData <- svydesign(id = ~SDMVPSU, 
                            strata = ~SDMVSTRA, 
                            weights = ~WTMEC11YR, 
                            nest = TRUE, 
                            data = rt2, 
                            survey.lonely.psu = "adjust")
    
    # 在每个分组中构建逻辑回归模型
    for(Group in levels(factor(rt[, Variable]))) {
      modelGroup <- svyglm(as.formula(paste("OP ~", expo)), 
                           design = subset(weightData, Variable == Group), 
                           family = quasibinomial())
      summ1 <- summary(modelGroup)
      # 计算OR值及其95%置信区间
      coefData <- summ1$coefficients[-1, , drop = FALSE]
      beta <- coefData[, "Estimate"]
      se <- coefData[, "Std. Error"]
      OR <- exp(beta)
      OR_lci95 <- exp(beta - 1.96 * se)
      OR_uci95 <- exp(beta + 1.96 * se)
      
      # 将结果合并到表格中
      result <- cbind(Exposure = expo,
                      Variable = Variable,
                      Group = Group,
                      coefData,
                      OR = OR,
                      OR_lci95 = OR_lci95,
                      OR_uci95 = OR_uci95)
      
      varTab <- rbind(varTab, result)
    }
    
    # 计算交互作用的p值
    model1 <- svyglm(as.formula(paste("OP ~", expo, "* Variable")), 
                     design = weightData, 
                     family = quasibinomial())
    model2 <- svyglm(as.formula(paste("OP ~", expo, "+ Variable")), 
                     design = weightData, 
                     family = quasibinomial())
    interaction <- anova(model1, model2, test = "Chisq")
    
    outTab <- rbind(outTab, 
                    cbind(varTab, 
                          interactionPval = c(interaction$p, rep("", nrow(varTab) - 1))))
  }
  csv_file <- paste0("亚组_", expo, ".csv")
  write.csv(outTab, file = csv_file)
}




# 森林图 --------------------
#引用包
library(grid)
library(readr)
library(forestploter)


options(device = "pdf")
# 定义输入文件列表
inputFiles <- c("亚组_MDS绘图.csv")

# 循环处理每个文件
for(inputFile in inputFiles) {
  # 读取数据
  data <- read.csv(inputFile, header=T, sep=",", check.names=F)
  
  # 计算beta值的波动范围
  # data$b_lci95 = data[,"Estimate"] - 1.96 * data[,"Std. Error"]
  # data$b_uci95 = data[,"Estimate"] + 1.96 * data[,"Std. Error"]
  varVec <- factor(data[,"Variable"], levels=unique(data[,"Variable"]))
  lineVec <- cumsum(c(1,table(varVec)))
  
  # 整理数据
  data$' ' <- paste(rep(" ", 13), collapse = " ")
  data$expoVar_Group <- rep(c("Medium", "High"), times = 20)
  data$'OR (95% CI)' <- ifelse(is.na(data$OR), "", sprintf("%.2f (%.2f to %.2f)", data$OR, data$OR_lci95, data$OR_uci95))
  data$pvalue <- ifelse(data[,"Pr(>|t|)"]<0.001, "<0.001", sprintf("%.3f", data[,"Pr(>|t|)"]))
  data$interactionPval <- ifelse(data$interactionPval<0.001, "<0.001", sprintf("%.3f", data$interactionPval))
  data[,"Interaction_pvalue"] <- ifelse(is.na(data$interactionPval), "", data$interactionPval)
  data$Variable <- ifelse(is.na(data$Variable), "", data$Variable)
  data$Group <- ifelse(is.na(data$Group), "", data$Group)
  
  # 定义图形的颜色
  boxcolor <- c("#E64B35","#4DBBD5","#00A087","#3C5488","#F39B7F","#8491B4","#91D1C2","#DC0000","#7E6148")
  boxcolor <- boxcolor[as.numeric(as.factor(data$Variable))]
  data[duplicated(data$Variable),]$Variable <- ""
  
  # 准备图形参数
  tm <- forest_theme(base_size = 16,
                     ci_pch = 16, ci_lty = 1, ci_lwd = 1.5, ci_col = "black", ci_Theight = 0.2,
                     refline_gp = gpar(lty = "dashed", lwd = 1, col = "grey20"),
                     xaxis_gp = gpar(fontsize = 16, lwd = 1, cex = 1),
                     footnote_gp = gpar(fontsize=16, cex = 0.6, col = "blue"))
  
  # 绘制图形
  plot <- forestploter::forest(data[, c("Variable","Group"," ",'expoVar_Group',"OR (95% CI)", "pvalue", "Interaction_pvalue")],
                               est = data$OR,
                               lower = data$OR_lci95,
                               upper = data$OR_uci95,
                               ci_column = 3,
                               ref_line = 1,
                               xlim = c(0, 5),
                               ticks_at = c(0, 1, 5),
                               theme = tm)
  
  # 修改图形中可信区间的颜色
  for(i in 1:nrow(data)){
    plot <- edit_plot(plot, col=3, row = i, which = "ci", gp = gpar(fill = boxcolor[i],fontsize=25))
  }
  
  # 在图形中增加线段
  plot <- add_border(plot, part = "header", row =1,where = "top",gp = gpar(lwd =2))
  plot <- add_border(plot, part = "header", row = lineVec, gp = gpar(lwd =1))
  
  # 设置字体的大小和居中
  plot <- edit_plot(plot, col=1:ncol(data),row = 1:nrow(data), which = "text", gp = gpar(fontsize=12))
  plot <- edit_plot(plot, col = 1:ncol(data), which = "text",hjust = unit(0.5, "npc"),part="header",
                    x = unit(0.5, "npc"))
  plot <- edit_plot(plot, col = 1:ncol(data), which = "text",hjust = unit(0.5, "npc"),
                    x = unit(0.5, "npc"))
  
  # 生成输出文件名（使用输入文件名的基本名称）
  outputFile <- gsub("\\.csv$", "_forest.pdf", inputFile)
  
  # 输出图形
  pdf(file=outputFile, width=13, height=13)
  print(plot)
  dev.off()
  
  # 打印进度信息
  message("已完成处理: ", inputFile, " -> ", outputFile)
}




write.csv(rt, 'modeldata.csv')




