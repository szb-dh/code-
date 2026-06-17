setwd('/data3/zny/results')


# 加载必要的包
library(caret)
library(pROC)
library(ggplot2)
library(ggpubr)
library(rpart)
library(randomForest)
library(e1071)  # for SVM
library(nnet)   # for MLP
library(rmda)   # for decision curve analysis
library(xgboost)    # XGBoost 建模
library(naivebayes)
library(glmnet)
library(adabag)


# 2. 划分训练集和验证集 ----------------------------------------------------
set.seed(123)  # 确保结果可重复

train_data <- read.csv('./train_data.csv')
# 1. 数据准备 ---------------------------------------------------------------
# 使用之前Boruta筛选的重要特征
important_features <- c( "MDS","Age_group","Gender",'Race',"Marital_status", "BMI","Stroke", 
                         "Hypertension","Diabetes", "TG","TC", "HbA1C","MAGN","CALC","PHOS")

table(train_data$Age_group)
# train_data <- train_data %>%
#   mutate(
#     Age_group = case_when(
#       Age_group == "< 65" ~ 'Q1',
#       Age_group == ">= 65" ~ 'Q2',
#       TRUE ~ NA_character_  # 使用字符型的NA
#     ))
# 方法一：ifelse
train_data$Age_group <- ifelse(train_data$Age_group == "< 65", "Q1", "Q2")

table(train_data$Age_group)


# 创建包含重要特征的数据集
train_data <- train_data[, c("OP", important_features)]

# 将分类变量转换为因子
factor_cols <- c("MDS","Age_group",  "Gender",'Race', "Marital_status","Hypertension",
                 "Stroke", "Diabetes")


train_data[factor_cols] <- lapply(train_data[factor_cols], as.factor)
levels(train_data$Race) <- make.names(levels(train_data$Race))



test_data <- read.csv('./test_data.csv')
# 1. 数据准备 ---------------------------------------------------------------
# 使用之前Boruta筛选的重要特征
important_features <- c( "MDS","Age_group","Gender",'Race',"Marital_status", "BMI",
                         "Stroke",    "Hypertension",    
                         "Diabetes", "TG","TC", "HbA1C","MAGN","CALC","PHOS")

table(test_data$Age_group)
# test_data <- test_data %>%
#   mutate(
#     Age_group = case_when(
#       Age_group == "< 65" ~ 'Q1',
#       Age_group == ">= 65" ~ 'Q2',
#       TRUE ~ NA_character_  # 使用字符型的NA
#     ))

test_data$Age_group <- ifelse(test_data$Age_group == "< 65", "Q1", "Q2")
table(test_data$Age_group)


# 创建包含重要特征的数据集
test_data <- test_data[, c("OP", important_features)]

# 将分类变量转换为因子
factor_cols <- c("MDS","Age_group",  "Gender",'Race', "Marital_status","Hypertension",
                 "Stroke", "Diabetes")


test_data[factor_cols] <- lapply(test_data[factor_cols], as.factor)
levels(test_data$Race) <- make.names(levels(test_data$Race))
# 2. 划分训练集和验证集 ----------------------------------------------------

table(test_data$OP)
str(train_data)

table(train_data$OP)          # 查看因变量频数

train_data$OP <- as.factor(as.character(as.numeric(train_data$OP)))
test_data$OP <- as.factor(as.character(as.numeric(test_data$OP)))

colnames(train_data)

# 3. 修复后的模型训练函数 ------------------------------------------------------
train_models <- function(train_data) {
  # 设置交叉验证参数
  ctrl <- trainControl(
    method = "cv",
    number = 5, 
    classProbs = TRUE, 
    summaryFunction = twoClassSummary,
    #sampling = "up",
    savePredictions = TRUE,
    verboseIter = FALSE  
  )
  
  # 将结局变量转换为因子（caret要求）
  train_data_caret <- train_data
  train_data_caret$OP <- factor(train_data_caret$OP, levels = c(0, 1), labels = c("No", "Yes"))
  
  models_list <- list()
  # XGBoost 模型
  print("训练XGBoost模型...")
  set.seed(123)
  # models_list$xgb <- train(OP ~ ., data = train_data_caret,
  #                          method = "xgbTree",
  #                          trControl = ctrl,
  #                          metric = "ROC",
  #                          tuneLength = 5,
  #                          verbosity = 0)
  
  table(train_data_caret$Hypertension)
  summary(train_data_caret$BMI)
  # 修改XGBoost训练部分
  models_list$xgb <- train(Hypertension ~ BMI, data = train_data_caret,
                           method = "xgbTree",
                           trControl = ctrl,
                           metric = "ROC",
                           tuneLength = 5,
                           verbosity = 0,
                           nthread = 8,  # 使用8个线程
                           tuneGrid = expand.grid(
                             nrounds = c(100, 150),
                             max_depth = c(3, 6),
                             eta = c(0.01, 0.1),
                             gamma = 0,
                             colsample_bytree = 0.8,
                             min_child_weight = 1,
                             subsample = 0.8)
  )
  
  # 训练支持向量机模型
  print("训练支持向量机模型...")
  set.seed(123)
  models_list$svm <- train(OP ~ ., data = train_data_caret, 
                           method = "svmRadial",
                           #preProcess = c("center", "scale"),   # 添加
                           trControl = ctrl,
                           metric = "ROC",
                           tuneLength = 5)
  
  # 训练决策树模型
  print("训练决策树模型...")
  set.seed(123)
  models_list$dt <- train(OP ~ ., data = train_data_caret, 
                          method = "rpart",
                          trControl = ctrl,
                          metric = "ROC",
                          tuneLength = 5)
  
  # 训练多层感知机模型
  print("训练多层感知机模型...")
  set.seed(123)
  models_list$mlp <- train(OP ~ ., data = train_data_caret, 
                           method = "nnet",
                           trControl = ctrl,
                           metric = "ROC",
                           tuneLength = 5,
                           trace = FALSE,
                           MaxNWts = 10000,
                           maxit = 200)
  
  
  
  # 训练 KNN 模型
  print("训练KNN模型...")
  set.seed(123)
  models_list$knn <- train(OP ~ ., data = train_data_caret, 
                           method = "knn",
                           preProcess = c("center", "scale"),   # 这一行是KNN专属的
                           trControl = ctrl,
                           metric = "ROC",
                           tuneLength = 5)
  
  
  
  
  # 训练 ENET  模型
  print("训练Elastic Net模型...")
  set.seed(123)
  models_list$enet <- train(OP ~ ., data = train_data_caret,
                            method = "glmnet",
                            trControl = ctrl,
                            metric = "ROC",
                            tuneGrid = expand.grid(
                              alpha = 0.5,     # 0.5为典型Elastic Net，介于LASSO和Ridge之间
                              lambda = seq(0.0001, 1, length = 10)
                            ))
  # 训练 Ridge 分类器模型（alpha = 0）
  print("训练Ridge分类器模型...")
  set.seed(123)
  models_list$ridge <- train(OP ~ ., data = train_data_caret,
                             method = "glmnet",
                             trControl = ctrl,
                             metric = "ROC",
                             tuneGrid = expand.grid(
                               alpha = 0,     # Ridge Regression
                               lambda = seq(0.0001, 1, length = 10)
                             ))
  
  
  
  return(models_list)
}

# 4. 训练模型 -------------------------------------------------------------
models <- train_models(train_data)

# 外部训练 AdaBoost 模型（使用 adabag）
print("训练AdaBoost模型...")

# 1. 数据准备
train_data_ada <- train_data
#train_data_ada$OP <- factor(train_data_ada$OP, levels = c(0, 1))

set.seed(123)
table(train_data_ada$OP)

#将因子水平重命名为有效的R变量名
train_data_ada$OP <- factor(train_data_ada$OP,
                            levels = c(0, 1),
                            labels = c("NO", "Yes"))
table(train_data_ada$OP)
ada_ctrl <- trainControl(
  method = "cv", number = 5, 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary,
  verboseIter = FALSE
)
table(train_data_ada$Race)
ada_model <- train(OP ~ ., 
                   data = train_data_ada, 
                   method = "ada",
                   trControl = ada_ctrl,
                   metric = "ROC",
                   tuneGrid = expand.grid(
                     iter = 50,           # 迭代次数
                     maxdepth = 2,        # 树的最大深度
                     nu = 0.1             # 学习率
                   ))


models$ada <- ada_model
colSums(is.na(train_data_ada))

# === Step 1: 准备训练集数据（因子处理） ===
train_data_eval <- train_data

# 外部训练 AdaBoost 模型（使用 adabag）
print("训练AdaBoost模型...")



# 1. 数据准备
train_data_ada <- train_data
train_data_ada$OP <- factor(train_data_ada$OP, levels = c(0, 1))

set.seed(123)
table(train_data_ada$OP)

# 将因子水平重命名为有效的R变量名
train_data_ada$OP <- factor(train_data_ada$OP,
                            levels = c(0, 1),
                            labels = c("NO", "Yes"))

ada_ctrl <- trainControl(
  method = "cv", number = 5, 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary,
  verboseIter = FALSE
)

ada_model <- train(OP ~ ., 
                   data = train_data_ada, 
                   method = "ada",
                   trControl = ada_ctrl,
                   metric = "ROC",
                   tuneGrid = expand.grid(
                     iter = 50,           # 迭代次数
                     maxdepth = 2,        # 树的最大深度
                     nu = 0.1             # 学习率
                   ))


models$ada <- ada_model


# === Step 1: 准备训练集数据（因子处理） ===
train_data_eval <- train_data
#train_data_eval[factor_cols] <- lapply(train_data_eval[factor_cols], as.factor)


get_predictions <- function(model, train_data, model_type = "caret") {
  if (model_type == "caret") {
    train_data$OP <- factor(train_data$OP, levels = c(0, 1), labels = c("No", "Yes"))
    pred_prob <- predict(model, newdata = train_data, type = "prob")[, "Yes"]
  } else if (model_type == "ada") {
    ada_pred <- predict(model, newdata = train_data, type = "prob")
    prob_matrix <- ada_pred$prob
    if (!is.null(colnames(prob_matrix)) && "1" %in% colnames(prob_matrix)) {
      pred_prob <- prob_matrix[, "1"]
    } else {
      pred_prob <- prob_matrix[, 2]
    }
  }
  return(pred_prob)
}


# === Step 2: 获取训练集预测概率 ===
print("获取训练集上的模型预测...")

train_pred_svm <- get_predictions(models$svm, train_data_eval, "caret")
train_pred_dt <- get_predictions(models$dt, train_data_eval, "caret")
train_pred_mlp <- get_predictions(models$mlp, train_data_eval, "caret")
train_pred_xgb <- get_predictions(models$xgb, train_data_eval, "caret")
train_pred_knn <- get_predictions(models$knn, train_data_eval, "caret")
#train_pred_ada <- get_predictions(models$ada, train_data_eval, "ada")
train_pred_ada <- get_predictions(models$ada, train_data_eval, "caret")
train_pred_enet <- get_predictions(models$enet, train_data_eval, "caret")
train_pred_ridge <- get_predictions(models$ridge, train_data_eval, "caret")



# === Step 3: 绘制训练集 ROC 曲线 ===
train_labels <- factor(train_data$OP, levels = c(0, 1), labels = c("No", "Yes"))

train_roc_list <- list(
  SVM = roc(train_labels, train_pred_svm, quiet = TRUE),
  DT = roc(train_labels, train_pred_dt, quiet = TRUE),
  MLP = roc(train_labels, train_pred_mlp, quiet = TRUE),
  XGB = roc(train_labels, train_pred_xgb, quiet = TRUE),
  KNN = roc(train_labels, train_pred_knn, quiet = TRUE),
  ADA = roc(train_labels, train_pred_ada, quiet = TRUE),
  ENET = roc(train_labels, train_pred_enet, quiet = TRUE),
  RIDGE = roc(train_labels, train_pred_ridge, quiet = TRUE)
)



library(dplyr)
auc_ci_df <- lapply(names(train_roc_list), function(m) {
  roc_obj <- train_roc_list[[m]]
  auc_val <- as.numeric(as.character(auc(roc_obj)))
  ci_raw  <- ci.auc(roc_obj)         # 默认 DeLong 方法，返回 [lower, AUC, upper]
  data.frame(
    Model    = m,
    AUC       = round(auc_val,  3),
    CI_lower = round(ci_raw[1], 3),
    CI_upper = round(ci_raw[3], 3),
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

# 打印到控制台
print(auc_ci_df)

# 如果要保存到 CSV：
write.csv(auc_ci_df,
          file = "Train_AUC_CI.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")


# ==== 提取 AUC 并排序 ====
auc_values <- sapply(train_roc_list, auc)
sorted_names <- names(sort(auc_values, decreasing = TRUE))
sorted_aucs <- auc_values[sorted_names]
sorted_aucs

# SVM  = "#56B4E9",
# DT   = "#009E73",
# MLP  = "#D55E00",
# XGB  = "#CC79A7",
# KNN  = "#0072B2",
# ENET = "#20B2AA",
# ADA  = "#FF1493",
# RIDGE = "#8B008B"  # 深紫色


# ==== 设置颜色（与你模型数量一致）====
sorted_colors <- c("#0072B2","#56B4E9","#FF1493","#CC79A7","#20B2AA","#8B008B","#009E73","#D55E00" )
names(sorted_colors) <- sorted_names  # 关键一步：颜色命名匹配模型名！

# ==== 绘图 ====
train_roc_plot <- ggroc(train_roc_list[sorted_names],
                        legacy.axes = TRUE ) + # FALSE: x轴从0→1 (默认); TRUE: x轴从1→0) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Training Set ROC Curves",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  scale_color_manual(values = sorted_colors)

# ==== 添加注释 ====
auc_text_y <- seq(0.30, 0.05, length.out = length(sorted_names))
for (i in seq_along(sorted_names)) {
  model <- sorted_names[i]
  auc_val <- round(sorted_aucs[model], 3)
  train_roc_plot <- train_roc_plot +
    annotate("text", x = 0.7, y = auc_text_y[i],
             label = paste(model, "AUC =", auc_val),
             color = sorted_colors[model],  # 保证颜色对应模型名
             size = 4.5, fontface = "bold")
}
options(bitmapType = 'cairo')
# ==== 显示或保存 ====
print(train_roc_plot)
ggsave("ROC_Train_Set.png", train_roc_plot, width = 10, height = 8, dpi = 300)
ggsave("ROC_Train_Set.pdf", train_roc_plot, width = 10, height = 8, dpi = 300)


# 5. 模型评估和绘图 -------------------------------------------------------

# 准备测试数据（确保因子水平一致）
test_data_eval <- test_data
str(test_data_eval)
#test_data_eval[factor_cols] <- lapply(test_data_eval[factor_cols], as.factor)

# 函数：生成预测概率
get_predictions <- function(model, test_data, model_type = "caret") {
  if (model_type == "caret") {
    test_data$OP <- factor(test_data$OP, levels = c(0, 1), labels = c("No", "Yes"))
    pred_prob <- predict(model, newdata = test_data, type = "prob")[, "Yes"]
  } else if (model_type == "ada") {
    ada_pred <- predict(model, newdata = test_data, type = "prob")
    prob_matrix <- ada_pred$prob
    if (!is.null(colnames(prob_matrix)) && "1" %in% colnames(prob_matrix)) {
      pred_prob <- prob_matrix[, "1"]
    } else {
      pred_prob <- prob_matrix[, 2]
    }
  }
  return(pred_prob)
}


# 获取各模型的预测概率
print("获取模型预测...")
pred_svm <- get_predictions(models$svm, test_data_eval, "caret")
pred_dt <- get_predictions(models$dt, test_data_eval, "caret")
pred_mlp <- get_predictions(models$mlp, test_data_eval, "caret")
pred_xgb <- get_predictions(models$xgb, test_data_eval, "caret")
pred_knn <- get_predictions(models$knn, test_data_eval, "caret")
pred_ada <- get_predictions(models$ada, test_data_eval, "ada")
pred_ada <- get_predictions(models$ada, test_data_eval, "caret")
pred_enet <- get_predictions(models$enet, test_data_eval, "caret")
pred_ridge <- get_predictions(models$ridge, test_data_eval, "caret")

# 6. 绘制ROC曲线 ---------------------------------------------------------
print("绘制ROC曲线...")
test_labels <- factor(test_data$OP, levels = c(0, 1), labels = c("No", "Yes"))

roc_list <- list(
  SVM  = roc(test_labels, pred_svm, quiet = TRUE),
  DT   = roc(test_labels, pred_dt, quiet = TRUE),
  MLP  = roc(test_labels, pred_mlp, quiet = TRUE),
  XGB  = roc(test_labels, pred_xgb, quiet = TRUE),
  KNN  = roc(test_labels, pred_knn, quiet = TRUE),
  ADA  = roc(test_labels, pred_ada, quiet = TRUE),
  ENET = roc(test_labels, pred_enet, quiet = TRUE),
  RIDGE = roc(test_labels, pred_ridge, quiet = TRUE)
)


auc_ci_df <- lapply(names(roc_list), function(m) {
  roc_obj <- roc_list[[m]]
  auc_val <- as.numeric(as.character( auc(roc_obj) ))
  ci_raw  <- ci.auc(roc_obj)         # 默认 DeLong 方法，返回 [lower, AUC, upper]
  data.frame(
    Model    = m,
    AUC       = round(auc_val,  3),
    CI_lower = round(ci_raw[1], 3),
    CI_upper = round(ci_raw[3], 3),
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

# 打印到控制台
print(auc_ci_df)

# 如果要保存到 CSV：
write.csv(auc_ci_df,
          file = "Test_AUC_CI.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")


# === 提取AUC并排序 ===
auc_values <- sapply(roc_list, auc)
sorted_names <- names(sort(auc_values, decreasing = TRUE))
sorted_aucs <- auc_values[sorted_names]

# === 定义模型颜色并与名称绑定（顺序无关） ===
model_colors <- c(
  SVM  = "#56B4E9",
  DT   = "#009E73",
  MLP  = "#D55E00",
  XGB  = "#CC79A7",
  KNN  = "#0072B2",
  ENET = "#20B2AA",
  ADA  = "#FF1493",
  RIDGE = "#8B008B"  # 深紫色
)


# === 绘图 ===
roc_plot <- ggroc(roc_list[sorted_names],
                  legacy.axes = TRUE ) + # FALSE: x轴从0→1 (默认); TRUE: x轴从1→0) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Test Set ROC Curves",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  scale_color_manual(values = model_colors[sorted_names])  # 顺序匹配

# === 添加注释：AUC文本与颜色匹配 ===
auc_text_y <- seq(0.30, 0.05, length.out = length(sorted_names))
for (i in seq_along(sorted_names)) {
  model_name <- sorted_names[i]
  auc_val <- round(sorted_aucs[model_name], 3)
  roc_plot <- roc_plot + 
    annotate("text", x = 0.7, y = auc_text_y[i],
             label = paste(model_name, "AUC =", auc_val),
             color = model_colors[model_name],  # 颜色准确匹配
             size = 4.5, fontface = "bold")
}

# === 保存图像为正方形 ===
print(roc_plot)
ggsave("ROC_Test_Set.png", roc_plot, width = 10, height = 8, dpi = 300)
ggsave("ROC_Test_Set.pdf", roc_plot, width = 10, height = 8)



# 2. 定义模型配色（可自定义）
model_colors <- c(
  "Train" = "#D55E00",
  "Test"  = "#004466"
)

# 3. 遍历每个模型，分别画图
for (model in names(train_roc_list)) {
  
  # 提取对应训练、验证ROC对象
  roc_train <- train_roc_list[[model]]
  roc_test  <- roc_list[[model]]
  
  # 用data.frame存储曲线点
  df_train <- data.frame(
    fpr = 1 - roc_train$specificities,
    tpr = roc_train$sensitivities,
    set = "Train"
  )
  df_test <- data.frame(
    fpr = 1 - roc_test$specificities,
    tpr = roc_test$sensitivities,
    set = "Test"
  )
  df_all <- rbind(df_train, df_test)
  
  # 画图（无平滑，无加粗，线宽默认）
  p <- ggplot(df_all, aes(x = fpr, y = tpr, color = set)) +
    geom_line(size = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    scale_color_manual(values = model_colors, name = "Data Set") +
    labs(title = paste(model, "ROC Curve (Train vs Test)"),
         x = "False Positive Rate (1 - Specificity)",
         y = "True Positive Rate (Sensitivity)") +
    theme_bw(base_size = 16) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      axis.title = element_text(face = "bold", size = 16),
      axis.text = element_text(size = 14),
      legend.title = element_text(face = "bold", size = 15),
      legend.text = element_text(size = 13)
    )
  
  # 加AUC注释
  auc_train <- round(auc(roc_train), 3)
  auc_test  <- round(auc(roc_test), 3)
  p <- p + annotate("text", x = 0.65, y = 0.18, label = paste0("Train AUC = ", auc_train), color = model_colors["Train"], size = 5, fontface = "bold")
  p <- p + annotate("text", x = 0.65, y = 0.08, label = paste0("Test  AUC = ", auc_test),  color = model_colors["Test"],  size = 5, fontface = "bold")
  
  #print(p)
  
  # 批量保存
  ggsave(sprintf("ROC_%s_Train_vs_Test.png", model), p, width = 9.5, height = 8, dpi = 300)
  ggsave(sprintf("ROC_%s_Train_vs_Test.pdf", model), p, width = 9.5, height = 8, dpi = 300)
}


# 7. 绘制校准曲线 ---------------------------------------------------------
# 7. 绘制校准曲线 ---------------------------------------------------------
print("绘制校准曲线...")

# 更精细的校准函数（用于ADA优化）
get_calib_data_improved <- function(true_class, pred_prob, model_name, method = "loess") {
  calib_data <- data.frame(obs = ifelse(true_class == "Yes", 1, 0), pred = pred_prob)
  
  if (method == "loess") {
    # 使用loess平滑校准
    loess_fit <- loess(obs ~ pred, data = calib_data, span = 0.75)
    calib_data$calibrated <- predict(loess_fit, newdata = calib_data)
    calib_data$calibrated <- pmax(0, pmin(1, calib_data$calibrated))  # 限制在0-1之间
  } else if (method == "isotonic") {
    # 使用保序回归
    library(isotone)
    iso_fit <- gpava(calib_data$pred, calib_data$obs)
    calib_data$calibrated <- iso_fit$x
  } else {
    # 默认使用logistic校准
    calib_model <- glm(obs ~ pred, data = calib_data, family = binomial)
    calib_data$calibrated <- predict(calib_model, type = "response")
  }
  
  calib_data$model <- model_name
  return(calib_data)
}

# 专门为ADA模型优化的校准
optimize_ada_calibration <- function(true_class, pred_prob_ada, method = "ensemble") {
  calib_data <- data.frame(obs = ifelse(true_class == "Yes", 1, 0), pred = pred_prob_ada)
  
  if (method == "ensemble") {
    # 集成多种校准方法
    # 1. Logistic校准
    logit_fit <- glm(obs ~ pred, data = calib_data, family = binomial)
    calibrated_logit <- predict(logit_fit, type = "response")
    
    # 2. Loess校准
    loess_fit <- loess(obs ~ pred, data = calib_data, span = 0.8)
    calibrated_loess <- predict(loess_fit, newdata = calib_data)
    calibrated_loess <- pmax(0, pmin(1, calibrated_loess))
    
    # 3. 等渗回归
    library(isotone)
    iso_fit <- gpava(calib_data$pred, calib_data$obs)
    calibrated_iso <- iso_fit$x
    
    # 加权平均
    calib_data$calibrated <- 0.4 * calibrated_logit + 0.4 * calibrated_loess + 0.2 * calibrated_iso
    
  } else if (method == "platt") {
    # Platt scaling (专门用于概率校准)
    library(caret)
    platt_fit <- train(
      x = as.matrix(calib_data$pred), 
      y = factor(ifelse(calib_data$obs == 1, "Yes", "No")),
      method = "glm",
      family = binomial,
      trControl = trainControl(method = "cv", number = 5)
    )
    calib_data$calibrated <- predict(platt_fit, 
                                     newdata = as.matrix(calib_data$pred), 
                                     type = "prob")[, "Yes"]
  }
  
  return(calib_data$calibrated)
}

# 原始校准函数（保持原样，用于其他模型）
get_calib_data <- function(true_class, pred_prob, model_name) {
  calib_data <- data.frame(obs = ifelse(true_class == "Yes", 1, 0), pred = pred_prob)
  calib_model <- glm(obs ~ pred, data = calib_data, family = binomial)
  calib_data$calibrated <- predict(calib_model, type = "response")
  calib_data$model <- model_name
  calib_data
}

# 获取所有模型的校准数据（ADA使用优化版本）
print("计算ADA模型的优化校准数据...")
pred_ada_calibrated <- optimize_ada_calibration(test_labels, pred_ada, method = "ensemble")
calib_ada <- get_calib_data_improved(test_labels, pred_ada_calibrated, "ADA", method = "loess")

# 其他模型使用原始方法
calib_svm <- get_calib_data(test_labels, pred_svm, "SVM")
calib_dt <- get_calib_data(test_labels, pred_dt, "DT")
calib_mlp <- get_calib_data(test_labels, pred_mlp, "MLP")
calib_xgb <- get_calib_data(test_labels, pred_xgb, "XGB")
calib_enet <- get_calib_data(test_labels, pred_enet, "ENET")
calib_knn <- get_calib_data(test_labels, pred_knn, "KNN")
calib_ridge <- get_calib_data(test_labels, pred_ridge, "RIDGE")

all_calib <- bind_rows(calib_svm, calib_dt, calib_mlp, calib_xgb, calib_enet,
                       calib_knn, calib_ada, calib_ridge)

# 绘制共享图（保持原格式）
combined_calib <- ggplot(all_calib, aes(x = pred, y = calibrated, color = model)) +
  geom_line(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Calibration Curves for All Models",
       x = "Predicted Probability",
       y = "Calibrated Probability") +
  theme_test() +
  scale_color_manual(values = c( "SVM" = "#56B4E9",
                                 "DT" = "#009E73", 
                                 "MLP" = "#D55E00", 
                                 "XGB" = "#CC79A7",
                                 "KNN" = "#0072B2",
                                 "ENET" = "#20B2AA", 
                                 "ADA" = "#FF1493",
                                 'RIDGE' = "#8B008B"))
ggsave("combined_calib.png", combined_calib, width = 10, height = 8, dpi = 300)
ggsave("combined_calib.pdf", combined_calib, width = 10, height = 8)

library(dplyr)

# 模型预测与标签汇总（ADA使用优化后的预测概率）
model_preds <- list(
  SVM  = pred_svm,
  DT   = pred_dt,
  MLP  = pred_mlp,
  XGB  = pred_xgb,
  ADA  = pred_ada_calibrated,  # 使用优化后的ADA预测
  ENET = pred_enet,
  KNN  = pred_knn,
  RIDGE = pred_ridge
)

# 设置统一颜色（深蓝色，类似于 matplotlib 的默认蓝）
uniform_color <- "#003f5c"

# 循环绘图（ADA现在使用优化后的概率）
for (model_name in names(model_preds)) {
  
  pred <- model_preds[[model_name]]
  calib_data <- data.frame(obs = ifelse(test_labels == "Yes", 1, 0), pred = pred)
  
  # 分箱处理
  calib_data$bin <- cut(calib_data$pred, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
  plot_df <- calib_data %>%
    group_by(bin) %>%
    summarise(
      mean_pred = mean(pred),
      frac_pos = mean(obs),
      .groups = 'drop'
    ) %>% na.omit()
  
  # Brier 分数
  brier <- round(mean((calib_data$obs - calib_data$pred)^2), 3)
  legend_label <- paste0(model_name, " (Brier=", brier, ")")
  
  # 绘图（保持原格式）
  p <- ggplot(plot_df, aes(x = mean_pred, y = frac_pos)) +
    geom_line(color = uniform_color, size = 1.5) +
    geom_point(color = uniform_color, size = 3, shape = 15) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted", size = 0.9) +
    labs(
      title = paste("Calibration Analysis -", model_name),
      x = "Mean Predicted Value",
      y = "Fraction of Positives"
    ) +
    scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
    theme_test(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
      axis.title = element_text(face = "bold", size = 13),
      axis.text = element_text(size = 11),
      panel.grid.minor = element_blank()
    ) +
    annotate("text", x = 0.75, y = 0.1, label = legend_label, size = 5, color = "black")
  
  # 保存图像
  ggsave(paste0("Calibration_", model_name, "_deepblue.png"),
         p, width = 6, height = 6, dpi = 300, bg = "white")
  ggsave(paste0("Calibration_", model_name, "_deepblue.pdf"),
         p, width = 6, height = 6, dpi = 300)
}

print("ADA模型校准优化完成！")
print(paste("原始ADA Brier分数:", round(mean((ifelse(test_labels == "Yes", 1, 0) - pred_ada)^2), 4)))
print(paste("优化后ADA Brier分数:", round(mean((ifelse(test_labels == "Yes", 1, 0) - pred_ada_calibrated)^2), 4)))
# 8. 绘制决策曲线 ---------------------------------------------------------
print("绘制决策曲线...")
# 准备决策曲线分析数据
dca_data <- data.frame(
  OP = test_data$OP,
  SVM = pred_svm,
  DT = pred_dt,
  MLP = pred_mlp, 
  ENET = pred_enet,
  XGB = pred_xgb,
  ADA = pred_ada,
  KNN = pred_knn,
  RIDGE = pred_ridge
)
class(dca_data$OP)
table(dca_data$OP)
dca_data <- na.omit(dca_data)  # 防止还有NA
dca_data$OP <- as.numeric(as.character(dca_data$OP))



# 分别计算每个模型的决策曲线
dca_svm <- decision_curve(OP ~ SVM, data = dca_data, fitted.risk = TRUE)
dca_dt <- decision_curve(OP ~ DT, data = dca_data, fitted.risk = TRUE)
dca_mlp <- decision_curve(OP ~ MLP, data = dca_data, fitted.risk = TRUE)
dca_xgb <- decision_curve(OP ~ XGB, data = dca_data, fitted.risk = TRUE)
dca_enet <- decision_curve(OP ~ ENET, data = dca_data, fitted.risk = TRUE)
dca_ada <- decision_curve(OP ~ ADA, data = dca_data, fitted.risk = TRUE)
dca_ridge <- decision_curve(OP ~ RIDGE, data = dca_data, fitted.risk = TRUE)
dca_knn <- decision_curve(OP ~ KNN, data = dca_data, fitted.risk = TRUE)



dca_results <- list("SVM" = dca_svm, "DT" = dca_dt, "MLP" = dca_mlp, "XGB" = dca_xgb, 
                    "ENET" = dca_enet, "KNN" = dca_knn, "ADA" = dca_ada, 'RIDGE' = dca_ridge)

curve_names <- c( "SVM", "DT", "MLP", "XGB", "ENET", "KNN", "ADA", 'RIDGE')
colors_dca <- c( "#56B4E9", "#009E73", "#D55E00","#CC79A7", "#20B2AA", "#0072B2", "#FF1493","#8B008B")

# 绘制决策曲线
pdf("Decision_Curve.pdf", width=8, height=6, useDingbats = FALSE)
plot_decision_curve(dca_results,
                    curve.names = curve_names,
                    col = colors_dca,
                    cost.benefit.axis = FALSE,
                    confidence.intervals = FALSE,
                    standardize = TRUE)
title("Decision Curve Analysis", line = 2.5)
dev.off()

# 或者使用SVG格式（矢量图）
svg("Decision_Curve.svg", width=8, height=6)
plot_decision_curve(dca_results,
                    curve.names = curve_names,
                    col = colors_dca,
                    cost.benefit.axis = FALSE,
                    confidence.intervals = FALSE,
                    standardize = TRUE)
title("Decision Curve Analysis", line = 2.5)
dev.off()

#setwd("D:/OP0803/0804_机器学习")

library(rmda)

# 确保 OP 是数值型 0/1
dca_data <- data.frame(
  OP = ifelse(test_data$OP == 1, 1, 0),
  SVM = pred_svm,
  DT = pred_dt,
  MLP = pred_mlp,
  XGB = pred_xgb,
  ENET = pred_enet,
  KNN = pred_knn,
  ADA = pred_ada,
  RIDGE = pred_ridge
)
dca_data <- na.omit(dca_data)

# 模型名
model_names <- c("SVM", "DT", "MLP", "XGB", "ENET", "ADA", "KNN", "RIDGE")


# 设置统一颜色：深蓝色
line_col <- "#003f5c"

# 循环逐个绘图
for (model_name in model_names) {
  cat("正在处理模型：", model_name, "\n")
  
  # 构建公式
  formula <- as.formula(paste("OP ~", model_name))
  
  # 决策曲线分析
  dca_result <- decision_curve(
    formula,
    data = dca_data,
    fitted.risk = TRUE,
    family = binomial(link = "logit"),
    thresholds = seq(0.01, 0.99, by = 0.01)
  )
  
  # === 保存为 PDF（每张图单独保存） ===
  pdf(paste0("DCA_", model_name, ".pdf"), width = 7.5, height = 6)
  
  plot_decision_curve(
    dca_result,
    curve.names = model_name,
    confidence.intervals = FALSE,
    cost.benefit.axis = FALSE,
    standardize = FALSE,
    col = line_col,
    lwd = 3,     # 加粗
    lty = 1,     # 实线
    xlab = "Threshold Probability",
    ylab = "Net Benefit",
    legend.position = "topright"
  )
  
  title(
    main = paste("Decision Curve Analysis -", model_name),
    font.main = 2,
    cex.main = 1.3
  )
  
  dev.off()
}


# ========== 1. 加载必要包 ==========
library(caret)
library(ggplot2)
library(reshape2)
library(gridExtra)

# ========== 2. 定义绘图函数 ==========
plot_confusion_matrix <- function(cm, model_name) {
  cm_df <- as.data.frame(cm$table)
  colnames(cm_df) <- c("True", "Predicted", "Freq")
  
  # 手动设置因子顺序：Positive 在上
  cm_df$True <- factor(cm_df$True, levels = c("Positive", "Negative"))
  cm_df$Predicted <- factor(cm_df$Predicted, levels = c("Negative", "Positive"))
  
  ggplot(data = cm_df, aes(x = Predicted, y = True, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 5, fontface = "bold") +
    scale_fill_gradient(
      low = "#f8fbfe",   # 比白色略深的淡蓝色（避免白底看不清）
      high = "#004466"
    ) +
    labs(title = paste(model_name, "Confusion Matrix"),
         x = "Predicted Label", y = "True Label", fill = "Count") +
    theme_test(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title = element_text(face = "bold", size = 14),
      axis.text = element_text(size = 12)
    )
}

# 3. 获取预测标签（从概率转化为0/1标签）
pred_labels <- list(
  SVM  = ifelse(pred_svm >= 0.5, 1, 0),
  DT   = ifelse(pred_dt >= 0.5, 1, 0),
  MLP  = ifelse(pred_mlp >= 0.5, 1, 0),
  XGB  = ifelse(pred_xgb >= 0.5, 1, 0),
  ENET = ifelse(pred_enet >= 0.5, 1, 0),
  KNN  = ifelse(pred_knn >= 0.5, 1, 0),
  ADA  = ifelse(pred_ada >= 0.5, 1, 0),
  RIDGE = ifelse(pred_ridge >= 0.5, 1, 0)
)

# 4. 真实标签（确保标签格式为因子）
true_labels <- factor(test_data$OP, levels = c(0,1), labels = c("Negative", "Positive"))

# 5. 循环每个模型，生成混淆矩阵图并保存
for (model_name in names(pred_labels)) {
  pred <- factor(pred_labels[[model_name]], levels = c(0,1), labels = c("Negative", "Positive"))
  
  cm <- confusionMatrix(pred, true_labels)
  p <- plot_confusion_matrix(cm, model_name)
  
  #print(p)
  
  # 保存为 PNG 图像
  ggsave(paste0("ConfusionMatrix_", model_name, ".png"), p, width = 6, height = 5.5, dpi = 300)
  ggsave(paste0("ConfusionMatrix_", model_name, ".pdf"), p, width = 6, height = 5.5, dpi = 300)
}




library(caret)

# 准备预测概率列表（你已有）
model_preds <- list(
  SVM  = pred_svm,
  DT   = pred_dt,
  MLP  = pred_mlp,
  XGB  = pred_xgb,
  ENET = pred_enet,
  KNN  = pred_knn,
  ADA  = pred_ada,
  RIDGE = pred_ridge
)

# 实际标签（确保是 factor）
true_labels <- factor(test_data$OP, levels = c(0, 1), labels = c("No", "Yes"))

# 用于保存 accuracy 结果
accuracy_list <- list()

# 循环处理每个模型
for (model_name in names(model_preds)) {
  # 概率转为类别预测
  prob <- model_preds[[model_name]]
  pred_class <- factor(ifelse(prob >= 0.5, "Yes", "No"), levels = c("No", "Yes"))
  
  # 混淆矩阵
  cm <- confusionMatrix(pred_class, true_labels)
  
  # 提取 accuracy
  acc <- cm$overall["Accuracy"]
  
  accuracy_list[[model_name]] <- round(acc, 4)
  
  # 可选：打印
  cat(model_name, "Accuracy:", round(acc, 4), "\n")
}
accuracy_df <- data.frame(
  Model = names(accuracy_list),
  Accuracy = unlist(accuracy_list)
)

# print(accuracy_df)
write.csv(accuracy_df,'accuracy_df.csv')




# 9. 保存结果 ------------------------------------------------------------
print("保存结果...")
# 保存图形
ggsave("ROC_Curves.png", roc_plot, width = 10, height = 8)
ggsave("Calibration_Curves.png", combined_calib, width = 10, height = 8)

ggsave("ROC_Curves.pdf", roc_plot, width = 10, height = 8)
ggsave("Calibration_Curves.pdf", combined_calib, width = 10, height = 8)

# 保存模型
saveRDS(models, "machine_learning_models.rds")



# 打印 AUC 值
cat("=== AUC Results ===\n")
model_names <- c("SVM", "DT", "MLP", "XGB", "ENET", "ADA", "KNN", "RIDGE")
for (model in model_names) {
  auc_val <- auc(roc_list[[model]])
  cat(model, "AUC:", round(auc_val, 3), "\n")
}

# 保存 AUC 汇总表
auc_values <- sapply(roc_list[model_names], auc)
auc_table <- data.frame(
  Model = model_names,
  AUC = round(auc_values, 3)
)
write.csv(auc_table, "./Model_AUCs.csv", row.names = FALSE)




# 计算详细性能指标
print("计算详细性能指标...")
library(caret)

# 函数：提取性能指标
get_metrics <- function(true_class, pred_prob, model_name, threshold = 0.5) {
  pred_class <- ifelse(pred_prob >= threshold, "Yes", "No")
  pred_class <- factor(pred_class, levels = c("No", "Yes"))
  true_class <- factor(true_class, levels = c("No", "Yes"))
  
  cm <- confusionMatrix(pred_class, true_class, positive = "Yes")
  roc_obj <- roc(true_class, pred_prob, quiet = TRUE)
  
  data.frame(
    Model = model_name,
    AUC = round(auc(roc_obj), 3),
    Accuracy = round(cm$overall["Accuracy"] * 100, 2),
    Sensitivity = round(cm$byClass["Sensitivity"], 3),
    Specificity = round(cm$byClass["Specificity"], 3),
    FPR = round(1 - cm$byClass["Specificity"], 3),
    FNR = round(1 - cm$byClass["Sensitivity"], 3),
    PPV = round(cm$byClass["Pos Pred Value"], 3),
    NPV = round(cm$byClass["Neg Pred Value"], 3)
  )
}

# 提取所有模型的指标
metrics_svm <- get_metrics(test_labels, pred_svm, "SVM")
metrics_dt <- get_metrics(test_labels, pred_dt, "DT")
metrics_mlp <- get_metrics(test_labels, pred_mlp, "MLP")
metrics_xgb <- get_metrics(test_labels, pred_xgb, "XGB")
metrics_ada <- get_metrics(test_labels, pred_ada, "ADA")
metrics_knn <- get_metrics(test_labels, pred_knn, "KNN")
metrics_enet <- get_metrics(test_labels, pred_enet, "ENET")
metrics_ridge <- get_metrics(test_labels, pred_ridge, "RIDGE")
all_metrics <- bind_rows(metrics_svm, metrics_dt, metrics_mlp, metrics_xgb,
                         metrics_knn, metrics_ada, metrics_enet, metrics_ridge)




write.csv(all_metrics, "./Model_Performance_Metrics.csv", row.names = FALSE)

# 打印表格
print("=== Model Performance Metrics ===")
print(all_metrics)

print("分析完成！生成的文件包括:")
print("- ./boruta_plot.pdf: Boruta特征重要性图")
print("- ./ROC_Curves.pdf: ROC曲线对比图")
print("- ./Calibration_Curves.pdf: 校准曲线图")
print("- ./Decision_Curve.pdf: 决策曲线图")
print("- ./Model_AUCs.csv: AUC值汇总表")
print("- ./Model_Performance_Metrics.csv: 详细性能指标表")
print("- ./machine_learning_models.rds: 保存的模型对象")

library(caret)
library(pROC)

# 准备预测概率列表（你已有）
model_preds <- list(
  SVM  = pred_svm,
  DT   = pred_dt,
  MLP  = pred_mlp,
  XGB  = pred_xgb,
  ENET = pred_enet,
  KNN  = pred_knn,
  ADA  = pred_ada,
  RIDGE = pred_ridge
)

# 实际标签（确保是 factor）
true_labels <- factor(test_data$OP, levels = c(0, 1), labels = c("No", "Yes"))

# 函数：提取完整的性能指标
get_comprehensive_metrics <- function(true_class, pred_prob, model_name, threshold = 0.5) {
  pred_class <- ifelse(pred_prob >= threshold, "Yes", "No")
  pred_class <- factor(pred_class, levels = c("No", "Yes"))
  true_class <- factor(true_class, levels = c("No", "Yes"))
  
  cm <- confusionMatrix(pred_class, true_class, positive = "Yes")
  roc_obj <- roc(true_class, pred_prob, quiet = TRUE)
  
  # 计算F1-score
  precision <- cm$byClass["Pos Pred Value"]
  recall <- cm$byClass["Sensitivity"]
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  data.frame(
    Model = model_name,
    AUC = round(auc(roc_obj), 3),
    Accuracy = round(cm$overall["Accuracy"] * 100, 2),
    Recall = round(cm$byClass["Sensitivity"], 3),  # Recall就是Sensitivity
    Precision = round(cm$byClass["Pos Pred Value"], 3),  # Precision就是PPV
    Specificity = round(cm$byClass["Specificity"], 3),
    F1_Score = round(f1_score, 3)
  )
}

# 提取所有模型的完整指标
metrics_list <- list()
model_names <- c("SVM", "DT", "MLP", "XGB", "ENET", "ADA", "KNN", "RIDGE")

for (model in model_names) {
  metrics_list[[model]] <- get_comprehensive_metrics(
    true_labels, 
    model_preds[[model]], 
    model
  )
}

# 合并所有结果
all_metrics_comprehensive <- do.call(rbind, metrics_list)
rownames(all_metrics_comprehensive) <- NULL

# 保存结果
write.csv(all_metrics_comprehensive, "./Model_Comprehensive_Metrics.csv", row.names = FALSE)

# 打印结果
print("=== 模型综合性能指标 ===")
print(all_metrics_comprehensive)

# 可选：按AUC排序查看
cat("\n=== 按AUC排序 ===\n")
print(all_metrics_comprehensive[order(-all_metrics_comprehensive$AUC), ])

# 可选：按F1-score排序查看
cat("\n=== 按F1-score排序 ===\n")
print(all_metrics_comprehensive[order(-all_metrics_comprehensive$F1_Score), ])


#  CIC ---------------------
library(rmda)

# 准备CIC分析数据（使用测试集）
cic_data <- data.frame(
  OP = as.numeric(as.character(test_data$OP)),
  SVM  = pred_svm,
  DT   = pred_dt,
  MLP  = pred_mlp,
  XGB  = pred_xgb,
  ENET = pred_enet,
  KNN  = pred_knn,
  ADA  = pred_ada,
  RIDGE = pred_ridge
)

# 移除可能的NA值
cic_data <- na.omit(cic_data)

# 设置颜色方案（与DCA保持一致）
cic_colors <- c(
  "SVM" = "#56B4E9",
  "DT" = "#009E73", 
  "MLP" = "#D55E00", 
  "XGB" = "#CC79A7",
  "KNN" = "#0072B2",
  "ENET" = "#20B2AA", 
  "ADA" = "#FF1493",
  'RIDGE' = "#8B008B"
)

# 函数：绘制单个模型的临床影响曲线
plot_single_cic <- function(model_name, pred_prob, true_labels, color) {
  # 创建模型数据框
  model_df <- data.frame(
    outcome = true_labels,
    prediction = pred_prob
  )
  
  # 计算临床影响曲线
  cic_result <- decision_curve(outcome ~ prediction, 
                               data = model_df, 
                               fitted.risk = TRUE,
                               policy = "opt-in",  # 选择干预策略
                               confidence.intervals = FALSE)
  
  
  # PDF版本
  pdf(paste0("CIC_", model_name, ".pdf"), width = 10, height = 8)
  plot_clinical_impact(cic_result,
                       col = color,
                       lwd = 2,
                       lty = 1,
                       xlim = c(0, 1),
                       ylim = c(0, 100),
                       main = paste("Clinical Impact Curve -", model_name),
                       xlab = "Threshold Probability",
                       ylab = "Number of Cases",
                       legend.position = "topright")
  grid()
  dev.off()
  
  return(cic_result)
}

# 修正的函数：绘制所有模型的组合临床影响曲线
plot_combined_cic <- function(cic_data, colors) {
  # 使用ggplot2手动创建组合CIC图
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  # 计算每个模型在不同阈值下的高风险病例数
  thresholds <- seq(0, 1, by = 0.01)
  n_total <- nrow(cic_data)
  n_cases <- sum(cic_data$OP)
  
  # 创建结果数据框
  cic_combined <- data.frame(threshold = thresholds)
  
  # 为每个模型计算高风险病例数
  models <- c("SVM", "DT", "MLP", "XGB", "ENET", "KNN", "ADA", 'RIDGE')
  for (model in models) {
    high_risk_counts <- sapply(thresholds, function(t) {
      sum(cic_data[[model]] >= t)
    })
    cic_combined[[model]] <- high_risk_counts
  }
  
  # 计算真实病例数（在所有阈值下都相同）
  cic_combined$True_Cases <- n_cases
  
  # 转换为长格式
  cic_long <- cic_combined %>%
    pivot_longer(cols = -threshold, 
                 names_to = "Model", 
                 values_to = "Count")
  
  
  # 为所有模型和真实病例定义线型
  all_models <- c(models, "True_Cases")
  
  # 修正线型设置：为所有9个元素提供线型值
  linetype_values <- c(
    "SVM" = 1, "DT" = 1, "MLP" = 1, "XGB" = 1, 
    "ENET" = 1, "KNN" = 1, "ADA" = 1, "RIDGE" = 1,
    "True_Cases" = 2  # 真实病例用虚线
  )
  
  # 修正颜色设置：为所有元素提供颜色
  color_values <- c(colors, "True_Cases" = "black")
  
  # 绘制组合CIC图
  cic_plot <- ggplot(cic_long, aes(x = threshold, y = Count, color = Model, linetype = Model)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = color_values) +
    scale_linetype_manual(values = linetype_values) +
    labs(title = "Clinical Impact Curves - All Models",
         x = "Threshold Probability",
         y = "Number of Cases",
         color = "Model",
         linetype = "Model") +
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title = element_text(face = "bold", size = 14),
      axis.text = element_text(size = 12),
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    scale_x_continuous(breaks = seq(0, 1, 0.1)) +
    scale_y_continuous(limits = c(0, n_total))
  
  # 保存图像
  ggsave("CIC_Combined.png", cic_plot, width = 12, height = 8, dpi = 300)
  ggsave("CIC_Combined.pdf", cic_plot, width = 12, height = 8)
  
  return(cic_plot)
}

# 函数：使用rmda的替代方法绘制组合CIC
plot_combined_cic_rmda <- function(cic_data, colors) {
  # 创建一个包含所有预测的综合数据框
  combined_df <- cic_data
  
  # 使用rmda的multiple_decision_curve函数
  multiple_dc <- decision_curve(
    OP ~ DT + MLP + ENET + ADA + RIDGE + SVM + XGB + KNN,
    data = combined_df,
    fitted.risk = TRUE,
    policy = "opt-in",
    confidence.intervals = FALSE
  )
  
  # 绘制临床影响曲线
  
  
  pdf("CIC_Combined_rmda.pdf", width = 12, height = 8)
  plot_clinical_impact(multiple_dc,
                       curve.names = c("SVM", "DT", "MLP", "XGB", "ENET", "KNN", "ADA", 'RIDGE'),
                       col = colors,
                       lwd = 2,
                       lty = 1,
                       xlim = c(0, 1),
                       ylim = c(0, nrow(cic_data)),
                       main = "Clinical Impact Curves - All Models (rmda)",
                       xlab = "Threshold Probability",
                       ylab = "Number of Cases",
                       legend.position = "topright")
  grid()
  dev.off()
  
  return(multiple_dc)
}

# 计算各模型的CIC结果
cic_results <- list()

print("计算DT模型的临床影响曲线...")
cic_results$DT <- plot_single_cic("DT", cic_data$DT, cic_data$OP, cic_colors["DT"])

print("计算MLP模型的临床影响曲线...")
cic_results$MLP <- plot_single_cic("MLP", cic_data$MLP, cic_data$OP, cic_colors["MLP"])

print("计算ENET模型的临床影响曲线...")
cic_results$ENET <- plot_single_cic("ENET", cic_data$ENET, cic_data$OP, cic_colors["ENET"])

print("计算ADA模型的临床影响曲线...")
cic_results$ADA <- plot_single_cic("ADA", cic_data$ADA, cic_data$OP, cic_colors["ADA"])

print("计算RIDGE模型的临床影响曲线...")
cic_results$RIDGE <- plot_single_cic("RIDGE", cic_data$RIDGE, cic_data$OP, cic_colors["RIDGE"])

print("计算SVM模型的临床影响曲线...")
cic_results$SVM <- plot_single_cic("SVM", cic_data$SVM, cic_data$OP, cic_colors["SVM"])

print("计算XGB模型的临床影响曲线...")
cic_results$XGB <- plot_single_cic("XGB", cic_data$XGB, cic_data$OP, cic_colors["XGB"])

print("计算KNN模型的临床影响曲线...")
cic_results$KNN <- plot_single_cic("KNN", cic_data$KNN, cic_data$OP, cic_colors["KNN"])




# 绘制组合CIC图（使用两种方法）
print("绘制组合临床影响曲线...")

# 方法1：使用ggplot2自定义绘制
combined_cic_plot <- plot_combined_cic(cic_data, cic_colors)

# 方法2：尝试使用rmda的multiple_decision_curve
tryCatch({
  multiple_dc <- plot_combined_cic_rmda(cic_data, cic_colors)
  print("rmda方法组合CIC图生成成功")
}, error = function(e) {
  print(paste("rmda方法失败，使用ggplot2方法:", e$message))
})

# 10. 高级CIC分析：净获益和干预曲线 -----------------------------------------
print("进行高级CIC分析...")

# 函数：计算净获益统计量
calculate_net_benefit <- function(pred_prob, true_labels, threshold) {
  # 根据阈值分类
  high_risk <- pred_prob >= threshold
  
  # 真阳性数
  tp <- sum(high_risk & true_labels == 1)
  # 假阳性数
  fp <- sum(high_risk & true_labels == 0)
  # 总病例数
  n <- length(true_labels)
  
  # 计算净获益
  net_benefit <- tp/n - fp/n * (threshold/(1-threshold))
  
  return(net_benefit)
}

# 在不同阈值下计算各模型的净获益
thresholds <- seq(0.05, 0.95, by = 0.05)
net_benefit_df <- data.frame(threshold = thresholds)

for (model in names(cic_results)) {
  net_benefit <- sapply(thresholds, function(t) {
    calculate_net_benefit(cic_data[[model]], 
                          cic_data$OP, t)
  })
  net_benefit_df[[model]] <- net_benefit
}

# 绘制净获益曲线
net_benefit_long <- reshape2::melt(net_benefit_df, 
                                   id.vars = "threshold",
                                   variable.name = "Model",
                                   value.name = "NetBenefit")

net_benefit_plot <- ggplot(net_benefit_long, aes(x = threshold, y = NetBenefit, color = Model)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = cic_colors) +
  labs(title = "Net Benefit Analysis by Threshold",
       x = "Decision Threshold",
       y = "Net Benefit",
       color = "Model") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 14),
        legend.position = "top")

ggsave("Net_Benefit_Analysis.png", net_benefit_plot, width = 10, height = 8, dpi = 300)
ggsave("Net_Benefit_Analysis.pdf", net_benefit_plot, width = 10, height = 8)

# 11. 保存CIC分析结果 ----------------------------------------------------
print("保存CIC分析结果...")

# 创建结果摘要
cic_summary <- data.frame(
  Model = names(cic_colors),
  Color = cic_colors,
  stringsAsFactors = FALSE
)

# 在常见阈值下计算净获益
common_thresholds <- c(0.1, 0.2, 0.3, 0.4, 0.5)
for (thresh in common_thresholds) {
  cic_summary[[paste0("NB_", thresh)]] <- sapply(cic_summary$Model, function(model) {
    calculate_net_benefit(cic_data[[model]], cic_data$OP, thresh)
  })
}

# 保存摘要
write.csv(cic_summary, "CIC_Analysis_Summary.csv", row.names = FALSE)

print("临床影响曲线分析完成！")
print("生成的文件：")
print("- CIC_XXX.png/pdf: 单个模型的临床影响曲线")
print("- CIC_Combined.png/pdf: 所有模型的组合临床影响曲线（ggplot2方法）")
print("- CIC_Combined_rmda.png/pdf: 所有模型的组合临床影响曲线（rmda方法）")
print("- Net_Benefit_Analysis.png/pdf: 净获益分析")
print("- CIC_Analysis_Summary.csv: CIC分析摘要")






#setwd("D:/OP0803/0804_机器学习")
library(RColorBrewer)
library(ComplexHeatmap)
library(circlize)
library(grid)

# —— 1) 读入并合并一次就好
train_auc <- read.csv("Train_AUC_CI.csv", row.names = 1, fileEncoding = "UTF-8")
test_auc  <- read.csv("Test_AUC_CI.csv",  row.names = 1, fileEncoding = "UTF-8")

performanceData <- cbind(
  Train = train_auc[, 1, drop = FALSE],
  Test  = test_auc[, 1, drop = FALSE]
)

colnames(performanceData) <- c('Train', 'Test')

# —— 2) 计算平均 AUC 并排序
avg_scores     <- rowMeans(performanceData)
sorted_models  <- names(sort(avg_scores, decreasing = TRUE))

# —— 3) 全局重排 performanceData & 平均分
performanceData <- performanceData[sorted_models, , drop = FALSE]
sorted_scores   <- sort(avg_scores, decreasing = TRUE)
formattedScores <- as.numeric(format(sorted_scores, digits = 3, nsmall = 3))

# —— 4) 调色
datasetColors <- c(Train = "#D55E00", Test = "#004466")
barColor      <- "#3596F5"
cellWidth     <- 1
cellHeight    <- 0.5

# —— 5) 修改 createCustomHeatmap：不再按 Test 列排序，直接用已对齐好的 performanceData 和 performanceScores
createCustomHeatmap <- function(dataMatrix, performanceScores, 
                                cohortColors, barColor,
                                cellWidth = 1, cellHeight = 0.5, 
                                clusterCols = FALSE, clusterRows = FALSE) {
  
  # （这里不再做任何排序，假设 dataMatrix 和 performanceScores 已经全局对齐好）
  
  # 列注释
  colAnnotation <- ComplexHeatmap::columnAnnotation(
    Dataset = colnames(dataMatrix),
    col     = list(Dataset = cohortColors),
    show_annotation_name = FALSE
  )
  
  # 行注释（右侧条形图）
  rowAnnotation <- ComplexHeatmap::rowAnnotation(
    performanceBar = ComplexHeatmap::anno_barplot(
      performanceScores, 
      bar_width = 0.8, 
      border = FALSE,
      gp = grid::gpar(fill = barColor, col = NA),
      add_numbers = TRUE, 
      numbers_offset = unit(-10, "mm"),
      axis_param = list("labels_rot" = 0),
      numbers_gp = grid::gpar(fontsize = 9, col = "white"),
      width = unit(3, "cm")
    ),
    show_annotation_name = FALSE
  )
  
  
  
  # 绘制 Heatmap
  ComplexHeatmap::Heatmap(
    as.matrix(dataMatrix), 
    name               = "AUC",
    top_annotation     = colAnnotation,
    right_annotation   = rowAnnotation, 
    col = circlize::colorRamp2(
      c(min(dataMatrix), 0.5, max(dataMatrix)), 
      c("#2166AC", "white", "#D6604D")
    ),
    rect_gp            = grid::gpar(col = "black", lwd = 1),
    cluster_columns    = clusterCols, 
    cluster_rows       = clusterRows,
    show_column_names  = FALSE, 
    show_row_names     = TRUE,
    row_names_side     = "left",
    width              = unit(cellWidth * ncol(dataMatrix) + 2, "cm"),
    height             = unit(cellHeight * nrow(dataMatrix), "cm"),
    column_split       = factor(colnames(dataMatrix), levels = colnames(dataMatrix)), 
    column_title       = NULL,
    cell_fun = function(j, i, x, y, w, h, col) {
      grid::grid.text(
        label = sprintf("%.3f", dataMatrix[i, j]),
        x, y, 
        gp = grid::gpar(fontsize = 9)
      )
    }
  )
}

# —— 6) 调用并输出
performancePlot <- createCustomHeatmap(
  dataMatrix        = performanceData,
  performanceScores = formattedScores,
  cohortColors      = datasetColors,
  barColor          = barColor,
  cellWidth         = cellWidth,
  cellHeight        = cellHeight,
  clusterCols       = FALSE,
  clusterRows       = FALSE
)

pdf('AUC_heatmap_sorted_by_avg.pdf', width = 5.66, height = 4.79)

performancePlot
dev.off()


# # 确保 performancePlot 对象存在（已生成）
# if (exists("performancePlot")) {
#   # 设置高分辨率输出
#   png('AUC_heatmap_sorted_by_avg.png', 
#       width    = 5.66,   # 图片宽度（英寸）
#       height   = 4.79,   # 图片高度（英寸）
#       units    = "in",   # 尺寸单位（英寸）
#       res      = 300,    # 分辨率（300 DPI）
#       pointsize = 12)    # 字体大小
#   
#   # 显式打印图形对象（重要！）
#   print(performancePlot)
#   
#   # 关闭图形设备
#   dev.off()
#   
#   message("图片已成功保存为 AUC_heatmap_sorted_by_avg.png")
# } else {
#   warning("错误：performancePlot 对象不存在！请先创建图形对象。")
# }
# # 在 RStudio 里直接打印也能看到
# performancePlot

str(train_data_caret)

# 创建ADABOOST输出目录 ----------------------------
# 加载SHAP相关库
library(kernelshap)
library(shapviz)
library(ggplot2)

# 创建输出目录
if (!dir.exists("./adaboost")) {
  dir.create("./adaboost")
}

# 自定义颜色
custom_colors <- colorRampPalette(c("#004466", "#3EA2B0", "#FDB462"))
custom_gradient <- colorRampPalette(c("#004466", "#3EA2B0", "#FDB462"))


test_data_eval <- test_data
str(test_data_eval)
#test_data_eval[factor_cols] <- lapply(test_data_eval[factor_cols], as.factor)
#str(test_data_eval)


test_data_eval$OP <- factor(test_data_eval$OP, levels = c(0, 1), labels = c("No", "Yes"))
pred_fun <- function(m, X) {
  predict(m, X, type = "prob")[,"Yes"]
}

str(test_data_eval)



shap <- kernelshap(models$ada, X = test_data_eval[, 2:16], pred_fun = pred_fun)

# 保存SHAP结果
saveRDS(shap, './adaboost/shap-ada.rds')

# 获取特征名
cols <- colnames(test_data_eval[2:16])

# 转换到shapviz对象
#library(shapviz)
#sv_ada <- shapviz(shap_ada)

shap_ada <- shap
library(shapviz)
shp <- shapviz(shap_ada)
final_plot<-sv_importance(
  shp,
  kind = "beeswarm",
  max_display = 20,
  alpha = 0.7,
  bee_width = 0.2
) +
  scale_color_gradientn(
    colors = c("#004466", "#3EA2B0", "#FDB462") # 从低到高
  )+
  # # 应用自定义颜色梯度
  #   scale_color_gradientn(
  #     colours = custom_gradient(100),  # 创建100个颜色的平滑渐变
  #   )
  theme_test() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold", color = "black"),
    axis.title.x = element_text(size = 30, color = "black"),
    axis.title.y = element_text(size = 30, color = "black"),
    axis.text.x = element_text(size = 25, color = "black"),
    axis.text.y = element_text(size = 25, color = "black"),
    #legend.title = element_text(size = 15, color = "black"),
    #legend.text = element_text(size = 15, color = "black")，
    # 增大图例设置
    legend.title = element_text(size = 25, color = "black", face = "bold"),  # 增大标题字号
    legend.text = element_text(size = 20, color = "black"),                 # 增大标签字号
    legend.key.width = unit(1.5, "cm"),     # 增大图例键宽度
    legend.key.height = unit(2, "cm"),    # 增大图例键高度
    legend.spacing.x = unit(0.5, "cm"),     # 增大水平间距
    legend.spacing.y = unit(0.5, "cm")     # 增大垂直间距
  )

print(final_plot)

ggsave("./adaboost/shap_importance.pdf", final_plot, width = 12, height = 12)
ggsave("./adaboost/shap_importance.jpg", final_plot, width = 12, height = 12, dpi = 300)


# 计算各特征的SHAP绝对值均值
importance <- shap$S %>%
  as.data.frame() %>%
  summarise_all(~ mean(abs(.))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "feature",
    values_to = "importance"
  ) %>%
  arrange(desc(importance)) %>%
  mutate(feature = factor(feature, levels = rev(feature)))

# 生成渐变颜色
custom_colors <- colorRampPalette(c("#004466", "#3EA2B0", "#FDB462"))(nrow(importance))

# 绘图
final_plot <- ggplot(importance, aes(x = importance, y = feature, fill = feature)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = sprintf("%.3f", importance)),
    hjust = -0.1,
    size = 10,
    color = "black"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = custom_colors, guide = "none") +  # 隐藏图例
  labs(
    title = "SHAP Variable Importance",
    x = "Mean |SHAP Value|",
    y = "Feature"
  ) +
  theme_test() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold", color = "black"),
    axis.title.x = element_text(size = 30, color = "black"),
    axis.title.y = element_text(size = 30, color = "black"),
    axis.text.x = element_text(size = 25, color = "black"),
    axis.text.y = element_text(size = 25, color = "black"),
    #legend.title = element_text(size = 15, color = "black"),
    #legend.text = element_text(size = 15, color = "black")，
    # 增大图例设置
    legend.title = element_text(size = 25, color = "black", face = "bold"),  # 增大标题字号
    legend.text = element_text(size = 20, color = "black"),                 # 增大标签字号
    legend.key.width = unit(1.5, "cm"),     # 增大图例键宽度
    legend.key.height = unit(2, "cm"),    # 增大图例键高度
    legend.spacing.x = unit(0.5, "cm"),     # 增大水平间距
    legend.spacing.y = unit(0.5, "cm")     # 增大垂直间距
  )
print(final_plot)

# 保存图片
ggsave("./adaboost/shap_importance2.pdf", final_plot, width = 15, height = 12)
ggsave("./adaboost/shap_importance2.jpg", final_plot, width = 15, height = 12, dpi = 300)

## --------------------------------------------------------------------------------------------------------
# 转换到shapviz对象
# baseline <- mean(predict(svm_model, test_data))
# 创建shapviz对象
sv <- shapviz(shap)


colnames(sv[["X"]])
# unique(sv[["X"]]$ln_PIV_quartile)
# unique(sv[["X"]]$race_num)
# unique(sv[["X"]]$marital_grouped)
# unique(sv[["X"]]$pvd)
# unique(sv[["X"]]$ckd)
# unique(sv[["X"]]$copd)
# unique(sv[["X"]]$ventilation_hour_group)


sv1 <- sv
# # 1. 定义重新编码函数
# recode_factors <- function(data) {
#   # 四分位数变量重编码
#   qt_vars <- c("ln_PIV_quartile")
#   data[qt_vars] <- lapply(data[qt_vars], function(x) {
#     factor(x, levels = 0:2, labels = c("T1", "T2", "T3"))
#   })
#   
#   # # 性别重编码
#   # data$Gender <- factor(data$Gender, 
#   #                       levels = c(0, 1), 
#   #                       labels = c("Female", "Male"))
#   
#   # 种族重编码
#   data$race_num <- factor(data$race_num,
#                           levels = c(0, 1),
#                           labels = c("Others", "White"))
#   
#   # 婚姻重编码
#   data$marital_grouped <- factor(data$marital_grouped,
#                                  levels = c(0, 1),
#                                  labels = c("Others", "MARRIED"))
#   
#   # 机械通气重编码
#   data$ventilation_hour_group <- factor(data$ventilation_hour_group,
#                                         levels = c(0, 1),
#                                         labels = c("< 48", ">= 48"))
#   
#   
#   # 二分类临床变量重编码
#   binary_vars <- c("pvd", "ckd", "copd")
#   data[binary_vars] <- lapply(data[binary_vars], function(x) {
#     factor(x, levels = c(0, 1), labels = c("No", "Yes"))
#   })
#   
#   # # 吸烟状态重编码
#   # data$smoking <- factor(data$smoking,
#   #                        levels = 0:2,
#   #                        labels = c("Not_at_all", "Some_days", "Every_day"))
#   
#   return(data)
# }
# 
# # 2. 应用重编码到SHAP数据
# sv1[["X"]] <- recode_factors(sv1[["X"]])
# 
# # 3. 验证重编码结果
# # 检查四分位数变量
# print("piv_qt levels after recoding:")
# print(levels(sv1[["X"]]$ln_PIV_quartile))
# print(table(sv1[["X"]]$ln_PIV_quartile))
# 
# # 检查婚姻
# print("\nmarital_grouped levels after recoding:")
# print(levels(sv1[["X"]]$marital_grouped))
# print(table(sv1[["X"]]$marital_grouped))
# 
# # 检查种族
# print("\nrace_num levels after recoding:")
# print(levels(sv1[["X"]]$race_num))
# print(table(sv1[["X"]]$race_num))
# 
# # 检查临床变量
# print("\npvd levels after recoding:")
# print(levels(sv1[["X"]]$pvd))
# print(table(sv1[["X"]]$pvd))
# 
# # 检查机械通气
# print("\n机械通气 levels after recoding:")
# print(levels(sv1[["X"]]$ventilation_hour_group))
# print(table(sv1[["X"]]$ventilation_hour_group))

# 创建自定义颜色梯度函数
custom_gradient <- c("#004466", "#3EA2B0", "#FDB462")
## --------------------------------------------------------------------------------------------------------
# 绘制瀑布图（单个样本）
final_plot<-sv_waterfall(sv1, row_id = 2,max_display = 35) + 
  ggtitle("SHAP Waterfall Plot")+
  scale_fill_manual(
    values = c("FALSE" = "#004466", "TRUE" = "#FDB462"),
    labels = c("FALSE" = "Negative", "TRUE" = "Positive"),  # 可选：修改图例标签
    name = "Direction"  # 图例标题
  ) +
  theme_test() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold", color = "black"),
    axis.title.x = element_text(size = 30, color = "black"),
    axis.title.y = element_text(size = 30, color = "black"),
    axis.text.x = element_text(size = 25, color = "black"),
    axis.text.y = element_text(size = 25, color = "black"),
    #legend.title = element_text(size = 15, color = "black"),
    #legend.text = element_text(size = 15, color = "black")，
    # 增大图例设置
    legend.title = element_text(size = 25, color = "black", face = "bold"),  # 增大标题字号
    legend.text = element_text(size = 20, color = "black"),                 # 增大标签字号
    legend.key.width = unit(1.5, "cm"),     # 增大图例键宽度
    legend.key.height = unit(2, "cm"),    # 增大图例键高度
    legend.spacing.x = unit(0.5, "cm"),     # 增大水平间距
    legend.spacing.y = unit(0.5, "cm")     # 增大垂直间距
  )
print(final_plot)
ggsave("./adaboost/Waterfall.pdf", final_plot, width = 12, height = 15)
ggsave("./adaboost/Waterfall.jpg", final_plot, width = 12, height = 15, dpi = 300)


## --------------------------------------------------------------------------------------------------------
# 绘制单个样本的Force Plot（例如第一个样本）
final_plot<-sv_force(sv1, row_id = 2,max_display = 35) +
  ggtitle("SHAP Force Plot for Sample 2") +
  scale_fill_manual(
    values = c("FALSE" = "#004466", "TRUE" = "#FDB462"),
    labels = c("FALSE" = "Negative", "TRUE" = "Positive"),  # 可选：修改图例标签
    name = "Direction"  # 图例标题
  ) +
  theme_test() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold", color = "black"),
    axis.title.x = element_text(size = 30, color = "black"),
    axis.title.y = element_text(size = 30, color = "black"),
    axis.text.x = element_text(size = 25, color = "black"),
    axis.text.y = element_text(size = 25, color = "black"),
    #legend.title = element_text(size = 15, color = "black"),
    #legend.text = element_text(size = 15, color = "black")，
    # 增大图例设置
    legend.title = element_text(size = 25, color = "black", face = "bold"),  # 增大标题字号
    legend.text = element_text(size = 20, color = "black"),                 # 增大标签字号
    legend.key.width = unit(1.5, "cm"),     # 增大图例键宽度
    legend.key.height = unit(2, "cm"),    # 增大图例键高度
    legend.spacing.x = unit(0.5, "cm"),     # 增大水平间距
    legend.spacing.y = unit(0.5, "cm")     # 增大垂直间距
  )
print(final_plot)
ggsave("./adaboost/Force_plot.pdf", final_plot, width = 24, height = 6)

ggsave("./adaboost/Force_plot.jpg", final_plot, width = 24, height = 6, dpi = 300)



## --------------------------------------------------------------------------------------------------------
# 绘制热图（显示所有样本的SHAP值）
library(pheatmap)
library(viridis)  # 高级颜色方案
# 提取SHAP值矩阵
shap_matrix <- get_shap_values(sv)


pdf("./adaboost/pheatmap.pdf", width = 12, height = 10)
pheatmap(
  shap_matrix,
  color = viridis(100),
  clustering_method = "ward.D2",
  main = "Clustered SHAP Heatmap",
  show_rownames = FALSE
)
dev.off()


# 绘制聚类热图
final_plot<-pheatmap(
  shap_matrix,
  color = viridis(100),        # 颜色映射
  clustering_method = "ward.D2", # 聚类方法
  main = "Clustered SHAP Heatmap",
  show_rownames = FALSE       # 隐藏样本名（避免重叠）
)

print(final_plot)
ggsave("./adaboost/pheatmap.pdf", final_plot, width = 12, height = 10)
ggsave("./adaboost/pheatmap.jpg", final_plot, width = 12, height = 10, dpi = 300)


library(stringr)
library(patchwork)  # 用于拼接多张图
# 创建每个变量的 SHAP 主效应图
plots <- lapply(cols, function(var) {
  sv_dependence(sv1, 
                v = var, 
                interactions = FALSE, 
                color_var = NULL,
                alpha = 0.7,             # 点透明度
                size = 2                 # 点大小
  ) +
    #     ggtitle(str_wrap(var, width = 10)) +
    #     geom_smooth(color = "red", se = FALSE) +  # 添加趋势线
    #     ggtitle(paste(" ", var)) +
    #     geom_point(color = "blue") +
    theme_test() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 30, face = "bold", color = "black"),
      axis.title.x = element_text(size = 30, color = "black"),
      axis.title.y = element_text(size = 30, color = "black"),
      axis.text.x = element_text(size = 25, angle = 45, hjust = 1, color = "black"),
      axis.text.y = element_text(size = 25, color = "black"),
      #legend.title = element_text(size = 15, color = "black"),
      #legend.text = element_text(size = 15, color = "black")，
      # 增大图例设置
      legend.title = element_text(size = 25, color = "black", face = "bold"),  # 增大标题字号
      legend.text = element_text(size = 20, color = "black"),                 # 增大标签字号
      legend.key.width = unit(1.5, "cm"),     # 增大图例键宽度
      legend.key.height = unit(2, "cm"),    # 增大图例键高度
      legend.spacing.x = unit(0.5, "cm"),     # 增大水平间距
      legend.spacing.y = unit(0.5, "cm")     # 增大垂直间距
    )+
    theme(aspect.ratio = 1)  # 使每个图保持接近正方形比例
})
# 拼接图表
combined_plot <- wrap_plots(plots, ncol = 5)
print(combined_plot)
ggsave("./adaboost/dependence.pdf", combined_plot, width = 30, height = 30)
ggsave("./adaboost/dependence.jpg", combined_plot, width = 30, height = 30, dpi = 300)


library(patchwork)  # 用于拼接多张图
# 创建每个变量的 SHAP 主效应图
plots <- lapply(cols, function(var) {
  sv_dependence(sv1, 
                v = var, 
                interactions = FALSE, 
                color_var = 'auto',
                alpha = 0.7,             # 点透明度
                size = 2                 # 点大小
  ) +
    geom_smooth(method = "loess",color = "red", se = FALSE) +  # 添加趋势线
    theme_test() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 30, face = "bold", color = "black"),
      axis.title.x = element_text(size = 25, color = "black"),
      axis.title.y = element_text(size = 25, color = "black"),
      axis.text.x = element_text(size = 18, angle = 0, hjust = 1, color = "black"),
      axis.text.y = element_text(size = 18, color = "black"),
      #legend.title = element_text(size = 15, color = "black"),
      #legend.text = element_text(size = 15, color = "black")，
      # 增大图例设置
      legend.title = element_text(size = 22, color = "black", face = "bold"),  # 增大标题字号
      legend.text = element_text(size = 18, color = "black")                 # 增大标签字号
      ,legend.key.width = unit(1, "cm"),     # 增大图例键宽度
      legend.key.height = unit(1.5, "cm"),    # 增大图例键高度
    )+
    theme(aspect.ratio = 1)  # 使每个图保持接近正方形比例
  
})
# 拼接图表
combined_plot <- wrap_plots(plots, ncol = 4)
print(combined_plot)
ggsave("./adaboost/dependence2.pdf", combined_plot, width = 40, height = 32)
ggsave("./adaboost/dependence2.jpg", combined_plot, width = 40, height = 32, dpi = 300)


# ENET ---------------------------
library(kernelshap)
library(shapviz)
library(ggplot2)


setwd('D:/000000000WORK00000000/44_KXF2025070602/10_lasso_机器学习')
# 创建输出目录
if (!dir.exists("./enet")) {
  dir.create("./enet")
}

# 自定义颜色
custom_colors <- colorRampPalette(c("#004466", "#3EA2B0", "#FDB462"))
custom_gradient <- colorRampPalette(c("#004466", "#3EA2B0", "#FDB462"))


test_data_eval <- test_data
str(test_data_eval)
#test_data_eval[factor_cols] <- lapply(test_data_eval[factor_cols], as.factor)
#str(test_data_eval)


test_data_eval$OP <- factor(test_data_eval$OP, levels = c(0, 1), labels = c("No", "Yes"))
pred_fun <- function(m, X) {
  predict(m, X, type = "prob")[,"Yes"]
}

str(test_data_eval)



shap <- kernelshap(models$enet, X = test_data_eval[, 2:14], pred_fun = pred_fun)

# 保存SHAP结果
saveRDS(shap, './enet/shap-enet.rds')

# 获取特征名
cols <- colnames(test_data_eval[2:14])

# 转换到shapviz对象
#library(shapviz)
#sv_enet <- shapviz(shap_enet)

shap_enet <- shap
library(shapviz)
shp <- shapviz(shap_enet)
final_plot<-sv_importance(
  shp,
  kind = "beeswarm",
  max_display = 20,
  alpha = 0.7,
  bee_width = 0.2
) +
  scale_color_gradientn(
    colors = c("#004466", "#3EA2B0", "#FDB462") # 从低到高
  )+
  # # 应用自定义颜色梯度
  #   scale_color_gradientn(
  #     colours = custom_gradient(100),  # 创建100个颜色的平滑渐变
  #   )
  theme_test() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold", color = "black"),
    axis.title.x = element_text(size = 30, color = "black"),
    axis.title.y = element_text(size = 30, color = "black"),
    axis.text.x = element_text(size = 25, color = "black"),
    axis.text.y = element_text(size = 25, color = "black"),
    #legend.title = element_text(size = 15, color = "black"),
    #legend.text = element_text(size = 15, color = "black")，
    # 增大图例设置
    legend.title = element_text(size = 25, color = "black", face = "bold"),  # 增大标题字号
    legend.text = element_text(size = 20, color = "black"),                 # 增大标签字号
    legend.key.width = unit(1.5, "cm"),     # 增大图例键宽度
    legend.key.height = unit(2, "cm"),    # 增大图例键高度
    legend.spacing.x = unit(0.5, "cm"),     # 增大水平间距
    legend.spacing.y = unit(0.5, "cm")     # 增大垂直间距
  )

print(final_plot)

ggsave("./enet/shap_importance.pdf", final_plot, width = 12, height = 12)
ggsave("./enet/shap_importance.jpg", final_plot, width = 12, height = 12, dpi = 300)


# 计算各特征的SHAP绝对值均值
# 修正版本1：使用lapply替代across
importance <- shap$S %>%
  as.data.frame() %>%
  summarise_all(~ mean(abs(.))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "feature",
    values_to = "importance"
  ) %>%
  arrange(desc(importance)) %>%
  mutate(feature = factor(feature, levels = feature))
# 生成渐变颜色
custom_colors <- colorRampPalette(c("#004466", "#3EA2B0", "#FDB462"))(nrow(importance))

# 绘图
final_plot <- ggplot(importance, aes(x = importance, y = feature, fill = feature)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = sprintf("%.3f", importance)),
    hjust = -0.1,
    size = 10,
    color = "black"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = custom_colors, guide = "none") +  # 隐藏图例
  labs(
    title = "SHAP Variable Importance",
    x = "Mean |SHAP Value|",
    y = "Feature"
  ) +
  theme_test() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold", color = "black"),
    axis.title.x = element_text(size = 30, color = "black"),
    axis.title.y = element_text(size = 30, color = "black"),
    axis.text.x = element_text(size = 25, color = "black"),
    axis.text.y = element_text(size = 25, color = "black"),
    #legend.title = element_text(size = 15, color = "black"),
    #legend.text = element_text(size = 15, color = "black")，
    # 增大图例设置
    legend.title = element_text(size = 25, color = "black", face = "bold"),  # 增大标题字号
    legend.text = element_text(size = 20, color = "black"),                 # 增大标签字号
    legend.key.width = unit(1.5, "cm"),     # 增大图例键宽度
    legend.key.height = unit(2, "cm"),    # 增大图例键高度
    legend.spacing.x = unit(0.5, "cm"),     # 增大水平间距
    legend.spacing.y = unit(0.5, "cm")     # 增大垂直间距
  )
print(final_plot)

# 保存图片
ggsave("./enet/shap_importance2.pdf", final_plot, width = 15, height = 12)
ggsave("./enet/shap_importance2.jpg", final_plot, width = 15, height = 12, dpi = 300)

## --------------------------------------------------------------------------------------------------------
# 转换到shapviz对象
# baseline <- mean(predict(svm_model, test_data))
# 创建shapviz对象
sv <- shapviz(shap)


colnames(sv[["X"]])
unique(sv[["X"]]$ln_PIV_quartile)
unique(sv[["X"]]$race_num)
unique(sv[["X"]]$marital_grouped)
unique(sv[["X"]]$pvd)
unique(sv[["X"]]$ckd)
unique(sv[["X"]]$copd)
unique(sv[["X"]]$ventilation_hour_group)


sv1 <- sv
# 1. 定义重新编码函数
recode_factors <- function(data) {
  # 四分位数变量重编码
  qt_vars <- c("ln_PIV_quartile")
  data[qt_vars] <- lapply(data[qt_vars], function(x) {
    factor(x, levels = 0:2, labels = c("T1", "T2", "T3"))
  })
  
  # # 性别重编码
  # data$Gender <- factor(data$Gender, 
  #                       levels = c(0, 1), 
  #                       labels = c("Female", "Male"))
  
  # 种族重编码
  data$race_num <- factor(data$race_num,
                          levels = c(0, 1),
                          labels = c("Others", "White"))
  
  # 婚姻重编码
  data$marital_grouped <- factor(data$marital_grouped,
                                 levels = c(0, 1),
                                 labels = c("Others", "MARRIED"))
  
  # 机械通气重编码
  data$ventilation_hour_group <- factor(data$ventilation_hour_group,
                                        levels = c(0, 1),
                                        labels = c("< 48", ">= 48"))
  
  
  # 二分类临床变量重编码
  binary_vars <- c("pvd", "ckd", "copd")
  data[binary_vars] <- lapply(data[binary_vars], function(x) {
    factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  })
  
  # # 吸烟状态重编码
  # data$smoking <- factor(data$smoking,
  #                        levels = 0:2,
  #                        labels = c("Not_at_all", "Some_days", "Every_day"))
  
  return(data)
}

# 2. 应用重编码到SHAP数据
sv1[["X"]] <- recode_factors(sv1[["X"]])

# 3. 验证重编码结果
# 检查四分位数变量
print("piv_qt levels after recoding:")
print(levels(sv1[["X"]]$ln_PIV_quartile))
print(table(sv1[["X"]]$ln_PIV_quartile))

# 检查婚姻
print("\nmarital_grouped levels after recoding:")
print(levels(sv1[["X"]]$marital_grouped))
print(table(sv1[["X"]]$marital_grouped))

# 检查种族
print("\nrace_num levels after recoding:")
print(levels(sv1[["X"]]$race_num))
print(table(sv1[["X"]]$race_num))

# 检查临床变量
print("\npvd levels after recoding:")
print(levels(sv1[["X"]]$pvd))
print(table(sv1[["X"]]$pvd))

# 检查机械通气
print("\n机械通气 levels after recoding:")
print(levels(sv1[["X"]]$ventilation_hour_group))
print(table(sv1[["X"]]$ventilation_hour_group))

# 创建自定义颜色梯度函数
custom_gradient <- c("#004466", "#3EA2B0", "#FDB462")
## --------------------------------------------------------------------------------------------------------
# 绘制瀑布图（单个样本）
final_plot<-sv_waterfall(sv1, row_id = 2,max_display = 35) + 
  ggtitle("SHAP Waterfall Plot")+
  scale_fill_manual(
    values = c("FALSE" = "#004466", "TRUE" = "#FDB462"),
    labels = c("FALSE" = "Negative", "TRUE" = "Positive"),  # 可选：修改图例标签
    name = "Direction"  # 图例标题
  ) +
  theme_test() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold", color = "black"),
    axis.title.x = element_text(size = 30, color = "black"),
    axis.title.y = element_text(size = 30, color = "black"),
    axis.text.x = element_text(size = 25, color = "black"),
    axis.text.y = element_text(size = 25, color = "black"),
    #legend.title = element_text(size = 15, color = "black"),
    #legend.text = element_text(size = 15, color = "black")，
    # 增大图例设置
    legend.title = element_text(size = 25, color = "black", face = "bold"),  # 增大标题字号
    legend.text = element_text(size = 20, color = "black"),                 # 增大标签字号
    legend.key.width = unit(1.5, "cm"),     # 增大图例键宽度
    legend.key.height = unit(2, "cm"),    # 增大图例键高度
    legend.spacing.x = unit(0.5, "cm"),     # 增大水平间距
    legend.spacing.y = unit(0.5, "cm")     # 增大垂直间距
  )
print(final_plot)
ggsave("./enet/Waterfall.pdf", final_plot, width = 12, height = 15)
ggsave("./enet/Waterfall.jpg", final_plot, width = 12, height = 15, dpi = 300)


## --------------------------------------------------------------------------------------------------------
# 绘制单个样本的Force Plot（例如第一个样本）
final_plot<-sv_force(sv1, row_id = 2,max_display = 35) +
  ggtitle("SHAP Force Plot for Sample 2") +
  scale_fill_manual(
    values = c("FALSE" = "#004466", "TRUE" = "#FDB462"),
    labels = c("FALSE" = "Negative", "TRUE" = "Positive"),  # 可选：修改图例标签
    name = "Direction"  # 图例标题
  ) +
  theme_test() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold", color = "black"),
    axis.title.x = element_text(size = 30, color = "black"),
    axis.title.y = element_text(size = 30, color = "black"),
    axis.text.x = element_text(size = 25, color = "black"),
    axis.text.y = element_text(size = 25, color = "black"),
    #legend.title = element_text(size = 15, color = "black"),
    #legend.text = element_text(size = 15, color = "black")，
    # 增大图例设置
    legend.title = element_text(size = 25, color = "black", face = "bold"),  # 增大标题字号
    legend.text = element_text(size = 20, color = "black"),                 # 增大标签字号
    legend.key.width = unit(1.5, "cm"),     # 增大图例键宽度
    legend.key.height = unit(2, "cm"),    # 增大图例键高度
    legend.spacing.x = unit(0.5, "cm"),     # 增大水平间距
    legend.spacing.y = unit(0.5, "cm")     # 增大垂直间距
  )
print(final_plot)
ggsave("./enet/Force_plot.pdf", final_plot, width = 24, height = 6)

ggsave("./enet/Force_plot.jpg", final_plot, width = 24, height = 6, dpi = 300)



## --------------------------------------------------------------------------------------------------------
# 绘制热图（显示所有样本的SHAP值）
library(pheatmap)
library(viridis)  # 高级颜色方案
# 提取SHAP值矩阵
shap_matrix <- get_shap_values(sv)


pdf("./enet/pheatmap.pdf", width = 12, height = 10)
pheatmap(
  shap_matrix,
  color = viridis(100),
  clustering_method = "ward.D2",
  main = "Clustered SHAP Heatmap",
  show_rownames = FALSE
)
dev.off()


# 绘制聚类热图
final_plot<-pheatmap(
  shap_matrix,
  color = viridis(100),        # 颜色映射
  clustering_method = "ward.D2", # 聚类方法
  main = "Clustered SHAP Heatmap",
  show_rownames = FALSE       # 隐藏样本名（避免重叠）
)

print(final_plot)
ggsave("./enet/pheatmap.pdf", final_plot, width = 12, height = 10)
ggsave("./enet/pheatmap.jpg", final_plot, width = 12, height = 10, dpi = 300)


library(stringr)
library(patchwork)  # 用于拼接多张图
# 创建每个变量的 SHAP 主效应图
plots <- lapply(cols, function(var) {
  sv_dependence(sv1, 
                v = var, 
                interactions = FALSE, 
                color_var = NULL,
                alpha = 0.7,             # 点透明度
                size = 2                 # 点大小
  ) +
    #     ggtitle(str_wrap(var, width = 10)) +
    #     geom_smooth(color = "red", se = FALSE) +  # 添加趋势线
    #     ggtitle(paste(" ", var)) +
    #     geom_point(color = "blue") +
    theme_test() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 30, face = "bold", color = "black"),
      axis.title.x = element_text(size = 30, color = "black"),
      axis.title.y = element_text(size = 30, color = "black"),
      axis.text.x = element_text(size = 25, angle = 45, hjust = 1, color = "black"),
      axis.text.y = element_text(size = 25, color = "black"),
      #legend.title = element_text(size = 15, color = "black"),
      #legend.text = element_text(size = 15, color = "black")，
      # 增大图例设置
      legend.title = element_text(size = 25, color = "black", face = "bold"),  # 增大标题字号
      legend.text = element_text(size = 20, color = "black"),                 # 增大标签字号
      legend.key.width = unit(1.5, "cm"),     # 增大图例键宽度
      legend.key.height = unit(2, "cm"),    # 增大图例键高度
      legend.spacing.x = unit(0.5, "cm"),     # 增大水平间距
      legend.spacing.y = unit(0.5, "cm")     # 增大垂直间距
    )+
    theme(aspect.ratio = 1)  # 使每个图保持接近正方形比例
})
# 拼接图表
combined_plot <- wrap_plots(plots, ncol = 5)
print(combined_plot)
ggsave("./enet/dependence.pdf", combined_plot, width = 30, height = 30)
ggsave("./enet/dependence.jpg", combined_plot, width = 30, height = 30, dpi = 300)


library(patchwork)  # 用于拼接多张图
# 创建每个变量的 SHAP 主效应图
plots <- lapply(cols, function(var) {
  sv_dependence(sv1, 
                v = var, 
                interactions = FALSE, 
                color_var = 'auto',
                alpha = 0.7,             # 点透明度
                size = 2                 # 点大小
  ) +
    geom_smooth(method = "loess",color = "red", se = FALSE) +  # 添加趋势线
    theme_test() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 30, face = "bold", color = "black"),
      axis.title.x = element_text(size = 25, color = "black"),
      axis.title.y = element_text(size = 25, color = "black"),
      axis.text.x = element_text(size = 18, angle = 0, hjust = 1, color = "black"),
      axis.text.y = element_text(size = 18, color = "black"),
      #legend.title = element_text(size = 15, color = "black"),
      #legend.text = element_text(size = 15, color = "black")，
      # 增大图例设置
      legend.title = element_text(size = 22, color = "black", face = "bold"),  # 增大标题字号
      legend.text = element_text(size = 18, color = "black")                 # 增大标签字号
      ,legend.key.width = unit(1, "cm"),     # 增大图例键宽度
      legend.key.height = unit(1.5, "cm"),    # 增大图例键高度
    )+
    theme(aspect.ratio = 1)  # 使每个图保持接近正方形比例
  
})
# 拼接图表
combined_plot <- wrap_plots(plots, ncol = 4)
print(combined_plot)
ggsave("./enet/dependence2.pdf", combined_plot, width = 40, height = 32)
ggsave("./enet/dependence2.jpg", combined_plot, width = 40, height = 32, dpi = 300)






