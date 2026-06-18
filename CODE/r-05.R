# 敏感性分析 1 ------
rm(list = ls())
setwd('G:/返修/KXF2025040303')
if (! dir.exists("./10_敏感性分析")){
  dir.create("./10_敏感性分析")
}
setwd("./10_敏感性分析")

library(dplyr)
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

rt <- rt %>%
  filter(Age >= 60)
summary(rt$Age)

colnames(rt)

# 检查新数据集
str(rt)
head(rt)

# 加载必要的包


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
setwd("G:/返修/KXF2025040303/10_敏感性分析/01_50岁以上")
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
setwd("G:/返修/KXF2025040303/10_敏感性分析/01_50岁以上")
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
setwd("G:/返修/KXF2025040303/10_敏感性分析/01_50岁以上")
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












# 敏感性分析 2 ------
rm(list = ls())


library(nhanesA)
library(tableone)
library(survey)
library(foreign)
library(plyr)
library(dplyr)#rename
#BiocManager::install("openxlsx")
library(foreign)    
library(dplyr)
library(purrr)
library("survey") 
library(tidyverse) 
library("broom")
library(haven)
setwd("G:/返修/KXF2025040303/data/DEMO")
demo_fs = list.files('G:/返修/KXF2025040303/data/DEMO/',pattern = '^demo',ignore.case = TRUE)

for(file in demo_fs){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(gsub(" ","",substr(file, 1, perpos-1)),read.xport(paste0('./',file)))
}

## # 总年数 = 2+2+2+2+3.2 = 11.2
# 标准周期（2年）权重因子 = 2/11.2 = 1/5.6
# 特殊周期（3.2年）权重因子 = 3.2/11.2 = 1.6/5.6
if(T){
  DEMO_D$Year <- "2005-2006"
  DEMO_E$Year <- "2007-2008"
  DEMO_F$Year <- "2009-2010"
  DEMO_H$Year <- "2013-2014"
  DEMO_P$Year <- "2017-2020"
}

## 权重
## WTINT2YR in-home interview
## WTMEC2YR in-home interview + MEC检查
## 相应子样本权重  空腹甘油三酯 WTSAF2YR
## WTDRD1/WTDRD2 24-hour dietary recall 24小时饮食回忆（不属于子样本变量）
if(T){
  DEMO_D$WTMEC11YR <- DEMO_D$WTMEC2YR*(1/5.6)
  DEMO_E$WTMEC11YR <- DEMO_E$WTMEC2YR*(1/5.6)
  DEMO_F$WTMEC11YR <- DEMO_F$WTMEC2YR*(1/5.6)
  DEMO_H$WTMEC11YR <- DEMO_H$WTMEC2YR*(1/5.6)
  DEMO_P$WTMEC11YR <- DEMO_P$WTMECPRP*(1.6/5.6)}

data_ID <- c('SEQN')
wt_list <- c('WTMEC11YR',"SDMVPSU","SDMVSTRA")
demo_list <- c('RIDAGEYR','RIAGENDR',"INDFMPIR",'RIDRETH1','DMDEDUC2')
#'RIDAGEYR'年龄,'RIDRETH1'种族,'RIAGENDR'性别,"DMDMARTL"婚姻,'DMDEDUC2'教育,')#
select_list <- c(data_ID,wt_list,demo_list)
DEMO_mat <- rbind(DEMO_D[,select_list],DEMO_E[,select_list],DEMO_F[,select_list],
                  DEMO_H[,select_list],DEMO_P[,select_list])


DEMO_dat <- plyr::rename(DEMO_mat,c(
  RIAGENDR="Gender",
  RIDAGEYR="Age",
  INDFMPIR="PIR",
  DMDEDUC2="Educational_level",
  RIDRETH1="Race"))
####暴露###
#####利尿剂  质子泵抑制剂  RXDUSE 名称RXDDRUG   代码RXDDRGID
setwd("G:/返修/KXF2025040303/data/RXQ_RX")
ques_fs = list.files('./',pattern = '^.',ignore.case = TRUE)

for(file in ques_fs){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(gsub(" ","",substr(file, 1, perpos-1)),read.xport(paste0('./',file)))
}
SMQ_list <- c('SEQN',"RXDUSE","RXDDRUG","RXDDRGID")
SMQ_dat <- rbind(RXQ_RX_D[,SMQ_list],RXQ_RX_E[,SMQ_list],RXQ_RX_F[,SMQ_list],
                 RXQ_RX_H[,SMQ_list],RXQ_RX_P[,SMQ_list])
RXD_dat <- dplyr::rename(SMQ_dat,
                         RXD = RXDUSE)
table(RXD_dat$RXD,useNA = 'always')
table(RXD_dat$RXDDRUG)
special_values <- c("d00253","d00260","d00192","d00070","d00179","d00373","d04815","d00169",
                    "d00396","d00161")
filtered_df1 <- RXD_dat[RXD_dat$RXDDRGID %in% special_values, ]
table(filtered_df1$RXDDRGID,useNA = 'always')

# 删除SEQN列有重复的行，仅保留首次出现的行
filtered_df1 <- filtered_df1[!duplicated(filtered_df1$SEQN), ]

# 查看结果
dim(filtered_df1)  # 查看数据框的维度
table(duplicated(filtered_df1$SEQN))  # 验证是否还有重复行
colnames(filtered_df1)
filtered_df1$new_column1 <- 1

test1 <- left_join(DEMO_mat,filtered_df1,by="SEQN") %>%
  select(SEQN,new_column1)
test1$new_column1[is.na(test1$new_column1)] <- 0
colnames(test1)
table(test1$new_column1,useNA = 'always')
filtered_df1=test1[,c("SEQN","new_column1")]
table(filtered_df1$new_column1,useNA = 'always')


special_values <- c("d00325","d03828","d04749","d04514","d04448")
filtered_df2 <- RXD_dat[RXD_dat$RXDDRGID %in% special_values, ]
table(filtered_df2$RXDDRGID,useNA = 'always')

# 删除SEQN列有重复的行，仅保留首次出现的行
filtered_df2 <- filtered_df2[!duplicated(filtered_df2$SEQN), ]

# 查看结果
dim(filtered_df2)  # 查看数据框的维度
table(duplicated(filtered_df2$SEQN))  # 验证是否还有重复行
colnames(filtered_df2)
filtered_df2$new_column2 <- 1
test2 <- left_join(DEMO_mat,filtered_df2,by="SEQN") %>%
  select(SEQN,new_column2)
test2$new_column2[is.na(test2$new_column2)] <- 0
colnames(test2)
table(test2$new_column2,useNA = 'always')
filtered_df2=test2[,c("SEQN","new_column2")]
table(filtered_df2$new_column2,useNA = 'always')












setwd('G:/返修/KXF2025040303')
if (! dir.exists("./10_敏感性分析")){
  dir.create("./10_敏感性分析")
}
setwd("./10_敏感性分析")


# 指定文件路径并读取
nhance_data <- read.csv(file = '../01_clean/nhanes_mat_choose.csv')

nhance_data <- left_join(nhance_data,filtered_df1,by="SEQN") 
nhance_data <- nhance_data %>%
  filter(new_column1 != 1)
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


summary(rt$Age)

colnames(rt)

# 检查新数据集
str(rt)
head(rt)

# 加载必要的包


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
setwd("G:/返修/KXF2025040303/10_敏感性分析/02_排除利尿剂使用/")
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
setwd("G:/返修/KXF2025040303/10_敏感性分析/02_排除利尿剂使用")
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
setwd("G:/返修/KXF2025040303/10_敏感性分析/02_排除利尿剂使用")
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














#敏感性分析3-------------------------

setwd('G:/返修/KXF2025040303')
if (! dir.exists("./10_敏感性分析")){
  dir.create("./10_敏感性分析")
}
setwd("./10_敏感性分析")


# 指定文件路径并读取
nhance_data <- read.csv(file = '../01_clean/nhanes_mat_choose.csv')

nhance_data <- left_join(nhance_data,filtered_df2,by="SEQN") 
nhance_data <- nhance_data %>%
  filter(new_column2 != 1)
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


summary(rt$Age)

colnames(rt)

# 检查新数据集
str(rt)
head(rt)

# 加载必要的包


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
setwd("G:/返修/KXF2025040303/10_敏感性分析/03_排除质子泵抑制剂/")
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
setwd("G:/返修/KXF2025040303/10_敏感性分析/03_排除质子泵抑制剂")
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
setwd("G:/返修/KXF2025040303/10_敏感性分析/03_排除质子泵抑制剂")
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










# 敏感性分析4------------------------
rm(list = ls())
setwd('G:/返修/KXF2025040303')
if (! dir.exists("./10_敏感性分析")){
  dir.create("./10_敏感性分析")
}
setwd("./10_敏感性分析")

library(dplyr)
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


summary(rt$Age)

colnames(rt)

# 检查新数据集
str(rt)
head(rt)

# 加载必要的包


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


# ==================== 多分类逆概率加权（IPTW）分析 ====================
# 目的：平衡 MDS 三组间的协变量（Age_group, Gender, Race, PIR, Marital_status, BMI）
#       然后评估 MDS 与 OP 的关联，同时保留 NHANES 抽样权重

library(WeightIt)
library(survey)
library(cobalt)

# 1. 准备数据：确保处理变量为因子，结局为0/1，无缺失关键变量
rt_clean <- rt %>%
  filter(!is.na(MDS), !is.na(OP)) %>%        # 删除 MDS 或 OP 缺失的样本
  droplevels()

# 2. 计算多分类倾向评分权重（IPTW）
#    method = "ps" 使用多项逻辑回归估计倾向评分
#    estimand = "ATE" 表示平均处理效应（比较各组与总体平均）
#    如果想以某组为参照（如 Low），可设置 estimand = "ATT"，并指定 focal = "Low"
#    sampw 参数纳入 NHANES 最终抽样权重（使权重同时代表全国人口）
iptw_obj <- weightit(
  MDS ~ Age_group + Gender + Race + PIR + Marital_status + BMI,
  data = rt_clean,
  method = "ps",
  estimand = "ATE",                    # 或 "ATT"（如需指定参照组）
  # focal = "Low",                     # 若用 ATT，指定参照组
  sampw = rt_clean$WTMEC11YR           # NHANES 抽样权重（变量名请确认）
)

# 3. 检查平衡性（标准化均差 SMD 应 <0.1）
bal.tab(iptw_obj, un = TRUE)
love.plot(iptw_obj, threshold = 0.1)

# 4. 将 IPTW 权重加到数据中（最终权重 = IPTW权重 × 调查权重？）
#    WeightIt 的 sampw 已经将调查权重整合进了最终权重，所以直接使用 iptw_obj$weights 即可
rt_clean$iptw_weight <- iptw_obj$weights
# 多分类逆概率加权（Multinomial IPTW）
# 5. 构建加权 survey 设计对象（使用 IPTW 权重，但保留抽样设计信息）
design_iptw <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~iptw_weight,      # 这里已经包含了调查权重 × IPTW 调整
  data = rt_clean,
  nest = TRUE,
  survey.lonely.psu = "adjust"
)

# 6. 拟合加权 logistic 回归（平衡后）
model_iptw <- svyglm(OP ~ MDS, design = design_iptw, family = quasibinomial)

# 7. 计算 OR 及 95% CI
summary_iptw <- summary(model_iptw)
coef_iptw <- summary_iptw$coefficients
or_iptw <- exp(coef_iptw[, "Estimate"])
ci_iptw <- exp(confint(model_iptw))
result_iptw <- data.frame(
  Variable = rownames(coef_iptw),
  OR = or_iptw,
  CI_lower = ci_iptw[, 1],
  CI_upper = ci_iptw[, 2],
  P_value = coef_iptw[, "Pr(>|t|)"]
)
print(result_iptw)

# 保存结果
write.csv(result_iptw, "./04_PSM/IPTW_MDS_OP_result1.csv", row.names = FALSE)


# 6. 拟合加权 logistic 回归（平衡后）
model_iptw <- svyglm(OP ~ MDS+ Age_group + Gender + Race + PIR + Marital_status + BMI, design = design_iptw, family = quasibinomial)

# 7. 计算 OR 及 95% CI
summary_iptw <- summary(model_iptw)
coef_iptw <- summary_iptw$coefficients
or_iptw <- exp(coef_iptw[, "Estimate"])
ci_iptw <- exp(confint(model_iptw))
result_iptw <- data.frame(
  Variable = rownames(coef_iptw),
  OR = or_iptw,
  CI_lower = ci_iptw[, 1],
  CI_upper = ci_iptw[, 2],
  P_value = coef_iptw[, "Pr(>|t|)"]
)
print(result_iptw)

# 保存结果
write.csv(result_iptw, "./04_PSM/IPTW_MDS_OP_result2.csv", row.names = FALSE)



# 6. 拟合加权 logistic 回归（平衡后）
model_iptw <- svyglm(OP ~ MDS+ Age_group + Gender + Race + PIR + Marital_status + BMI
                     +Stroke+Hypertension+Diabetes+TG+TC+HbA1C+MAGN+CALC+PHOS, design = design_iptw, family = quasibinomial)

# 7. 计算 OR 及 95% CI
summary_iptw <- summary(model_iptw)
coef_iptw <- summary_iptw$coefficients
or_iptw <- exp(coef_iptw[, "Estimate"])
ci_iptw <- exp(confint(model_iptw))
result_iptw <- data.frame(
  Variable = rownames(coef_iptw),
  OR = or_iptw,
  CI_lower = ci_iptw[, 1],
  CI_upper = ci_iptw[, 2],
  P_value = coef_iptw[, "Pr(>|t|)"]
)
print(result_iptw)

# 保存结果
write.csv(result_iptw, "./04_PSM/IPTW_MDS_OP_result3.csv", row.names = FALSE)




# 敏感性分析4------------------------
rm(list = ls())
setwd('G:/返修/KXF2025040303')
if (! dir.exists("./10_敏感性分析")){
  dir.create("./10_敏感性分析")
}
setwd("./10_敏感性分析")

library(dplyr)
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


summary(rt$Age)

colnames(rt)

# 检查新数据集
str(rt)
head(rt)

# 加载必要的包


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


# ==================== 多分类逆概率加权（IPTW）分析 ====================
# 目的：平衡 MDS 三组间的协变量（Age_group, Gender, Race, PIR, Marital_status, BMI）
#       然后评估 MDS 与 OP 的关联，同时保留 NHANES 抽样权重

library(WeightIt)
library(survey)
library(cobalt)

# 1. 准备数据：确保处理变量为因子，结局为0/1，无缺失关键变量
rt_clean <- rt %>%
  filter(!is.na(MDS), !is.na(OP)) %>%        # 删除 MDS 或 OP 缺失的样本
  droplevels()

# 2. 计算多分类倾向评分权重（IPTW）
#    method = "ps" 使用多项逻辑回归估计倾向评分
#    estimand = "ATE" 表示平均处理效应（比较各组与总体平均）
#    如果想以某组为参照（如 Low），可设置 estimand = "ATT"，并指定 focal = "Low"
#    sampw 参数纳入 NHANES 最终抽样权重（使权重同时代表全国人口）
iptw_obj <- weightit(
  MDS ~ Age_group + Gender + Race + PIR + Marital_status + BMI,
  data = rt_clean,
  method = "ps",
  estimand = "ATE",                    # 或 "ATT"（如需指定参照组）
  # focal = "Low",                     # 若用 ATT，指定参照组
  sampw = rt_clean$WTMEC11YR           # NHANES 抽样权重（变量名请确认）
)

# 3. 检查平衡性（标准化均差 SMD 应 <0.1）
bal.tab(iptw_obj, un = TRUE)
love.plot(iptw_obj, threshold = 0.1)

# 4. 将 IPTW 权重加到数据中（最终权重 = IPTW权重 × 调查权重？）
#    WeightIt 的 sampw 已经将调查权重整合进了最终权重，所以直接使用 iptw_obj$weights 即可
rt_clean$iptw_weight <- iptw_obj$weights
# 多分类逆概率加权（Multinomial IPTW）
# 5. 构建加权 survey 设计对象（使用 IPTW 权重，但保留抽样设计信息）
design_iptw <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~iptw_weight,      # 这里已经包含了调查权重 × IPTW 调整
  data = rt_clean,
  nest = TRUE,
  survey.lonely.psu = "adjust"
)

# 6. 拟合加权 logistic 回归（平衡后）
model_iptw <- svyglm(OP ~ MDS1, design = design_iptw, family = quasibinomial)

# 7. 计算 OR 及 95% CI
summary_iptw <- summary(model_iptw)
coef_iptw <- summary_iptw$coefficients
or_iptw <- exp(coef_iptw[, "Estimate"])
ci_iptw <- exp(confint(model_iptw))
result_iptw <- data.frame(
  Variable = rownames(coef_iptw),
  OR = or_iptw,
  CI_lower = ci_iptw[, 1],
  CI_upper = ci_iptw[, 2],
  P_value = coef_iptw[, "Pr(>|t|)"]
)
print(result_iptw)

# 保存结果
write.csv(result_iptw, "./04_PSM/IPTW_MDS1_OP_result1.csv", row.names = FALSE)


# 6. 拟合加权 logistic 回归（平衡后）
model_iptw <- svyglm(OP ~ MDS1+ Age_group + Gender + Race + PIR + Marital_status + BMI, design = design_iptw, family = quasibinomial)

# 7. 计算 OR 及 95% CI
summary_iptw <- summary(model_iptw)
coef_iptw <- summary_iptw$coefficients
or_iptw <- exp(coef_iptw[, "Estimate"])
ci_iptw <- exp(confint(model_iptw))
result_iptw <- data.frame(
  Variable = rownames(coef_iptw),
  OR = or_iptw,
  CI_lower = ci_iptw[, 1],
  CI_upper = ci_iptw[, 2],
  P_value = coef_iptw[, "Pr(>|t|)"]
)
print(result_iptw)

# 保存结果
write.csv(result_iptw, "./04_PSM/IPTW_MDS1_OP_result2.csv", row.names = FALSE)



# 6. 拟合加权 logistic 回归（平衡后）
model_iptw <- svyglm(OP ~ MDS1+ Age_group + Gender + Race + PIR + Marital_status + BMI
                     +Stroke+Hypertension+Diabetes+TG+TC+HbA1C+MAGN+CALC+PHOS, design = design_iptw, family = quasibinomial)

# 7. 计算 OR 及 95% CI
summary_iptw <- summary(model_iptw)
coef_iptw <- summary_iptw$coefficients
or_iptw <- exp(coef_iptw[, "Estimate"])
ci_iptw <- exp(confint(model_iptw))
result_iptw <- data.frame(
  Variable = rownames(coef_iptw),
  OR = or_iptw,
  CI_lower = ci_iptw[, 1],
  CI_upper = ci_iptw[, 2],
  P_value = coef_iptw[, "Pr(>|t|)"]
)
print(result_iptw)

# 保存结果
write.csv(result_iptw, "./04_PSM/IPTW_MDS1_OP_result3.csv", row.names = FALSE)
