rm(list = ls())
setwd("G:/返修/KXF2025040303")

if(!dir.exists("./01_clean/")){
  dir.create("./01_clean/")
}
setwd("./01_clean/")


# 加载包 ---------------------------------------------------------------------
# install.packages("plyr") 
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

# 数据加载 --------------------------------------------------------------------
# DEMO_P <- read_xpt('G:/返修/KXF2025040303/data/DEMO/DEMO_D.XPT')
# colnames(DEMO_P)

# 人口统计学变量 -----------------------------------------------------------------
## RIDAGEYR(age)  RIDRETH1(race) RIAGENDR(gender) DMDEDUC(Education) DMDHRMAR(marital status) INDFMPIR(RIP)
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

###婚姻
setwd("G:/返修/KXF2025040303/data/DEMO")
exam_fs = list.files('G:/返修/KXF2025040303/data/DEMO/',pattern = '^demo',ignore.case = TRUE)

for(file in exam_fs){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(gsub(" ","",substr(file, 1, perpos-1)),read.xport(paste0('./',file)))
}

select_list <- c('SEQN',"DMDMARTL")
EXAM_mat <- rbind(DEMO_D[,select_list],DEMO_E[,select_list],DEMO_F[,select_list],
                  DEMO_H[,select_list])


Marital_dat <- dplyr::rename(EXAM_mat,
                              Marital_status = DMDMARTL
)





# BMX - BMXBMI(BMI)
setwd("G:/返修/KXF2025040303/data/BMX")
exam_fs = list.files('G:/返修/KXF2025040303/data/BMX/',pattern = '^bmx',ignore.case = TRUE)

for(file in exam_fs){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(gsub(" ","",substr(file, 1, perpos-1)),read.xport(paste0('./',file)))
}
#BMXWAIST腰围，BMXHT身高
exam_list <- c('SEQN',"BMXBMI")
EXAM_mat <- rbind(BMX_D[,exam_list],BMX_E[,exam_list],BMX_F[,exam_list],
                  BMX_H[,exam_list],BMX_P[,exam_list])


EXAM_dat <- dplyr::rename(EXAM_mat,
                          BMI = BMXBMI
)


#糖尿病家族史
setwd("G:/返修/KXF2025040303/data/DIQ")
ques_fs = list.files('G:/返修/KXF2025040303/data/DIQ/',pattern = '^diq',ignore.case = TRUE)

for(file in ques_fs){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(gsub(" ","",substr(file, 1, perpos-1)),read.xport(paste0('./',file)))
}
DIQ_list <- c('SEQN',"DIQ010")
DIQ_dat <- rbind(DIQ_D[,DIQ_list],DIQ_E[,DIQ_list],DIQ_F[,DIQ_list],
                 DIQ_H[,DIQ_list],DIQ_P[,DIQ_list])
DIQ_dat<- dplyr::rename(DIQ_dat,Diabetes=DIQ010)



#高血压
setwd("G:/返修/KXF2025040303/data/BPQ")
ques_fs = list.files('./',pattern = '^BPQ',ignore.case = TRUE)

for(file in ques_fs){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(gsub(" ","",substr(file, 1, perpos-1)),read.xport(paste0('./',file)))
}
BPQ_list <- c('SEQN',"BPQ020")
BPQ_dat <- rbind(BPQ_D[,BPQ_list],BPQ_E[,BPQ_list],BPQ_F[,BPQ_list],
                 BPQ_H[,BPQ_list],BPQ_P[,BPQ_list])
BPQ_dat <- dplyr::rename(BPQ_dat,Hypertension=BPQ020)




#中风
setwd("G:/返修/KXF2025040303/data/MCQ")
ques_fs = list.files('G:/返修/KXF2025040303/data/MCQ/',pattern = '^mcq',ignore.case = TRUE)

for(file in ques_fs){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(gsub(" ","",substr(file, 1, perpos-1)),read.xport(paste0('./',file)))
}
MCQ_list <- c('SEQN',"MCQ160F")
MCQ_dat <- rbind(MCQ_D[,MCQ_list],MCQ_E[,MCQ_list],MCQ_F[,MCQ_list],
                 MCQ_H[,MCQ_list],MCQ_P[,MCQ_list])
MCQ_dat <- dplyr::rename(MCQ_dat,Stroke=MCQ160F)






#吸烟
setwd("G:/返修/KXF2025040303/data/SMQ")
ques_fs = list.files('./',pattern = '^.',ignore.case = TRUE)

for(file in ques_fs){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(gsub(" ","",substr(file, 1, perpos-1)),read.xport(paste0('./',file)))
}
SMQ_list <- c('SEQN',"SMQ040")
SMQ_dat <- rbind(SMQ_D[,SMQ_list],SMQ_E[,SMQ_list],SMQ_F[,SMQ_list],
                 SMQ_H[,SMQ_list],SMQ_P[,SMQ_list])
SMQ_dat1 <- dplyr::rename(SMQ_dat,
                          Smoking_status = SMQ040)

#####甘油三酯（TG）
setwd("G:/返修/KXF2025040303/data/BIOPRO")
ques_fs = list.files('./',pattern = '^.',ignore.case = TRUE)

for(file in ques_fs){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(gsub(" ","",substr(file, 1, perpos-1)),read.xport(paste0('./',file)))
}
SMQ_list <- c('SEQN',"LBDSTRSI")
SMQ_dat <- rbind(BIOPRO_D[,SMQ_list],BIOPRO_E[,SMQ_list],BIOPRO_F[,SMQ_list],
                 BIOPRO_H[,SMQ_list],BIOPRO_P[,SMQ_list])
TG_dat <- dplyr::rename(SMQ_dat,
                        TG = LBDSTRSI)


#####总胆固醇（TC）
setwd("G:/返修/KXF2025040303/data/TCHOL")
ques_fs = list.files('./',pattern = '^.',ignore.case = TRUE)

for(file in ques_fs){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(gsub(" ","",substr(file, 1, perpos-1)),read.xport(paste0('./',file)))
}
SMQ_list <- c('SEQN',"LBXTC")
SMQ_dat <- rbind(TCHOL_D[,SMQ_list],TCHOL_E[,SMQ_list],TCHOL_F[,SMQ_list],
                 TCHOL_H[,SMQ_list],TCHOL_P[,SMQ_list])
TC_dat <- dplyr::rename(SMQ_dat,
                        TC = LBXTC)



#####糖化血红蛋白（HbA1C）
setwd("G:/返修/KXF2025040303/data/GHB")
ques_fs = list.files('./',pattern = '^.',ignore.case = TRUE)

for(file in ques_fs){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(gsub(" ","",substr(file, 1, perpos-1)),read.xport(paste0('./',file)))
}
SMQ_list <- c('SEQN',"LBXGH")
SMQ_dat <- rbind(GHB_D[,SMQ_list],GHB_E[,SMQ_list],GHB_F[,SMQ_list],
                 GHB_H[,SMQ_list],GHB_P[,SMQ_list])
HbA1C_dat <- dplyr::rename(SMQ_dat,
                           HbA1C = LBXGH)

#####镁摄入（DR1TMAGN）  钙摄入(DR1TCALC)   磷摄入(DR1TPHOS)  维生素D摄入(DR1TVD)
#*"Dietary intakes of magnesium, calcium, and phosphorus were assessed by 24-hour dietary recalls on two separate days. For each participant, we calculated the mean of the two recalls to approximate usual intake. If only one recall was available, the value from that single day was used."*
#####第一天摄入
setwd("G:/返修/KXF2025040303/data/DR1TOT")
ques_fs = list.files('./',pattern = '^.',ignore.case = TRUE)

for(file in ques_fs){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(gsub(" ","",substr(file, 1, perpos-1)),read.xport(paste0('./',file)))
}
SMQ_list <- c('SEQN',"DR1TMAGN","DR1TCALC","DR1TPHOS")
SMQ_dat <- rbind(DR1TOT_D[,SMQ_list],DR1TOT_E[,SMQ_list],DR1TOT_F[,SMQ_list],
                 DR1TOT_H[,SMQ_list],DR1TOT_P[,SMQ_list])
sheru_dat1 <- dplyr::rename(SMQ_dat,
                            MAGN1 = DR1TMAGN,
                            CALC1=DR1TCALC,
                            PHOS1=DR1TPHOS)


#####第二天摄入
setwd("G:/返修/KXF2025040303/data/DR2TOT")
ques_fs = list.files('./',pattern = '^.',ignore.case = TRUE)

for(file in ques_fs){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(gsub(" ","",substr(file, 1, perpos-1)),read.xport(paste0('./',file)))
}
SMQ_list <- c('SEQN',"DR2TMAGN","DR2TCALC","DR2TPHOS")
SMQ_dat <- rbind(DR2TOT_D[,SMQ_list],DR2TOT_E[,SMQ_list],DR2TOT_F[,SMQ_list],
                 DR2TOT_H[,SMQ_list],DR2TOT_P[,SMQ_list])
sheru_dat2 <- dplyr::rename(SMQ_dat,
                            MAGN2 = DR2TMAGN,
                            CALC2=DR2TCALC,
                            PHOS2=DR2TPHOS)

# sheru_dat <- full_join(sheru_dat1,sheru_dat2,by="SEQN")
# colnames(sheru_dat)
# 
# sheru_dat$MAGN=sheru_dat$MAGN2-sheru_dat$MAGN1
# sheru_dat$CALC=sheru_dat$CALC2-sheru_dat$CALC1
# sheru_dat$PHOS=sheru_dat$PHOS2-sheru_dat$PHOS1

# 第一天摄入数据 (已加载为 sheru_dat1)
# 第二天摄入数据 (已加载为 sheru_dat2)

# 合并两天数据
sheru_dat <- full_join(sheru_dat1, sheru_dat2, by = "SEQN")

# 计算两天的均值，处理缺失值
# 若只有一天有值，则直接用该天的值；若两天都缺失，则结果为NA
sheru_dat <- sheru_dat %>%
  mutate(
    MAGN1 = ifelse(is.na(MAGN1), NA, MAGN1),   # 确保缺失值规范
    MAGN2 = ifelse(is.na(MAGN2), NA, MAGN2),
    CALC1 = ifelse(is.na(CALC1), NA, CALC1),
    CALC2 = ifelse(is.na(CALC2), NA, CALC2),
    PHOS1 = ifelse(is.na(PHOS1), NA, PHOS1),
    PHOS2 = ifelse(is.na(PHOS2), NA, PHOS2),
    
    # 计算均值：na.rm = TRUE 会忽略缺失值，但当两个都是NA时结果为NaN，需再处理为NA
    MAGN = rowMeans(cbind(MAGN1, MAGN2), na.rm = TRUE),
    MAGN = ifelse(is.nan(MAGN), NA, MAGN),
    
    CALC = rowMeans(cbind(CALC1, CALC2), na.rm = TRUE),
    CALC = ifelse(is.nan(CALC), NA, CALC),
    
    PHOS = rowMeans(cbind(PHOS1, PHOS2), na.rm = TRUE),
    PHOS = ifelse(is.nan(PHOS), NA, PHOS)
  )
colnames(sheru_dat)
sheru_dat=sheru_dat[,c("SEQN","MAGN","CALC","PHOS")]
#######结局####

#####骨质疏松  OSQ060 
setwd("G:/返修/KXF2025040303/data/OSQ")
ques_fs = list.files('./',pattern = '^.',ignore.case = TRUE)

for(file in ques_fs){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(gsub(" ","",substr(file, 1, perpos-1)),read.xport(paste0('./',file)))
}

OSQ_P <- read_xpt('G:/返修/KXF2025040303/data/OSQ/P_OSQ.xpt')
SMQ_list <- c('SEQN',"OSQ060")
SMQ_dat <- rbind(OSQ_D[,SMQ_list],OSQ_E[,SMQ_list],OSQ_F[,SMQ_list],
                 OSQ_H[,SMQ_list],OSQ_P[,SMQ_list])

OP_dat <- dplyr::rename(SMQ_dat,
                        OP = OSQ060)


setwd("G:/返修/KXF2025040303/data/DXXFEM")
ques_fs = list.files('./',pattern = '^.',ignore.case = TRUE)

for(file in ques_fs){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(gsub(" ","",substr(file, 1, perpos-1)),read.xport(paste0('./',file)))
}
SMQ_list <- c('SEQN',"DXXNKBMD")
SMQ_dat <- rbind(DXXFEM_D[,SMQ_list],DXXFEM_E[,SMQ_list],DXXFEM_F[,SMQ_list],
                 DXXFEM_H[,SMQ_list],DXXFEM_P[,SMQ_list])
DXXFEM_dat <- dplyr::rename(SMQ_dat,
                            BMD = DXXNKBMD)

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

# # 创建新列，空白值为0，其余为1
# filtered_df1$new_column1 <- ifelse(
#   is.na(filtered_df1$RXDDRGID) | filtered_df1$RXDDRGID == "",  # 条件：NA或空字符串
#   0,  # 满足条件时赋值0
#   1   # 不满足条件时赋值1
# )
# 
# # 查看结果
# table(filtered_df1$new_column1, useNA = "always")

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

# # 创建新列，空白值为0，其余为1
# filtered_df2$new_column2 <- ifelse(
#   is.na(filtered_df2$RXDDRGID) | filtered_df2$RXDDRGID == "",  # 条件：NA或空字符串
#   0,  # 满足条件时赋值0
#   1   # 不满足条件时赋值1
# )

# 查看结果
#able(filtered_df2$new_column2, useNA = "always")

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




####eGFR  LBXSCR  
setwd("G:/返修/KXF2025040303/data/BIOPRO")
ques_fs = list.files('./',pattern = '^.',ignore.case = TRUE)

for(file in ques_fs){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(gsub(" ","",substr(file, 1, perpos-1)),read.xport(paste0('./',file)))
}
SMQ_list <- c('SEQN',"LBXSCR")
SMQ_dat <- rbind(BIOPRO_D[,SMQ_list],BIOPRO_E[,SMQ_list],BIOPRO_F[,SMQ_list],
                 BIOPRO_H[,SMQ_list],BIOPRO_P[,SMQ_list])
LBXSCR_dat <- dplyr::rename(SMQ_dat,
                            Scr = LBXSCR)


####ALQ150  喝酒  
setwd("G:/返修/KXF2025040303/data/ALQ")
ques_fs = list.files('./',pattern = '^.',ignore.case = TRUE)

for(file in ques_fs){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(gsub(" ","",substr(file, 1, perpos-1)),read.xport(paste0('./',file)))
}
SMQ_list <- c('SEQN',"ALQ150")
SMQ_dat <- rbind(ALQ_D[,SMQ_list],ALQ_E[,SMQ_list],ALQ_F[,SMQ_list])
ALQ_dat1 <- dplyr::rename(SMQ_dat,
                          ALQ = ALQ150)

SMQ_list <- c('SEQN',"ALQ151")
SMQ_dat <- rbind(ALQ_H[,SMQ_list],ALQ_P[,SMQ_list])
ALQ_dat2 <- dplyr::rename(SMQ_dat,
                          ALQ = ALQ151)
ALQ_dat=rbind(ALQ_dat1,ALQ_dat2)


# 合并协变量+结局信息 ------------------------------------------------------------------
nhanes_mat <- full_join(DEMO_dat,Marital_dat,by="SEQN")
nhanes_mat <- full_join(nhanes_mat,EXAM_mat,by="SEQN")
nhanes_mat <- full_join(nhanes_mat,SMQ_dat1,by="SEQN")
nhanes_mat <- full_join(nhanes_mat,DIQ_dat,by="SEQN")
nhanes_mat <- full_join(nhanes_mat,BPQ_dat,by="SEQN")
nhanes_mat <- full_join(nhanes_mat,MCQ_dat,by="SEQN")
nhanes_mat <- full_join(nhanes_mat,TG_dat,by="SEQN")
nhanes_mat <- full_join(nhanes_mat,TC_dat,by="SEQN")
nhanes_mat <- full_join(nhanes_mat,HbA1C_dat,by="SEQN")
nhanes_mat <- full_join(nhanes_mat,sheru_dat,by="SEQN")
nhanes_mat <- full_join(nhanes_mat,DXXFEM_dat,by="SEQN")
nhanes_mat <- full_join(nhanes_mat,filtered_df1,by="SEQN")
nhanes_mat <- full_join(nhanes_mat,filtered_df2,by="SEQN")
nhanes_mat <- full_join(nhanes_mat,ALQ_dat,by="SEQN")
nhanes_mat <- full_join(nhanes_mat,LBXSCR_dat,by="SEQN")
nhanes_mat <- full_join(nhanes_mat,OP_dat,by="SEQN")



setwd("G:/返修/KXF2025040303/01_clean")
save(nhanes_mat,file = 'nhanes_mat_raw.rda')

#load('G:/返修/KXF2025040303/result/nhanes_mat_raw.rda')

print(colMeans(is.na(nhanes_mat))*100)

nhanes_mat_choose=nhanes_mat
dim(nhanes_mat_choose)
#56769

table(nhanes_mat_choose$new_column1,useNA = 'always')
table(nhanes_mat_choose$new_column2,useNA = 'always')

#年龄
table(nhanes_mat_choose$Age)
nhanes_mat_choose <- subset(nhanes_mat_choose,nhanes_mat_choose$Age>=18)
dim(nhanes_mat_choose)
table(nhanes_mat_choose$new_column1,useNA = 'always')
table(nhanes_mat_choose$new_column2,useNA = 'always')

# 
# nhanes_mat_choose$Age[nhanes_mat_choose$Age < 60] = '45-60'
# 
# nhanes_mat_choose$Age[nhanes_mat_choose$Age >= 60] = '>=60'
# table(nhanes_mat_choose$Age)
# >=60 45-60 
# 11330  8118
# dim(nhanes_mat_choose)
#[1] 9492   30
#34124


#Gender
colnames(nhanes_mat_choose)
table(nhanes_mat_choose$Gender,useNA = 'always')
nhanes_mat_choose$Gender <- factor(nhanes_mat_choose$Gender,levels = c(1,2),labels = c("Male","Female"))
table(nhanes_mat_choose$Gender)



####暴露
colnames(nhanes_mat_choose)
table(nhanes_mat_choose$OP,useNA = 'always')
table(nhanes_mat_choose$BMD,useNA = 'always')
nhanes_mat_choose$BMD1[nhanes_mat_choose$BMD < 0.556] = 'Yes'
nhanes_mat_choose$BMD1[nhanes_mat_choose$BMD >= 0.556000000000000] = 'No'
table(nhanes_mat_choose$BMD1,useNA = 'always')

nhanes_mat_choose$OP <- factor(nhanes_mat_choose$OP,levels = c(1,2),labels = c("Yes","No"))
table(nhanes_mat_choose$OP,useNA = 'always')

nhanes_mat_choose <- nhanes_mat_choose[!(is.na(nhanes_mat_choose$OP) & 
                                           is.na(nhanes_mat_choose$BMD1)), ]

dim(nhanes_mat_choose)
table(nhanes_mat_choose$new_column1,useNA = 'always')
table(nhanes_mat_choose$new_column2,useNA = 'always')
library(dplyr)

# 创建新列
nhanes_mat_choose <- nhanes_mat_choose %>%
  mutate(OP = case_when(
    BMD1 == "Yes" | OP == "Yes" ~ "Yes",
    TRUE ~ "No"  # 默认情况（包括NA）为"No"
  ))

# 验证结果
table(nhanes_mat_choose$OP, useNA = "always")
# No   Yes  <NA> 
#   24450  2392     0 
dim(nhanes_mat_choose)
# 26842    28

###暴露
colnames(nhanes_mat_choose)




table(nhanes_mat_choose$ALQ,useNA = 'always')
nhanes_mat_choose$ALQ <- factor(nhanes_mat_choose$ALQ,levels = c(1,2),labels = c("Yes","No"))

nhanes_mat_choose <- subset(nhanes_mat_choose,!is.na(nhanes_mat_choose$ALQ))
dim(nhanes_mat_choose)
table(nhanes_mat_choose$new_column1,useNA = 'always')
table(nhanes_mat_choose$new_column2,useNA = 'always')
table(nhanes_mat_choose$ALQ,useNA = 'always')
nhanes_mat_choose$ALQ <- factor(nhanes_mat_choose$ALQ,levels = c("Yes","No"),labels = c(1,0))

colnames(nhanes_mat_choose)
table(nhanes_mat_choose$new_column1,useNA = 'always')
nhanes_mat_choose <- subset(nhanes_mat_choose,!is.na(nhanes_mat_choose$new_column1))
nhanes_mat_choose <- subset(nhanes_mat_choose,!is.na(nhanes_mat_choose$new_column2))
nhanes_mat_choose <- subset(nhanes_mat_choose,!is.na(nhanes_mat_choose$Scr))
dim(nhanes_mat_choose)
table(nhanes_mat_choose$new_column1,useNA = 'always')
table(nhanes_mat_choose$new_column2,useNA = 'always')

nhanes_mat_choose$Scr
calculate_eGFR <- function(Scr, Age, Gender) {
  if (Gender == "Female") {
    if (Scr <= 0.7) {
      eGFR <- 144 * (Scr / 0.7)^(-0.329) * 0.993^Age
    } else {
      eGFR <- 144 * (Scr / 0.7)^(-1.209) * 0.993^Age
    }
  } else if (Gender == "Male") {
    if (Scr <= 0.9) {
      eGFR <- 141 * (Scr / 0.9)^(-0.411) * 0.993^Age
    } else {
      eGFR <- 141 * (Scr / 0.9)^(-1.209) * 0.993^Age
    }
  } else {
    # 若性别不是 Male 或 Female，返回 NA 或根据实际情况处理
    eGFR <- NA
    warning("Gender value is not recognized, returning NA for eGFR.")
  }
  return(eGFR)
}

nhanes_mat_choose$eGFR <- mapply(calculate_eGFR, 
                                 nhanes_mat_choose$Scr, 
                                 nhanes_mat_choose$Age, 
                                 nhanes_mat_choose$Gender)
range(nhanes_mat_choose$eGFR)
library(dplyr)

# 创建分类变量
nhanes_mat_choose <- nhanes_mat_choose %>%
  mutate(
    eGFR_category = case_when(
      is.na(eGFR) ~ 0,
      eGFR >= 60 & eGFR <= 90 ~ 1,
      eGFR < 60 ~ 2,
      TRUE ~ 0  # 默认情况（如eGFR>90）为0
    )
  )

# 验证结果
table(nhanes_mat_choose$eGFR_category, useNA = "always")
table(nhanes_mat_choose$new_column1, useNA = "always")
table(nhanes_mat_choose$new_column2, useNA = "always")
table(nhanes_mat_choose$ALQ, useNA = "always")

# 假设nhanes_mat_choose是你的数据框
# 基于现有四列计算MDS并分级

# 确保所有需要的列都存在
nhanes_mat_choose$eGFR_category=as.numeric(nhanes_mat_choose$eGFR_category)
nhanes_mat_choose$new_column1=as.numeric(nhanes_mat_choose$new_column1)
nhanes_mat_choose$new_column2=as.numeric(nhanes_mat_choose$new_column2)
nhanes_mat_choose$ALQ=as.numeric(nhanes_mat_choose$ALQ)
nhanes_mat_choose$ALQ[nhanes_mat_choose$ALQ == 2] <- 0

required_cols <- c("eGFR_category", "new_column1", "new_column2", "ALQ")
if(!all(required_cols %in% colnames(nhanes_mat_choose))) {
  missing_cols <- required_cols[!required_cols %in% colnames(nhanes_mat_choose)]
  stop(paste("缺少必要的列:", paste(missing_cols, collapse = ", ")))
}

# 计算MDS总分
nhanes_mat_choose$MDS <- rowSums(nhanes_mat_choose[, required_cols])
table(nhanes_mat_choose$MDS)
# 0    1    2    3    4    5 
# 831 1358  445  183  187   39 

nhanes_mat_choose <- nhanes_mat_choose %>%
  mutate(MDS_category = case_when(
    MDS == 0 ~ "Low",
    MDS == 1 ~ "Low",
    MDS == 2 ~ "Medium",
    MDS > 2  ~ "High",
    TRUE     ~ NA_character_  # 防止意外值
  ))

# 设置为有序因子（推荐，便于后续分析）
nhanes_mat_choose$MDS_category <- factor(nhanes_mat_choose$MDS_category,
                                      levels = c("Low", "Medium", "High"),
                                      ordered = TRUE)
# # 根据MDS值创建分级
# nhanes_mat_choose$MDS_category <- cut(
#   nhanes_mat_choose$MDS,
#   breaks = c(-Inf, 2, Inf),
#   labels = c( "Low", "Medium", "High"),
#   right = FALSE
# )

# 查看MDS分级的分布
print("MDS分级分布:")
table(nhanes_mat_choose$MDS_category, useNA = "always")
# Low Medium   High   <NA> 
#   13373   3453   1729      0 
# 查看前几行结果
head(nhanes_mat_choose[, c(required_cols, "MDS", "MDS_category")])

dim(nhanes_mat_choose)
# 18555    32


###糖尿病
colnames((nhanes_mat_choose))
table(nhanes_mat_choose$Diabetes,useNA = 'always')
nhanes_mat_choose <- subset(nhanes_mat_choose, Diabetes != 3)
nhanes_mat_choose <- subset(nhanes_mat_choose, Diabetes != 9)
nhanes_mat_choose$Diabetes <- factor(nhanes_mat_choose$Diabetes,levels = c(1,2),labels = c("Yes","No"))
table(nhanes_mat_choose$Diabetes)
# Yes    No 
dim(nhanes_mat_choose)
# [1] 18062    32


###高血压
colnames((nhanes_mat_choose))
table(nhanes_mat_choose$Hypertension,useNA = 'always')
nhanes_mat_choose <- subset(nhanes_mat_choose, Hypertension != 9)
nhanes_mat_choose$Hypertension <- factor(nhanes_mat_choose$Hypertension,levels = c(1,2),labels = c("Yes","No"))
table(nhanes_mat_choose$Hypertension)
# Yes   No 
dim(nhanes_mat_choose)
#18040    32

# ###Smoking
# colnames((nhanes_mat_choose))
# table(nhanes_mat_choose$Smoking_status,useNA = 'always')
# nhanes_mat_choose <- subset(nhanes_mat_choose,!is.na(nhanes_mat_choose$Smoking_status))
# nhanes_mat_choose$Smoking_status <- factor(nhanes_mat_choose$Smoking_status,levels = c(1,2,3),labels = c("Yes","Yes","No"))
# table(nhanes_mat_choose$Smoking_status)
# # Yes   No 
# # 6537 7547
# dim(nhanes_mat_choose)
# # 14085    24

###Marital
colnames((nhanes_mat_choose))
table(nhanes_mat_choose$Marital_status,useNA = 'always')
# nhanes_mat_choose <- subset(nhanes_mat_choose,!is.na(nhanes_mat_choose$Marital_status))
nhanes_mat_choose <- subset(nhanes_mat_choose, Marital_status != 99)
nhanes_mat_choose <- subset(nhanes_mat_choose, Marital_status != 77)
nhanes_mat_choose$Marital_status <- factor(nhanes_mat_choose$Marital_status,levels = c(1,2,3,4,5,6),labels = c("Married","Married","unMarried","unMarried","unMarried","unMarried"))
table(nhanes_mat_choose$Marital_status)

dim(nhanes_mat_choose)
# 7264   30

###种族
colnames((nhanes_mat_choose))
table(nhanes_mat_choose$Race,useNA = 'always')
nhanes_mat_choose$Race <- factor(nhanes_mat_choose$Race,levels = c(1,2,3,4,5),labels = c("Mexican_american","Other_hispanic","Non_hispanic_white","Non_hispanic_black ","Other"))
table(nhanes_mat_choose$Race)
# Married unMarried 
# 4133      1846
dim(nhanes_mat_choose)
# 14580    32

###教育水平
colnames((nhanes_mat_choose))
table(nhanes_mat_choose$Educational_level,useNA = 'always')
nhanes_mat_choose <- subset(nhanes_mat_choose, Educational_level != 9)
nhanes_mat_choose <- subset(nhanes_mat_choose, Educational_level != 7)
nhanes_mat_choose$Educational_level <- factor(nhanes_mat_choose$Educational_level,levels = c(1,2,3,4,5),
                                              labels = c("Below high school",
                                                         "Below high school",
                                                         "High school",
                                                         "Above high school",
                                                         "Above high school"))
table(nhanes_mat_choose$Educational_level,useNA = 'always')
dim(nhanes_mat_choose)
# 14567    32

####贫困收入比
colnames((nhanes_mat_choose))
table(nhanes_mat_choose$PIR,useNA = 'always')
# nhanes_mat_choose <- subset(nhanes_mat_choose,!is.na(nhanes_mat_choose$PIR))
nhanes_mat_choose$PIR2[nhanes_mat_choose$PIR <1]="Poor"
nhanes_mat_choose$PIR2[nhanes_mat_choose$PIR <3&nhanes_mat_choose$PIR>=1]="Close to poverty"
nhanes_mat_choose$PIR2[3<=nhanes_mat_choose$PIR]="Rich"

dim(nhanes_mat_choose)
# 14567    33

####BMI
colnames((nhanes_mat_choose))
table(nhanes_mat_choose$BMXBMI,useNA = 'always')
# nhanes_mat_choose <- subset(nhanes_mat_choose,!is.na(nhanes_mat_choose$BMXBMI))
dim(nhanes_mat_choose)
#  7255   33
####TG  TC  HbA1C
colnames((nhanes_mat_choose))
table(nhanes_mat_choose$HbA1C,useNA = 'always')
# nhanes_mat_choose <- subset(nhanes_mat_choose,!is.na(nhanes_mat_choose$HbA1C))
# nhanes_mat_choose <- subset(nhanes_mat_choose,!is.na(nhanes_mat_choose$TG))
# nhanes_mat_choose <- subset(nhanes_mat_choose,!is.na(nhanes_mat_choose$TC))
dim(nhanes_mat_choose)
# 6572   31

####MAGN  CALC  PHOS
colnames((nhanes_mat_choose))
table(nhanes_mat_choose$MAGN,useNA = 'always')
# nhanes_mat_choose <- subset(nhanes_mat_choose,!is.na(nhanes_mat_choose$MAGN))
# nhanes_mat_choose <- subset(nhanes_mat_choose,!is.na(nhanes_mat_choose$CALC))
# nhanes_mat_choose <- subset(nhanes_mat_choose,!is.na(nhanes_mat_choose$PHOS))
dim(nhanes_mat_choose)
# 5566   31

####中风
colnames(nhanes_mat_choose)
table(nhanes_mat_choose$Stroke,useNA = 'always')
nhanes_mat_choose$Stroke <- factor(nhanes_mat_choose$Stroke,levels = c(1,2),labels = c("Yes","No"))
nhanes_mat_choose <- subset(nhanes_mat_choose,!is.na(nhanes_mat_choose$Stroke))
table(nhanes_mat_choose$Stroke,useNA = 'always')
dim(nhanes_mat_choose)
# 14541    33


##列名变更
colnames((nhanes_mat_choose))
a <- c("SEQN", "Age","Gender","BMXBMI","Race","Marital_status","Hypertension","Diabetes","WTMEC11YR","Stroke","TG","TC","HbA1C",            
       "MAGN","CALC","PHOS","MDS","MDS_category","PIR2","Educational_level","OP","SDMVPSU","SDMVSTRA")
nhanes_mat_all <- nhanes_mat_choose[,a]
print(colMeans(is.na(nhanes_mat_all))*100)

nhanes_mat_all <- dplyr::rename(nhanes_mat_all,
                                BMI = BMXBMI,
                                PIR=PIR2,
                                MDS1=MDS)
nhanes_mat_all <- dplyr::rename(nhanes_mat_all,
                                MDS = MDS_category)
table(nhanes_mat_all$OP)
# No   Yes 
# 13546   995 
table(nhanes_mat_all$MDS)
# Low Medium   High 
# 10951   2402   1188 



# 进行随机森林缺失值插补
set.seed(123)
library(missForest)
library(randomForest)
str(nhanes_mat_all)
nhanes_mat_all$PIR=as.factor(nhanes_mat_all$PIR)
nhanes_mat_all$OP=as.factor(nhanes_mat_all$OP)


##插补
nhanes_mat_all1 <- missForest(nhanes_mat_all)$ximp
print(colMeans(is.na(nhanes_mat_all1))*100)
colnames(nhanes_mat_all1)

# 


write.csv(nhanes_mat_all1,file = "nhanes_mat_choose.csv")


#nhanes_mat_all1 <- read.csv('G:/返修/KXF2025040303/01_clean/nhanes_mat_choose.csv')

######基线表
library(tableone)
####处理好数据直接分析-------------------------------------
concat_df <- nhanes_mat_all1
colnames(concat_df)
######OP
factor <- 'MDS'
## 指定需要分析的变量

model_first <- c('Age', 'Gender', "Race","Marital_status")
model_second <- c("BMI","Educational_level","Stroke",
                  "Hypertension","Diabetes","PIR","TG","TC","HbA1C","MAGN", "CALC","PHOS")
all_variable <- c(model_first,model_second,factor)


## 指定分类变量，否则分类数据也将以定量数据进行展示
factor_variable <- c('Gender', "Race","Marital_status","MDS",
                     "Hypertension","Educational_level","Diabetes","Stroke","PIR")

# 2. 制作基线表
table_statistics <- CreateTableOne(vars = all_variable, 
                                   strata = 'OP', 
                                   data = concat_df, 
                                   addOverall = TRUE,
                                   factorVars = factor_variable)
table_statistics <- print(table_statistics, 
                          showAllLevels = TRUE ## 表示展示分类变量所有分类因子的结果
)
write.csv(table_statistics, 'G:/返修/KXF2025040303/02_基线表/01.OP_table_statistics(未加权).csv')


######MDS
concat_df <- nhanes_mat_all1
colnames(concat_df)

factor <- 'OP'
## 指定需要分析的变量

model_first <- c('Age', 'Gender', "Race","Marital_status")
model_second <- c("BMI","Educational_level","Stroke",
                  "Hypertension","Diabetes","PIR","TG","TC","HbA1C","MAGN", "CALC","PHOS")
all_variable <- c(model_first,model_second,factor)


## 指定分类变量，否则分类数据也将以定量数据进行展示
factor_variable <- c('Gender', "Race","Marital_status","OP",
                     "Hypertension","Educational_level","Diabetes","Stroke","PIR")

# 2. 制作基线表
table_statistics <- CreateTableOne(vars = all_variable, 
                                   strata = 'MDS', 
                                   data = concat_df,
                                   addOverall = TRUE,
                                   factorVars = factor_variable)
table_statistics <- print(table_statistics, 
                          showAllLevels = TRUE ## 表示展示分类变量所有分类因子的结果
)
write.csv(table_statistics, 'G:/返修/KXF2025040303/02_基线表/02.MDS_table_statistics(未加权).csv')




####MDS组成成分
colnames((nhanes_mat_choose))
a <- c("OP","new_column1","new_column2","ALQ","eGFR_category")
nhanes_mat_MDS <- nhanes_mat_choose[,a]
print(colMeans(is.na(nhanes_mat_MDS))*100)
nhanes_mat_MDS <- dplyr::rename(nhanes_mat_MDS,
                                Diuretic = new_column1,
                                PPI = new_column2,
                                Alcoholism=ALQ,
                                eGFR=eGFR_category)
nhanes_mat_MDS$Diuretic <- factor(nhanes_mat_MDS$Diuretic,levels = c(1,0),labels = c("Yes","No"))
nhanes_mat_MDS$PPI <- factor(nhanes_mat_MDS$PPI,levels = c(1,0),labels = c("Yes","No"))
nhanes_mat_MDS$Alcoholism <- factor(nhanes_mat_MDS$Alcoholism,levels = c(1,0),labels = c("Yes","No"))



######MDS
concat_df <- nhanes_mat_MDS
colnames(concat_df)

factor <- 'Diuretic'
## 指定需要分析的变量

model_first <- c('PPI')
model_second <- c("Alcoholism",'eGFR')
all_variable <- c(factor,model_first,model_second)


## 指定分类变量，否则分类数据也将以定量数据进行展示
factor_variable <- c('Diuretic', "PPI","Alcoholism","eGFR")

# 2. 制作基线表
table_statistics <- CreateTableOne(vars = all_variable, 
                                   strata = 'OP', 
                                   data = concat_df,
                                   addOverall = TRUE,
                                   factorVars = factor_variable)
table_statistics <- print(table_statistics, 
                          showAllLevels = TRUE ## 表示展示分类变量所有分类因子的结果
)
write.csv(table_statistics, 'G:/返修/KXF2025040303/02_基线表/02.MDS_GROUP_table_statistics(未加权).csv')


# 加权

# ================================
# 3. 加权基线表（OP 分组）
# ================================
cat("处理 OP 分组加权表...\n")

# 提取数据
concat_df <- nhanes_mat_all1

# 指定变量
model_first <- c('Age', 'Gender', "Race", "Marital_status")
model_second <- c("BMI", "Educational_level", "Stroke",
                  "Hypertension", "Diabetes", "PIR", "TG", "TC", 
                  "HbA1C", "MAGN", "CALC", "PHOS")
all_variable <- c(model_first, model_second, "MDS")

factor_variable <- c('Gender', "Race", "Marital_status", "MDS",
                     "Hypertension", "Educational_level", "Diabetes", 
                     "Stroke", "PIR")

# 创建 survey 设计对象（考虑分层、集群、权重）
design_op <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTMEC11YR,
  data = concat_df,
  nest = TRUE
)

# 加权基线表
weighted_table_op <- svyCreateTableOne(
  vars = all_variable,
  strata = "OP",
  data = design_op,
  addOverall = TRUE,
  factorVars = factor_variable
)
out_dir <- 'G:/返修/KXF2025040303/02_基线表/'
# 打印并保存
weighted_table_op_print <- print(weighted_table_op,
                                 showAllLevels = TRUE,
                                 quote = FALSE,
                                 noSpaces = TRUE)
write.csv(weighted_table_op_print,
          file = file.path(out_dir, "01.OP_table_statistics(加权).csv"),
          row.names = TRUE)

# ================================
# 4. 加权基线表（MDS 分组）
# ================================
cat("处理 MDS 分组加权表...\n")

# 使用同样的设计对象，只需改变 strata
weighted_table_mds <- svyCreateTableOne(
  vars = all_variable,
  strata = "MDS",
  data = design_op,
  addOverall = TRUE,
  factorVars = factor_variable
)

weighted_table_mds_print <- print(weighted_table_mds,
                                  showAllLevels = TRUE,
                                  quote = FALSE,
                                  noSpaces = TRUE)
write.csv(weighted_table_mds_print,
          file = file.path(out_dir, "02.MDS_table_statistics(加权).csv"),
          row.names = TRUE)

# ================================
# 5. 加权基线表（MDS 组成成分，按 OP 分组）
# ================================
cat("处理 MDS 组成成分加权表...\n")

# 从 nhanes_mat_choose 提取所需列（必须包含权重、分层、PSU）
mds_cols <- c("OP", "new_column1", "new_column2", "ALQ", "eGFR_category",
              "WTMEC11YR", "SDMVPSU", "SDMVSTRA")
nhanes_mat_MDS <- nhanes_mat_choose[, mds_cols]

# 重命名
nhanes_mat_MDS <- nhanes_mat_MDS %>%
  rename(Diuretic = new_column1,
         PPI = new_column2,
         Alcoholism = ALQ,
         eGFR = eGFR_category)

# 将二分类变量转为因子（并指定标签）
nhanes_mat_MDS$Diuretic <- factor(nhanes_mat_MDS$Diuretic,
                                  levels = c(1, 0),
                                  labels = c("Yes", "No"))
nhanes_mat_MDS$PPI <- factor(nhanes_mat_MDS$PPI,
                             levels = c(1, 0),
                             labels = c("Yes", "No"))
nhanes_mat_MDS$Alcoholism <- factor(nhanes_mat_MDS$Alcoholism,
                                    levels = c(1, 0),
                                    labels = c("Yes", "No"))
# eGFR_category 如果是分类变量，也转为因子（假设已经是分类）
if (!is.factor(nhanes_mat_MDS$eGFR)) {
  nhanes_mat_MDS$eGFR <- as.factor(nhanes_mat_MDS$eGFR)
}

# 创建 survey 设计对象
design_mds <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTMEC11YR,
  data = nhanes_mat_MDS,
  nest = TRUE
)

# 指定分析变量
factor_variable_mds <- c("Diuretic", "PPI", "Alcoholism", "eGFR")
all_variable_mds <- c("Diuretic", "PPI", "Alcoholism", "eGFR")

# 加权基线表（按 OP 分组）
weighted_table_mds_group <- svyCreateTableOne(
  vars = all_variable_mds,
  strata = "OP",
  data = design_mds,
  addOverall = TRUE,
  factorVars = factor_variable_mds
)

weighted_table_mds_group_print <- print(weighted_table_mds_group,
                                        showAllLevels = TRUE,
                                        quote = FALSE,
                                        noSpaces = TRUE)
write.csv(weighted_table_mds_group_print,
          file = file.path(out_dir, "03.MDS_GROUP_table_statistics(加权).csv"),
          row.names = TRUE)

cat("所有加权基线表已导出至：", out_dir, "\n")


























