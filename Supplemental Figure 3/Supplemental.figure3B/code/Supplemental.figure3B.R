#######################################################################################
## Supplemental Figure 3B Very early PTB model
#######################################################################################
library(ggplot2)
library(pROC)
pred_result_su <- readRDS("//172.18.1.31/mProbe.model/yijin/mprobe-model-ms/PTB/Manuscript/PTB.paper.2024.Urine/data/Supplemental.figure3B.SU.data.very.early.PTB.rds")
pred_result_uab <- readRDS("//172.18.1.31/mProbe.model/yijin/mprobe-model-ms/PTB/Manuscript/PTB.paper.2024.Urine/data/Supplemental.figure3B.UAB.data.very.early.PTB.rds")
####################################
##  SU Cohort
####################################
rocobj <- pROC::roc(pred_result_su$truth,pred_result_su$predictedValue)
auc_ci <- ci.auc(pred_result_su$truth,pred_result_su$predictedValue) 
ciobj <- ci.se(rocobj, specificities=seq(0, 1, 0.01))
data_ci<- data.frame(x=as.numeric(rownames(as.data.frame(ciobj))),ciobj)
### ROCAUC plot
ggroc(rocobj,alpha = 0.9, colour = "black",size = 1) +
  geom_ribbon(data = data_ci,aes(x=x,ymin=X2.5.,ymax=X97.5.), fill = 'grey',alpha=0.5)+
  theme_bw() + 
  labs(x = "1 - Specificity", y = "Sensitivity") +
  annotate("text", x = .25, y = .20, label = paste0("AUC = 0.950")) +
  annotate("text", x = .25, y = .10, label = paste0("95% CI [",round(auc_ci[1],3),",",round(auc_ci[2], 3),"]"))
### confusion matrix
cm_test <- as.data.frame.matrix(table(pred_result_su[,c("qualitativeRisk","Sample.Type")]))
cm_test$Total <- rowSums(cm_test)
cm_test["Total",]<- colSums(cm_test)
rownames(cm_test)<- c("Test negative","Test postive","Total")
colnames(cm_test)<- c("Postive","Negative","Total")
print(cm_test)
####################################
## UAB Cohort
####################################
pred_result_uab$truth <- ifelse(pred_result_uab$Sample.Type=="Case",1,0)

rocobj <- pROC::roc(pred_result_uab$truth,pred_result_uab$predictedValue)
auc_ci <- ci.auc(pred_result_uab$truth,pred_result_uab$predictedValue) 
ciobj <- ci.se(rocobj, specificities=seq(0, 1, 0.01))
data_ci<- data.frame(x=as.numeric(rownames(as.data.frame(ciobj))),ciobj)
### ROCAUC plot
ggroc(rocobj,alpha = 0.9, colour = "black",size = 1) +
  geom_ribbon(data = data_ci,aes(x=x,ymin=X2.5.,ymax=X97.5.), fill = 'grey',alpha=0.5)+
  theme_bw() + 
  labs(x = "1 - Specificity", y = "Sensitivity") +
  annotate("text", x = .25, y = .20, label = paste0("AUC = ", round(rocobj$auc, 3))) +
  annotate("text", x = .25, y = .10, label = paste0("95% CI [",round(auc_ci[1],3),",",round(auc_ci[3], 3),"]"))
### confusion matrix
cm_test <- as.data.frame.matrix(table(pred_result_uab[,c("qualitativeRisk","Sample.Type")]))
cm_test$Total <- rowSums(cm_test)
cm_test["Total",]<- colSums(cm_test)
rownames(cm_test)<- c("Test negative","Test postive","Total")
colnames(cm_test)<- c("Postive","Negative","Total")
print(cm_test)
