#######################################################################################
## Supplemental Figure 4A Early PTB model
#######################################################################################
library(survival)
pred_result_su <- readRDS("~/data/Supplemental.figure4A.SU.data.early.PTB.rds")
pred_result_uab <- readRDS("~/data/Supplemental.figure4A.UAB.data.early.PTB.rds")
####################################
##  SU Cohort
####################################
fit <- survival::survfit(survival::Surv(Del, Status) ~ qualitativeRisk, data = pred_result_su)
survminer::ggsurvplot(fit,  size = 1,  # change line size
                      ylab = c("Percentage of ongoing pregnancies"),
                      xlab = c("Gestational age at delivery (weeeks)"),
                      break.time.by = 4, # break time axis by 250
                      palette = "Dark2",
                      conf.int = TRUE, # Add confidence interval
                      pval = TRUE, # Add p-value,
                      pval.size=3.5,
                      pval.coord = c(10, 0.15),
                      risk.table = TRUE,
                      risk.table.col = "strata",
                      risk.table.fontsize = 4,
                      ggtheme = theme_bw(),
                      legend.labs = c("Low Risk","High Risk"),
                      xlim = c(8, 40),
                      font.x = c(12), font.y = c(12),font.tickslab = 12
)

####################################
## UAB Cohort
####################################
pred_result_uab$Status <- 1
pred_result_uab$Del <- pred_result_uab$GA.at.Delivery
pred_result_uab$qualitativeRisk <- factor(pred_result_uab$qualitativeRisk, levels=c("low","high"))
fit <- survival::survfit(survival::Surv(Del, Status) ~ qualitativeRisk, data = pred_result_uab)
survminer::ggsurvplot(fit,  size = 1,  # change line size
                      ylab = c("Percentage of ongoing pregnancies"),
                      xlab = c("Gestational age at delivery (weeeks)"),
                      break.time.by = 4, # break time axis by 250
                      palette = "Dark2",
                      conf.int = TRUE, # Add confidence interval
                      pval = TRUE, # Add p-value,
                      pval.size=3.5,
                      pval.coord = c(10, 0.15),
                      risk.table = TRUE,
                      risk.table.col = "strata",
                      risk.table.fontsize = 4,
                      ggtheme = theme_bw(),
                      legend.labs = c("Low Risk","High Risk"),
                      xlim = c(8, 40),
                      font.x = c(12), font.y = c(12),font.tickslab = 12
)
