#######################################################################################
## Figure 3B
#######################################################################################
library(ggplot2)
library(ggrepel)
figure.data <- readRDS("//172.18.1.31/mProbe.model/yijin/mprobe-model-ms/PTB/Manuscript/PTB.paper.2024.Urine/data/figure.3B.data.rds")
figure.data$group <- factor(figure.data$group,levels = c("Both","significant.model1","significant.model2","Not"))

ggplot(figure.data) +
  geom_point(aes(x = PVAL, y = PVAL.model2,color=group),size=3) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", size=0.2) +
  geom_vline(xintercept = -log10(0.05), linetype = "dashed", size=0.2)  +
  scale_color_manual(labels = c("Both"="Significant in both","significant.model1"="Significant in early PTB",
                                "significant.model2"="Significant in very early PTB","Not"="No Significant"), 
                     values = c("Both"="red","significant.model1"="#F9CF84","significant.model2"="#BDE086","Not"="grey"))+
  geom_text_repel(data = figure.data[figure.data$group=="Both",],aes(x = PVAL, y = PVAL.model2,label = GENE.SET), nudge_x = 0.05, nudge_y = 0.05,size=4.5)+
  xlab("Early PTB P-value(-log10)") +
  ylab("Very Early PTB P-value(-log10)") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border  = element_blank(),
        legend.position = "right",
        axis.title = element_text(size = rel(1),face = 'bold'))+
  scale_x_continuous(limits=c(0,12),breaks=seq(0, 15, 3)) +
  scale_y_continuous(limits=c(0,12),breaks=seq(0, 15, 3))
