#######################################################################################
## Figure 3A 
#######################################################################################
library(ggplot2)
library(ggrepel)
###########################################
## Figure 3A Early PTB
###########################################
figure.data <- readRDS("~/data/figure.3A.earlyPTB.data.rds")

ggplot(figure.data) +
  geom_point(aes(x = ENRICHMENT.RATIO, y = pval.new, color = as.factor(significant),size=pval.new)) + #pval.new=-log10(pvalue)
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", size=0.2) +
  geom_text(aes(0, -log10(0.05), label = paste("p value:",0.05), vjust = 1, hjust = 0.25), size=2.5) +
  scale_color_manual(values = c("Yes"="red","NO"=="grey"))+
  geom_text_repel(data = figure.data[figure.data$label=="Label",],aes(x = ENRICHMENT.RATIO, y = pval.new,label = GENE.SET),
                  direction = "both",nudge_y=0.015,size = rel(4),face = 'bold')+
  labs(title="Early PTB vs full-term")+
  xlab("Pathway enrichment") +
  ylab("P-value (-log10)") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border  = element_blank(),
        axis.title = element_text(size = rel(1),face = 'bold'),
        legend.position = "none")+
  scale_x_continuous(limits=c(0,150),breaks=seq(0, 200, 50)) +
  scale_y_continuous(limits=c(0,ceiling(max(figure.data$pval.new))),breaks=seq(0, ceiling(max(figure.data$pval.new)), 2))
###########################################
## Figure 3A Very Early PTB
###########################################
figure.data <- readRDS("~/data/figure.3A.VeryEarlyPTB.data.rds")

ggplot(figure.data) +
  geom_point(aes(x = ENRICHMENT.RATIO.model2, y = pval.new, color = as.factor(significant),size=pval.new)) + #pval.new=-log10(pvalue)
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", size=0.2) +
  geom_text(aes(0, -log10(0.05), label = paste("p value:",0.05), vjust = 1, hjust = 0.25), size=2.5) +
  scale_color_manual(values = c("Yes"="red","NO"=="grey"))+
  geom_text_repel(data = figure.data[figure.data$label=="Label",,drop=F],aes(x = ENRICHMENT.RATIO.model2, y = pval.new,label = GENE.SET),face = 'bold')+
  labs(title="Very early PTB vs full-term")+
  xlab("Pathway impact") +
  ylab("-log10 p-value") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border  = element_blank(),
        # text = element_text(size = rel(2),face = 'bold'),
        axis.title = element_text(size = rel(1),face = 'bold'),
        legend.position = "none")+
  scale_x_continuous(limits=c(0,150),breaks=seq(0, 200, 50)) +
  scale_y_continuous(limits=c(0,ceiling(max(figure.data$pval.new))),breaks=seq(0, ceiling(max(figure.data$pval.new)), 2))



