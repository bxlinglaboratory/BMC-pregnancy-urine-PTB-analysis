#######################################################################################
## Supplemental Figure 1
#######################################################################################
library(ggplot2)
figure.data <- readRDS("~/data/Supplemental.figure1.data.rds")

xlim1 <- boxplot.stats(figure.data$p1)$stats[c(1, 5)]
ylim1 <- boxplot.stats(figure.data$p2)$stats[c(1, 5)]
figure.data$Sample.Type <- ifelse(figure.data$Sample.Type=="Case","Preterm","Full-term")
figure.data$Sample.Type <- factor(figure.data$Sample.Type,levels = c("Preterm","Full-term"))
####################################
## 1st trimester
####################################
ggplot(figure.data[figure.data$Trimester=="1st trimester",], aes(x = p1, y = p2, color = Sample.Type)) +
  geom_point(size=1.5) +
  scale_shape_manual(values = c(16,1)) +
  coord_cartesian(ylim = c(ylim1[1]-( ylim1[2]- ylim1[1])*0.05, ylim1[2]+(ylim1[2]- ylim1[1])*0.05),
                  xlim = c(xlim1[1]-( xlim1[2]- xlim1[1])*0.05, xlim1[2]+(xlim1[2]- xlim1[1])*0.05))+  #Hide outliers
  theme_bw() +
  labs(title="PLS-DA on 1st trimester",x="Component 1", y = "Component 2")+
  stat_ellipse(geom="polygon", aes(fill = Sample.Type), alpha = 0.25, show.legend = T,level = 0.9)  +
  theme(legend.position = "top")
####################################
## 2nd trimester
####################################
ggplot(figure.data[figure.data$Trimester=="2nd trimester",], aes(x = p1, y = p2, color = Sample.Type)) +
  geom_point(size=1.5) +
  scale_shape_manual(values = c(16,1)) +
  coord_cartesian(ylim = c(ylim1[1]-( ylim1[2]- ylim1[1])*0.05, ylim1[2]+(ylim1[2]- ylim1[1])*0.05),
                  xlim = c(xlim1[1]-( xlim1[2]- xlim1[1])*0.05, xlim1[2]+(xlim1[2]- xlim1[1])*0.05))+  #Hide outliers
  theme_bw() +
  labs(title="PLS-DA on 2nd trimester",x="Component 1", y = "Component 2")+
  stat_ellipse(geom="polygon", aes(fill = Sample.Type), alpha = 0.25, show.legend = T,level = 0.9)  +
  theme(legend.position = "top")
