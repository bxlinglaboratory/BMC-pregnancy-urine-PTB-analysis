#######################################################################################
## Figure 2c
#######################################################################################
library(ggbreak)
library(ggplot2)
figure.data <- readRDS("~/data/figure2c.data.rds")

ggplot() +
  geom_point(data=figure.data, aes(x=RANK, y=Gini, group=1,colour = Selected),size = 2,alpha=1)+
  xlab("Rank") +
  ylab("Gini importance") +
  scale_color_manual(values=c("black","brown3")) +     
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_line(size = 2),
        axis.line = element_line(size = 1),
        panel.border =  element_blank()) +
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.5), hjust = 0.5),
        axis.title = element_text(size = rel(1.25))) +
  ggbreak::scale_x_break(breaks=c(46,7000),scales = 0.25,space = 0,expand = T) +
  ggplot2::scale_x_continuous(expand = c(1,0), limits = c(1,7913),breaks = c(1,seq(10,50,by=10),7000,7913)) 
