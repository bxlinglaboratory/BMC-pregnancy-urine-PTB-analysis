#######################################################################################
## Figure 1
#######################################################################################
library(dplyr)
dataset_merge_su <- readRDS("~/data/dataset_merge_su.rds")
dataset_merge_uab <- readRDS("~/data/dataset_merge_uab.rds")
###########################################
## Figure 1B SU
###########################################
df <- dataset_merge_su$injodr[,c("Sample.Type","GA","GA Wks at Del","File.Name")]
colnames(df)[3] <- "Del"
df <- melt(df, id.vars="Sample.Type", measure.vars=c("GA","Del"),variable_name = "variable") %>% as.data.frame()
df$group <- paste0(df$variable,"_",df$Sample.Type)
df[df=="GA_Case"] <- "Pre-term colletion"
df[df=="GA_Control"] <- "Terms colletion"
df[df=="Del_Case"] <- "Pre-term delivery"
df[df=="Del_Control"] <- "Terms delivery"

ggplot(df) +
  geom_density(data = subset(df,df$variable=="GA"), aes(x=value,color=Sample.Type), linetype="dashed",size=1) + # Collection
  scale_color_manual(values=c("#F9847C","#00B7BC")) + #Collection color
  geom_density(data = subset(df,df$variable=="Del"), aes(x=value,color=group),size=1) + #Del
  scale_color_manual(values=c("#F9847C","#00B7BC","#F9847C","#00B7BC")) +
  labs(title=NULL,x="Gestational age (weeeks)", y = "Density") +
  scale_x_continuous(expand = c(0,0), limits = c(6,43),breaks = seq(6,43,by=5))+
  theme_classic()+
  theme(legend.position="none")
###########################################
## Figure 1B UAB
###########################################
df <- dataset_merge_uab$injodr[,c("Sample.Type","GA","GA Wks at Del","File.Name")]
colnames(df)[3] <- "Del"
df <- melt(df, id.vars="Sample.Type", measure.vars=c("GA","Del"),variable_name = "variable") %>% as.data.frame()
df$group <- paste0(df$variable,"_",df$Sample.Type)
df[df=="GA_Case"] <- "Pre-term colletion"
df[df=="GA_Control"] <- "Terms colletion"
df[df=="Del_Case"] <- "Pre-term delivery"
df[df=="Del_Control"] <- "Terms delivery"

ggplot(df) +
  geom_density(data = subset(df,df$variable=="GA"), aes(x=value,color=Sample.Type), linetype="dashed",size=1) + # Collection
  scale_color_manual(values=c("#F9847C","#00B7BC")) + #Collection color
  geom_density(data = subset(df,df$variable=="Del"), aes(x=value,color=group),size=1) + #Del
  scale_color_manual(values=c("#F9847C","#00B7BC","#F9847C","#00B7BC")) +
  labs(title=NULL,x="Gestational age (weeeks)", y = "Density") +
  scale_x_continuous(expand = c(0,0), limits = c(6,43),breaks = seq(6,43,by=5))+
  theme_classic()+
  theme(legend.position="bottom")
