#######################################################################################
## Figure 4B
#######################################################################################
##########################################
## Function
##########################################
library(ggplot2)
library(ggrepel)
attach_meta_to_sam <- function (sam, injodr, attcols = c("Sample.Type"), by = "File.Name") {
  check_diff_col <- setdiff(attcols, colnames(injodr))
  if (length(check_diff_col) > 0) {
    stop(sprintf("Cannot find column [%s] in injection order!", 
                 paste(check_diff_col, collapse = "],[")))
  }
  newsam <- merge(unique(injodr[, c(by, attcols)]), sam, by.x = by, 
                  by.y = 0, sort = F)
  rownames(newsam) <- newsam[, 1]
  newsam[, 1] <- NULL
  return(newsam)
}

calc_boxplot_stat <- function(x) {
  coef <- 1
  n <- sum(!is.na(x))
  # calculate quantiles
  stats <- quantile(x, probs = c(0.0, 0.25, 0.5, 0.75, 1.0))
  names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
  iqr <- diff(stats[c(2, 4)])
  # set whiskers
  outliers <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
  if (any(outliers)) {
    stats[c(1, 5)] <- range(c(stats[2:4], x[!outliers]), na.rm = TRUE)
  }
  return(stats)
}
##########################################
## data load
##########################################
dataset_merge_su <- readRDS("~/data/dataset_merge_su.rds")
ms2_label <- readRDS("~/data/figure.4B.data.rds")
##########################################
## Calculate
##########################################
df <- attach_meta_to_sam(sam = t(dataset_merge_su$sam[ms2_label$Feature,]), injodr = dataset_merge_su$injodr,attcols = c("Sample.Type","GA"))
melt_data <- reshape::melt(df,id.vars = c("Sample.Type", "GA"),measure.vars = colnames(df)[3:ncol(df)])
melt_data <- merge(melt_data,ms2_label,by.x="variable",by.y="Feature",all.x=T)
melt_data$ga_interval <- cut(melt_data$GA,breaks=seq(8,40,2),include.lowest = T)

error_method <- NULL
feature_plot_ <- aggregate(value~ga_interval+abbr+Sample.Type, data=melt_data, FUN = function(x) {
  if(is.null(error_method))
    mn = median(x,na.rm=T)
  else if(error_method=="extreme")
    c(mn = median(x,na.rm=T), h = max(x,na.rm=T),  l = min(x,na.rm=T))
  else
    stop(sprintf("error_method [%s] is not supported yet!", error_method))
})
feature_plot_$ga_interval_2 <- as.numeric(feature_plot_$ga_interval)

feature_stat <- aggregate(value~Sample.Type+abbr, data=feature_plot_, calc_boxplot_stat)
feature_stat$ymin <- feature_stat$value[,2]; feature_stat$ymax <- feature_stat$value[,4]
feature_plot_ <- merge(feature_plot_,feature_stat[,c("abbr","ymin","ymax")],by="abbr",all.x=T)
##########################################
## Plot
##########################################
ggplot(feature_plot_)+
  geom_smooth(data = subset(feature_plot_,feature_plot_$Sample.Type=="Control"),
              aes(x=ga_interval_2,y=value,color=Sample.Type),se=F,col="#00B7BC", size=1.2) +
  stat_smooth(data = subset(feature_plot_,feature_plot_$Sample.Type=="Control"),
              aes(x=ga_interval_2,y=value,color=Sample.Type),linetype="dotted",colour = "#00B7BC", geom = "ribbon", fill = NA, fullrange = T,size=0.8) +
  geom_smooth(data = subset(feature_plot_,feature_plot_$Sample.Type=="Case"),
              aes(x=ga_interval_2,y=value,color=Sample.Type),se=F, col="#F9847C", size=1.2) +
  stat_smooth(data = subset(feature_plot_,feature_plot_$Sample.Type=="Case"),
              aes(x=ga_interval_2,y=value,color=Sample.Type),linetype="dotted",colour = "#F9847C", geom = "ribbon", fill = NA, fullrange = TRUE,size=0.8) +
  scale_x_continuous(expand = c(0,2), limits = c(1,8),breaks = seq(1,8,by=1),labels=c("1" = "[8,10]", "2" = "(10,12]", "3" = "(12,14]","4" = "(14,16]", 
                                                                                      "5" = "(16,18]", "6" = "(18,20]", "7" = "(20,22]", "8" = "(22,24]"))+
  
  ggforce::facet_wrap_paginate(~abbr, ncol = 4, nrow = 3, scales = "free_y") +
  labs(x="Gestational age interval (weeks)", y = "Intensity value") +
  theme_classic() +
  theme(strip.text.x  = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(angle=45,hjust=1),
        axis.title = element_text(size = rel(1),face = 'bold'),
        plot.title = element_blank(),
        legend.position = "none")
