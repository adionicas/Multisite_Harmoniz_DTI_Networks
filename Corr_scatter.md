Import data

```R
library(readxl)

params_before_combat <- read_excel(file.choose())

data_after_combat_on_params <- read_excel(file.choose())

data_after_combat_on_weights <- read_excel(file.choose())


params_before_combat <- within(params_before_combat, {
  Site <- factor(Study_MRI)
})
data_after_combat_on_params <- within(data_after_combat_on_params, {
  Site <- factor(Study_MRI)
})
data_after_combat_on_weights <- within(data_after_combat_on_weights, {
  Site <- factor(Study_MRI)
})

```




Make actual plots

```R

library("ggpubr")


site_label=params_before_combat$Study_MRI

site_label <- replace(site_label, site_label==1, "Calgary")
site_label <- replace(site_label, site_label==2, "Edmonton")
site_label <- replace(site_label, site_label==3, "Montreal 1")
site_label <- replace(site_label, site_label==4, "Montreal 2")
site_label <- replace(site_label, site_label==5, "Ottawa")
site_label <- replace(site_label, site_label==6, "Vancouver")


metric_label <- colnames(params_before_combat)

metric_label[3] <- "Betweenness centrality"
metric_label[5] <- "Degree centrality"
metric_label[8] <- "Clustering coefficient"
metric_label[9] <- "Shortest path"
metric_label[10] <- "Small worldness"


for(t in 2:12){

tmp_df_param_nocbt_by_site <- data.frame(params_before_combat[,t],site_label,params_before_combat$age)
colnames(tmp_df_param_nocbt_by_site)[1] <- "metric"
colnames(tmp_df_param_nocbt_by_site)[3] <- "age"


tmp_df_param_cbtwei_by_site <- data.frame(data_after_combat_on_weights[,t],site_label,data_after_combat_on_weights$age)
colnames(tmp_df_param_cbtwei_by_site)[1] <- "metric"
colnames(tmp_df_param_cbtwei_by_site)[3] <- "age"


tmp_df_param_cbtparam_by_site <- data.frame(data_after_combat_on_params[,t],site_label,data_after_combat_on_params$age)
colnames(tmp_df_param_cbtparam_by_site)[1] <- "metric"
colnames(tmp_df_param_cbtparam_by_site)[3] <- "age"


tmp_max_val = max(max(tmp_df_param_nocbt_by_site$metric),max(tmp_df_param_cbtwei_by_site$metric),max(tmp_df_param_cbtparam_by_site$metric))
tmp_min_val = min(min(tmp_df_param_nocbt_by_site$metric),min(tmp_df_param_cbtwei_by_site$metric),min(tmp_df_param_cbtparam_by_site$metric))

if ((tmp_max_val-tmp_min_val) > 2) {
nrdigits <- 0
} else {
nrdigits <- 2
}

low_limit_y <- tmp_min_val - (tmp_max_val - tmp_min_val)/5
max_limit_y <- tmp_max_val + (tmp_max_val - tmp_min_val)/3

sp <- ggscatter(tmp_df_param_nocbt_by_site, x = "age", y = "metric", size = 0, color = "dimgrey", 
          #add = "reg.line", add.params = list(color = "black", fill = "steelblue3"), conf.int = TRUE, 
          #cor.method = "pearson",
          xlab = "Age (years)", ylab = paste("Metric value\n(", metric_label[t], ")", sep=""))
sp + stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 6) + scale_y_continuous(limits = c(low_limit_y, max_limit_y), breaks = floor_dec(seq(low_limit_y, max_limit_y, by = round((tmp_max_val - tmp_min_val)/4,nrdigits)),nrdigits)) + geom_jitter(aes(color = site_label),size=0.8) +  scale_colour_manual(values = c("#FFA500","#2E8B57","#AB82FF","#FF0000","#5CACEE","#A0522D")) + geom_smooth(method='lm', formula= y~x, colour="black", size=0.5) + theme(axis.text=element_text(size=12), axis.title=element_text(size=14), legend.position="none")
#theme(legend.text = element_text(size = 8), legend.justification=c(1,0), legend.direction = "horizontal") + guides(colour = guide_legend(nrow = 2), title.position = "none")
filename = paste("corr_plot2_unharmonized", colnames(data_after_combat_on_params)[t], ".png", sep="_")
ggsave(filename, device='png', width = 4.5, height = 3.5, dpi=700)



sp <- ggscatter(tmp_df_param_cbtwei_by_site, x = "age", y = "metric", size = 0, color = "dimgrey", 
          #add = "reg.line", add.params = list(color = "black", fill = "steelblue3"), conf.int = TRUE, 
          #cor.method = "pearson",
          xlab = "Age (years)", ylab = paste("Metric value\n(", metric_label[t], ")", sep=""))

sp + stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 6) + scale_y_continuous(limits = c(low_limit_y, max_limit_y), breaks = floor_dec(seq(low_limit_y, max_limit_y, by = round((tmp_max_val - tmp_min_val)/4,nrdigits)),nrdigits)) + geom_jitter(aes(color = site_label),size=0.8) +  scale_colour_manual(values = c("#FFA500","#2E8B57","#AB82FF","#FF0000","#5CACEE","#A0522D")) + geom_smooth(method='lm', formula= y~x, colour="black", size=0.5) + theme(axis.text=element_text(size=12), axis.title=element_text(size=14), legend.position="none")
#theme(legend.text = element_text(size = 8), legend.justification=c(1,0), legend.direction = "horizontal") + guides(colour = guide_legend(nrow = 2), title.position = "none")

filename = paste("corr_plot2_weight_harmonized", colnames(data_after_combat_on_params)[t], ".png", sep="_")
ggsave(filename, device='png', width = 4.5, height = 3.5, dpi=700)



sp <- ggscatter(tmp_df_param_cbtparam_by_site, x = "age", y = "metric", size = 0, color = "dimgrey", 
         # add = "reg.line", add.params = list(color = "black", fill = "steelblue3"), conf.int = TRUE, 
          #cor.method = "pearson",
          xlab = "Age (years)", ylab = paste("Metric value\n(", metric_label[t], ")", sep=""))

sp + stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 6) + scale_y_continuous(limits = c(low_limit_y, max_limit_y), breaks = floor_dec(seq(low_limit_y, max_limit_y, by = round((tmp_max_val - tmp_min_val)/4,nrdigits)),nrdigits)) + geom_jitter(aes(color = site_label),size=0.8) + scale_colour_manual(values = c("#FFA500","#2E8B57","#AB82FF","#FF0000","#5CACEE","#A0522D")) + geom_smooth(method='lm', formula= y~x, colour="black", size=0.5) + theme(axis.text=element_text(size=12), axis.title=element_text(size=14), legend.position="none")

filename = paste("corr_plot2_param_harmonized", colnames(data_after_combat_on_params)[t], ".png", sep="_")
ggsave(filename, device='png', width = 4.5, height = 3.5, dpi=700)
}
```

Combine begore, after harmonization in bash:

```bash
parameters="Assortativity ClustCoeff Efficiency Modularity SmallWorld Density BetweennessCentrality Degree_centrality Hierarchy ShortestPath Synchronization"
for param in $parameters; do
convert corr_plot2_unharmonized_"$param"_.png corr_plot2_weight_harmonized_"$param"_.png corr_plot2_param_harmonized_"$param"_.png +append corr_plot2_"$param"_combined_edit.jpeg
done




```


