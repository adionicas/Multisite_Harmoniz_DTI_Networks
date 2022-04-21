
### Import data


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

library(ggplot2)
library(reshape2)

floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)


```


### Make plots

```R


site_label=params_before_combat$Study_MRI

site_label <- replace(site_label, site_label==1, "Calgary")
site_label <- replace(site_label, site_label==2, "Edmonton")
site_label <- replace(site_label, site_label==3, "Montreal 1")
site_label <- replace(site_label, site_label==4, "Montreal 2")
site_label <- replace(site_label, site_label==5, "Ottawa")
site_label <- replace(site_label, site_label==6, "Vancouver")


for(t in 2:12){

tmp_df_param_nocbt_by_site <- data.frame(params_before_combat[,t],site_label)
colnames(tmp_df_param_nocbt_by_site)[1] <- "metric"

tmp_df_param_cbtwei_by_site <- data.frame(data_after_combat_on_weights[,t],site_label)
colnames(tmp_df_param_cbtwei_by_site)[1] <- "metric"

tmp_df_param_cbtparam_by_site <- data.frame(data_after_combat_on_params[,t],site_label)
colnames(tmp_df_param_cbtparam_by_site)[1] <- "metric"

tmp_max_val = max(max(tmp_df_param_nocbt_by_site$metric),max(tmp_df_param_cbtwei_by_site$metric),max(tmp_df_param_cbtparam_by_site$metric))
tmp_min_val = min(min(tmp_df_param_nocbt_by_site$metric),min(tmp_df_param_cbtwei_by_site$metric),min(tmp_df_param_cbtparam_by_site$metric))

if ((tmp_max_val-tmp_min_val) > 2) {
nrdigits <- 0
} else {
nrdigits <- 2
}

low_limit_y <- tmp_min_val - (tmp_max_val - tmp_min_val)/3.7
max_limit_y <- tmp_max_val + (tmp_max_val - tmp_min_val)/2

dp <- ggplot(tmp_df_param_nocbt_by_site, aes(x=site_label, y=metric,fill=site_label)) + geom_violin(trim = FALSE) + geom_boxplot(width = 0.12) + scale_fill_manual(values = c("#edf8b1","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8")) + labs(title="",x="", y = paste("Metric value\n(", metric_label[t], ")", sep=""))

dp + theme_minimal() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), legend.title=element_blank(), legend.justification=c(1,0), legend.position="none") + scale_y_continuous(limits = c(low_limit_y, max_limit_y), breaks = round(seq(low_limit_y, max_limit_y, by = round((tmp_max_val - tmp_min_val)/4,nrdigits)),nrdigits))

filename = paste("Violin_plot_before_combat", colnames(params_before_combat)[t], ".png", sep="_")
ggsave(filename, device='png', width = 4.7, height = 3.5, dpi=1000)

dp <- ggplot(tmp_df_param_cbtwei_by_site, aes(x=site_label, y=metric,fill=site_label)) + geom_violin(trim = FALSE) + geom_boxplot(width = 0.12) + scale_fill_manual(values = c("#edf8b1","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8")) + labs(title="",x="", y = paste("Metric value\n(", metric_label[t], ")", sep=""))

dp + theme_minimal() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), legend.title=element_blank(), legend.justification=c(1,0), legend.position="none") + scale_y_continuous(limits = c(low_limit_y, max_limit_y), breaks = round(seq(low_limit_y, max_limit_y, by = round((tmp_max_val - tmp_min_val)/4,nrdigits)),nrdigits))

filename = paste("Violin_plot_combat_on_weights", colnames(data_after_combat_on_weights)[t], ".png", sep="_")
ggsave(filename, device='png', width = 4.7, height = 3.5, dpi=1000)

dp <- ggplot(tmp_df_param_cbtparam_by_site, aes(x=site_label, y=metric,fill=site_label)) + geom_violin(trim = FALSE) + geom_boxplot(width = 0.12) + scale_fill_manual(values = c("#edf8b1","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8")) + labs(title="",x="", y = paste("Metric value\n(", metric_label[t], ")", sep=""))

dp + theme_minimal() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), legend.title=element_blank(), legend.justification=c(1,0), legend.position="none") + scale_y_continuous(limits = c(low_limit_y, max_limit_y), breaks = round(seq(low_limit_y, max_limit_y, by = round((tmp_max_val - tmp_min_val)/4,nrdigits)),nrdigits))

filename = paste("Violin_plot_combat_on_param", colnames(data_after_combat_on_params)[t], ".png", sep="_")
ggsave(filename, device='png', width = 4.7, height = 3.5, dpi=1000)
}
```

### Combine in bash

```bash
parameters="Assortativity ClustCoeff Efficiency Modularity SmallWorld Density BetweennessCentrality Degree_centrality Hierarchy ShortestPath Synchronization"
for param in $parameters; do
convert Box_plot_before_combat_"$param"_.png Box_plot_combat_on_weights_"$param"_.png Box_plot_combat_on_param_"$param"_.png +append Boxplots_"$param"_combined_edit.jpeg
done


parameters="Assortativity ClustCoeff Efficiency Modularity SmallWorld Density BetweennessCentrality Degree_centrality Hierarchy ShortestPath Synchronization"
for param in $parameters; do
convert Violin_plot_before_combat_"$param"_.png Violin_plot_combat_on_weights_"$param"_.png Violin_plot_combat_on_param_"$param"_.png +append Violinplots_"$param"_combined_edit.jpeg
done

```
