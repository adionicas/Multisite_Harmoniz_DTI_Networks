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

### Generate t heatmap for each harmonization


```R

for(t in 2:12){

tvalmat_ncbt=matrix(data=NA, nrow=6, ncol=6)
pvalmat=matrix(data=NA, nrow=6, ncol=6)

for(i in 1:6) {
	for (j in 1:6) {
	if (i != j) {
v1_ncbt <- params_before_combat[which(params_before_combat$Site==i | params_before_combat$Site==j),t]
v2_ncbt <- params_before_combat$Site[which(params_before_combat$Site==i | params_before_combat$Site==j)]
tmp_df_ttest_ncbt <- data.frame(v1_ncbt,v2_ncbt)
# names(tmp_df_ttest_ncbt) <- c(variable,group)
pvalmat[i,j] <- t.test(tmp_df_ttest_ncbt[,1]~tmp_df_ttest_ncbt$v2_ncbt,mu=0,alternative="two.sided",paired=F)$p.value
tvalmat_ncbt[i,j] <- t.test(tmp_df_ttest_ncbt[,1]~tmp_df_ttest_ncbt$v2_ncbt,mu=0,alternative="two.sided",paired=F)$statistic[["t"]]
}}}

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

colnames(tvalmat_ncbt)  <- c("Calgary","Edmonton","Montreal 1","Montreal 2","Ottawa","Vancouver")
rownames(tvalmat_ncbt)  <- c("Calgary","Edmonton","Montreal 1","Montreal 2","Ottawa","Vancouver")

melted_cormat_ncbt <- melt(get_upper_tri(tvalmat_ncbt))

melted_cormat_ncbt$value <- round(melted_cormat_ncbt$value, digits = 1)

tvalmat_cbprm=matrix(data=NA, nrow=6, ncol=6)
pvalmat=matrix(data=NA, nrow=6, ncol=6)

for(i in 1:6) {
	for (j in 1:6) {
	if (i != j) {
v1_cbprm <- data_after_combat_on_params[which(data_after_combat_on_params$Site==i | data_after_combat_on_params$Site==j),t]
v2_cbprm <- data_after_combat_on_params$Site[which(data_after_combat_on_params$Site==i | data_after_combat_on_params$Site==j)]
tmp_df_ttest_cbprm <- data.frame(v1_cbprm,v2_cbprm)
# names(tmp_df_ttest_cbprm) <- c(variable,group)
pvalmat[i,j] <- t.test(tmp_df_ttest_cbprm[,1]~tmp_df_ttest_cbprm$v2_cbprm,mu=0,alternative="two.sided",paired=F)$p.value
tvalmat_cbprm[i,j] <- t.test(tmp_df_ttest_cbprm[,1]~tmp_df_ttest_cbprm$v2_cbprm,mu=0,alternative="two.sided",paired=F)$statistic[["t"]]
}}}

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

colnames(tvalmat_cbprm)  <- c("Calgary","Edmonton","Montreal 1","Montreal 2","Ottawa","Vancouver")
rownames(tvalmat_cbprm)  <- c("Calgary","Edmonton","Montreal 1","Montreal 2","Ottawa","Vancouver")

# melted_cormat_cbprm <- melt(a, na.rm = TRUE)
melted_cormat_cbprm <- melt(get_upper_tri(tvalmat_cbprm))

melted_cormat_cbprm$value <- round(melted_cormat_cbprm$value, digits = 1)

tvalmat_cbwei=matrix(data=NA, nrow=6, ncol=6)
pvalmat=matrix(data=NA, nrow=6, ncol=6)

for(i in 1:6) {
	for (j in 1:6) {
	if (i != j) {
v1_cbwei <- data_after_combat_on_weights[which(data_after_combat_on_weights$Site==i | data_after_combat_on_weights$Site==j),t]
v2_cbwei <- data_after_combat_on_weights$Site[which(data_after_combat_on_weights$Site==i | data_after_combat_on_weights$Site==j)]
tmp_df_ttest_cbwei <- data.frame(v1_cbwei,v2_cbwei)
# names(tmp_df_ttest_cbwei) <- c(variable,group)
pvalmat[i,j] <- t.test(tmp_df_ttest_cbwei[,1]~tmp_df_ttest_cbwei$v2_cbwei,mu=0,alternative="two.sided",paired=F)$p.value
tvalmat_cbwei[i,j] <- t.test(tmp_df_ttest_cbwei[,1]~tmp_df_ttest_cbwei$v2_cbwei,mu=0,alternative="two.sided",paired=F)$statistic[["t"]]
}}}

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}


colnames(tvalmat_cbwei)  <- c("Calgary","Edmonton","Montreal 1","Montreal 2","Ottawa","Vancouver")
rownames(tvalmat_cbwei)  <- c("Calgary","Edmonton","Montreal 1","Montreal 2","Ottawa","Vancouver")
# cormat <- reorder_cormat(tvalmat_cbwei)

# melted_cormat_cbwei <- melt(a, na.rm = TRUE)
melted_cormat_cbwei <- melt(get_upper_tri(tvalmat_cbwei))

melted_cormat_cbwei$value <- round(melted_cormat_cbwei$value, digits = 1)

#grand_max <- max(max(melted_cormat_cbwei$value, na.rm=T),max(melted_cormat_cbprm$value, na.rm=T), max(melted_cormat_ncbt$value, na.rm=T))

#if(grand_max > 0){
#grand_max <- ceiling_dec(grand_max,1)
#} else {
#grand_max <- floor_dec(grand_max,1)}


#grand_min <- min(min(melted_cormat_cbwei$value, na.rm=T),min(melted_cormat_cbprm$value, na.rm=T), min(melted_cormat_ncbt$value, na.rm=T))

#grand_min <- floor_dec(grand_min,1)

grand_max <- 45
grand_min <- -45

ggheatmap <- ggplot(melted_cormat_ncbt, aes(melted_cormat_ncbt$Var2, melted_cormat_ncbt$Var1, fill = value))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "forestgreen", high = "steelblue4", mid = "ghostwhite", na.value = "white",
   midpoint = 0, limit = c(grand_min,grand_max), space = "Lab", 
    name="t value",breaks=c(grand_min,0,grand_max),labels=c(grand_min,0,grand_max)) +
  theme_minimal()+ # minimal theme
 theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 13, hjust = 1), axis.text.y = element_text(angle = 45, size = 13))+
 coord_fixed()

ggheatmap + 
geom_text(aes(Var2, Var1, label = value), color = "black", size = 5, fontface = "bold") +
theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
  panel.grid.major = element_blank(), panel.border = element_blank(),
  panel.background = element_blank(), axis.ticks = element_blank(),
	legend.title=element_blank(), legend.justification=c(1,0), legend.position="none") +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                title.position = "top", title.hjust = 0.5))

filename = paste("tval_plot_unharmonized", colnames(params_before_combat)[t], ".png", sep="_")
ggsave(filename, width = 4.8, height = 4.3, device='png', dpi=700)


ggheatmap <- ggplot(melted_cormat_cbprm, aes(melted_cormat_cbprm$Var2, melted_cormat_cbprm$Var1, fill = value))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "forestgreen", high = "steelblue4", mid = "ghostwhite", na.value = "white",
   midpoint = 0, limit = c(grand_min,grand_max), space = "Lab", 
    name="t value", breaks=c(grand_min,0,grand_max), labels=c(grand_min,0,grand_max)) +
  theme_minimal()+
 theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 13, hjust = 1), axis.text.y = element_text(angle = 45, size = 13))+ coord_fixed()

ggheatmap + 
geom_text(aes(Var2, Var1, label = value), color = "black", size = 5, fontface = "bold") +
theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.grid.major = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.ticks = element_blank(),
	legend.title=element_blank(), legend.justification=c(1,0), legend.position="none") +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                title.position = "top", title.hjust = 0.5))

filename = paste("tval_plot_harmonized_params", colnames(data_after_combat_on_params)[t], ".png", sep="_")
ggsave(filename, width = 4.8, height = 4.3, device='png', dpi=700)


ggheatmap <- ggplot(melted_cormat_cbwei, aes(melted_cormat_cbwei$Var2, melted_cormat_cbwei$Var1, fill = value))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "forestgreen", high = "steelblue4", mid = "ghostwhite", na.value = "white",
   midpoint = 0, limit = c(grand_min,grand_max), space = "Lab", 
    name="t value", breaks=c(grand_min,0,grand_max),labels=c(grand_min,0,grand_max)) +
  theme_minimal()+ # minimal theme
 theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 13, hjust = 1), axis.text.y = element_text(angle = 45, size = 13))+
 coord_fixed()

ggheatmap + 
geom_text(aes(Var2, Var1, label = value), color = "black", size = 5,fontface = "bold") +
theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
  panel.grid.major = element_blank(), panel.border = element_blank(),
  panel.background = element_blank(), axis.ticks = element_blank(),
  # legend.justification = c(1, 0), legend.position = c(0.6, 0.7),
  # legend.direction = "horizontal")+
	legend.title=element_blank(), legend.justification=c(1,0), legend.position="none") +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                title.position = "top", title.hjust = 0.5))

filename = paste("tval_plot_combat_on_wei", colnames(data_after_combat_on_weights)[t], ".png", sep="_")
ggsave(filename, width = 4.8, height = 4.3, device='png', dpi=700)
}


```


### Make ICC on ptincipal diagonal


```R
library(psych)

for(t in 2:12){

ICC_combat_on_params=matrix(data=NA, nrow=6, ncol=6)
ICC_combat_on_weights=matrix(data=NA, nrow=6, ncol=6)


for(i in 1:6) {
before_combat <- params_before_combat[which(params_before_combat$Site==i),t]
after_combat_params <- data_after_combat_on_params[which(data_after_combat_on_params$Site==i),t]
after_combat_weights <- data_after_combat_on_weights[which(data_after_combat_on_weights$Site==i),t]

df_before_after_params <- data.frame(before_combat,after_combat_params)
df_before_after_weights <- data.frame(before_combat,after_combat_weights)

ICC_results_params <- ICC(df_before_after_params)
ICC_combat_on_params[i,i] <- ICC_results_params$results$ICC[3]

ICC_results_weights <- ICC(df_before_after_weights)
ICC_combat_on_weights[i,i] <- ICC_results_weights$results$ICC[3]
}


colnames(ICC_combat_on_params)  <- c("Calgary","Edmonton","Montreal 1","Montreal 2","Ottawa","Vancouver")
rownames(ICC_combat_on_params)  <- c("Calgary","Edmonton","Montreal 1","Montreal 2","Ottawa","Vancouver")
colnames(ICC_combat_on_weights)  <- c("Calgary","Edmonton","Montreal 1","Montreal 2","Ottawa","Vancouver")
rownames(ICC_combat_on_weights)  <- c("Calgary","Edmonton","Montreal 1","Montreal 2","Ottawa","Vancouver")

melted_cormat_ICC_combat_on_params <- melt(get_upper_tri(ICC_combat_on_params))
melted_cormat_ICC_combat_on_params$value <- round(melted_cormat_ICC_combat_on_params$value, digits = 2)

melted_cormat_ICC_combat_on_weights <- melt(get_upper_tri(ICC_combat_on_weights))
melted_cormat_ICC_combat_on_weights$value <- round(melted_cormat_ICC_combat_on_weights$value, digits = 2)


grand_max <- max(max(melted_cormat_ICC_combat_on_params$value, na.rm=T),max(melted_cormat_ICC_combat_on_weights$value, na.rm=T))

if(grand_max > 0){
grand_max <- ceiling_dec(grand_max,1)
} else {
grand_max <- floor_dec(grand_max,1)}

grand_min <- min(min(melted_cormat_ICC_combat_on_params$value, na.rm=T),min(melted_cormat_ICC_combat_on_weights$value, na.rm=T))
grand_min <- floor_dec(grand_min,1)


ggheatmapICC <- ggplot(melted_cormat_ICC_combat_on_weights, aes(melted_cormat_ICC_combat_on_weights$Var2, melted_cormat_ICC_combat_on_weights$Var1, fill = value))+


 geom_tile(color = "white")+
 scale_fill_gradient2(low = "antiquewhite", high = "red2", mid = "darkgoldenrod1", na.value = "white",

   midpoint = 0.5, limit = c(0,1), space = "Lab", 
    name="ICC", breaks=c(0,0.5,1),labels=c(0,0.5,1)) +
  theme_minimal()+ # minimal theme
 theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 13, hjust = 1), axis.text.y = element_text(angle = 45, size = 13))+
 coord_fixed()


ggheatmapICC + 
geom_text(aes(Var2, Var1, label = value), color = "black", size = 5, fontface = "bold") +
theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
  panel.grid.major = element_blank(), panel.border = element_blank(),
  panel.background = element_blank(), axis.ticks = element_blank(),
  # legend.justification = c(1, 0), legend.position = c(0.6, 0.7),
  # legend.direction = "horizontal") +
	legend.title=element_blank(), legend.justification=c(1,0), legend.position="none") +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                title.position = "top", title.hjust = 0.5))

filename = paste("ICC_plot_combat_on_wei", colnames(data_after_combat_on_weights)[t], ".png", sep="_")
ggsave(filename, width = 4.8, height = 4.3, device='png', dpi=700)


ggheatmapICC <- ggplot(melted_cormat_ICC_combat_on_params, aes(melted_cormat_ICC_combat_on_params$Var2, melted_cormat_ICC_combat_on_params$Var1, fill = value))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "antiquewhite", high = "red2", mid = "darkgoldenrod1", na.value = "white",
   midpoint = 0.5, limit = c(0,1), space = "Lab", 
    name="ICC", breaks=c(0,0.5,1),labels=c(0,0.5,1)) +
  theme_minimal()+ # minimal theme
 theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 13, hjust = 1), axis.text.y = element_text(angle = 45, size = 13))+
 coord_fixed()


ggheatmapICC + 
geom_text(aes(Var2, Var1, label = value), color = "black", size = 5, fontface = "bold") +
theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
  panel.grid.major = element_blank(), panel.border = element_blank(),
  panel.background = element_blank(), axis.ticks = element_blank(),
  # legend.justification = c(1, 0), legend.position = c(0.6, 0.7),
  # legend.direction = "horizontal") +
	legend.title=element_blank(), legend.justification=c(1,0), legend.position="none") +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                title.position = "top", title.hjust = 0.5))

filename = paste("ICC_plot_combat_on_param", colnames(data_after_combat_on_params)[t], ".png", sep="_")
# ggsave(path = filename, width = width, height = height, device='tiff', dpi=700)
ggsave(filename, width = 4.8, height = 4.3, device='png', dpi=700)
}

```


### Combine t with ICC [bash code]


```R
files=`ls | grep ICC| cut -d "." -f 1`

for file in $files; do
convert "$file".png -transparent white "$file"transparent.png
done

parameters="Assortativity ClustCoeff Efficiency Modularity SmallWorld Density BetweennessCentrality Degree_centrality Hierarchy ShortestPath Synchronization"
for param in $parameters; do
composite ICC_plot_combat_on_param_"$param"_transparent.png tval_plot_harmonized_params_"$param"_.png -dissolve "100%" ICC_tval_plot_harmonized_params_"$param".png
composite ICC_plot_combat_on_wei_"$param"_transparent.png tval_plot_combat_on_wei_"$param"_.png -dissolve "100%" ICC_tval_plot_harmonized_weights_"$param".png
done

parameters="Assortativity ClustCoeff Efficiency Modularity SmallWorld Density BetweennessCentrality Degree_centrality Hierarchy ShortestPath Synchronization"
for param in $parameters; do
convert tval_plot_unharmonized_"$param"_.png ICC_tval_plot_harmonized_weights_"$param".png ICC_tval_plot_harmonized_params_"$param".png +append ICC_tval_plot_harmonized_params_"$param"_combined.png
done

```


