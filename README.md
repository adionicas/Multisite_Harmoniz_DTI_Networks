# Multisite_Harmoniz_DTI_Networks



```R
dat <- read.table("connectome_weights_BEFORE_COMBAT_DTI_B0.txt", header = FALSE, sep = " ", dec = ".")
data <- read.table("DEMO_TABLE_DTI_B0.txt", header = TRUE, sep = "\t", dec = ".")

batch=data$Study_MRI
age=data$age
injury=as.factor(data$injury)
gender=as.factor(data$gender)
gender <- factor(gender, levels=c("Male","Female"), labels=c(0,1))

library(matrixStats)
library(neuroCombat)

mod <- model.matrix(~injury+age+gender)

dat=t(dat)

data.harmonized <- neuroCombat(dat=dat, batch=batch, mod=mod, parametric=TRUE)

write.table(t(data.harmonized[["dat.combat"]]), file="connectome_weights_AFTER_COMBAT_DTI_B0.txt", sep="\t", col.names = F, row.names = F)

```

2. Reconstruct connectivity matrices after combat

```matlab
% import relevant data
vals_under_diag_before_combat = importdata('connectome_weights_BEFORE_COMBAT_DTI_B0.txt');
vals_under_diag_after_combat = importdata('connectome_weights_AFTER_COMBAT_DTI_B0.txt');
% check
isequal(size(vals_under_diag_before_combat),size(vals_under_diag_after_combat))
% set the 0 vals back to 0
vals_under_diag_after_combat = vals_under_diag_after_combat.*(vals_under_diag_before_combat>=0.1);
%isequal(size(vals_under_diag_after_combat), size(vals_under_diag_before_combat))
vals_under_diag_after_combat = vals_under_diag_after_combat.*(vals_under_diag_after_combat>=0);

% Reconstruct into connectivity materices for each sub
a=rand(90);
idx_under_diag=find(tril(a,-1));

sub_order = importdata('sublist_DTI_B0.txt');

% Loop through all the subs
for m = 1 : size(vals_under_diag_after_combat,1)
% Make an empty sub matrix
sub_mat=zeros(90);
sub_mat(idx_under_diag) = vals_under_diag_after_combat(m,:);
% MAke it symmetric
sub_mat = sub_mat'+sub_mat;
% write outputs as txt
%filename = ["../data_after_combat/" + sub_order(m) + "_connectome_mat_COMBAT_DTI_B0.txt"];
%dlmwrite(filename,sub_mat,'delimiter',' ')
% write outputs as mat
%filename = ["../data_after_combat/" + sub_order(m) + "_connectome_mat_COMBAT_DTI_B0"];
%save(filename,'sub_mat')

end

```
