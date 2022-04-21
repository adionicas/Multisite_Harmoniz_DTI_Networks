# Multisite_Harmoniz_DTI_Networks


## A. Matrix harmonization

### 1. Extract elements below diagonal
This step is done with matlab - extracts elements from under diagonal of adjacency matrices in vals_under_diag and exports as .txt across sub and a list of sub_id for refrence. It also save matrices after cleaning the diagnonal.
```matlab
mkdir no_diag_matrices_DTI
mkdir DATA_BEFORE_COMBAT
% make a list of files containing the matrices
matrix_dir_files = dir('DTI_raw_FC/fc/sub-*_ses-B0_FP_sorted_GR_TV_MD_C_native_Tracts_DTI_FA_PASS.mat');
folder_path = {matrix_dir_files.folder};
matrix_files={matrix_dir_files.name};
a=rand(90);
idx_under_diag=find(tril(a,-1));
for i = 1 : length(matrix_files)
	W = importdata(folder_path{i} + "/" +  matrix_files{i});
	W(isnan(W)) = 0;
	W(1:size(W,1)+1:end)=0; % clear diagonal
	% find indices below diag
	% obtain a vector from the indices; store vals_under_diag across scans 
	vals_under_diag(i,:)=W(idx_under_diag);
	% save sub_id of this sub
	sub_order{i} = [matrix_files{i}(5:12)];

% save mat with diag=0 as txt
	filename = ["no_diag_matrices_DTI/" + matrix_files{i}(1:12) + "_DTI_B0.txt"];
	save(filename,'W')

% save as mat with diag=0 .mat (can be inserted in Gretna)
	filename = ["no_diag_matrices_DTI/" + matrix_files{i}(1:12) + "_DTI_B0"];
	save(filename,'W')
end;
% save a sublist so we know the correspondence between sub_id and column in the dataframe
	filename = ["DATA_BEFORE_COMBAT/" + "sublist_DTI_B0.txt"];
	dlmwrite(filename,sub_order','delimiter','')
% save actual lower diagnonal dataframe
	filename = ["DATA_BEFORE_COMBAT/" + "connectome_weights_BEFORE_COMBAT_DTI_B0.txt"];
	dlmwrite(filename,vals_under_diag,'delimiter',' ')
	clear sub_order vals_under_diag


```
### 2. Run harmonization in R

```R
# import
dat <- read.table("connectome_weights_BEFORE_COMBAT_DTI_B0.txt", header = FALSE, sep = " ", dec = ".")
data <- read.table("DEMO_TABLE_DTI_B0.txt", header = TRUE, sep = "\t", dec = ".")

# prepare variables
batch=data$Study_MRI
age=data$age
injury=as.factor(data$injury)
gender=as.factor(data$gender)
gender <- factor(gender, levels=c("Male","Female"), labels=c(0,1))

library(matrixStats)
library(neuroCombat)

# Define matrix used to preserve the variance of group (injury), age and gender
mod <- model.matrix(~injury+age+gender)

dat=t(dat)

# run actual harmonization
data.harmonized <- neuroCombat(dat=dat, batch=batch, mod=mod, parametric=TRUE)

write.table(t(data.harmonized[["dat.combat"]]), file="connectome_weights_AFTER_COMBAT_DTI_B0.txt", sep="\t", col.names = F, row.names = F)

```

### 3. Reconstruct connectivity matrices after harmonization

```matlab
% import weights
vals_under_diag_before_combat = importdata('connectome_weights_BEFORE_COMBAT_DTI_B0.txt');
vals_under_diag_after_combat = importdata('connectome_weights_AFTER_COMBAT_DTI_B0.txt');
% check
isequal(size(vals_under_diag_before_combat),size(vals_under_diag_after_combat))
% set the initial 0 vals back to 0
vals_under_diag_after_combat = vals_under_diag_after_combat.*(vals_under_diag_before_combat>=0);

% Reconstruct connectivity materices for each sub

% Define indices under diagonal for a 90 x 90 matrix
a=rand(90);
idx_under_diag=find(tril(a,-1));

sub_order = importdata('sublist_DTI_B0.txt');

% Loop through all the subs
for m = 1 : size(vals_under_diag_after_combat,1)
% Make an empty sub matrix
sub_mat=zeros(90);
% Fill it
sub_mat(idx_under_diag) = vals_under_diag_after_combat(m,:);
% Make it symmetric
sub_mat = sub_mat'+sub_mat;
% Write outputs as txt
filename = ["../data_after_combat/" + sub_order(m) + "_connectome_mat_COMBAT_DTI_B0.txt"];
dlmwrite(filename,sub_mat,'delimiter',' ')
% Write outputs as mat
filename = ["../data_after_combat/" + sub_order(m) + "_connectome_mat_COMBAT_DTI_B0"];
save(filename,'sub_mat')

end

```

## B. Parameter harmonization


```R
library(readxl)
# Import dfata with no combat
data <- read_excel(file.choose())

# cp orig data to presevre subid for harmonized data (1st column)
data_after_combat_on_params <- data
# From replace param vals with nans (will be refilled with harmonized values)
data_after_combat_on_params[,2:12] <- NA


batch=data$Study_MRI
age=data$age
injury=as.factor(data$injury)
gender=as.factor(data$gender)
gender <- factor(gender, levels=c("Male","Female"), labels=c(0,1))

library(matrixStats)
library(neuroCombat)

mod <- model.matrix(~injury+age+gender)

for(i in 2:12){
data.harmonized <- neuroCombat(dat=t(data[,i]), batch=batch, mod=mod, parametric=TRUE, eb=FALSE)
data_after_combat_on_params[,i] <- t(data.harmonized[["dat.combat"]])}

library("writexl")
write_xlsx(data_after_combat_on_params,"AAL_ses-B0_global_params_GRETNA_propthr_wei_combat_on_parameters.xlsx")

```
