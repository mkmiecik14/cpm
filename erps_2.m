% Script for averaging and saving out ERPs
% Matt Kmiecik
% Started 23 AUG 2021

workspace_prep % Prepares workspace (see src/...)

% Loads in preprocessed data ----
data = 'kevin-cpm-epochs-2.set';
EEG = pop_loadset('filename',data,'filepath',output_dir);

% Extracting epochs by condition and then saving out ----

% Baseline
baseline = pop_selectevent(EEG,'type',{'zap_baseline'},'deleteevents','on','deleteepochs','on','invertepochs','off');
save(fullfile(output_dir, 'baseline.mat'), 'baseline') % saves out

% CPM
cpm = pop_selectevent(EEG,'type',{'zap_cpm'},'deleteevents','on','deleteepochs','on','invertepochs','off');
save(fullfile(output_dir, 'cpm.mat'), 'cpm') % saves out

% Post-CPM
postcpm = pop_selectevent(EEG,'type',{'zap_postcpm'},'deleteevents','on','deleteepochs','on','invertepochs','off');
save(fullfile(output_dir, 'postcpm.mat'), 'postcpm') % saves out

% OLD
% baseline_erp.trials = size(baseline.data, 3); % number of trials in grandaverage
% baseline_erp.data = baseline.data; % grand average data
% baseline_erp.erp = mean(baseline.data, 3); % grand average
% save(fullfile(output_dir, 'baseline-erp.mat'), 'baseline_erp') % saves out
% cpm_erp.trials = size(cpm.data, 3); % number of trials in grandaverage
% cpm_erp.data = cpm.data; % grand average data
% cpm_erp.erp = mean(baseline.data, 3); % grand average
% postcpm.trials = size(postcpm.data, 3); % number of trials in grandaverage
% baseline_erp.data = baseline.data; % grand average data
% baseline_erp.erp = mean(baseline.data, 3); % grand average


