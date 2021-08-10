% ICA Decomposition
% Matt Kmiecik
% Started 6 AUGUST 2021

workspace_prep % Prepares workspace (see src/...)

% Loads in preprocessed data ----
data = 'kevin-cpm-prepro.set';
EEG = pop_loadset('filename',data,'filepath',output_dir);

% Runs ICA ----
% Channel indices the ica will be performed on
% Importantly skips the EOG channel, as it doesn't have the same reference
% as EEG channels
this_chan_index = 1:64; % ensures ICA is performed on EEG channels only
EEG = pop_runica(EEG,'icatype','runica','extended',1,'interrupt','on','pca',64,'chanind', this_chan_index);

% Saving out results ----

% ICA settings and results
res.ss = data;                      % subject 
res.icaweights = EEG.icaweights;    % ica weights matrix
res.icachansind = this_chan_index;  % ica channel indices
  
% Saves out results to file ----
save(fullfile(output_dir, 'kevin-cpm-ica'),'res'); % saves out as matlab struct

