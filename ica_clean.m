% Script for ICA artifact correction
% Matt Kmiecik
% Started 09 AUG 2021

workspace_prep % Prepares workspace (see src/...)

% Loads in preprocessed data ----
data = 'kevin-cpm-prepro.set';
EEG = pop_loadset('filename',data,'filepath',output_dir);

% Loads ICA weights ----
load(fullfile(output_dir, 'kevin-cpm-ica.mat')); % loads res structure
EEG = pop_editset(EEG, 'icaweights', res.icaweights, 'icachansind', res.icachansind); % assigns ICA weights

EEG = pop_iclabel(EEG,'default'); % labels IC components
threshold = ...
    [NaN NaN;   ... % Brain
    NaN NaN;    ... % Muscle
    0.8 1;      ... % Eye
    NaN NaN;    ... % Heart
    NaN NaN;    ... % Line Noise
    NaN NaN;    ... % Channel Noise
    NaN NaN];      % Other  
EEG = pop_icflag(EEG,threshold);

% Removes ICs that were classified as artifactual ----
art_comps =  find(EEG.reject.gcompreject > 0);
EEG = pop_subcomp(EEG, art_comps', 0);



