% Workspace Preparation
% This script is meant to be run at the beginning of each script in this
% project to prepare MATLAB with paths and other code that is redundant in
% each script
%
% Matt Kmiecik
%
% Started 5 August 2021
%

% Sets working directory ----
main_dir = 'M:\kevin-cpm\'; % 'C:\Analysis\kevin-cpm\'
data_dir = fullfile(main_dir, 'data\');
output_dir = fullfile(main_dir, 'output\');

% Initializations
% raw_data_path = 'C:\Users\pains\Desktop\matt-eeglab\data\1-raw\'; % path to raw data
% chan_loc_path = 'C:\Users\pains\Desktop\matt-eeglab\data\0-global-vars\matts-32-chan-TP9.bvef';
% outpath = 'C:\Users\pains\Desktop\matt-eeglab\data\2-prepro'; % save out path
%subjs = string({RAW{2:size(RAW,1),1}}); %{'324'}; % Initializes subjects for batch processing (if applicable)

% Starts EEGLAB ----
[ALLEEG, EEG, CURRENTSET, ALLCOM] = eeglab;

% Loads in participant information ----
% [NUM,TXT,RAW] = xlsread();
