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

% ICA Label procedure ----
EEG = pop_iclabel(EEG,'default'); % labels IC components
threshold = ...
    [NaN NaN;   ... % Brain
    NaN NaN;    ... % Muscle
    0.8 1;      ... % Eye
    NaN NaN;    ... % Heart
    0.95 1;    ... % Line Noise
    NaN NaN;    ... % Channel Noise
    NaN NaN];       % Other  
EEG = pop_icflag(EEG,threshold); % flags ICs that exceed threshold
art_comps = find(EEG.reject.gcompreject > 0); % finds these flagged ICs
EEG = pop_subcomp(EEG,art_comps',0); % removes these artifactual ICs 

% You could lowpass here
EEG = pop_eegfiltnew(EEG, 'hicutoff',30,'channels',...
    {'G1','G2','G3','G4','G5','G6','G7','G8','G9','G10','G11','G12','G13',...
    'G14','G15','G16','G17','G18','G19','G20','G21','G22','G23','G24','G25',...
    'G26','G27','G28','G29','G30','G31','G32','Y1','Y2','Y3','Y4','Y5','Y6',...
    'Y7','Y8','Y9','Y10','Y11','Y12','Y13','Y14','Y15','Y16','Y17','Y18',...
    'Y19','Y20','Y21','Y22','Y23','Y24','Y25','Y26','Y27','Y28',...
    'Y29','Y30','Y31','Y32'});

% Extracting epochs ---
EEG = pop_epoch(EEG,{'zap_baseline' 'zap_cpm' 'zap_post_cpm'}, [-0.2 0.8],...
    'newname','epochs','epochinfo','yes');
epoch_start = EEG.times(1); % start in seconds
epoch_end = EEG.times(EEG.pnts); % stop in seconds
EEG = pop_rmbase(EEG,[epoch_start 0],[],[1:64]); % baseline corrects only EEG

% Epoch thresholding for extreme values (+- 100uv) - only EEG channels
EEG = pop_eegthresh(EEG,...
    1,...                   % 1 = raw data
    [1:64],...              % EEG chans
    -100,100,...            % +- 100 microvolts
    epoch_start/1000,...    % using vars defined above for beginning and end
    epoch_end/1000,...     % using vars defined above for beginning and end
    0,...                   % 0 = do not superimpose
    1);                     % 1 = immediately rej trials
[ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'setname','epochs','gui','off');

% Draw ERPs
EEG = pop_selectevent( EEG, 'type',{'zap_baseline'},'deleteevents','on','deleteepochs','on','invertepochs','off');
[ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'setname','baseline','gui','off'); 

[ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 2,'retrieve',1,'study',0); % switch back

EEG = pop_selectevent( EEG, 'type',{'zap_cpm'},'deleteevents','on','deleteepochs','on','invertepochs','off');
[ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'setname','cpm','gui','off'); 

[ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 3,'retrieve',1,'study',0); % switch back

EEG = pop_selectevent( EEG, 'type',{'zap_post_cpm'},'deleteevents','on','deleteepochs','on','invertepochs','off');
[ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 4,'setname','post_cpm','gui','off');

% Baseline vs. post-cpm
pop_comperp(ALLEEG, 1, 2,4,'addavg','on','addstd','off','subavg','on','diffavg','off','diffstd','off','chans',[1:64] ,'tplotopt',{'ydir',1});

% Baseline vs. cpm
pop_comperp(ALLEEG, 1, 2,3,'addavg','on','addstd','off','subavg','on','diffavg','off','diffstd','off','chans',[1:64] ,'tplotopt',{'ydir',1});
