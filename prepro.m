% EEG Preprocessing Pipeline Step 1
% Matt Kmiecik
% Started 5 August 2021

workspace_prep % Prepares workspace

% Preprocessing ----

% Creating variables
% this_subj = dir(strcat(raw_data_path, "visual_", subjs(i), "*.vhdr")); % grabs subject info
% this_subj.id = strcat('vis-',char(subjs(i))); % eeglab file name
% outname = strcat(this_subj.id,'-prepro.set'); % save out subject name
this_ss = "kevin-cpm.vhdr";
this_ss_name = 'kevin-cpm';

% Loads in raw data using loadbv() from BrainVision plugin ----
EEG = pop_loadbv(data_dir, this_ss, [], []);
[ALLEEG, EEG, CURRENTSET] = pop_newset(ALLEEG, EEG, 0,'setname',this_ss_name,'gui','off');

% Importing channel locations
% usetemplate-dig
EEG=pop_chanedit(EEG, 'load',{'M:\\kevin-cpm\\data\\template-dig.elp' 'filetype' 'autodetect'},...
    'changefield',{4 'datachan' 0},'changefield',{37 'datachan' 0},...
    'append',69,'append',70,'changefield',{70 'labels' 'Stimulator'},...
    'changefield',{70 'type' 'Stimulator'},'changefield',{70 'datachan' 1},...
    'changefield',{71 'labels' 'EMG'},'changefield',{71 'type' 'EMG'},...
    'changefield',{71 'datachan' 1});

% Removing DC offset by subtracting the mean signal from each electrode
EEG = pop_rmbase(EEG, [],[]);

% Downsampling to 256 Hz ----
EEG = pop_resample( EEG, 256);

% Highpass filter at 1Hz (-6dB @ 1Hz, 425 point highpass, 2Hz transition band width)
EEG = pop_eegfiltnew(EEG,'locutoff',2,'plotfreqz',0);

% Cleanline - Removing electrical line noise @ 60 Hz
EEG = pop_cleanline(EEG, 'bandwidth',2,'chanlist',[1:66] ,'computepower',1,'linefreqs',60,'normSpectrum',0,'p',0.01,'pad',2,'plotfigures',0,'scanforlines',1,'sigtype','Channels','tau',100,'verb',1,'winsize',4,'winstep',1);

% I think here is a good place to mark for stims ----
% First step, find durations of button presses which indicate start/stop of
% CPM blocks
nevents = length(EEG.event);
for i = 1:nevents
    if EEG.event(i).type == "BtnPressed"
        btn_dur(i) = EEG.event(i).latency;
    else
        % skip
    end
end
btn_dur = nonzeros(btn_dur)'; % lazy way of eliminating index errors

% Next, cycle through samples to detect large spikes indicating shock and
% sort zaps accordingly
n_samples = EEG.pnts; % the amount of samples in recording
for i = 1:n_samples
    nevents = length(EEG.event); % number of current EEG events
    if EEG.data(65,i) < -1000 %stim_volt
        if i < btn_dur(1) % baseline
        EEG.event(nevents + 1).latency = i;             % latency of marker
        EEG.event(nevents + 1).type = 'zap_baseline';   % new marker name
        EEG.event(nevents + 1).duration = 1;            % arbitratry dur
        
        elseif i >= btn_dur(1) && i <= btn_dur(2) % CPM block 1
        EEG.event(nevents + 1).latency = i;         % latency of marker
        EEG.event(nevents + 1).type = 'zap_cpm';    % new marker name
        EEG.event(nevents + 1).duration = 1;        % arbitratry dur
        
        elseif i >= btn_dur(2) && i <= btn_dur(3) % post-cpm-1
        EEG.event(nevents + 1).latency = i;         % latency of marker
        EEG.event(nevents + 1).type = 'zap_post_cpm';    % new marker name
        EEG.event(nevents + 1).duration = 1;        % arbitratry dur
        
        elseif i >= btn_dur(3) && i <= btn_dur(4) % CPM block 2
        EEG.event(nevents + 1).latency = i;         % latency of marker
        EEG.event(nevents + 1).type = 'zap_cpm';    % new marker name
        EEG.event(nevents + 1).duration = 1;        % arbitratry dur
        
        elseif i > btn_dur(4) % post-cpm-2
        EEG.event(nevents + 1).latency = i;             % latency of marker
        EEG.event(nevents + 1).type = 'zap_post_cpm';   % new marker name
        EEG.event(nevents + 1).duration = 1;            % arbitratry dur
        end
    end
end

% Check for consistency and reorder the events chronologically
EEG = eeg_checkset(EEG, 'eventconsistency');
%eeglab redraw;

% Lowpass ---- I'm debating whether to lowpass
% EEG = pop_eegfiltnew(EEG, 'hicutoff',30,'chantype',{'EEG'});

% Saves out preprocessed data for inspection
EEG = pop_saveset(EEG,'filename','kevin-cpm-prepro','filepath',output_dir);

