% EEG Preprocessing Pipeline Step 1 - second experiment
% This is for the second experiment
% Matt Kmiecik
% Started 5 August 2021

workspace_prep % Prepares workspace

% Preprocessing ----

% Creating variables
% this_subj = dir(strcat(raw_data_path, "visual_", subjs(i), "*.vhdr")); % grabs subject info
% this_subj.id = strcat('vis-',char(subjs(i))); % eeglab file name
% outname = strcat(this_subj.id,'-prepro.set'); % save out subject name
this_ss = "kevin-cpm-2.vhdr";
this_ss_name = 'kevin-cpm-2';

% Loads in raw data using loadbv() from BrainVision plugin ----
EEG = pop_loadbv(data_dir, this_ss, [], []);
[ALLEEG, EEG, CURRENTSET] = pop_newset(ALLEEG, EEG, 0,'setname',this_ss_name,'gui','off');

% Importing channel locations
% FIGURE THIS OUT LATER
% https://github.com/arnodelorme/bva-io/blob/master/loadbvef.m
% chanlocs = loadbvef(fullfile(data_dir, 'CACS-64-X7_M43-V1_no-REF.bvef'));
% EEG.chanlocs = chanlocs(2:65); %  removes ground

EEG=pop_chanedit(EEG, 'load',{'C:\Analysis\kevin-cpm\data\template-dig.elp' 'filetype' 'autodetect'},...
    'changefield',{4 'datachan' 0},'changefield',{37 'datachan' 0},...
    'append',69,'append',70,'changefield',{70 'labels' 'EMG'},...
    'changefield',{70 'type' 'EMG'},'changefield',{70 'datachan' 1},...
    'changefield',{71 'labels' 'Stimulator'},'changefield',{71 'type' 'Stimulator'},...
    'changefield',{71 'datachan' 1});

% Removing DC offset by subtracting the mean signal from each electrode
EEG = pop_rmbase(EEG, [],[]);

% Downsampling to 256 Hz ----
EEG = pop_resample( EEG, 256);

% Highpass filter at 1Hz (-6dB @ 1Hz, 425 point highpass, 2Hz transition band width)
EEG = pop_eegfiltnew(EEG,'locutoff',2,'plotfreqz',0);

% Cleanline - Removing electrical line noise @ 60 Hz
EEG = pop_cleanline(EEG, 'bandwidth',2,'chanlist',[1:66],'computepower',1,...
    'linefreqs',60,'normSpectrum',0,'p',0.01,'pad',2,'plotfigures',0,...
    'scanforlines',1,'sigtype','Channels','tau',100,'verb',1,'winsize',4,'winstep',1);

% visualizes the pulses
% plot(EEG.data(66,40000:41000)) % zoom in on the zaps (4 pulses; 5ms apart)

% I think here is a good place to mark for stims ----
% First step, find durations of button presses which indicate start of
% CPM blocks; these lasted no more than 35 seconds
nevents = length(EEG.event);
for i = 1:nevents
    if EEG.event(i).type == "BtnPressed"
        btn_dur(i) = EEG.event(i).latency;
    else
        % skip
    end
end
btn_dur = nonzeros(btn_dur)'; % lazy way of eliminating index errors

% Next step is to determine the first spike; given their regular occurence,
% once every second, we can project these from the first spike
n_samples = EEG.pnts; % the amount of samples in recording
first_zap_dur = 0; % init
for i = 1:n_samples
    if EEG.data(66,i) > 100000
        first_zap_dur = i;
        if first_zap_dur > 0
            break
        end
    else
        %skip
    end
end

% Next, add an event every second from first_zap_dur to indicate a zap
nevents = length(EEG.event);
zap_durs = first_zap_dur:EEG.srate:EEG.pnts;
for i = 1:length(zap_durs)
    nevents = length(EEG.event);
    if zap_durs(i) < btn_dur(1)
        EEG.event(nevents + 1).latency = zap_durs(i);   % latency of marker
        EEG.event(nevents + 1).type = 'zap_baseline';   % new marker name
        EEG.event(nevents + 1).duration = 1;            % arbitratry dur
    elseif zap_durs(i) > btn_dur(1) && zap_durs(i) < btn_dur(1)+(EEG.srate*35)
        EEG.event(nevents + 1).latency = zap_durs(i);   % latency of marker
        EEG.event(nevents + 1).type = 'zap_cpm';        % new marker name
        EEG.event(nevents + 1).duration = 1;            % arbitratry dur
    elseif zap_durs(i) > btn_dur(1)+(EEG.srate*35) && zap_durs(i) < btn_dur(2) 
        EEG.event(nevents + 1).latency = zap_durs(i);   % latency of marker
        EEG.event(nevents + 1).type = 'zap_postcpm';    % new marker name
        EEG.event(nevents + 1).duration = 1;            % arbitratry dur
    elseif zap_durs(i) > btn_dur(2) && zap_durs(i) < btn_dur(2)+(EEG.srate*35)
        EEG.event(nevents + 1).latency = zap_durs(i);   % latency of marker
        EEG.event(nevents + 1).type = 'zap_cpm';        % new marker name
        EEG.event(nevents + 1).duration = 1;            % arbitratry dur
    elseif zap_durs(i) > btn_dur(2)+(EEG.srate*35)
        EEG.event(nevents + 1).latency = zap_durs(i);   % latency of marker
        EEG.event(nevents + 1).type = 'zap_postcpm';    % new marker name
        EEG.event(nevents + 1).duration = 1;            % arbitratry dur
    end
end

% Check for consistency and reorder the events chronologically
EEG = eeg_checkset(EEG, 'eventconsistency');
%eeglab redraw;

% Lowpass ---- I'm debating whether to lowpass
% EEG = pop_eegfiltnew(EEG, 'hicutoff',30,'chantype',{'EEG'});

% Manually cut off the end where stimulation stopped !_!_!_!_!_!_!_!_!_!_!_
EEG = eeg_eegrej( EEG, [181986 184525] );
[ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'overwrite','on','gui','off'); 

% Saves out preprocessed data for inspection
EEG = pop_saveset(EEG,'filename','kevin-cpm-prepro-2','filepath',output_dir);

