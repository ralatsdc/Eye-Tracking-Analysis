function dataMatrix = processorP9E1_v1_1()
%--------------------------------------------------------------------------
%function dataMatrix = processorP9E1_v1_0()
%data[1] = Subject
%data[2] = Session
%data[3] = ID
%data[4] = TETTime
%data[5] = RTTime
%data[6] = CursorX
%data[7] = CursorY
%data[8] = TimeStampSec
%data[9] = TimeStampMicroSec
%data[10] =
%data[11] =
%data[12] =
%data[13] =
%data[14] =
%data[15] =
%data[16] =
%data[17] =
%data[18] =
%data[19] =
%data[20] =
%data[21] =
%data[22] =
%data[23] =
%data[24] =
%data[25] =
%data[26] =
%data[27] =
%data[28] = RT
%data[29] = OnScreen
%data[30] = DirectionFG
%data[31] = DirectionBG
%data[32] = TargetAOI
%data[33] = TrialTypeFG
%data[34] = TrialTypeBG
%data[35] = TrialName
%data[36] = Cue
%data[37] = LeftAOI
%data[38] = RightAOI
%data[39] = AOIFixation
%data[40] = AOI
%data[41] = FixationLength
%data[42] = TotalFixations
%data[43] = SaccadicAmplitude
%data[44] = SaccadicLatency
%data[45] =
%--------------------------------------------------------------------------
%Adam S. Cohen, created on 2010/10/31
%last modified: 2012/01/24
%--------------------------------------------------------------------------

%open caselist here and store filepaths to caseFiles
%--------------------------------------------------------------------------
%caseFileID = fopen('caselist-2012-11-30_2EastAs.txt','r');
caseFileID = fopen('case_list.txt','r');
caseFiles = textscan(caseFileID,'%s');
totalSubj = size(caseFiles{1},1);
fclose(caseFileID);

%open output files for # of fixation and fixation duration, create headers
%--------------------------------------------------------------------------
%filename = strcat('results/firstFixLatencyTrial_P9E1_', datestr(now,30), '.csv');
filename = strcat('firstFixLatencyTrial.csv');
firstFixLatencyTrial = fopen(filename,'a');
fprintf(firstFixLatencyTrial, 'subject,session,trialNumber,RTTime,trialType,SOA,TrialTypeFG,TrialTypeBG,Latency\n');

for subj = 1:totalSubj
    
    %get filepath from caseFiles, open file, get headers, and store data to
    %dataraw
    %----------------------------------------------------------------------
    subjFile = caseFiles{1}{subj};
    file = fopen(subjFile);
    fprintf('%s\n',char(subjFile));
    [colHeads,dataStart] = textscan(file,'%s',44,'Delimiter','\t');
    fprintf('creating data matrix...\n')
    while(~feof(file))
        dataraw = textscan(file,'%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%s%s%f%f%s%s%s%s%s%s%s%s%s%s%s%s%f%f%f%f','Delimiter','\t');
    end
    
    %re-index dataraw to allow easier access of data, store to data
    %----------------------------------------------------------------------
    fprintf('re-indexing data in data matrix...\n')
    data = cell(size(dataraw{1},1),size(dataraw,2));
    for a = 1:size(dataraw{1},1)
        for b = 1:size(dataraw,2)
            data{a,b} = dataraw{1,b}(a,1);
        end
    end
    
    %transpose col heads and store in xls
    %----------------------------------------------------------------------
    colHeadsTransp = colHeads{:}';
    xls = [colHeadsTransp;data];
    
    %initialize trialNum counter and dataMatrix
    %----------------------------------------------------------------------
    trialNum = 0;
    [r,c] = size(data);
    dataMatrix = [];
    i=1;
    fprintf('processing data matrix...\n')
    
    while i <= r
        %count the rows
        %------------------------------------------------------------------
        fprintf('%6.0f',i)
        fprintf('\b\b\b\b\b\b')
        
        %only look at rows for test trials and cue is up
        %determine SOA by storing start of cue phase and end of cue phase  
        %and taking the difference
        %index through to get to end of cue phase, target onset
        %------------------------------------------------------------------        
        if strcmp(data{i,35},'test') && strcmp(data{i,29},'Cue')
            startSOA = data{i,5};
            while strcmp(data{i,29},'Cue') && i < r
                i = i + 1;
            end
            endSOA = data{i,5};
            SOA = endSOA - startSOA;
            
            subjNum = data{i,1};
            sesnNum = data{i,2};
            trialNum = trialNum + 1;
            trialType = data{i,35};
            
            %if target is up, index through rows as long as there is a zero
            %in either the fixation length column or the latency column and
            %the next row is not the next trial
            
            %only if both of these 
            %are not zero and the next row is not the next trial is there a saccade before
            %the target expires; otherwise there was either:            
            %a) a fixation before the fixation to the target
            %b) the fixation to the target has occurred but the script is looking 
            %at one of the rows where the latency info is repeated
            %c) there was no recorded saccade to the target
            
            %N.B. if it's the last trial of the experiment, the test
            %condition in the while loop will exceed matrix dimension of
            %data when looking to the next row to see if a new trial is
            %starting, so keep i < r and check before checking next-trial
            %--------------------------------------------------------------
            if strcmp(data{i,29},'Target')
                while (data{i,41} == 0 || data{i,44} == 0) && (i < r && ~strcmp(data{i+1,29},'Neutral'))
                    i = i + 1;
                end
                TrialTypeFG = data{i,33};
                TrialTypeBG = data{i,34};
                latency = data{i,44};
                RTTime = data{i,5};
            else
                TrialTypeFG = fprintf('error 1: %f', data{i,5});
                TrialTypeBG = fprintf('error 1: %f', data{i,5});
                latency = fprintf('error 1: %f', data{i,5});
                RTTime = data{i,5};                
            end
            
            dataMatrixNew = [subjNum sesnNum trialNum RTTime trialType SOA TrialTypeFG TrialTypeBG latency];
            dataMatrix = [dataMatrix; dataMatrixNew];            
        elseif strcmp(data{i,35},'catch') && strcmp(data{i,29},'Cue')
            startSOA = data{i,5};
            while strcmp(data{i,29},'Cue') && i < r
                i = i + 1;
            end
            endSOA = data{i,5};
            SOA = endSOA - startSOA;
            
            subjNum = data{i,1};
            subjNum = data{i,2};
            trialNum = trialNum + 1;
            trialType = data{i,35};
            
            if strcmp(data{i,29},'Target')
                while (data{i,41} == 0 || data{i,44} == 0) && (i < r && ~strcmp(data{i+1,29},'Neutral'))
                    i = i + 1;
                end
                TrialTypeFG = data{i,33};
                TrialTypeBG = data{i,34};
                latency = data{i,44};
                RTTime = data{i,5};                
            else
                TrialTypeFG = fprintf('error 2: %f', data{i,5});
                TrialTypeBG = fprintf('error 2: %f', data{i,5});
                latency = fprintf('error 2: %f', data{i,5});
                RTTime = data{i,5};                
            end
            
            %store variables in dataMatrixNew and append to dataMatrix
            %--------------------------------------------------------------
            dataMatrixNew = [subjNum sesnNum trialNum RTTime trialType SOA TrialTypeFG TrialTypeBG latency];
            dataMatrix = [dataMatrix; dataMatrixNew];
        end
        i = i + 1;
    end
    
    %write data to disk, one subject at a time
    %----------------------------------------------------------------------
    fprintf('writing data...\n')
    dataMatrixRows = size(dataMatrix,1);
    for j = 1:dataMatrixRows
        fprintf(firstFixLatencyTrial, '%d,',dataMatrix{j,1});
        fprintf(firstFixLatencyTrial, '%d,',dataMatrix{j,2});
        fprintf(firstFixLatencyTrial, '%d,',dataMatrix{j,3});
        fprintf(firstFixLatencyTrial, '%d,',dataMatrix{j,4});
        fprintf(firstFixLatencyTrial, '%s,',char(dataMatrix(j,5)));
        fprintf(firstFixLatencyTrial, '%d,',dataMatrix{j,6});
        fprintf(firstFixLatencyTrial, '%s,',char(dataMatrix(j,7)));
        fprintf(firstFixLatencyTrial, '%s,',char(dataMatrix(j,8)));
        fprintf(firstFixLatencyTrial, '%d,',dataMatrix{j,9});
        fprintf(firstFixLatencyTrial,'\n');
    end
end
fclose(firstFixLatencyTrial);
fclose('all');
