clear all
close all

%% IMPORT DATA

dives = readtable("C:/Users/lucie/Dropbox/codes/data_SES_polynya/filtered_dive_ses_polynya_5meters_bathy_hunt_bentpela_NumPolMult_shelf_BehaInd.txt");

%% ADD EMPTY COLUMN

MeanDepthHT = zeros(size(dives,1),1);
dives.MeanDepthHT = MeanDepthHT;

%% LOOP TO COMPUTE MEAN DEPTH HUNTING TIME

for i=1:size(dives,1)
    %disp(i)
    k = find(dives{i,38:42} == 1); % index of k that are equal to 1
    if length(k) ~= 0
        tim = dives{i,33:37}; % select the tim values
        [tim_max,tim_max_ind] = max(tim(k)); % find the index of the max tim for which k=1
        depth = dives{i,7:12}; % select the 5 points depth
        mean_depth = (depth(k(tim_max_ind))+depth(k(tim_max_ind)+1))/2; % compute the mean
        dives.MeanDepthHT(i) = mean_depth; % add the value to the column
    end
end

%% SAVE THE NEW DIVES TABLE

writetable(dives,"C:/Users/lucie/Dropbox/codes/data_SES_polynya/filtered_dive_ses_polynya_5meters_bathy_hunt_bentpela_NumPolMult_shelf_BehaInd_MNDepHT.txt");


