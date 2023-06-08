%% Loop to assign num pol to each dive

new_polycontour3_buff04 = load("C:/Users/lucie/Dropbox/codes/scripts_stage_Lucie/Matlab/new_polycontour_21_multiple_buff04.mat");
new_polycontour3_buff04 = new_polycontour3_buff04.new_polycontour3_buff04;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% START ICI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dives = readtable("C:/Users/lucie/Dropbox/codes/data_SES_polynya/filtered_dive_ses_polynya_5meters_bathy_hunt_bentpela_NumPolMult04_shelf_BehaInd_MNDepHT");

% Create a zeros column for polynya number
pol = zeros(size(dives,1),1);
dives.pol = pol;

% Extract the year/month/day of each dive
[y,m,d] = ymd(dives.date_fin);
year_date = y; % To save the original years

% Modify each year to match with the numbers used by Esther (1 to 16)
k = 1;
years = unique(y);
for num = 1:numel(years)
    YEAR = years(num);
    y(y == YEAR) = k;
    k = k+1;
end

% Loop
for i = 1:size(dives,1)
    %disp(i)
    month = m(i);
    year = y(i);
    for j = 1:21
        for L = 1:length(new_polycontour3_buff04{year}{month}{j})
            if length(new_polycontour3_buff04{year}{month}{j}{L}) ~= 1 % La condition est qu'il faut qu'on ait les contours pour cette polynie pour le mois considéré. length = 1 signifie NaN
                in = inpolygon(dives.lon(i),dives.lat(i),new_polycontour3_buff04{year}{month}{j}{L}(:,2),new_polycontour3_buff04{year}{month}{j}{L}(:,1)); 
                if in == 1
                    dives.pol(i)=j;
                end
%             else
%                 dives.pol(i)=NaN;
            end 
        end
    end
end

writetable(dives,"C:/Users/lucie/Dropbox/codes/data_SES_polynya/filtered_dive_ses_polynya_5meters_bathy_hunt_bentpela_NumPolMult04_shelf_BehaInd_MNDepHT.txt");

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STOP ICI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Figure check association num pol to dive

dives_num = readtable("C:/Users/lucie/Dropbox/codes/data_SES_polynya/filtered_dive_ses_polynya_5meters_bathy_hunt_bentpela_NumPolMult.txt");

cd /Users/lucie/Dropbox/codes/m_map/

year = 7;
month = 4;

figure
m_proj('stereographic','lat',-90,'long',0,'radius',30);
[CS,CH]=m_etopo2('contourf',[-6000:1000:-1000 -500 -200 0 200:100:500 1000:500:3000],'edgecolor','none');
m_grid('xtick',12,'tickdir','in','ytick',[-60 -70 -80],'linest','--');
colormap([ m_colmap('blues',60); m_colmap('green',30)]);
brighten(.2);
caxis([-6000 3000]);

hold on
for jpo=1:21
   m_plot(polon(jpo,:),polat(jpo,:),'k','linewidth',1);
   m_plot(new_polycontour2{year}{month}{jpo}(:,2),new_polycontour2{year}{month}{jpo}(:,1),'r','linewidth',1);
   a = find(dives_num.id == "ct77-891-12");% & dives_num.pol ~= 0); % Select the dives of the individual
   dives_id = dives_num(a,:); % Table with only the dives of the individual
   dives_id_m = find(m(a) == month); % Select the line number of the current month
   m_plot(dives_id.lon(dives_id_m),dives_id.lat(dives_id_m),'b','linewidth',1);
end

%% Figure check association num pol to dive (multiple contours)
cd /Users/lucie/Dropbox/codes/m_map/

year = 7;
month = 4;

figure
m_proj('stereographic','lat',-90,'long',0,'radius',30);
[CS,CH]=m_etopo2('contourf',[-6000:1000:-1000 -500 -200 0 200:100:500 1000:500:3000],'edgecolor','none');
m_grid('xtick',12,'tickdir','in','ytick',[-60 -70 -80],'linest','--');
colormap([ m_colmap('blues',60); m_colmap('green',30)]);
brighten(.2);
caxis([-6000 3000]);

hold on
for jpo=1:21
   m_plot(polon(jpo,:),polat(jpo,:),'k','linewidth',1);
   for L=1:length(new_polycontour3{year}{month}{jpo})
       if length(new_polycontour3{year}{month}{jpo}{L}) ~= 1
           m_plot(new_polycontour3{year}{month}{jpo}{L}(:,2),new_polycontour3{year}{month}{jpo}{L}(:,1),'r','linewidth',1);
       end
   end
end
a = find(dives_num.id == "ct77-891-12" & dives_num.pol ~= 0); % Select the dives of the individual
dives_id = dives_num(a,:); % Table with only the dives of the individual
dives_id_m = find(m(a) == month); % Select the line number of the current month
m_plot(dives_id.lon(dives_id_m),dives_id.lat(dives_id_m),'b','linewidth',1);

%% Plot the polynya mask from Esther

addpath 'C:\Users\lucie\Dropbox\codes\m_map'
figure
m_proj('stereographic','lat',-90,'long',0,'radius',30);
[CS,CH]=m_etopo2('contourf',[-6000:1000:-1000 -500 -200 0 200:100:500 1000:500:3000],'edgecolor','none');
m_grid('xtick',12,'tickdir','in','ytick',[-60 -70 -80],'linest','--');
colormap([ m_colmap('blues',60); m_colmap('green',30)]);
brighten(.2);
caxis([-6000 3000]);
hold on
m_plot(lomask{year}{month},lamask{year}{month},'b')
for L=1:length(polycontour{year}{month})
    m_plot(polycontour{year}{month}{L}(:,2),polycontour{year}{month}{L}(:,1),'g')
end
for jpo=1:21
   m_plot(polon(jpo,:),polat(jpo,:),'k','linewidth',1);
   m_plot(new_polycontour2{year}{month}{jpo}(:,2),new_polycontour2{year}{month}{jpo}(:,1),'r','linewidth',1);
end


%% Assign num pol to dives with the bigest contour

new_polycontour3_bigest = load("C:/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/codes/scripts_stage_Lucie/Matlab/new_polycontour_21_multiple_bigest_month.mat");
new_polycontour3_bigest = new_polycontour3_bigest.new_polycontour3_bigest;

dives = readtable("C:/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/codes/data_SES_polynya/filtered_dive_ses_polynya_5meters_bathy_hunt_bentpela_NumPolMult04_shelf_BehaInd_MNDepHT.txt");
save=dives;

% Create a zeros column for polynya number
pol = zeros(size(dives,1),1);
dives.pol = pol;

% Extract the year/month/day of each dive
[y,m,d] = ymd(dives.date_fin);

% Loop
for i = 1:size(dives,1)
    %disp(i)
    month = m(i);
    for j = 1:21
        for L = 1:length(new_polycontour3_bigest{month}{j})
            if length(new_polycontour3_bigest{month}{j}{L}) ~= 1 % La condition est qu'il faut qu'on ait les contours pour cette polynie pour le mois considéré. length = 1 signifie NaN
                in = inpolygon(dives.lon(i),dives.lat(i),new_polycontour3_bigest{month}{j}{L}(:,2),new_polycontour3_bigest{month}{j}{L}(:,1)); 
                if in == 1
                    dives.pol(i)=j;
                end
            end 
        end
    end
end

writetable(dives,"C:/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/codes/data_SES_polynya/filtered_dive_ses_polynya_5meters_bathy_hunt_bentpela_NumPolMultBigest_shelf_BehaInd_MNDepHT.txt");
