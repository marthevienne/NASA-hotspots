clear;
close all;

%% IMPORT DATA

% new_polycontour3_buff04 = load("C:/Users/lucie/Dropbox/codes/scripts_stage_Lucie/Matlab/new_polycontour_21_multiple_buff04.mat");
% new_polycontour3_buff04 = new_polycontour3_buff04.new_polycontour3_buff04;

% new_polycontour_21_multiple.mat contains all polynya contours for the 21
% polynyas identified (by Esther?) for each month (1 to 12) of the 16 years
% (2004 to 2019). For each of the 21 polynyas sometimes there is more than
% one contour, that's normal.

new_polycontour3 = load("~/Dropbox/Data_codes_Marthe_janvier2023_LUCIE/contour_polynie/new_polycontour_21_multiple.mat");
new_polycontour3 = new_polycontour3.new_polycontour3;

%% COMPUTE AREA OF EACH POLYGON AND SELECT THE BIGEST PER MONTH REGARDLESS OF THE YEAR

%% Tests of polyarea function
% plot(new_polycontour3_buff04{1}{1}{1}{1}(:,1),new_polycontour3_buff04{1}{1}{1}{1}(:,2))
% polyarea(new_polycontour3_buff04{1}{1}{1}{1}(:,1),new_polycontour3_buff04{1}{1}{1}{1}(:,2))

plot(new_polycontour3{1}{1}{1}{1}(:,1),new_polycontour3{1}{1}{1}{1}(:,2))
polyarea(new_polycontour3{1}{1}{1}{1}(:,1),new_polycontour3{1}{1}{1}{1}(:,2))

% To delete rows with NaN
X = new_polycontour3{1}{1}{1}{1};
test = X;
X(any(isnan(X), 2), :) = [];

plot(X(:,1),X(:,2))
polyarea(X(:,1),X(:,2))


%% Loop

new_polycontour3_bigest = repelem({repelem({repelem({nan},20)},21)},12); % Empty cell array to store the new contours
summary=zeros(12,21);

for pol=1:21
    for month=1:12
        area=zeros(20,16);
        for year=1:16
            cont=new_polycontour3{year}{month}{pol};
            for i=1:length(cont)
                X=cont{i};
                X(any(isnan(X), 2), :) = [];
                if isempty(X)==0
                    area(i,year) = polyarea(X(:,1),X(:,2));
                end
            end
            S=sum(area,1); % sum the area of each contour (if there is more than one contour for the polynya)
            [M,ind]=max(S);
            new_polycontour3_bigest{month}{pol}=new_polycontour3{ind}{month}{pol};
            summary(month,pol)=ind;
        end
    end
end







