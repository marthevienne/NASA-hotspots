clear
close all

output_dir = '~/Dropbox/data/outputs_Marthe_2023/';

interval = "monthly";
%interval = "daily";

if interval == "monthly"
    file = "~/Dropbox/data/polynya_contours_NCAR_2023/SSMI.CDR.85%thresh.polynya2_sh.197901-202012.nc";
    output_file = strcat(output_dir, "polynyas_contours_monthly.gif");

    t1 = datetime(1979, 01, 01, "Format","MM-uuuu");
    t2 = datetime(2020, 12, 01, "Format", "MM-uuuu");
    t_array = t1:calmonths(1):t2;
else
    file = "~/Dropbox/data/polynya_contours_NCAR_2023/SSMI.CDR.85%thresh.polynya2_d_sh.19790101-20201231.nc";
    output_file = strcat(output_dir, "polynyas_contours_daily.gif");

    t1 = datetime(1979, 01, 01, "Format","dd-MM-uuuu");
    t2 = datetime(2020, 12, 31, "Format", "dd-MM-uuuu");
    t_array = t1:caldays(1):t2;
    t_array = t_array(~(t_array.Month == 2 & t_array.Day == 29));
end

polynyas = ncread(file, "polynyas");

lat = ncread(file, "tlat1d");
lon = ncread(file, "tlon1d");
res_lat = (max(lat) - min(lat))/length(lat); % constante
res_lon = (max(lon) - min(lon))/length(lon); % constante

% Focus on East Antarctica
subset = subset_zone(lat, lon, polynyas, -90, -40, 0, 160);
%subset = subset_zone(lat, lon, polynyas, -90, -40);

%Test loc
% loc.lon = 49.4375;
% loc.lat = -64.2624;
% x_grid = find(subset.lon <= loc.lon, 1, 'last');
% y_grid = find(subset.lat <= loc.lat, 1, 'last');
% map_loc =  NaN(length(subset.lon), length(subset.lat));
% map_loc(x_grid, y_grid) = 1;
% map_loc = transpose(map_loc);

for index = 1:length(t_array)
   map = transpose(subset.pol(:, : , index));
   t = datetime(t_array(index)); 
   h = pcolor(subset.lon, fliplr(subset.lat), map);
   set(h,'edgecolor','none')   
   h;
   title(string(t));
   if index == 1
       exportgraphics(gcf,output_file,'Append',false);
   else        
       exportgraphics(gcf,output_file,'Append',true);
   end
end

% Test: assign id pol to location
% for t = 1:504
%     loc.lon = 49.4375;
%     loc.lat = -64.2624;
%     %loc.date = datetime(1979, 05, 01, "Format","dd-MM-uuuu");
%     loc.date = t_array(t);
% 
%     i_loc = find(subset.lon <= loc.lon, 1, 'last');
%     j_loc = find(subset.lat <= loc.lat, 1, 'last');
%     map_loc =  NaN(length(subset.lat), length(subset.lon));
%     map_loc(j_loc, i_loc) = 1;
% 
%     [y,m,d] = ymd(loc.date);
% 
%     pol = get_contour_pol(subset.pol, t_array, y, m);
% 
%     if ~isnan(pol(~isnan(pol.*map_loc)))
%        "SES is in a polynya"
%        figure;
%        h = pcolor(subset.lon, fliplr(subset.lat), pol);
%        set(h,'edgecolor','none')   
%        h;
%        hold on
%        scatter(loc.lon, loc.lat);
%     else
%         "SES isn't in a polynya"
%     end
% end

%% USEFUL FUNCTIONS %%
%default args?
function m = subset_zone(lat, lon, polynyas, min_lat, max_lat, min_lon, max_lon)
    m.lat = lat(lat > min_lat & lat < max_lat);
    m.lon = lon(lon > min_lon & lon < max_lon);
    m.pol = polynyas(lon > min_lon & lon < max_lon, lat > min_lat & lat < max_lat, : );
end


function c = get_contour_pol(contours, t_array, year, month)
    index = t_array.Year == year & t_array.Month == month;
    c = transpose(contours(:, :, index));
end

% isInPol

% Test: Extract contours for all polynyas (code = {1, 2})
% id_pol = ncread(file, "polyID");
% n_pol = length(id_pol);
% n_months = 12;
% 
% polynyas_id = ncread(file, "polynya_ID");
% polynyas_t = transpose(polynyas_id(:,:,1));
% 
% % For now, pol with only one cell constraint are removed
% area = zeros(1, n_pol);
% for id = 1:length(id_pol)
%     if length(polynyas_t(polynyas_t == id)) > 1
%         pol = polynyas_t;
%         pol(pol ~= id) = 0;
%         figure;
%         [c, h] = contour(lon, lat, pol);
%         ind = find(c(1,:) == id);
%         for i = 1:length(ind)
%             n_points = c(2, ind(i)); % number of points along the contour
%             % pb avec nb de points
%             contour_pol.lon = c(1,ind(i) + 1:ind(i) + n_points);
%             contour_pol.lat = c(2,ind(i) + 1:ind(i) + n_points);
%             area(1, id) = polyarea(contour_pol.lat, contour_pol.lon); 
%             if polyarea(contour_pol.lat, contour_pol.lon) > 0
%                 figure;
%                 plot(contour_pol.lat, contour_pol.lon);
%             end
%         end
%     else
%        area(1,id) = 0; 
%     end
% end