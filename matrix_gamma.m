clear;

t = -2:1e-3:1; %% Temperature
SP = 34:1e-3:36; %% Practical salinity

SP_ar = repelem(SP, length(t));
t_ar = repmat(t, 1, length(SP));
p = zeros(1,length(t_ar));
%p = repelem(0, length(t_ar));
lon = 0;
lat = -65;

gamma = eos80_legacy_gamma_n(SP_ar, t_ar, p, lon, lat);

df = transpose(vertcat(t_ar, SP_ar, gamma));

writematrix(df, "~/Desktop/WHOI/Data/ctd_data/gamma_n_data/gamma_matrix.csv", "Delimiter", ";")



