lat = readtable("~/Desktop/WHOI/Data/ctd_data/gamma_n_data/lat.csv");
lon = readtable("~/Desktop/WHOI/Data/ctd_data/gamma_n_data/lon.csv");

sal = importdata('~/Desktop/WHOI/Data/ctd_data/gamma_n_data/psalinity.csv', ',');
sal = sal.data;
sal(sal==-999) = NaN;

temp = importdata('~/Desktop/WHOI/Data/ctd_data/gamma_n_data/pot_temp.csv', ',');
temp = temp.data;
temp(temp==-999) = NaN;

pressure = importdata('~/Desktop/WHOI/Data/ctd_data/gamma_n_data/pressure.csv', ',');
pressure = pressure.data;
pressure(pressure==-999) = NaN;

lon = lon.x;
lat = lat.x;

[g,dg_lo,dg_hi] = eos80_legacy_gamma_n(sal, temp, pressure, lon, lat);

writematrix(g, "~/Desktop/WHOI/Data/ctd_data/gamma_n_data/gamma.csv", "Delimiter", ";")
