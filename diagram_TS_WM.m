clear all
ctd = readtable("~/Desktop/WHOI/Data/output_data/test_gamma_n/ctd_profiles_table_WM.csv");

%% plot TS with WM%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scatter(ctd.psal, ctd.potTemp, 5, ctd.water_mass, 'filled');
grid on;
xlabel('Salinity (PSU)');
ylabel('Potential Temperature (°C)');
colormap(flipud(jet));
axis([33.9,34.9,-2.2,0]);
cb=colorbar()
cpos=get(cb,'Position');
cpos(3) = cpos(3)/4;
caxis([1 6])
set(cb,'YTick',[1,2,3,4,5],'YTickLabel',{'AASW' ,'mCDW','ISW','DWS', 'mSW'})
title(cb, 'Water Masses');
%set(get(cb,'title'),'string','Prey encounter events per day','Rotation',90.0)
Ylim_avantcontour= get(cb,'ylim');

% hold on
% % freezing point line
% cd /Users/saralabrousse/Dropbox/All_PHD_2/PHD/Projet_Weddell_Sea_2017/Seal_analysis_oceano/function_matlab
% fp_line = sw_fp([33.7:0.01:35],0);
% plot([33.7:0.01:35],fp_line,'c--','linewidth',2)
% 
% hold on 
% %contour(Gamma.S,Gamma.T, squeeze(Gamma.grid(2,:,:))',[28.03 28.27],'k','linewidth',2)
% axis([34,34.9,-2.2,0]);
% set(gca,'Fontsize',16);xlabel('Salinity (psu)'); ylabel('Potential Temperature ( \circ C)');
% title('All hunting TS data');
% % a=text(34.6,1.7 , ['28.03'],'Fontsize',14);
% % b=text(34.8,1.2 , ['28.27'],'Fontsize',14);
% 
% caxis(Ylim_avantcontour);
% set(gca,'Fontsize',16);
% cd /Users/saralabrousse/Dropbox/All_PHD_2/PHD/Projet_Weddell_Sea_2017/Seal_analysis_oceano/plots
% print -dpng hunting_WM
