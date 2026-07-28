
%%%This code generates projections (for the next eight decades) for
%%%catch-per-trip of both cod and haddock for Area=1 and Mode=3, using 2019 as
%%%baseline year for the projections. It creates catch-per trip prohections
%%%under six alternative assumptions about the jointness in residuals for
%%%the cod and haddock equations: independence, and five different copulas
%%%(Frank, Gumbel, Clayton, Guassian, and Plackett). This code also
%%%estimates the parameters for these five alternative
%%%copulas, as well as 95% confidence intervals.

% Repository directories
script_file = mfilename('fullpath');
step_code_dir = fileparts(script_file);
code_dir = fileparts(step_code_dir);
project_dir = fileparts(code_dir);

catch_reg_dir = fullfile(project_dir, 'data', 'processed', 'catch_regressions');
bottom_temp_dir = fullfile(project_dir, 'data', 'processed', 'bottom_temperature');
copula_input_dir = fullfile(project_dir, 'data', 'processed', 'copula_projection_inputs');
copula_output_dir = fullfile(project_dir, 'work', '05_copula_projection');
toolbox_dir = fullfile(project_dir, 'environment', 'Patton_copula_toolbox');

addpath(genpath(toolbox_dir));

if ~isfolder(copula_output_dir)
    mkdir(copula_output_dir);
end

tic
rng('default') % For reproducibility
parpool(32);
%Loading the catch-per-trip estimates
parameters=importdata(fullfile(catch_reg_dir, 'parameters_SUEST_area1newmode3.xlsx'));
sigma=importdata(fullfile(catch_reg_dir, 'sigma_SUEST_area1newmode3.xlsx'));
residuals_cod=importdata(fullfile(catch_reg_dir, 'residuals_area1newmode3_cod.txt'));
residuals_had=importdata(fullfile(catch_reg_dir, 'residuals_area1newmode3_haddock.txt'));
temp_GOM=readtable(fullfile(bottom_temp_dir, 'decadal_bottom_temp_distributions_levels.xlsx'));
ffdays12=readtable(fullfile(copula_input_dir, 'ffdays12_area13_2019.xlsx'));
ssb_summary=readtable(fullfile(copula_input_dir, 'normalized_ssb.xlsx'));


%Creating the table "residuals"
residuals=table(residuals_cod.data,residuals_had.data);
residuals=renamevars(residuals,["Var1","Var2"],["r_cod","r_had"]);
R=corrcoef(residuals.r_cod,residuals.r_had);

if ismember('sd_temp', temp_GOM.Properties.VariableNames)
    temp_GOM.sd_temp=[];
end
if ismember('Var1', temp_GOM.Properties.VariableNames)
    temp_GOM.Var1=[];
end
N=10000; %Setting the number of pairs of catch-per-trip for cod and haddock that we will generate for each state, month and decade.
varNames=["cod_ssb","had_ssb"];
varTypes=["double","double"];
ssb=table('Size',[size(temp_GOM,1) 2],'VariableTypes',varTypes,'VariableNames',varNames);%
ssb.cod_ssb=ssb_summary{ssb_summary.year==2019,2}*ones(size(temp_GOM,1),1); %Level for 2019 normalized by the mean of the time series for cod
ssb.had_ssb=ssb_summary{ssb_summary.year==2019,3}*ones(size(temp_GOM,1),1); %Level for 2019 normalized by the mean of the time series for haddock

%Create a table "dat" that combines the table temp_GOM and two vectors
%containing the value of ssb for cod and haddock
dat=horzcat(temp_GOM,ssb);
dat=movevars(dat,3,'After',4); 
%Expanding table dat N times vertically
info=repmat(dat,N,1);
toDelete = info.month<3;
info(toDelete,:) = [];
info.season=info.month;
info.season(info.season==3)=4;
info.season(info.season==12)=11;

%Fitting a Normal distribution to the residuals from the Tobit 2 model
pd_cod=fitdist(residuals.r_cod,'Normal');
pd_had=fitdist(residuals.r_had,'Normal');

info.season4=(info.season==4);
info.season5=(info.season==5);
info.season6=(info.season==6);
info.season7=(info.season==7);
info.season8=(info.season==8);
info.season9=(info.season==9);
info.season10=(info.season==10);
info.season11=(info.season==11);
data1=info;
data1.MA=ones(size(info,1),1);
data1.ME=zeros(size(info,1),1);
data1.NH=zeros(size(info,1),1);
data2=info;
data2.MA=zeros(size(info,1),1);
data2.ME=ones(size(info,1),1);
data2.NH=zeros(size(info,1),1);
data3=info;
data3.MA=zeros(size(info,1),1);
data3.ME=zeros(size(info,1),1);
data3.NH=ones(size(info,1),1);
data=[data1;data2;data3];
clearvars data1 data2 data3 dat info toDelete

T=horzcat(data.season4,data.season5,data.season6,data.season7,data.season8,data.season9,data.season10,data.season11);
M=horzcat(data.MA,data.ME,data.NH);

%Generate a sample of parameters for the cod and haddock equations, by drawing fom the Tobit model estimates
param=mvnrnd(parameters.data,sigma.data,size(data,1));
Index1 =find(contains(parameters.textdata(1,:), 'cod13_selection'),1,'last'); %Finding the number of estimated parameters for cod.
Index2 =find(contains(parameters.textdata(1,:), 'cod13_lntot_cat_cod'),1,'last'); 
Index3 =find(contains(parameters.textdata(1,:), 'cod13_lnsigma'),1,'first'); 
Index4 =find(contains(parameters.textdata(1,:), 'haddock13_selection'),1,'last'); %Finding the number of estimated parameters for haddock.
Index5 =find(contains(parameters.textdata(1,:), 'haddock13_lnsigma'),1,'first'); 
param_cod_selec=param(:,1:Index1);
param_cod_catch=param(:,Index1+1:Index2);
param_had_selec=param(:,Index3+2:Index4);
param_had_catch=param(:,Index4+1:end-2);

%Demeaning the temperature data (since that is how it was estimated using
%the Tobit model)
data.average_temp=data.average_temp-ssb_summary{ssb_summary.year==2019,4}; %Demeaning the bottom temperatures, the last column of the table ssb_summary contains the mean T for the series. 

ffdays=randsample(ffdays12.ffdays12,size(data.cod_ssb,1),true);
data.ffdays12=ffdays;

%Generating the predicted values (not including the residuals yet) 
obs_cod=horzcat(data.cod_ssb,data.average_temp,data.average_temp.*data.average_temp,data.cod_ssb.*data.average_temp,data.cod_ssb.*data.cod_ssb,data.had_ssb,data.cod_ssb.*data.had_ssb,T,data.cod_ssb.*T,data.average_temp.*T,M,ffdays.*T,ffdays,ones(size(data,1),1),T,T,data.cod_ssb,data.cod_ssb,ones(size(data,1),1));
obs_had=horzcat(data.had_ssb,data.average_temp,data.average_temp.*data.average_temp,data.had_ssb.*data.average_temp,data.had_ssb.*data.had_ssb,data.cod_ssb,data.cod_ssb.*data.had_ssb,T,data.had_ssb.*T,data.average_temp.*T,M,ffdays.*T,ffdays,ones(size(data,1),1),ffdays,ffdays,data.had_ssb,data.had_ssb,ones(size(data,1),1));
%Determining the prob. of "observing positive catch-per-trip
data.prob_cod=normcdf(sum(param_cod_selec.*obs_cod,2));
data.observed_cod=(data.prob_cod>rand(length(data.prob_cod),1));
data.prob_had=normcdf(sum(param_had_selec.*obs_had,2));
data.observed_had=(data.prob_had>rand(length(data.prob_had),1));
%Calculating expected catch-per-trip, conditional on catching some positive
%number of fish
data.predicted_cod=exp(sum(param_cod_catch.*obs_cod,2)+0.5*exp(param(:,Index3)).^2).*data.observed_cod; %Note that for computing the predicted catch-per-trip, we need to correct for the variance of out estimates in ln when doing the back-transportation (i.e., the censored prediction on line 661 of STATA function code nehurdle_p.ado, located N:\Faculty\JHolzer\ado\Harris_Steinback_DePiper_Holzer\simulation\cod_haddock\data, you can see that the unconditional prediction (latent variable prediction I gave you yesterday) is defined as exp(`varn' + `sig'^2 / 2)). Moreover, we are multiplying for the pob. of being observed because we are ourselves doing the sensoring of the latent vatianle. THAT is precisely why we are using the latent varuiable y* in the fisrt place.
data.predicted_had=exp(sum(param_had_catch.*obs_had,2)+0.5*exp(param(:,Index5)).^2).*data.observed_had;

data_c=data;
data_f=data;
data_g=data;
data_p=data;
data_n=data;

options = optimset('Display','iter','TolCon',10^-12,'TolFun',10^-4,'TolX',10^-6);

%Estimating the five copulas from draws from the residuals pairs 
n=10000;
paramhat_c=zeros(1,n);
paramhat_f=zeros(1,n);
paramhat_g=zeros(1,n);
paramhat_p=zeros(1,n);
paramhat_n=zeros(1,n);
LL=zeros(1,N*10); %Log-likelihood for the case of the Plackett copula (from Patton's toolbox)

for i=1:n
x=randi(length(residuals.r_cod),1,7500);
u=ksdensity(residuals.r_cod(x),residuals.r_cod(x),'function','cdf');
v=ksdensity(residuals.r_had(x),residuals.r_had(x),'function','cdf');
paramhat_c(i) = copulafit('Clayton',[u v]);
paramhat_f(i)= copulafit('Frank',[u v]);
paramhat_g(i)= copulafit('Gumbel',[u v]);
rhohat=copulafit('Gaussian',[u v]);
paramhat_n(i)=rhohat(1,2);
%Estimating the Plackett copula using Andrew Patton's copula toolbox for
%Matlab, available at: https://public.econ.duke.edu/~ap172/
lower=0.0001; 
theta0=1;
[paramhat_p(i), LL(i)] = fmincon('plackettCL',theta0,[],[],[],[],lower,[],[],options,[u,v]);
end

%Determining the mean, variance and t-statistic for the parameters  estimated above (for the five copulas)
mparamhat_c=mean(paramhat_c);
mparamhat_f=mean(paramhat_f);
mparamhat_g=mean(paramhat_g);
mparamhat_p=mean(paramhat_p);
mparamhat_n=mean(paramhat_n);
covparam_c=cov(paramhat_c);
covparam_f=cov(paramhat_f);
covparam_g=cov(paramhat_g);
covparam_p=cov(paramhat_p);
covparam_n=cov(paramhat_n);
statistic_c=mparamhat_c/sqrt(covparam_c);
statistic_f=mparamhat_f/sqrt(covparam_f);
statistic_g=mparamhat_g/sqrt(covparam_g);
statistic_p=mparamhat_p/sqrt(covparam_p);
statistic_n=mparamhat_n/sqrt(covparam_n);
ci_c=quantile(paramhat_c,[0.025, 0.975]);
ci_f=quantile(paramhat_f,[0.025, 0.975]);
ci_g=quantile(paramhat_g,[0.025, 0.975]);
ci_p=quantile(paramhat_p,[0.025, 0.975]);
ci_n=quantile(paramhat_n,[0.025, 0.975]);


clearvars x u v residuals ssb varNames varTypes temp_GOM param_cod param_had param

%Generating residuals from the three copulas using the inverse CDF
%transform based on the estimated normal distributions pd_cod and pd_had
rc_c=[];
rh_c=[];
rc_f=[];
rh_f=[];
rc_g=[];
rh_g=[];
rc_p=[];
rh_p=[];
rc_n=[];
rh_n=[];

z=size(data_c,1)/1000;

parfor j=1:N/1000
u_c=copularnd('Clayton',mparamhat_c,z*100);    
u_f=copularnd('Frank',mparamhat_f,z*100);    
u_g=copularnd('Gumbel',mparamhat_g,z*100); 
u_p=plackett_rnd(mparamhat_p,z*100,7);
u_n=copularnd('Gaussian',mparamhat_n,z*100); 
r_cod_c=icdf(pd_cod,u_c(:,1));
r_had_c=icdf(pd_had,u_c(:,2));
r_cod_f=icdf(pd_cod,u_f(:,1));
r_had_f=icdf(pd_had,u_f(:,2));
r_cod_g=icdf(pd_cod,u_g(:,1));
r_had_g=icdf(pd_had,u_g(:,2));
r_cod_p=icdf(pd_cod,u_p(:,1));
r_had_p=icdf(pd_had,u_p(:,2));
r_cod_n=icdf(pd_cod,u_n(:,1));
r_had_n=icdf(pd_had,u_n(:,2));
rc_c=vertcat(rc_c,r_cod_c);
rh_c=vertcat(rh_c,r_had_c);
rc_f=vertcat(rc_f,r_cod_f);
rh_f=vertcat(rh_f,r_had_f);
rc_g=vertcat(rc_g,r_cod_g);
rh_g=vertcat(rh_g,r_had_g);
rc_p=vertcat(rc_p,r_cod_p);
rh_p=vertcat(rh_p,r_had_p);
rc_n=vertcat(rc_n,r_cod_n);
rh_n=vertcat(rh_n,r_had_n);
end

%Generating the independent pairs of residuals (by randomly permutating the corresponding vectors)
r_cod=rc_g(randperm(length(rc_g)));
r_had=rh_g(randperm(length(rh_g)));
%Determining the linear correlation between the generated pairs of residuals 
corrcoef(rc_c,rh_c)
corrcoef(rc_f,rh_f)
corrcoef(rc_g,rh_g)
corrcoef(rc_p,rh_p)
corrcoef(rc_n,rh_n)
corrcoef(r_cod,r_had) %The correlation in this case should be close to zero

%Generating the correlated pairs of catch-per-trip (for the five copulas)
data_c.cod_corr=data_c.predicted_cod+rc_c;
data_c.cod_corr(data_c.observed_cod==0 | data_c.cod_corr<0)=0;
data_c.had_corr=data_c.predicted_had+rh_c;
data_c.had_corr(data_c.observed_had==0 | data_c.had_corr<0)=0;
data_f.cod_corr=data_f.predicted_cod+rc_f;
data_f.cod_corr(data_f.observed_cod==0| data_f.cod_corr<0)=0;
data_f.had_corr=data_f.predicted_had+rh_f;
data_f.had_corr(data_f.observed_had==0 | data_f.had_corr<0)=0;
data_g.cod_corr=data_g.predicted_cod+rc_g;
data_g.cod_corr(data_g.observed_cod==0| data_g.cod_corr<0)=0;
data_g.had_corr=data_g.predicted_had+rh_g;
data_g.had_corr(data_g.observed_had==0 | data_g.had_corr<0)=0;
data_p.cod_corr=data_p.predicted_cod+rc_p;
data_p.cod_corr(data_p.observed_cod==0| data_p.cod_corr<0)=0;
data_p.had_corr=data_p.predicted_had+rh_p;
data_p.had_corr(data_p.observed_had==0 | data_p.had_corr<0)=0;
data_n.cod_corr=data_n.predicted_cod+rc_n;
data_n.cod_corr(data_n.observed_cod==0| data_n.cod_corr<0)=0;
data_n.had_corr=data_n.predicted_had+rh_n;
data_n.had_corr(data_n.observed_had==0 | data_n.had_corr<0)=0;

%Generating the independent pairs of catch-per-trip
data_c.cod_ind=data_c.predicted_cod+r_cod;
data_c.cod_ind(data_c.observed_cod==0 | data_c.cod_ind<0)=0;
data_c.had_ind=data_c.predicted_had+r_had;
data_c.had_ind(data_c.observed_had==0 | data_c.had_ind<0)=0;
data_c.area=1*ones(size(data_c,1),1);
data_c.state=repmat("A1",size(data_c,1),1);
data_f.cod_ind=data_f.predicted_cod+r_cod;
data_f.cod_ind(data_f.observed_cod==0 | data_f.cod_ind<0)=0;
data_f.had_ind=data_f.predicted_had+r_had;
data_f.had_ind(data_f.observed_had==0 | data_f.had_ind<0)=0;
data_f.area=1*ones(size(data_f,1),1);
data_f.state=repmat("A1",size(data_f,1),1);
data_g.cod_ind=data_g.predicted_cod+r_cod;
data_g.cod_ind(data_g.observed_cod==0 | data_g.cod_ind<0)=0;
data_g.had_ind=data_g.predicted_had+r_had;
data_g.had_ind(data_g.observed_had==0| data_g.had_ind<0)=0;
data_g.area=1*ones(size(data_g,1),1);
data_g.state=repmat("A1",size(data_g,1),1);
data_p.cod_ind=data_p.predicted_cod+r_cod;
data_p.cod_ind(data_p.observed_cod==0 | data_p.cod_ind<0)=0;
data_p.had_ind=data_p.predicted_had+r_had;
data_p.had_ind(data_p.observed_had==0| data_p.had_ind<0)=0;
data_p.area=1*ones(size(data_p,1),1);
data_p.state=repmat("A1",size(data_p,1),1);
data_n.cod_ind=data_n.predicted_cod+r_cod;
data_n.cod_ind(data_n.observed_cod==0 | data_n.cod_ind<0)=0;
data_n.had_ind=data_n.predicted_had+r_had;
data_n.had_ind(data_n.observed_had==0| data_n.had_ind<0)=0;
data_n.area=1*ones(size(data_n,1),1);
data_n.state=repmat("A1",size(data_n,1),1);

%Saving the output (correlated and independent samples for the projected eight decades):
writetable(data_c, fullfile(copula_output_dir, 'correlated_sample_Clayton_area1_newmode3_2019.csv'));
writetable(data_f, fullfile(copula_output_dir, 'correlated_sample_Frank_area1_newmode3_2019.csv'));
writetable(data_g, fullfile(copula_output_dir, 'correlated_sample_Gumbel_area1_newmode3_2019.csv'));
writetable(data_p, fullfile(copula_output_dir, 'correlated_sample_Plackett_area1_newmode3_2019.csv'));
writetable(data_n, fullfile(copula_output_dir, 'correlated_sample_Gaussian_area1_newmode3_2019.csv'));

clearvars T r_cod r_had rc_c rc_f rc_g rc_n rc_p rh_c rh_f rh_g rh_n rh_p

save(fullfile(copula_output_dir, 'correlated_data_area1_newmode3_2019.mat'), '-v7.3'); 
toc;
delete(gcp('nocreate'));
