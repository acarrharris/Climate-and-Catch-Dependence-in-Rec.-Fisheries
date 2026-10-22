
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% September, 2026 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%This code estimates the five copulas in the paper, including 95% CI, and
%it simulates pairs of residuals that will be used in the projections of
%catch-per-trip for cod and haddock as ocean temperatures change.

%Note: This code has been optimized with the help of Claude.

%Set global directories - user edit here
cd 'C:\Users\andrew.carr-harris\Desktop\Git\Climate-and-Catch-Dependence-in-Rec.-Fisheries\data'

clear; close all;
rng('default');  

%Reading the data:
data=readtable('logresiduals_Sept_28_2026_area2newmode2.csv');
label_output='area2_newmode2';
label_c='cod212';
label_h ='haddock212';
%Defining key parameters:
n_draws=2000000;
n_boots=5000;
COPULAS={'Clayton','Frank','Gumbel','Gaussian'}; %We use Patton's toolbox to deal with the Plackett copula

uc=data.(['u_' label_c]);  %logresidual for cod
uh=data.(['u_' label_h]); %logresidual for haddock
pc=data.(['pos_' label_c])==1;  %1 if trip caught cod, zero otherwise
ph=data.(['pos_' label_h])==1; %1 if trip caught haddock, zero otherwise
wt=data.new_wp_int; %Ssurvey weight (i.e., # of trips the trip actually represents)
psu=data.psu_id; %primary sampling unit (i.e., cluster)
stratum=data.var_id; %sampling stratum (from svyset)

keep=wt>0;
both=pc & ph & keep; %both species caught during the trip
c_only=pc & ~ph & keep; %only cod caugh during the trip
h_only=~pc &ph & keep; %only haddock caught during the trip

%Warning in case of too few PSUs
if numel(unique(psu(both)))<30
warning('Fewer than 30 PSUs in the both species caught category: family selection among copulas weakly supported')
end

%Pull out the three different categories of trips:
uc_b=uc(both);
uh_b=uh(both);
wt_b=wt(both);
psu_b=psu(both);
st_b=stratum(both);
uc_o=uc(c_only);
wt_co=wt(c_only);
uh_o=uh(h_only);
wt_ho=wt(h_only);

% Unconditional pools: every trip that caught the species
uc_all=uc(pc & keep);
wt_call=wt(pc & keep);
uh_all=uh(ph & keep);
wt_hall=wt(ph & keep);

% Report the unconditional pool sizes and assert they equal both + own-only, so
% a mis-specified filter shows up immediately rather than silently.
fprintf('unconditional pools: cod %d (= both %d + cod-only %d) | haddock %d (= both %d + haddock-only %d)\n', ...
        numel(uc_all), sum(both), sum(c_only), numel(uh_all), sum(both), sum(h_only));
assert(numel(uc_all)==sum(both)+sum(c_only), 'cod unconditional pool does not match both + cod-only');
assert(numel(uh_all)==sum(both)+sum(h_only), 'haddock unconditional pool does not match both + haddock-only');

% Estimate the five different copulas (point estimates):
u=pseudo_obs_w(uc_b,wt_b);  %creating the weighted pesudo-observations
v=pseudo_obs_w(uh_b,wt_b);
theta=fit_copulas(u,v,wt_b,COPULAS); %function returns the point estimate for the five copulas (max. weighted LL)
[gamma_w, tied_frac] = gamma_wt(uc_b, uh_b, wt_b);
fprintf('weighted Goodman-Kruskal gamma (both-caught) = %+.4f\n', gamma_w);  %identival to Kendall's tau-b when there are no ties.
fprintf('pairs tied on either species: %.3f%%\n', 100*tied_frac);
if tied_frac > 0.05
    warning('Ties exceed 5% of pairs, which implies gamma and Kendall tau-b will differ');
end


% Determine 95% CI by CLUSTER boostrap:
[ustr,~,gs]=unique(st_b); %mapping each trip to its statum index through gs
nc=numel(COPULAS)+1; %Five copulas
boot=nan(n_boots,nc); %creates (i.e., prepopulates) array of all NaN values of dimension n_boots by n_c 
ALLC=[COPULAS,{'Plackett'}];

for bidx=1:n_boots
idx=[]; %An empty accumulator. It gatthers the row numbers of the trips comprising this replication's dataset. Reset at the top of every pass.. It keeps row indices, not data (data recovered later via command: uc_b(idx)).
for s=1:numel(ustr)
rows_s=find(gs==s);
p_s=unique(psu_b(rows_s));
select=p_s(randi(numel(p_s),numel(p_s),1));

for j=1:numel(select)
    idx=[idx;rows_s(psu_b(rows_s)==select(j))];
end
end
if numel(idx)<20, continue; end %if this condition is met, the rest of the code (i.e., lines 333-340) is not run.
ub=pseudo_obs_w(uc_b(idx),wt_b(idx));
vb=pseudo_obs_w(uh_b(idx),wt_b(idx));
try
    boot(bidx,:)=fit_copulas(ub,vb,wt_b(idx),COPULAS);
catch  %it supresses the error in the lie above entirely and allows the code to continue executing.
end
end

ci=nan(nc,2);
for i=1:nc
    bi=boot(~isnan(boot(:,i)),i);
    if~isempty(bi), ci(i,:)=quantile(bi,[0.025,0.975]);
    end
end

fprintf('\n%-10s %10s %24s %8s\n','family', 'theta','95% CI (PSU cluster)', 'reps')
for k=1:nc
    fprintf('%-10s %10.4f [%9.4f, %9.4f] %8d\n',ALLC{k},theta(k),ci(k,1),ci(k,2),sum(~isnan(boot(:,k))));
end

%Results Table:
reps = sum(~isnan(boot), 1)';
res = table(ALLC(:), theta(:), ci(:,1), ci(:,2), reps, repmat(gamma_w, nc, 1), ...
    'VariableNames', {'family', 'theta', 'ci_lo', 'ci_hi', 'reps', 'gamma_weighted'});
disp(res);
writetable(res, sprintf('copula_estimates_%s.csv', label_output));

%Generate the draws of log_residuals:
%residuals draws for trips with catch of both species and dependence
for i=1:nc
   cpl=ALLC{i};
   if strcmp(cpl,'Plackett')
       U=plackett_rnd(theta(i),n_draws,1);
   else
       U=copularnd(cpl,theta(i),n_draws);
   end
   writetable(table((1:n_draws)',quantile_w(uc_b,wt_b,U(:,1)),quantile_w(uh_b,wt_b,U(:,2)),'VariableNames',{'draw_id','r_cod','r_had'}),sprintf('residual_draws_%s_%s.csv',cpl,label_output));
end

%residuals draws for trips that caught either cod or haddock, not not both
writetable(table((1:n_draws)',quantile_w(uc_o,wt_co,rand(n_draws,1)),quantile_w(uh_o,wt_ho,rand(n_draws,1)),'VariableNames',{'draw_id','r_cod_only','r_had_only'}),sprintf('residual_draws_onespecies_%s.csv',label_output));

% Residual draws for the FULLY INDEPENDENT arm.
writetable(table((1:n_draws)',quantile_w(uc_all,wt_call,rand(n_draws,1)),quantile_w(uh_all,wt_hall,rand(n_draws,1)),'VariableNames',{'draw_id','r_cod_all','r_had_all'}),sprintf('residual_draws_uncond_%s.csv',label_output));

fprintf('\nWrote %d draws per copula type for %s,\n',n_draws,label_output);

%%%%%% FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function param=fit_copulas(u,v,w,COPULAS)
u=min(max(u(:),1e-10),1-1e-10);
v=min(max(v(:),1e-10),1-1e-10);
w=w(:);
U=[u,v];
options=optimset('TolX',1e-8,'Display','off');
param=zeros(1,numel(COPULAS)+1);

for i=1:numel(COPULAS)
    cop=COPULAS{i};
    switch cop
        case 'Clayton', l=1e-4; h=50;
        case'Frank', l=-50; h=50;
        case 'Gumbel', l=1+1e-6; h=20;
        case 'Gaussian', l=-0.999; h=0.999;
    end
if strcmp(cop,'Gaussian')
    nll=@(x)-sum(w.*log(max(copulapdf('Gaussian',U,[1 x; x 1]),realmin)));
else
    nll=@(r)-sum(w.*log(max(copulapdf(cop,U,r),realmin)));
end
param(i)=fminbnd(nll,l,h,options);
end

nllp=@(t)-sum(w.*log(max(plackett_pdf(u,v,t),realmin))); %uses Patton's tolbox for the Placket copula
param(end)=fminbnd(nllp,1e-4,200,options);
end


function [g, tied_frac]=gamma_wt(x,y,w)
x=x(:);
y=y(:);
w = w(:);
n=numel(x);
num=0;
den=0;
allpairs = 0;
for i=1:n-1
    dx=x(i)-x(i+1:end);
    dy=y(i)-y(i+1:end);
    ww=w(i)*w(i+1:end);
    num=num+sum(ww.*sign(dx).*sign(dy));
    den=den+sum(ww.*(dx~=0).*(dy~=0));
    allpairs = allpairs + sum(ww);
end
g=num/max(den,eps);
tied_frac = 1 - den / max(allpairs, eps);
end


function u=pseudo_obs_w(x,w)
x=x(:);
w=w(:);
n=numel(x);
[~,ord]=sortrows([x,rand(n,1)]);
cw=cumsum(w(ord));
uu= cw/(cw(end) * (1 + 1/n));     % weighted R/(n+1)
u=zeros(n,1);
u(ord)=uu;
end


function q=quantile_w(x,w,p)
n  = numel(x);
x=x(:);
w=w(:);
[xs,ord]=sort(x);
ws=w(ord);
cw=cumsum(ws);
pp= cw/(cw(end) * (1 + 1/n));     % weighted R/(n+1)
q=interp1(pp,xs,p(:),'linear');
q(p(:)<=pp(1))=xs(1);
q(p(:)>=pp(end))=xs(end);
q=reshape(q,size(p));
end