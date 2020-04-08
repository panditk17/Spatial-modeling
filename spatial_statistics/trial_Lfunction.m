
% Point cross L test function *****************
% Coded by Karun Pandit ***********************
% 10-10-2011 **********************************
% *********************************************

% clear files and load the datasets
clear all; close all; clc
load ('H:\pine_analysis\Project_Datasets\disease_data.mat')

%%
% Define inputs required for L test function;
InMat2 = datain;
numbin2 = 5000;
numsim = 20;


% extract points with type 1 attribute

[Index]=find(InMat2(:,3)==1);

One  = InMat2(Index, 1:2);

% extract points with type 2 attribute


[Index1]= find(InMat2(:,3)==0);

Two = InMat2(Index1, 1:2);

[a,b] = size(One);
[c,d] = size(Two);


% calculate distance matrix

Dis=zeros(a,c);

for i=1:a
    for j=1:c
      
                       
       Dis(i,j)=((One(i,1)-Two(j,1))^2 + (One(i,2)-Two(j,2))^2)^0.5;
          
    end
end




% create bin values

high = max(Dis);

highest = max(high);

% calculate R

lowest = min(InMat2);
upper = max(InMat2);

R = (upper(1,1) - lowest(1,1)) * (upper(1,2) - lowest(1,2));


[binval,K,L] = pandit_point_cross_K_function(InMat2, numbin2, R,highest);



% number of 0 and 1 types

type_0_count = c;
type_1_count = a;



for i=1:numsim
    

[type_vector] = random_labeling_help (type_0_count,type_1_count); 

label = type_vector';

InMat3=[InMat2(:,1) InMat2(:,2) label(:,1)];



[binval2(i,:),Ksim(i,:),Lsim(i,:)] = pandit_point_cross_K_function(InMat3,numbin2,R,highest);

      
end

Lsim_max = max(Lsim);
Lsim_min = min(Lsim);


% standardize upper, lower and estimated L with respect to average

%up_val = Lsim_max';
%low_val = Lsim_min';


%average = (up_val+low_val)/2;

%upper = up_val-average;
%lower = low_val-average;
%Lhat = L-average;

average = mean(Lsim);
av_val = average';

%SE = std(Lsim)/sqrt(numsim);
%SE_val = SE';
SD = std(Lsim);
SD_val = SD';

CI_upper = av_val + 1.96*SD_val;
CI_lower = av_val - 1.96*SD_val;
%av_new = (CI_upper+CI_lower)/2;

upper = CI_upper - av_val;
lower = CI_lower - av_val;
Lhat = L - av_val;

plot(binval,Lhat,'k-','LineWidth',2.5);
hold on
plot(binval,upper,'k-.','LineWidth',2);
hold on
plot (binval,lower,'k-.','LineWidth',2);
hold on
plot(binval,K);
hold off


