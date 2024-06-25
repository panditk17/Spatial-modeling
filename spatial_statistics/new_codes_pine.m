% CALL CROSS L FUNCTION
% matlab codes


% clear files and load the datasets
clear all; close all; clc
load ('H:\pine_analysis\Project_Datasets\disease_data.mat')

%%
% Define inputs required for L test function;
InMat1 = datain;
numbin2 = 5;
numsim = 10;

%%
% call cross L test function
[binval,upper,lower,Lhat]= pandit_point_cross_L_test1(InMat2,numbin2,numsim,boundary);


% plot the graph
figure('Name','Cross L test for FIRE and TIMBER CRIMES','NumberTitle','off')

plot(binval,Lhat,'k-','LineWidth',2.5);
hold on
plot(binval,upper,'k-.','LineWidth',2);
hold on
plot (binval,lower,'k-.','LineWidth',2);
hold off

xlabel('Distance (m)');
ylabel ('Cross L value (L(h))')
legend ('Estimated L','Upper L','Lower L','Location','SouthEast');
grid on;

%%
% clear files and load the datasets

% Define inputs required for L test function;
InMat1 = FIRE_OCCUPANCY;
numbin2 = 681;
numsim = 100;


InMat3 = InMat1(InMat1(:,1)< 682417.7,:);
InMat4 = InMat3(InMat3(:,1)> 653737.13,:);

InMat5 = InMat4(InMat4(:,2)< 4207776.73,:);
InMat2 = InMat5(InMat4(:,2)> 4143142.37,:);

%scatter(InMat2(:,1),InMat2(:,2));

% define study area
boundary = REC_AREA;

% calculate area R
R = polyarea(boundary(:,1), boundary(:,2));


% call cross L test function
[binval,upper,lower,Lhat]= pandit_point_cross_L_test1(InMat2,numbin2,numsim,boundary);


% plot the graph
figure('Name','Cross L test for FIRE and OCCUPANCY CRIMES','NumberTitle','off')

plot(binval,Lhat,'k-','LineWidth',2.5);
hold on
plot(binval,upper,'k-.','LineWidth',2);
hold on
plot (binval,lower,'k-.','LineWidth',2);
hold off

xlabel('Distance (m)');
ylabel ('Cross L value (L(h))')
legend ('Estimated L','Upper L','Lower L','Location','SouthEast');
grid on;
