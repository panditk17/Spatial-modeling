
% clear files and load the datasets
clear all; close all; clc
load ('H:\pine_analysis\Project_Datasets\disease_data.mat')

%%
% Define inputs required for L test function;
InMat2 = datain;
numbin2 = 5;
numsim = 10;

% calculate bounding box R
lowest = min(InMat2);
upper = max(InMat2);

R = (upper(1,1) - lowest(1,1)) * (upper(1,2) - lowest(1,2));

[Index]= find(InMat2(:,3)==1);

One = InMat2(Index, 1:2);
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

high = max(Dis);

highest = max(high);
%%
% call cross test function
[binval,upper,lower,Lhat]= pandit_point_cross_L_test(InMat2,numbin2,numsim,R);

% plot the graph
figure('Name','DIFFERENCE IN K BETWEEN CHANGE AND NO CHANGE','NumberTitle','off')

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
