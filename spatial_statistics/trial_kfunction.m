
% Cross K Function *****************
% Coded by Karun Pandit ************
% 10-10-2011 ***********************
% **********************************
% clear files and load the datasets
clear all; close all; clc
load ('H:\pine_analysis\Project_Datasets\disease_data.mat')
%%

InMat2 = datain;
numbin2 = 10;
numsim = 5;

% calculate bounding box R
lowest = min(InMat2);
upper = max(InMat2);

R = (upper(1,1) - lowest(1,1)) * (upper(1,2) - lowest(1,2));

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

high = max(Dis);

highest = max(high);
%create bin values

step = highest/numbin2;

z = 0 :step:highest;

W = z';

[s,t]=size(W);


[m,n]=size(Dis);

% calculate K value

count = zeros(1,s);

for k=1:s

for i=1:m   
    for j = 1:n     
                 
       if Dis(i,j)<= W(k,1) 
          count(k) = count(k)+1;                   
   
       end   
         
    end
end

end

%

for h = 1:s;

K(h) = (R/(m*n))*count(h);
end;

Z = W';

for f=1:s
    
Lfunc(f) = sqrt(K(f)/pi) - Z(f);

end
 
L = Lfunc';

binval = W;


figure('Name','TRIAL K BETWEEN CHANGE AND NO CHANGE','NumberTitle','off')

plot(binval,K,'k-','LineWidth',2.5);


