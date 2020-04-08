function [L] = pandit_point_cross_K_function_1( InMat2, R, binval )

% Cross K Function *****************
% Coded by Karun Pandit ************
% 12-15-2014 ***********************
% **********************************

% extract points with type 1 attribute
[Index]=find(InMat2(:,3)==0);
One  = InMat2(Index, 1:2);

% extract points with type 2 attribute
[Index1]= find(InMat2(:,3)==1);
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


%create bin values
[s,t]=size(binval);

[m,n]=size(Dis);

% calculate K value
count = zeros(1,s);

for k=1:s
    for i=1:m    
        for j = 1:n     
                 
            if Dis(i,j)<= binval(k,1) 
            count(k) = count(k)+1;             
            end
        end
    end
end

% calculate K
for h = 1:s;
K(h) = (R/(m*n))*count(h);
end;

Z = binval';

% calculate L
for f=1:s
Lfunc(f) = sqrt(K(f)/pi) - Z(f);
end
 
L = Lfunc';
end

