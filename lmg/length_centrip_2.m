% trees = load_tree('GS_centrip_bias.mtr');
% trees = trees{1};

trees = {};
trees2 = {};

paths = readtable("pathsgs_isotropic_3d.csv",'Delimiter',',','ReadVariableNames',false);
paths2 = readtable("pathsgs_isotropic_2d.csv",'Delimiter',',','ReadVariableNames',false);

for i = 1:size(paths,1)%10%9644
     trees{i} = load_tree([paths.Var1{i}(47:end),'.radius.swc_sorted.swc']);
end
for i = 1:size(paths2,1)%10%9644
     trees2{i} = load_tree([paths2.Var1{i}(47:end),'.radius.swc_sorted.swc']);
end

spread_tree(trees,[],[],'-s')
figure
spread_tree(trees2,[],[],'-s')


%% Get SIP scale and centripetal bias k for 3D trees

L={};
k={};
SIP_scale={};
rootangle={};

parfor i = 1:length(trees)
    disp(i);
    L{i} = sum(len_tree(trees{i}));
%     try 
        output = dissectSholl_tree(trees{i},'-3d -a');
        k{i} = output.k
        SIP_scale{i} = output.tScale;
        rootangle{i} = rootangle_tree(trees{i});
%     end
end

csvwrite('L_3D.csv',cell2mat(L)');
csvwrite('SIP_scale_3D.csv',cell2mat(SIP_scale)');
csvwrite('k_3D.csv',cell2mat(k)');
csvwrite('rootangle_dists_3D.csv',padcat(rootangle{:}));

%% Get SIP scale and centripetal bias k for 2D trees

L={};
k={};
SIP_scale={};
rootangle={};

parfor i = 1:length(trees2)
    disp(i);
    L{i} = sum(len_tree(trees2{i}));
%     try 
        output = dissectSholl_tree(trees2{i},'-2d -a');
        k{i} = output.k
        SIP_scale{i} = output.tScale;
        rootangle{i} = rootangle_tree(trees2{i});
%     end
end

csvwrite('L_2D.csv',cell2mat(L)');
csvwrite('SIP_scale_2D.csv',cell2mat(SIP_scale)');
csvwrite('k_2D.csv',cell2mat(k)');
csvwrite('rootangle_dists_2D.csv',padcat(rootangle{:}));