trees = load_tree('GS_centrip_bias.mtr');
% trees = trees{1};

% trees = {};

%paths = readtable("pathsgs.csv",'Delimiter',',','ReadVariableNames',false);
%paths = dir("*/*.swc")

% dirs = dir()

for i = 1:size(trees,2)
    trees{i}.R(:)=2;
    trees{i}.R(1)=1;
    trees{i}.rnames={'1','3'};
%     swc_tree(treesgs{i},['GS_treestb_swcs/',num2str(i),'.swc']);
end

% for i = 1:size(paths,1)%10%9644
% %     trees{i} = load_tree(paths.Var1{i}(47:end));%load_tree([paths.Var1{i}(47:end),'_sorted.swc']);
%     trees{i} = load_tree([dirs(i+3).name,'/',paths(i).name,'_sorted.swc']);
% end

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

csvwrite('L.csv',cell2mat(L)');
csvwrite('SIP_scale.csv',cell2mat(SIP_scale)');
csvwrite('k.csv',cell2mat(k)');
csvwrite('rootangle_dists.csv',padcat(rootangle{:}));