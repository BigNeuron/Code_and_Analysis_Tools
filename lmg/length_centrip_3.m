% trees = load_tree('GS_centrip_bias.mtr');
% trees = trees{1};

trees = {};
species = {'Fruitfly','Human-cultured','Human','Zebrafish','Mouse','Frog','Silkmoth','Chicken'};
Fruitfly = {};
Humancultured = {};
Human = {};
Zebrafish = {};
Mouse = {};
Frog = {};
Silkmoth = {};
Chicken = {};

paths = readtable("metadatags.csv",'Delimiter',',','ReadVariableNames',true);

iff=1;
ihc=1;
ih=1;
iz=1;
im=1;
ifr=1;
is=1;
ic=1;

for i = 127:size(paths,1)%10%9644
     trees{i} = load_tree(['gold_163_all_soma_sort_s1_isotropic/',paths.ids{i},'/',paths.paths{i}]);
     switch paths.Model_organism{i}
         case 'Fruitfly'
            Fruitfly{iff} = trees{i};
            iff = iff+1;
         case 'Human-cultured'
             Humancultured{ihc} = trees{i};
             ihc = ihc+1;
         case 'Human'
             Human{ih} = trees{i};
             ih = ih+1;
         case 'Zebrafish'
             Zebrafish{iz} = trees{i};
             iz = iz+1;
         case 'Mouse'
             Mouse{im} = trees{i};
             im = im+1;
         case 'Frog'
             Frog{ifr} = trees{i};
             ifr = ifr+1;
         case 'Silkmoth'
             Silkmoth{is} = trees{i};
             is = is+1;
         case 'Chicken'
             Chicken{ic} = trees{i};
             ic = ic+1;
     end     
end

% spread_tree(trees,[],[],'-s')
% figure
% spread_tree(trees2,[],[],'-s')


%% Get SIP scale and centripetal bias k for 3D trees

L={};
k={};
SIP_scale={};
% rootangle={};

species = {'Fruitfly','Humancultured','Human','Zebrafish','Mouse','Frog','Silkmoth','Chicken'};

for i = 1:length(species)
    disp(i);
    L{i}=0;
    SIP_scale{i}=0;
    for j = 1:length(eval(species{i}))
        trees = eval(species{i});
        L{i} = L{i} + sum(len_tree(trees{j}));
        
        eucs=eucl_tree(trees{j});
        rmax=max(eucs(:));
        RVec=linspace(0,rmax,25);
        [Strue]=sholl_tree(trees{j},2*RVec); % Real Sholl
        
        SIP_scale{i} = SIP_scale{i}+trapz(RVec,Strue);
    end
    L{i} = L{i}/length(eval(species{i}));
    SIP_scale{i} = SIP_scale{i}/length(eval(species{i}));
%     try 
%         output = dissectSholl_tree(trees{i},'-3d -a');
        if i==2 || i==4
            k{i} = vonMises_tree(eval(species{i}),'-2d');
        else
            k{i} = vonMises_tree(eval(species{i}),'-3d');
        end
        
%         rootangle{i} = rootangle_tree(trees{i});
%     end
end

csvwrite('L_species.csv',cell2mat(L)');
csvwrite('SIP_scale_species.csv',cell2mat(SIP_scale)');
csvwrite('k_species.csv',cell2mat(k)');
% csvwrite('rootangle_dists_species.csv',padcat(rootangle{:}));
