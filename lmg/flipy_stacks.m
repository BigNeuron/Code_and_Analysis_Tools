pixelsize = csvread("scaling_gold.csv",1,1);

for i = 1:125%length(pixelsize(:,1))
    if pixelsize(i,2) ~= pixelsize(i,3)
        fileName = ['../BigNeurongit/Data/gold166_wids_vols/',num2str(pixelsize(i,1)),'/',num2str(pixelsize(i,1)),'.v3dpbd.tif'];
        tiffInfo = imfinfo(fileName);  %# Get the TIFF file information
        no_frame = numel(tiffInfo);    %# Get the number of images in the file
        stack = cell(no_frame,1);      %# Preallocate the cell array
        I=imread_big(fileName);
%         I = [];
%         for iFrame = 1:no_frame
%           stack{iFrame} = imread(fileName,'Index',iFrame,'Info',tiffInfo);
%           I = cat(3, I, stack{iFrame});
%         end
        I2 = imresize3(I,[size(I,1) size(I,2) pixelsize(i,3)/pixelsize(i,2)*size(I,3)],'method','lanczos2');
%         delete(fileName);
    %     I2 = flip(I,2);
        I2 = flip(I,1);
        fileName2 = ['../BigNeurongit/Data/gold166_wids_vols/',num2str(pixelsize(i,1)),'/',num2str(pixelsize(i,1)),'.v3dpbd.resized.tif'];
        for K=1:length(I2(1, 1, :))
           imwrite(I2(:, :, K), fileName2, 'WriteMode', 'append','Compression','none');
        end
    end
end