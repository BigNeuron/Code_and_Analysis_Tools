options("repos" = c("CRAN" = "https://cran.rstudio.com",
                    "rforge" = "http://R-Forge.R-project.org"))
library(shiny)
library(ggbiplot)
library(plotly)
library(Rtsne)
library(corrplot)
library(heatmaply)
library(ggpubr)
library(nat)
library(nat.nblast)
# if(!require("nat.nblast")) devtools::install_github("natverse/neuromorphr")
library(dendroextras)
library(dendextend)
library(factoextra)
# library(magick)
library(tiff)
library(jpeg)
library(grid)
library(rmarkdown)
# library(imager)
library(rgl)
# library(rglwidget)
# library(Cairo)
# library(plyr)
# library(orca)
# library(ggnewscale)
library(moments)
library(mclust)
# library(fpc)
# library(cluster)
# library(pvclust)
# library(snow)

shinyServer(function(input, output, session) {
  values <- reactiveValues()
  
  sholl_analysis <- function(neuron, start = colMeans(xyzmatrix(neuron)), 
                            starting.radius = radius.step, ending.radius = 1000, 
                            radius.step = ending.radius/100){
    unit.vector <- function(x) {x / sqrt(sum(x^2))}
    dend = neuron$d
    dend$dists = nabor::knn(data = matrix(start,ncol=3), query = nat::xyzmatrix(neuron),k=1)$nn.dists
    if(is.null(ending.radius)){
      ending.radius = max(dend$dists)
    }
    radii = seq(from = starting.radius, to = ending.radius, by = radius.step)
    sholl = data.frame(radii = radii, intersections = 0)
    for(n in 1:length(radii)){
      r = radii[n]
      segments = neuron$SegList
      for(segment in segments){
        p = dend[segment,]
        dists = (nabor::knn(data = matrix(start,ncol=3), query = nat::xyzmatrix(p),k=1)$nn.dists - r) >= 0
        sholl[n,]$intersections = sholl[n,]$intersections + lengths(regmatches(paste(dists,collapse=""), gregexpr("TRUEFALSE|FALSETRUE", paste(dists,collapse=""))))
      }
    }
    sholl
  }
  
  
  upDataorig <- reactive({#if(is.null(input$file1))return(NULL) 
    #inFile <- input$file1
    #dat <- read.csv(inFile$datapath)
    load('subsetdata.Rdata')
    data <- my_data
    data[is.na(data)] <- 0
    # data <- data[complete.cases(data)==T,]
    load('groupsdf.Rdata')
    # groupsdf <- groupsdf[complete.cases(my_data)==T,]
    ids <- sapply(strsplit(as.character(groupsdf$paths),'/'), "[", 8)
    data <- data[ids!='9',]
    
    if(!is.null(input$file1))
    {
      # Run blastneuron for each uploaded reconstruction
      inFile <- input$file1
      
      for(i in 1:length(inFile$datapath)){
        #Xvfb :100 -ac &
        #export DISPLAY=:100.0
        log0 <- system('Xvfb :100 -ac & export DISPLAY=:100.0', intern=T)
        output$console <- renderPrint({
          return(print(log0))
        })
        # log0 <- system('xhost local:allencenter', intern=T)
        # output$console <- renderPrint({
        #   return(print(log0))
        # })
        log0 <- system('ls', intern=T)
        output$console <- renderPrint({
          return(print(log0))
        })
        log <- system(paste0('Vaa3D/vaa3d -x blastneuron -f pre_processing -p "#i ',inFile$datapath[i],' #o "',inFile$datapath[i],'"_prep.swc #l 3 #s 2 #r 1"'),
                      intern=T)
        output$console <- renderPrint({
          return(print(log))
        })
        pathswc <- data.frame(file=paste0("SWCFILE=",inFile$datapath[i],"_prep.swc"))
        write.table(pathswc,file="mydatabase.ano",col.names=F,row.names=F,quote=F)
        log2 <- system('Vaa3D/vaa3d -x blastneuron -f batch_compute -p \"#i mydatabase.ano #o features.csv"',
                      intern=T)
        # output$console <- renderPrint({
        #   return(print(log2))
        # })
        try(nbdata <-  read.csv("features.csv"))
        try(print(nbdata))
        nbdata <- nbdata[c(3:22,36)]
        # output$console <- renderPrint({
        #   return(print(nbdata))
        # })
        
        # Add distance metrics
        gspath <- Sys.glob(file.path(paste0("gold_163_all_soma_sort_s1_onlyswc/",strsplit(inFile$name[i],"[.]")[[1]][1]), "*.swc")) 
        log3 <- system(paste0('Vaa3D/vaa3d -x neuron_distance -f neuron_distance -i ',gspath,' ',inFile$datapath[i],' -p 2 -o dists/',i,'.txt'),
                       intern=T)
        dataFile <- readLines(paste0("dists/",i,".txt"))
        
        dfdist <- data.frame(esa1t2=NA,esa2t1=NA,ab_esa=NA,dsa=NA,pds1t2=NA,pds2t1=NA,a_pds=NA)
        dfdist$esa1t2 <- as.numeric(strsplit(dataFile[3],'=')[[1]][2])
        dfdist$esa2t1 <- as.numeric(strsplit(dataFile[4],'=')[[1]][2])
        dfdist$ab_esa <- as.numeric(strsplit(dataFile[5],'=')[[1]][2])
        dfdist$dsa <- as.numeric(strsplit(dataFile[6],'=')[[1]][2])
        dfdist$pds1t2 <- as.numeric(strsplit(dataFile[7],'=')[[1]][2])
        dfdist$pds2t1 <- as.numeric(strsplit(dataFile[8],'=')[[1]][2])
        dfdist$a_pds <- as.numeric(strsplit(dataFile[9],'=')[[1]][2])
        
        # Add image metrics
        # which(ids=="5") this is index for data to get image metrics
        dfim <- data[which(ids==strsplit(inFile$name[i],"[.]")[[1]][1])[1],29:43]
        
        # Add row to data for each uploaded reconstruction
        idata = c(as.numeric(nbdata),as.numeric(dfdist),as.numeric(dfim))
        data <- rbind(data,idata)
        
        # Remove remote files
        system("rm dists/*",
               intern=T)
        system("rm features.csv",
               intern=T)
        system("rm mydatabase.ano",
               intern=T)
      }
      
    }
    
    
    return(data)
  })
  
  values <- reactiveValues()
  
  upDatanrnlist <- reactive({
    load('meta_nrnlist.Rdata')
    return(df)
  })
  
  # output$inputfile <- renderTable({
  #   upDataorig()
  # })
  
  # treeData <- reactive({
  #   load("nrnlist.Rdata")
  #   return(nrnlisttest)
  # })
  # anntreeData <- reactive({
  #   load("annrnlist.Rdata")
  #   return(annrnlist)
  # })
  
  upDatagroups <- reactive({
    load('groupsdf.Rdata')
    groupsdf$algorithm[groupsdf$algorithm=="app2"] <- "none"#"app2"
    groupsdf$algorithm[groupsdf$algorithm=="app2new1"] <- "none"#"app2"
    groupsdf$algorithm[groupsdf$algorithm=="app2new2"] <- "none"#"app2"
    groupsdf$algorithm[groupsdf$algorithm=="app2new3"] <- "app2"
    groupsdf$algorithm[groupsdf$algorithm=="Advantra"] <- "none"
    groupsdf$algorithm[groupsdf$algorithm=="Advantra_updated"] <- "Advantra"
    groupsdf$algorithm[groupsdf$algorithm=="neutube"] <- "none"
    groupsdf$algorithm[groupsdf$algorithm=="neutube_updated"] <- "neutube"
    groupsdf$algorithm[groupsdf$algorithm=="pyzh"] <- "none"
    groupsdf$algorithm[groupsdf$algorithm=="pyzh_updated"] <- "pyzh"
    groupsdf$algorithm[groupsdf$algorithm=="LCMboost"] <- "nones"
    groupsdf$algorithm[groupsdf$algorithm=="LCMboost_updated"] <- "none"#"LCMboost"
    groupsdf$algorithm[groupsdf$algorithm=="LCMboost_3"] <- "LCMboost"
    groupsdf$algorithm[groupsdf$algorithm=="fastmarching_spanningtree"] <- "none"
    groupsdf$algorithm[groupsdf$algorithm=="fastmarching_spanningtree_updated"] <- "fastmarching_spanningtree"
    groupsdf$algorithm[groupsdf$algorithm=="axis_analyzer"] <- "none"
    groupsdf$algorithm[groupsdf$algorithm=="axis_analyzer_updated"] <- "axis_analyzer"
    groupsdf$algorithm[groupsdf$algorithm=="NeuronChaser"] <- "none"
    groupsdf$algorithm[groupsdf$algorithm=="NeuronChaser_updated"] <- "NeuronChaser"
    groupsdf$algorithm[groupsdf$algorithm=="meanshift"] <- "none"
    groupsdf$algorithm[groupsdf$algorithm=="meanshift_updated"] <- "meanshift"
    groupsdf$algorithm[groupsdf$algorithm=="NeuroGPSTree"] <- "none"
    groupsdf$algorithm[groupsdf$algorithm=="NeuroGPSTree_updated"] <- "NeuroGPSTree"
    groupsdf$algorithm[groupsdf$algorithm=="EnsembleNeuronTracerBasic"] <- "none"
    groupsdf$algorithm[groupsdf$algorithm=="ENT_updated"] <- "EnsembleNeuronTracerBasic"
    
    groupsdf$dataset <- substring(groupsdf$dataset,12)
    
    groupsdf$dataset[groupsdf$dataset=="chick_uw"] <- "Chicken UW"
    groupsdf$dataset[groupsdf$dataset=="frog_scrippts"] <- "Frog Scripps"
    groupsdf$dataset[groupsdf$dataset=="fruitfly_larvae_gmu"] <- "Fly larvae GMU"
    groupsdf$dataset[groupsdf$dataset=="human_allen_confocal"] <- "Human Allen"
    groupsdf$dataset[groupsdf$dataset=="human_culturedcell_Cambridge_in_vitro_confocal_GFP"] <- "Human cultured CU"
    groupsdf$dataset[groupsdf$dataset=="janelia_flylight_part1"] <- "Flylight"
    groupsdf$dataset[groupsdf$dataset=="janelia_flylight_part2"] <- "Flylight"
    groupsdf$dataset[groupsdf$dataset=="mouse_culturedcell_Cambridge_in_vivo_2_photon_PAGFP"] <- "Mouse cultured CU"
    groupsdf$dataset[groupsdf$dataset=="mouse_korea"] <- "Mouse KIT"
    groupsdf$dataset[groupsdf$dataset=="mouse_RGC_UW"] <- "Mouse RGC UW"
    groupsdf$dataset[groupsdf$dataset=="mouse_tufts"] <- "Mouse Allen"
    groupsdf$dataset[groupsdf$dataset=="silkmoth_utokyo"] <- "Silkmoth UT"
    groupsdf$dataset[groupsdf$dataset=="taiwan_flycirciut"] <- "Flycircuit"
    groupsdf$dataset[groupsdf$dataset=="utokyo_fly"] <- "Fly UT"
    groupsdf$dataset[groupsdf$dataset=="zebrafish_adult_RGC_UW"] <- "Zebrafish RGC UW"
    groupsdf$dataset[groupsdf$dataset=="zebrafish_horizontal_cells_UW"] <- "Zebrafish HC UW"
    groupsdf$dataset[groupsdf$dataset=="zebrafish_larve_RGC_UW"] <- "Zebrafish larvae RGC UW"
    
    load('subsetdata.Rdata')
    data <- my_data
    # is.na(data)<-sapply(data, is.infinite)
    data[is.na(data)] <- 0
    # data <- data[complete.cases(data)==T,]
    # load('groupsdf.Rdata')
    # groupsdf <- groupsdf[complete.cases(my_data)==T,]
    
    ids <- sapply(strsplit(as.character(groupsdf$paths),'/'), "[", 8)
    groupsdf$ids <- ids
    groupsdf <- groupsdf[ids!='9',]
    
    if(!is.null(input$file1))
    {
      inFile <- input$file1

      for(i in 1:length(inFile$datapath)){
        # Get dataset, and define group, algorithm and paths
        idataset <- groupsdf[which(ids==strsplit(inFile$name[i],"[.]")[[1]][1])[1],1]
        igroup <- input$inputalg
        ialgorithm <- input$inputalg
        ipaths <- paste0("/x/x/x/x/x/x/",strsplit(inFile$name[i],"[.]")[[1]][1])
        iids <- strsplit(inFile$name[i],"[.]")[[1]][1]

        # Merge with groupsdf data
        igroupsdf <- data.frame(dataset=idataset,group=igroup,algorithm=ialgorithm,paths=ipaths,ids=iids)
        groupsdf <- rbind(groupsdf,igroupsdf)
      }
    }
    
    return(groupsdf)
  })
  
  observe({
    updateSelectInput(
      session,
      "variablegroups",
      choices=c("dataset","group","algorithm","Model_organism","Age","Brain_region","Cell_type","Labeling_general","Labeling_acronym","Planar"),
      selected="Model_organism"
    )
    updateSelectInput(
      session,
      "variableclust",
      choices=c("none","clusters_dend","clusters_IQ","clusters_both"),
      selected="none"
    )
    updateSelectInput(
      session,
      "variableclust2",
      choices=c("none","clusters_dend","clusters_IQ","clusters_both"),
      selected="none"
    )
    updateSelectInput(
      session,
      "variableclust3",
      choices=c("none","clusters_dend","clusters_IQ","clusters_both"),
      selected="none"
    )
    
    updateCheckboxGroupInput(
      session,
      "variablemorph",
      choices=c("soma_surface","num_stems","num_bifurcations","num_branches","num_of_tips",
                "overall_x_span","overall_y_span","overall_z_span","average_diameter",
                "total_length","total_surface","total_volume","max_euclidean_distance",
                "max_path_distance","max_branch_order","average_contraction",
                "average_fragmentation","parent_daughter_ratio","bifurcation_angle_local",
                "bifurcation_angle_remote","ave_R","xy_pixel_size","z_pixel_size"),
      # selected=names(upDataorig())[-c(1,3,4,11,12,21,23,27,33,34,36)]
      selected=c("num_stems","num_of_tips","average_diameter","total_length","max_path_distance",
                 "max_branch_order","bifurcation_angle_remote")
      # selected=names(upDataorig())[c(2,5,9,14,16,20)]#,22,28,29,30,35,38,40)]
      #selected=names(upDataorig())[c(2,5,9,14,16,20,22,28,29,30,35,38,40)]
    )
    
    updateCheckboxGroupInput(
      session,
      "variableiq",
      choices=c("Correlation","FocusScore","LocalFocusScore","MADIntensity","MaxIntensity",
                "MeanIntensity","MedianIntensity","MinIntensity","PercentMaximal",
                "PercentMinimal","PowerLogLogSlope","StdIntensity","ThresholdOtsu",
                "TotalArea","TotalIntensity",
                "Correlation_swc","FocusScore_swc","LocalFocusScore_swc","MADIntensity_swc","MaxIntensity_swc",
                "MeanIntensity_swc","MedianIntensity_swc","MinIntensity_swc","PercentMaximal_swc",
                "PercentMinimal_swc","PowerLogLogSlope_swc","StdIntensity_swc","ThresholdOtsu_swc",
                "TotalArea_swc","TotalIntensity_swc"),
      # choices=names(upDataorig()[29:43]),
      # selected=names(upDataorig())[-c(1,3,4,11,12,21,23,27,33,34,36)]
      selected=c("Correlation","FocusScore","MedianIntensity","PercentMinimal","StdIntensity",
                 "Correlation_swc","FocusScore_swc","MedianIntensity_swc","PercentMinimal_swc","StdIntensity_swc")
      # selected=names(upDataorig())[c(29,30,35,38,40)]
      #selected=names(upDataorig())[c(2,5,9,14,16,20,22,28,29,30,35,38,40)]
    )
    
    
    
    updateCheckboxGroupInput(
      session,
      "variabledist",
      choices=c("entire.structure.average..from.neuron.1.to.2.",
                "entire.structure.average..from.neuron.2.to.1.",      
                "average.of.bi.directional.entire.structure.averages",
                "different.structure.average",                        
                "percent.of.different.structure..from.neuron.1.to.2.",
                "percent.of.different.structure..from.neuron.2.to.1.",
                "percent.of.different.structure"),
      # choices=names(upDataorig()[22:28]),
      # selected=names(upDataorig())[-c(1,3,4,11,12,21,23,27,33,34,36)]
      selected=c("entire.structure.average..from.neuron.1.to.2.",
                 "average.of.bi.directional.entire.structure.averages",
                 "percent.of.different.structure")
      # selected=names(upDataorig())[c(22,24,28)]
      #selected=names(upDataorig())[c(2,5,9,14,16,20,22,28,29,30,35,38,40)]
    )
    
    updateSelectInput(
      session,
      "filtermetrics",
      choices=names(upDataorig()[c(1:21,29:43,22:28,44:58)]),
      # choices=c(input$variablemorph,input$variabledist,input$variableiq),
      selected='PercentMinimal'#names(upDataorig())[2]
    )
    
    updateCheckboxGroupInput(
      session,
      "variablealg",
      choices=c("Advantra","Annotated","app1","app2","axis_analyzer","Consensus",                
                "Cwlab_ver1","EnsembleNeuronTracerBasic","EnsembleNeuronTracerV2n",  
                "EnsembleNeuronTracerV2s","fastmarching_spanningtree","LCMboost",                 
                "meanshift","MOST","MST_Tracing","nctuTW","nctuTW_GD","NeuroGPSTree",             
                "NeuronChaser","NeuroStalker","neutu_autotrace","neutube","pyzh","Rayshooting",              
                "Rivulet","Rollerball","simple","smartTracing","snake","tubularity_model_S",       
                "XY_3D_TreMap",input$inputalg),
      selected=c("Annotated")
      # selected=c("Advantra","Annotated","app1","app2","axis_analyzer","Consensus",
      #            "Cwlab_ver1","EnsembleNeuronTracerBasic","EnsembleNeuronTracerV2n",
      #            "EnsembleNeuronTracerV2s","fastmarching_spanningtree","LCMboost",
      #            "meanshift","MOST","MST_Tracing","nctuTW","nctuTW_GD","NeuroGPSTree",
      #            "NeuronChaser","NeuroStalker","neutu_autotrace","neutube","pyzh","Rayshooting",
      #            "Rivulet","Rollerball","simple","smartTracing","snake","tubularity_model_S",
      #            "XY_3D_TreMap")
    )
    
    updateCheckboxGroupInput(
      session,
      "variabledat",
      # choices=c("chick_uw","frog_scrippts","fruitfly_larvae_gmu","human_allen_confocal",                               
      #           "human_culturedcell_Cambridge_in_vitro_confocal_GFP", 
      #           "janelia_flylight_part1","janelia_flylight_part2",
      #           "mouse_culturedcell_Cambridge_in_vivo_2_photon_PAGFP",
      #           "mouse_korea","mouse_RGC_uw","mouse_tufts","silkmoth_utokyo",
      #           "taiwan_flycirciut","utokyo_fly","zebrafish_adult_RGC_UW",
      #           "zebrafish_horizontal_cells_UW","zebrafish_larve_RGC_UW"),
      # selected=c("fruitfly_larvae_gmu","human_allen_confocal",                               
      #            "janelia_flylight_part2",
      #            "mouse_RGC_uw",
      #            "taiwan_flycirciut")
      choices=c("Chicken UW"
                ,"Frog Scripps"
                ,"Fly larvae GMU"
                ,"Human Allen"
                ,"Human cultured CU"
                ,"Flylight"
                ,"Mouse cultured CU"
                ,"Mouse KIT"
                ,"Mouse RGC UW"
                ,"Mouse Allen"
                ,"Silkmoth UT"
                ,"Flycircuit"
                ,"Fly UT"
                ,"Zebrafish RGC UW"
                ,"Zebrafish HC UW"
                ,"Zebrafish larvae RGC UW"),
      selected=c("Chicken UW"
                 ,"Frog Scripps"
                 ,"Fly larvae GMU"
                 ,"Human Allen"
                 ,"Human cultured CU"
                 ,"Flylight"
                 ,"Mouse cultured CU"
                 ,"Mouse KIT"
                 ,"Mouse RGC UW"
                 ,"Mouse Allen"
                 ,"Silkmoth UT"
                 ,"Flycircuit"
                 ,"Fly UT"
                 ,"Zebrafish RGC UW"
                 ,"Zebrafish HC UW"
                 ,"Zebrafish larvae RGC UW")
    )
    
    # updateCheckboxGroupInput(
    #   session,
    #   "variableinpdat",
    #   choices=unique(upDatagroups()$ids),
    #   selected=unique(upDatagroups())[1]
    # )
    
  })
  
  observe({
    updateSelectInput(
      session,
      "dendclust",
      choices=1:16,#length(input$variabledat),
      selected=1#length(input$variabledat)
    )
  })
  
  observe({
    updateSelectInput(
      session,
      "distclust",
      choices=1:16,#length(input$variabledat),
      selected=1#length(input$variabledat)
    )
  })
  
  observeEvent(input$selectall,{
    if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(
        session,
        "variablemorph",
        choices=names(upDataorig()[c(1:21,59,60)]),
        selected=c()
      )
    }
    else if (input$selectall%%2 == 1) 
    {
      updateCheckboxGroupInput(
        session,
        "variablemorph",
        choices=names(upDataorig()[c(1:21,59,60)]),
        selected=names(upDataorig()[c(1:21,59,60)])
      )
    }
  })
  observeEvent(input$selectall3,{
    #########################################################
    if (input$selectall3%%2 == 0)
    {
      updateCheckboxGroupInput(
        session,
        "variableiq",
        choices=names(upDataorig()[29:58]),
        selected=c()
      )
    }
    else if (input$selectall3%%2 == 1)
    {
      updateCheckboxGroupInput(
        session,
        "variableiq",
        choices=names(upDataorig()[29:58]),
        selected=names(upDataorig()[29:58])
      )
    }
  })
  observeEvent(input$selectall2,{
    ############################################################
    if (input$selectall2%%2 == 0)
    {
      updateCheckboxGroupInput(
        session,
        "variabledist",
        choices=names(upDataorig()[22:28]),
        selected=c()
      )
    }
    else if (input$selectall2%%2 == 1)
    {
      updateCheckboxGroupInput(
        session,
        "variabledist",
        choices=names(upDataorig()[22:28]),
        selected=names(upDataorig()[22:28])
      )
    }
  })
  observeEvent(input$selectall4,{
    ############################################################
    if (input$selectall4%%2 == 0)
    {
      updateCheckboxGroupInput(
        session,
        "variablealg",
        choices=c("Advantra","Annotated","app1","app2","axis_analyzer","Consensus",                
                  "Cwlab_ver1","EnsembleNeuronTracerBasic","EnsembleNeuronTracerV2n",  
                  "EnsembleNeuronTracerV2s","fastmarching_spanningtree","LCMboost",                 
                  "meanshift","MOST","MST_Tracing","nctuTW","nctuTW_GD","NeuroGPSTree",             
                  "NeuronChaser","NeuroStalker","neutu_autotrace","neutube","pyzh","Rayshooting",              
                  "Rivulet","Rollerball","simple","smartTracing","snake","tubularity_model_S",       
                  "XY_3D_TreMap",input$inputalg),
        selected=c()
      )
    }
    else if (input$selectall4%%2 == 1)
    {
      updateCheckboxGroupInput(
        session,
        "variablealg",
        choices=c("Advantra","Annotated","app1","app2","axis_analyzer","Consensus",                
                  "Cwlab_ver1","EnsembleNeuronTracerBasic","EnsembleNeuronTracerV2n",  
                  "EnsembleNeuronTracerV2s","fastmarching_spanningtree","LCMboost",                 
                  "meanshift","MOST","MST_Tracing","nctuTW","nctuTW_GD","NeuroGPSTree",             
                  "NeuronChaser","NeuroStalker","neutu_autotrace","neutube","pyzh","Rayshooting",              
                  "Rivulet","Rollerball","simple","smartTracing","snake","tubularity_model_S",       
                  "XY_3D_TreMap",input$inputalg),
        selected=c("Advantra","Annotated","app1","app2","axis_analyzer","Consensus",                
                   "Cwlab_ver1","EnsembleNeuronTracerBasic","EnsembleNeuronTracerV2n",  
                   "EnsembleNeuronTracerV2s","fastmarching_spanningtree","LCMboost",                 
                   "meanshift","MOST","MST_Tracing","nctuTW","nctuTW_GD","NeuroGPSTree",             
                   "NeuronChaser","NeuroStalker","neutu_autotrace","neutube","pyzh","Rayshooting",              
                   "Rivulet","Rollerball","simple","smartTracing","snake","tubularity_model_S",       
                   "XY_3D_TreMap",input$inputalg)
      )
    }
  })
    
  observeEvent(input$selectall5,{
    ############################################################
    if (input$selectall5%%2 == 0)
    {
      updateCheckboxGroupInput(
        session,
        "variabledat",
        choices=c("Chicken UW"
                  ,"Frog Scripps"
                  ,"Fly larvae GMU"
                  ,"Human Allen"
                  ,"Human cultured CU"
                  ,"Flylight"
                  ,"Mouse cultured CU"
                  ,"Mouse KIT"
                  ,"Mouse RGC UW"
                  ,"Mouse Allen"
                  ,"Silkmoth UT"
                  ,"Flycircuit"
                  ,"Fly UT"
                  ,"Zebrafish RGC UW"
                  ,"Zebrafish HC UW"
                  ,"Zebrafish larvae RGC UW"),
        selected=c()
      )
    }
    else if (input$selectall5%%2 == 1)
    {
      updateCheckboxGroupInput(
        session,
        "variabledat",
        choices=c("Chicken UW"
                  ,"Frog Scripps"
                  ,"Fly larvae GMU"
                  ,"Human Allen"
                  ,"Human cultured CU"
                  ,"Flylight"
                  ,"Mouse cultured CU"
                  ,"Mouse KIT"
                  ,"Mouse RGC UW"
                  ,"Mouse Allen"
                  ,"Silkmoth UT"
                  ,"Flycircuit"
                  ,"Fly UT"
                  ,"Zebrafish RGC UW"
                  ,"Zebrafish HC UW"
                  ,"Zebrafish larvae RGC UW"),
        selected=c("Chicken UW"
                   ,"Frog Scripps"
                   ,"Fly larvae GMU"
                   ,"Human Allen"
                   ,"Human cultured CU"
                   ,"Flylight"
                   ,"Mouse cultured CU"
                   ,"Mouse KIT"
                   ,"Mouse RGC UW"
                   ,"Mouse Allen"
                   ,"Silkmoth UT"
                   ,"Flycircuit"
                   ,"Fly UT"
                   ,"Zebrafish RGC UW"
                   ,"Zebrafish HC UW"
                   ,"Zebrafish larvae RGC UW")
      )
    }
  })
  
  ###############################################################
  
  
  upData <- reactive({
    dat <- upDataorig()
    dat <- subset(upDataorig(),select = c(input$variablemorph,input$variabledist,input$variableiq))
    filtdat <- dat[,which(names(dat) %in% input$filtermetrics)]
    
    # print(dat)
    
    # dat <- dat[input$range[1] <= filtdat*100/max(filtdat) &
    #                 filtdat*100/max(filtdat) <= input$range[2],]
    # dat <- c(upDataorig()[input$variablemorph,],upDataorig()[input$variabledist,],upDataorig()[input$variableiq,])
    dat <- dat[(upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
                                 filtdat*100/max(filtdat) <= input$range[2],3] %in% input$variablealg) &
                 (upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
                                   filtdat*100/max(filtdat) <= input$range[2],1] %in% input$variabledat),]
    # dat <- dat[(upDatagroups()[,3] %in% input$variablealg &
    #               upDatagroups()[,1] %in% input$variabledat),]
    # 
    # dat2 <- subset(dat,select = c(input$variablemorph,input$variabledist,input$variableiq))
    
    dat <- dat[,apply(dat, 2, var, na.rm=TRUE) != 0]
    
    return(dat)
  })
  
  upDataIds <- reactive({
    dat <- upDataorig()
    dat <- subset(upDataorig(),select = c(input$variablemorph,input$variabledist,input$variableiq))
    filtdat <- dat[,which(names(dat) %in% input$filtermetrics)]
    filtdat <- as.numeric(filtdat)
    
    grps <- upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
                             filtdat*100/max(filtdat) <= input$range[2] & 
                             upDatagroups()[,3] %in% input$variablealg &
                             upDatagroups()[,1] %in% input$variabledat ,4]
    
    grps <- sapply(strsplit(as.character(grps),'/'), "[", 8)
    # grps <- c("1","2")
    return(grps)
  })
  
  
  observe({
    updateSelectInput(
      session,
      "clustset",
      choices=c('Dendr_Morph','Image_Qual','Both'),
      selected='Dendr_Morph'
    )
  })
  
  observe({
    if(input$clustset=='Dendr_Morph'){
      updateSelectInput(
        session,
        "dendplot",
        choices=subset(values$ids,values$memb_dend %in% input$dendclust),
        selected=subset(values$ids,values$memb_dend %in% input$dendclust)[1]
      )
    }else if(input$clustset=='Image_Qual'){
      updateSelectInput(
        session,
        "dendplot",
        choices=subset(values$ids,values$memb_iq %in% input$dendclust),
        selected=subset(values$ids,values$memb_iq %in% input$dendclust)[1]
      )
    }
    else{
      updateSelectInput(
        session,
        "dendplot",
        choices=subset(values$ids,values$memb_both %in% input$dendclust),
        selected=subset(values$ids,values$memb_both %in% input$dendclust)[1]
      )
    }
  })

  observe({
    updateSelectInput(
      session,
      "recgroup",
      choices=c('Consensus','auto','processed'),
      selected='Consensus'
    )
  })
  
  observe({
    updateSelectInput(
      session,
      "recgroup2",
      choices=c('Auto','Processed'),
      selected='Auto'
    )
  })
  
  observe({
    updateSelectInput(
      session,
      "recalg",
      choices=unique(upDatanrnlist()[upDatanrnlist()$ids %in% as.numeric(input$dendplot) & upDatanrnlist()$group %in% input$recgroup,]$alg),
      selected=unique(upDatanrnlist()[upDatanrnlist()$ids %in% as.numeric(input$dendplot) & upDatanrnlist()$group %in% input$recgroup,]$alg)[1]
    )
  })
  
  
  ###################
  ###################
  # output$PCA <- renderPlotly({
  plotPCA <- reactive({
    pcadata <- upData()
    # print(length(pcadata))
    for(i in 1:length(pcadata)){
      if(skewness(pcadata[,i]) > 1){
        pcadata[,i] <- log10(pcadata[,i])
      }
      else if(skewness(pcadata[,i]) < (-1)){
        pcadata[,i] <- log10(max(pcadata[,i]+1) - pcadata[,i])
      }
    }
    pcadata[is.na(pcadata)]<-0
    pcadata[pcadata=='-Inf']<-0
    
    upData.pca <- prcomp(pcadata, scale = TRUE)
    
    dat <- upDataorig()
    # dat <- subset(upDataorig(),select = c(input$variablemorph,input$variabledist,input$variableiq))
    # dat <- subset(dat,select = c(input$variablemorph,input$variabledist,input$variableiq))
    filtdat <- dat[,which(names(dat) %in% input$filtermetrics)]
    dat <- dat[input$range[1] <= filtdat*100/max(filtdat) &
                 filtdat*100/max(filtdat) <= input$range[2],]
    
    groupsdf <- upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
                                         filtdat*100/max(filtdat) <= input$range[2] & 
                                         upDatagroups()[,3] %in% input$variablealg &
                                         upDatagroups()[,1] %in% input$variabledat ,]
    
    # if(input$variablegroups=='clusters_dend'){
    #   dat <- upDataorig()
    #   filtdat <- dat[,which(names(dat)==input$filtermetrics)]
    #   dat <- dat[input$range[1] <= filtdat*100/max(filtdat) &
    #                filtdat*100/max(filtdat) <= input$range[2],]
    #   dat <- subset(upDataorig(),select = c(input$variablemorph))
    #   
    #   grps <- upDatagroups()[(upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
    #                                            filtdat*100/max(filtdat) <= input$range[2],3]%in%input$variablealg & 
    #                             upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
    #                                              filtdat*100/max(filtdat) <= input$range[2],1] %in% input$variabledat),]
    #   
    #   cdat <- upData()
    #   cdat <- subset(cdat,select = c(input$variablemorph))
    #   cdat <- cdat[grps$algorithm=='Annotated',]
    #   grps <- grps[grps$algorithm=='Annotated',]
    #   grps$ids <-  sapply(strsplit(as.character(grps$paths),'/'),"[", 8)
    #   rownames(cdat) <- paste0(grps$ids,'_',grps$dataset)
    #   hclust <- hclust(dist(cdat),method="ward.D2")
    #   memb <- cutree(hclust, k = length(unique(input$variabledat)))
    #   idsclusts <- data.frame(ids=grps$ids,clusters_dend=as.factor(memb))
    #   
    #   groupsdf <- merge(groupsdf,idsclusts, by='ids')
    # }
    
    colsv <- groupsdf[,input$variablegroups]
    # colsv <- groupsdf[(groupsdf[input$range[1] <= filtdat*100/max(filtdat) &
    #                               filtdat*100/max(filtdat) <= input$range[2],3]%in%input$variablealg & 
    #                      groupsdf[input$range[1] <= filtdat*100/max(filtdat) &
    #                                 filtdat*100/max(filtdat) <= input$range[2],1] %in% input$variabledat),
    #                   input$variablegroups]
    
    if(input$variableclust=='clusters_dend' | input$variableclust=='clusters_IQ' | input$variableclust=='clusters_both'){
      if(input$variableclust=='clusters_dend'){
        cdat <- pcadata[,which(names(pcadata) %in% input$variablemorph)]
      }else if(input$variableclust=='clusters_IQ'){
        cdat <- pcadata[,which(names(pcadata) %in% input$variableiq)]
      }
      else{
        cdat <- pcadata[,which(names(pcadata) %in% input$variablemorph | names(pcadata) %in% input$variableiq)]
      }

      # cdat <- cdat[groupsdf$algorithm=='Annotated',]
      # grps <- groupsdf[groupsdf$algorithm=='Annotated',]
      groupsdf$ids <-  sapply(strsplit(as.character(groupsdf$paths),'/'),"[", 8)
      rownames(cdat) <- paste0(groupsdf$ids,'_',groupsdf$dataset)
      # print(cdat)
      # pamk.best <- pamk(cdat,krange=2:20)
      # memb <- pam(cdat, pamk.best$nc)$clustering
      ###############
      # cl <- makeCluster(10, type="MPI")
      # cdat.pv <- pvclust(cl,t(df))
      # nclust <- length(pvpick(cdat.pv))
      # hclust <- hclust(dist(cdat),method="ward.D2")
      # dend <- colour_clusters(hclust, k=nclust, groupLabels=T)
      # memb <- cutree(dend, k = nclust)
      # memb <- abs(memb-nclust-1)
      # print(memb)
      ###############
      BIC <- Mclust(scale(cdat),G=2:10)
      # save(cdat,file="clustdat.Rdata")
      print(BIC$classification)
      memb <- BIC$classification
      plot(mclustBIC(cdat))
      ##############
      # hclust <- hclust(dist(cdat),method="ward.D2")
      # dend <- colour_clusters(hclust, k=7, groupLabels=T)
      # memb <- cutree(dend, k = 7)
      # memb <- abs(memb-7-1)
      # print(memb)
      if(input$variableclust=='clusters_dend'){
        idsclusts <- data.frame(ids=groupsdf$ids,clusters_dend=memb)
        # save(idsclusts,file="clusters_dend.Rdata")
      }else if(input$variableclust=='clusters_IQ'){
        idsclusts <- data.frame(ids=groupsdf$ids,clusters_IQ=memb)
        # save(idsclusts,file="clusters_iq.Rdata")
      }
      else{
        idsclusts <- data.frame(ids=groupsdf$ids,clusters_both=memb)
        # save(idsclusts,file="clusters_both.Rdata")
      }
      # print(idsclusts)

      groupsdf <- merge(groupsdf,idsclusts, by='ids')
      colsp <- as.factor(as.numeric(as.factor(groupsdf[,input$variableclust])))

      # print(ggplotly(plotPCA()))
      # Make data frame for plotting
      scores = as.data.frame(upData.pca$x)
      pca.df <- data.frame(PCA1 = scores$PC1, PCA2 = scores$PC2, clusters = colsp)

      # Find the hulls around our data for the different cell types
      find_hull <- function(pca.df) pca.df[chull(pca.df$PCA1, pca.df$PCA2), ]
      hulls <- ddply(pca.df, "clusters", find_hull)

      ggbiplot(upData.pca, obs.scale = 1, var.scale = 1,
               ellipse = T,
               alpha=0.3,
               groups=colsv) +
        theme_classic(base_family = 'Arial') +
        theme(aspect.ratio=1) +
        # scale_fill_brewer(palette = 'Set1') +
        # scale_colour_brewer(palette = 'Set1') +
        # new_scale("fill") +
        # new_scale("color") +
        geom_polygon(data=hulls,aes(x=PCA1, y=PCA2, fill = clusters, linetype=clusters), alpha = 0.2) +
        # scale_fill_brewer(palette = 'Set2') +
        scale_color_brewer(palette = 'Set3')
    }
    else{
    # print(
      # ggplotly(
    if(input$variablegroups=="dataset"){
      print(gc())
      ggbiplot(upData.pca, obs.scale = 1, var.scale = 1,
               # varname.size = 4,
               # varname.adjust=50,
               ellipse = T,
               alpha=0.3,
               groups=colsv) +
        # xlim(-5,10) + 
        # ylim(-5,5) +
        # geom_polygon
        theme_classic() +
        scale_color_manual(name="Dataset", values=c('#fc8d62','#76e2be','#3eb58d','#66c2a5','#54a389','#a6d854','#b3b3b3','#ffd92f','#e5c494','#e09e46','#c19963','#e78ac3','#a6bde8','#8da0cb','#6691dd')) +  
        geom_point(aes(colour=colsv),size=1.7) +
        # theme_classic(base_family = 'Arial') +
        theme(aspect.ratio=1) #+
      # brewer_pal("qual")
      # scale_fill_brewer(palette = 'Set1') +
      # scale_colour_brewer(palette = 'Set1')
      #geom_point(aes_string(color=upDatagroups()[,input$variablegroups],alpha=0.1)) 
      # )
      # )
    }
      else{
        print(gc())
        ggbiplot(upData.pca, obs.scale = 1, var.scale = 1,
                 # varname.size = 4,
                 # varname.adjust=50,
                 ellipse = T,
                 alpha=0.3,
                 groups=colsv) +
          # xlim(-5,10) + 
          # ylim(-5,5) +
          # geom_polygon
          theme_classic() +
          scale_color_brewer(palette="Set2") +  
          geom_point(aes(colour=colsv),size=1.7) +
          # theme_classic(base_family = 'Arial') +
          theme(aspect.ratio=1)
      }
    }
  })
  
  output$PCA <- renderPlotly({
    print(plotPCA())
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function(){paste('PCA', '.pdf', sep = '')},
    
    content = function(file){
      ggsave(file,width=10,height=7,device=cairo_pdf,plotPCA())
    },
    
    contentType = "application/pdf"
  )
  
  plotPCAdim1 <- reactive({
    pcadata <- upData()
    for(i in 1:length(pcadata)){
      if(skewness(pcadata[,i]) > 1){
        pcadata[,i] <- log10(pcadata[,i])
      }
      else if(skewness(pcadata[,i]) < (-1)){
        pcadata[,i] <- log10(max(pcadata[,i]+1) - pcadata[,i])
      }
    }
    pcadata[is.na(pcadata)]<-0
    pcadata[pcadata=='-Inf']<-0
    
    upData.pca <- prcomp(pcadata, scale = TRUE)
    # Contributions of variables to PC1
    fviz_contrib(upData.pca, choice = "var", axes = 1, top = 10)
  })
  
  output$PCAdim1 <- renderPlotly({
    print(plotPCAdim1())
  })
  
  output$downloadPlotdim1 <- downloadHandler(
    filename = function(){paste('PCA_dim1', '.pdf', sep = '')},
    
    content = function(file){
      ggsave(file,width=5,height=3.5,device=cairo_pdf,plotPCAdim1())
    },
    
    contentType = "application/pdf"
  )
  
  plotPCAdim2 <- reactive({
    pcadata <- upData()
    for(i in 1:length(pcadata)){
      if(skewness(pcadata[,i]) > 1){
        pcadata[,i] <- log10(pcadata[,i])
      }
      else if(skewness(pcadata[,i]) < (-1)){
        pcadata[,i] <- log10(max(pcadata[,i]+1) - pcadata[,i])
      }
    }
    pcadata[is.na(pcadata)]<-0
    pcadata[pcadata=='-Inf']<-0
    
    upData.pca <- prcomp(pcadata, scale = TRUE)
    # Contributions of variables to PC2
    fviz_contrib(upData.pca, choice = "var", axes = 2, top = 10)
  })
  
  output$PCAdim2 <- renderPlotly({
    print(plotPCAdim2())
  })
  
  output$downloadPlotdim2 <- downloadHandler(
    filename = function(){paste('PCA_dim2', '.pdf', sep = '')},
    
    content = function(file){
      ggsave(file,width=5,height=3.5,device=cairo_pdf,plotPCAdim2())
    },
    
    contentType = "application/pdf"
  )
  

  ################
  # output$tSNE <- renderPlotly({
  plottSNE <-  reactive({
    #upData.pca <- prcomp(upData(), scale = TRUE)
    # colors = rainbow(length(unique(upDatagroups()[,input$variablegroups])))
    # names(colors) = unique(upDatagroups()[,input$variablegroups])
    
    dat <- upDataorig()
    # dat <- subset(upDataorig(),select = c(input$variablemorph,input$variabledist,input$variableiq))
    
    filtdat <- dat[,which(names(dat) %in% input$filtermetrics)]
    dat <- dat[input$range[1] <= filtdat*100/max(filtdat) &
                 filtdat*100/max(filtdat) <= input$range[2],]
    
    groupsdf <- upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
                                 filtdat*100/max(filtdat) <= input$range[2] & 
                                 upDatagroups()[,3] %in% input$variablealg &
                                 upDatagroups()[,1] %in% input$variabledat ,]
    
    tsnedata <- upData()
    for(i in 1:length(tsnedata)){
      if(skewness(tsnedata[,i])>1){
        tsnedata[,i] <- log10(tsnedata[,i])
      }
      else if(skewness(tsnedata[,i])<(-1)){
        tsnedata[,i] <- log10(max(tsnedata[,i]+1) - tsnedata[,i])
      }
    }
    tsnedata[is.na(tsnedata)]<-0
    tsnedata[tsnedata=='-Inf']<-0
    
    pca.tsnedata <- prcomp(tsnedata, scale = TRUE)
    
    tsne <- Rtsne(pca.tsnedata$x, pca = FALSE)
    # tsne <- Rtsne(tsnedata, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500, check_duplicates=FALSE, num_threads=24)
    tsne_plot <- data.frame(x = tsne$Y[,1], y = tsne$Y[,2], col = upDatagroups()[
       (upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
                         filtdat*100/max(filtdat) <= input$range[2],3]%in%input$variablealg & 
          upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
                           filtdat*100/max(filtdat) <= input$range[2],1] %in% input$variabledat),input$variablegroups])
    
    colsv <- upDatagroups()[(upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
                                              filtdat*100/max(filtdat) <= input$range[2],3]%in%input$variablealg & 
                               upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
                                                filtdat*100/max(filtdat) <= input$range[2],1] %in% input$variabledat),
                            input$variablegroups]
    
    if(input$variableclust2=='clusters_dend' | input$variableclust2=='clusters_IQ' | input$variableclust2=='clusters_both'){
      # if(input$variableclust=='clusters_dend'){
      #   cdat <- tsnedata[,which(names(tsnedata) %in% input$variablemorph)]
      # }else if(input$variableclust=='clusters_IQ'){
      #   cdat <- tsnedata[,which(names(tsnedata) %in% input$variableiq)]
      # }
      # else{
      #   cdat <- tsnedata[,which(names(tsnedata) %in% input$variablemorph | names(tsnedata) %in% input$variableiq)]
      # }
      # 
      # # cdat <- cdat[groupsdf$algorithm=='Annotated',]
      # # grps <- groupsdf[groupsdf$algorithm=='Annotated',]
      # groupsdf$ids <-  sapply(strsplit(as.character(groupsdf$paths),'/'),"[", 8)
      # rownames(cdat) <- paste0(groupsdf$ids,'_',groupsdf$dataset)
      # # print(cdat)
      # BIC <- Mclust(cdat)
      # print(BIC$classification)
      # memb <- BIC$classification
      # # hclust <- hclust(dist(cdat),method="ward.D2")
      # # dend <- colour_clusters(hclust, k=7, groupLabels=T)
      # # memb <- cutree(dend, k = 7)
      # # memb <- abs(memb-7-1)
      # # print(memb)
      if(input$variableclust2=='clusters_dend'){
        # idsclusts <- data.frame(ids=groupsdf$ids,clusters_dend=memb)
        load("clusters_dend.Rdata")
      }else if(input$variableclust2=='clusters_IQ'){
        # idsclusts <- data.frame(ids=groupsdf$ids,clusters_IQ=memb)
        load("clusters_iq.Rdata")
      }
      else{
        # idsclusts <- data.frame(ids=groupsdf$ids,clusters_both=memb)
        load("clusters_both.Rdata")
      }
      print(idsclusts)
      
      groupsdf <- merge(groupsdf,idsclusts, by='ids')
      colsp <- as.factor(as.numeric(as.factor(groupsdf[,input$variableclust2])))
      
      # print(ggplotly(plotPCA()))
      # Make data frame for plotting
      print(tsne_plot$x)
      print(tsne_plot$y)
      print(colsp)
      pca.df <- data.frame(PCA1 = tsne_plot$x, PCA2 = tsne_plot$y, clusters = colsp)
      print(pca.df)
      
      # Find the hulls around our data for the different cell types
      find_hull <- function(pca.df) pca.df[chull(pca.df$PCA1, pca.df$PCA2), ]
      hulls <- ddply(pca.df, "clusters", find_hull)
      
      ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=colsv)) +
        theme_classic(base_family = 'Arial') +
        theme(aspect.ratio=1) +
        # scale_fill_brewer(palette = 'Set1') +
        # scale_colour_brewer(palette = 'Set1') +
        # new_scale("fill") +
        # new_scale("color") +
        geom_polygon(data=hulls,aes(x=PCA1, y=PCA2, fill = clusters, linetype=clusters), alpha = 0.2) +
        scale_fill_brewer(palette = 'Set3') #+
      # scale_color_brewer(palette = 'Set2')
    }
    else{
    # print(
      # ggplotly(
        ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=colsv)) +
          #xlim(-.03,.025) +
          #ylim(-0.05,0.05) +
          # theme_minimal()
          theme_classic() +
          theme(aspect.ratio=1)
        #geom_point(aes_string(color=upDatagroups()[,input$variablegroups],alpha=0.1)) 
      # )
    # )
    }
  })

  output$tSNE <- renderPlotly({
    print(ggplotly(plottSNE()))
  })
  
  output$downloadPlot2 <- downloadHandler(
    filename = function(){paste('tSNE', '.pdf', sep = '')},
    
    content = function(file){
      ggsave(file,width=10,height=7,plottSNE())
    },
    
    contentType = "application/pdf"
  )
  
  ######################
  # output$Clustering <- renderPlotly({
  plotClust <- reactive({
    cor.test.p <- function(x){
      FUN <- function(x, y) cor.test(x, y)[["p.value"]]
      z <- outer(
        colnames(x), 
        colnames(x), 
        Vectorize(function(i,j) FUN(x[,i], x[,j]))
      )
      dimnames(z) <- list(colnames(x), colnames(x))
      z
    }
    
    dat <- upDataorig()
    # dat <- subset(upDataorig(),select = c(input$variablemorph,input$variabledist,input$variableiq))
    filtdat <- dat[,which(names(dat) %in% input$filtermetrics)]
    dat <- dat[input$range[1] <= filtdat*100/max(filtdat) &
                 filtdat*100/max(filtdat) <= input$range[2],]
    
    grps <- upDatagroups()[(upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
                                             filtdat*100/max(filtdat) <= input$range[2],3]%in%input$variablealg & 
                               upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
                                                filtdat*100/max(filtdat) <= input$range[2],1] %in% input$variabledat),]
    
    # grps <- upDatagroups()[(upDatagroups()[,3] %in% input$variablealg & 
                              # upDatagroups()[,1] %in% input$variabledat),]
    
    # print(as.numeric(as.factor(data$dataset)))
    # print(as.numeric(data$group))
    # print(as.numeric(data$algorithm))
    
    load("clusters_both.Rdata")
    for(i in 1:length(grps$ids)){
      grps$clust[i] <- idsclusts$clusters_both[which(idsclusts$ids == grps[i,]$ids)]
    }
    
    data <- upData()
    
    data <- cbind(upData(),grps[,1:3])

    data$dataset <- as.numeric(as.factor(data$dataset))
    data$group <- as.numeric(data$group)
    data$algorithm <- as.numeric(data$algorithm)

    spdata <- cbind(upData(),grps[,1:3])
    spdata$clust <- as.factor(grps$clust)
    spdata <- subset(spdata,spdata$algorithm == 'Consensus')
    # spdata <- plyr::rename(spdata,c('percent.of.different.structure'='percent_of_different_structure'))
    # sp <- ggscatter(spdata, x = "Correlation", y = 'percent_of_different_structure',
    #                 color = "dataset", palette = "jco",
    #                 add = "reg.line", conf.int = TRUE)
    # sp + stat_cor(aes(color = dataset), label.x = 3) + xlim(0,1)
    print(spdata)
    print("N")
    print(length(spdata$clust))

    # ggscatter(spdata, x = "Correlation", y = "percent.of.different.structure", add="reg.line") +
    # # ggscatter(spdata, x = "Correlation", y = "percent.of.different.structure", color = "clust") +
    #   # geom_smooth(aes(group=1),color="black",method='lm')+
    #   stat_cor(
    #     aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    #     label.x = .5
    #   ) + xlim(0,1)
    # ggsave('spcorr_percent.pdf',width=10,height=6,device=cairo_pdf)
    # 
    # ggscatter(spdata, x = 'parent_daughter_ratio', y = "percent.of.different.structure", add="reg.line") +
    #   # ggscatter(spdata, x = 'parent_daughter_ratio', y = "percent.of.different.structure", color = "clust") +
    #   # geom_smooth(aes(group=1),color="black",method='lm')+
    #   stat_cor(
    #     aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    #     label.x = 0.85
    #   )
    # ggsave('spcorr_parentdaughterratio.pdf',width=10,height=6,device=cairo_pdf)
    # 
    # # ggscatter(spdata, x = 'bifurcation_angle_remote', y = "percent.of.different.structure", color = "clust") +
    # ggscatter(spdata, x = 'bifurcation_angle_remote', y = "percent.of.different.structure", add="reg.line") +
    #   # geom_smooth(aes(group=1),color="black",method='lm')+
    #   stat_cor(
    #     aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    #     label.x = 75
    #   )
    # ggsave('spcorr_bifangle.pdf',width=10,height=6,device=cairo_pdf)
    # 
    # ggscatter(spdata, x = "average_diameter", y = "percent.of.different.structure", add = "reg.line") +
    # # ggscatter(spdata, x = "average_diameter", y = "percent.of.different.structure", color="clust") +
    #   # geom_smooth(aes(group=1),color="black",method='lm')+
    #   stat_cor(
    #     aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    #     label.x = 1.05
    #   ) + xlim(1,1.8)
    # ggsave('spcorr_diameter.pdf',width=10,height=6,device=cairo_pdf)
    # 
    # ggscatter(spdata, x = "PercentMinimal", y = "percent.of.different.structure", add = "reg.line") +
    #   # ggscatter(spdata, x = "average_diameter", y = "percent.of.different.structure", color="clust") +
    #   # geom_smooth(aes(group=1),color="black",method='lm')+
    #   stat_cor(
    #     aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    #     label.x = 1.05
    #   ) #+ xlim(1,1.8)
    # ggsave('spcorr_percentminimal.pdf',width=10,height=6,device=cairo_pdf)
    # 
    # ggscatter(spdata, x = "Correlation_swc", y = "percent.of.different.structure", add = "reg.line") +
    #   # ggscatter(spdata, x = "average_diameter", y = "percent.of.different.structure", color="clust") +
    #   # geom_smooth(aes(group=1),color="black",method='lm')+
    #   stat_cor(
    #     aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    #     label.x = 1.05
    #   ) #+ xlim(1,1.8)
    # ggsave('spcorr_corrswc.pdf',width=10,height=6,device=cairo_pdf)
    # 
    # ggscatter(spdata, x = "MedianIntensity", y = "percent.of.different.structure", add = "reg.line") +
    #   # ggscatter(spdata, x = "average_diameter", y = "percent.of.different.structure", color="clust") +
    #   # geom_smooth(aes(group=1),color="black",method='lm')+
    #   stat_cor(
    #     aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    #     label.x = 1.05
    #   ) #+ xlim(1,1.8)
    # ggsave('spcorr_medint.pdf',width=10,height=6,device=cairo_pdf)
    
    data <- upData()
    
    print(data)
    
    M <- cor(data)
    p <- cor.test.p(data)
    # M <- cor(upData())
    # p <- cor.test.p(upData())
    # print(
      # ggplotly(
        heatmaply_cor(M,
                      # node_type = "scatter",
                      point_size_mat = -log10(p), 
                      point_size_name = "-log10(p-value)",
                      label_names = c("x", "y", "Correlation"),
                      hclust_method="ward.D2",
                      file='Clustering.pdf',
                      height=1300,
                      width=1300
        )
      # )
    # )
    # corrplot(M, tl.cex=1.5) +
      # theme_classic()
  })
  
  output$Clustering <- renderPlotly({
    print(ggplotly(plotClust()))
  })
  
  output$downloadPlot3 <- downloadHandler(
    filename = function(){paste('Clustering', '.pdf', sep = '')},
    
    # content = function(file){
    #   orca(plotClust(),file=file,width=10,height=7)
    # },
    content <- function(file) {
      file.copy("Clustering.pdf", file)
    },
    
    contentType = "application/pdf"
  )
  
  # output$DendClust <- renderPlot({
  plotDendClust <- reactive({
    ########## Clustering nblast
    # # annrnlist <- anntreeData() 
    # load('dftotgrp.Rdata')
    # # nb <- nblast_allbyall(anntreeData())
    # # nb <- nblast_allbyall(annrnlist)
    # # save(nb,file="nb.Rdata")
    # load('nb.Rdata')
    # 
    # hclust <- nhclust(scoremat = nb)
    # # plot(hclust)
    # dend <- colour_clusters(hclust, k=6, groupLabels=T)
    # nmannrnlist <- subset(annrnlist,rval="names")
    # labels(dend)=sapply(strsplit(nmannrnlist,'/'), "[", 8)
    # dfdt <- data.frame(ids=dftotgrp$ids,dataset=dftotgrp$dataset)
    # dfdt <- dfdt[!duplicated(dfdt), ]
    # labels(dend)=paste0(labels(dend),'_',dfdt$dataset)
    # par(cex=1, mar=c(1, 1, 1, 30))
    # plot(dend,horiz=T)
    
    ######## Clustering BLASTneuron
    # dat <- upDataorig()
    # filtdat <- dat[,which(names(dat) %in% input$filtermetrics)]
    # dat <- dat[input$range[1] <= filtdat*100/max(filtdat) &
    #              filtdat*100/max(filtdat) <= input$range[2],]
    # dat <- subset(upDataorig(),select = c(input$variablemorph))
    # 
    # grps <- upDatagroups()[(upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
    #                                          filtdat*100/max(filtdat) <= input$range[2],3]%in%input$variablealg & 
    #                           upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
    #                                            filtdat*100/max(filtdat) <= input$range[2],1] %in% input$variabledat),]
    # 
    # cdat <- upData()
    # cdat <- subset(cdat,select = c(input$variablemorph))
    # cdat <- cdat[grps$algorithm=='Annotated',]
    # grps <- grps[grps$algorithm=='Annotated',]
    # grps$ids <-  sapply(strsplit(as.character(grps$paths),'/'),"[", 8)
    # rownames(cdat) <- paste0(grps$ids,'_',grps$dataset)
    # hclust <- hclust(dist(cdat),method="ward.D2")
    # dend <- colour_clusters(hclust, k=7, groupLabels=T)
    # memb <- cutree(dend, k = 7)
    # values$ids <- grps$ids
    # values$memb_dend <- abs(memb-7-1)
    load("clusters_iq.Rdata")
    values$ids <- idsclusts$ids
    values$memb_dend <- idsclusts$clusters_IQ
    # USArrests[memb == k,]
    # labels(dend)=grps[grps$algorithm=="Annotated",]$dataset
    
    par(cex=1, mar=c(3, 1, 1, 20))
    ggsave("Clustering_dend.pdf",plot(rev(dend),main="Hier. Clustering - Tree Morph. metrics",horiz=T))
    plot(rev(dend),main="Hier. Clustering - Tree Morph. metrics",horiz=T)
  })
  
  output$DendClust <- renderPlot({
    print(plotDendClust())
  })
  
  output$downloadPlot4 <- downloadHandler(
    filename = function(){paste('Clustering_dend', '.pdf', sep = '')},
    
    content = function(file){
      # ggsave(file,width=10,height=7,device=cairo_pdf,plotDendClust())
      file.copy("Clustering_dend.pdf", file)
    },
    
    contentType = "application/pdf"
  )
  
  # output$DendIm <- renderPlot({
  plotImClust <- reactive({
    ######## Clustering Images
    dat <- upDataorig()
    filtdat <- dat[,which(names(dat) %in% input$filtermetrics)]
    dat <- dat[input$range[1] <= filtdat*100/max(filtdat) &
                 filtdat*100/max(filtdat) <= input$range[2],]
    dat <- subset(upDataorig(),select = c(input$variableiq))
    
    grps <- upDatagroups()[(upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
                                             filtdat*100/max(filtdat) <= input$range[2],3]%in%input$variablealg & 
                              upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
                                               filtdat*100/max(filtdat) <= input$range[2],1] %in% input$variabledat),]
    
    cdat <- upData()
    cdat <- subset(cdat,select = c(input$variableiq))
    cdat <- cdat[grps$algorithm=='Annotated',]
    grps <- grps[grps$algorithm=='Annotated',]
    grps$ids <-  sapply(strsplit(as.character(grps$paths),'/'),"[", 8)
    rownames(cdat) <- paste0(grps$ids,'_',grps$dataset)
    # print(rownames(cdat))
    hclust <- hclust(dist(cdat),method="ward.D2")
    # saveRDS(hclust,"hclust.Rdata")
    # saveRDS(cdat,"cdat.Rdata")
    # saveRDS(grps$ids,"cids.Rdata")
    dend <- colour_clusters(hclust, k=7, groupLabels=T)
    memb <- cutree(dend, k=7)
    values$memb_iq <- abs(memb-7-1)
    # cdat[memb == k, , drop = FALSE]
    # dend <- colour_clusters(hclust, k=7, groupLabels=T)
    # labels(dend)=grps[grps$algorithm=="Annotated",]$dataset
    
    par(cex=1, mar=c(3, 1, 1, 20))
    ggsave("Clustering_IQ.pdf",plot(rev(dend),main="Hier. Clustering - Im. Qual. metrics",horiz=T))
    plot(rev(dend),main="Hier. Clustering - Im. Qual. metrics",horiz=T)
  })
  
  output$DendIm <- renderPlot({
    print(plotImClust())
  })
  
  output$downloadPlot5 <- downloadHandler(
    filename = function(){paste('Clustering_IQ', '.pdf', sep = '')},
    
    content = function(file){
      # ggsave(file,width=10,height=7,device=cairo_pdf,print(plotImClust()))
      file.copy("Clustering_IQ.pdf", file)
    },
    
    contentType = "application/pdf"
  )
  
  
  plotBothClust <- reactive({
    ######## Clustering Images
    dat <- upDataorig()
    filtdat <- dat[,which(names(dat) %in% input$filtermetrics)]
    dat <- dat[input$range[1] <= filtdat*100/max(filtdat) &
                 filtdat*100/max(filtdat) <= input$range[2],]
    dat <- subset(upDataorig(),select = c(input$variableiq))
    
    grps <- upDatagroups()[(upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
                                             filtdat*100/max(filtdat) <= input$range[2],3]%in%input$variablealg & 
                              upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
                                               filtdat*100/max(filtdat) <= input$range[2],1] %in% input$variabledat),]
    
    cdat <- upData()
    cdat <- subset(cdat,select = c(input$variablemorph,input$variableiq))
    cdat <- cdat[grps$algorithm=='Annotated',]
    grps <- grps[grps$algorithm=='Annotated',]
    grps$ids <-  sapply(strsplit(as.character(grps$paths),'/'),"[", 8)
    rownames(cdat) <- paste0(grps$ids,'_',grps$dataset)
    # print(rownames(cdat))
    hclust <- hclust(dist(cdat),method="ward.D2")
    # saveRDS(hclust,"hclust.Rdata")
    # saveRDS(cdat,"cdat.Rdata")
    # saveRDS(grps$ids,"cids.Rdata")
    # cdat[memb == k, , drop = FALSE]
    # dend <- colour_clusters(hclust, k=length(unique(input$variabledat)), groupLabels=T)
    dend <- colour_clusters(hclust, k=7, groupLabels=T)
    # labels(dend)=grps[grps$algorithm=="Annotated",]$dataset
    memb <- cutree(dend, k=7)#length(unique(input$variabledat)))
    values$memb_both <- abs(memb-7-1)
    
    par(cex=1, mar=c(3, 1, 1, 20))
    ggsave("Clustering_Both.pdf",plot(rev(dend),main="Hier. Clustering - Both metrics",horiz=T))
    plot(rev(dend),main="Hier. Clustering - Both metrics",horiz=T)
  })
  
  output$DendBoth <- renderPlot({
    print(plotBothClust())
  })
  
  output$downloadPlot7 <- downloadHandler(
    filename = function(){paste('Clustering_Both', '.pdf', sep = '')},
    
    content = function(file){
      # ggsave(file,width=10,height=7,device=cairo_pdf,print(plotImClust()))
      file.copy("Clustering_Both.pdf", file)
    },
    
    contentType = "application/pdf"
  )
  
  output$DendPlot <- renderPlot({
    load('dftotgrp.Rdata')
    load('nb.Rdata')
    load('annrnlist.Rdata')
    
    nmannrnlist <- subset(annrnlist,rval="names")
    annids <- sapply(strsplit(nmannrnlist,'/'), "[", 8)
    
    plot(annrnlist[annids %in% input$dendplot],main='')
  })
  
  
  DensPlot <- reactive({
    load('groupsdf.Rdata')
    load('nb.Rdata')
    load('annrnlist.Rdata')
    
    nmannrnlist <- subset(annrnlist,rval="names")
    ann.summary = summary(annrnlist)
    annrnlist[,]
    
    annids <- data.frame(ids=sapply(strsplit(nmannrnlist,'/'), "[", 8))
    groups <- merge(annids,unique(groupsdf[,c(1,5:13)]),by='ids',all.x=T)
    
    ann.summary$Model_organism = groups$Model_organism
    annrnlist[,"Model_organism"] = groups$Model_organism
    ann.summary$bbx = sapply(annrnlist[rownames(ann.summary)], function(x) prod(abs(boundingbox(x)[1,]-boundingbox(x)[2,])))
    a = aggregate(ann.summary$branchpoints/ann.summary$cable.length, list(ann.summary$Model_organism), mean)
    Model_organism.order = a[order(a$x, decreasing = TRUE),]$Group.1
    ann.summary$Model_organism = factor(ann.summary$Model_organism, levels = Model_organism.order)
    
    ## Now we can explore things, lke the density of branchpoints
    # pdf("images/nat_neuromorpho_principals_branchpoints_over_cable.pdf", width = 7, height = 10)
    # ann.summary = subset(ann.summary, branchpoints/cable.length < 0.02)
    ggplot(ann.summary, aes(x=Model_organism, y=branchpoints/cable.length, color=Model_organism)) +
      geom_boxplot(outlier.shape = NA, position=position_dodge(0.8), col = "black", alpha = 0)+
      geom_jitter(width=0.25)+
      scale_color_brewer(palette="Set2") +
      theme_pubr() +
      theme(text = element_text(size=10),
            axis.text.x = element_text(angle=45, hjust=1),
            legend.position="none") #+ 
      # coord_flip()
  })
  
  output$DensPlot <- renderPlotly({
    print(ggplotly(DensPlot()))
  })
  
  output$downloadPlot9 <- downloadHandler(
    filename = function(){paste('BP_dens', '.pdf', sep = '')},
    
    content = function(file){
      ggsave(file,width=7,height=4,DensPlot())
    },
    
    contentType = "application/pdf"
  )
  
  
  VolPlot <- reactive({
    load('groupsdf.Rdata')
    load('nb.Rdata')
    load('annrnlist_isotropic.Rdata')
    
    nmannrnlist <- subset(annrnlist,rval="names")
    ann.summary = summary(annrnlist)
    annrnlist[,]
    
    annids <- data.frame(ids=sapply(strsplit(nmannrnlist,'/'), "[", 8))
    groups <- merge(annids,unique(groupsdf[,c(1,5:13)]),by='ids',all.x=T)
    
    ann.summary$Model_organism = groups$Model_organism
    annrnlist[,"Model_organism"] = groups$Model_organism
    ann.summary$bbx = sapply(annrnlist[rownames(ann.summary)], function(x) prod(abs(boundingbox(x)[1,]-boundingbox(x)[2,])))
    a = aggregate(ann.summary$branchpoints/ann.summary$cable.length, list(ann.summary$Model_organism), mean)
    Model_organism.order = a[order(a$x, decreasing = TRUE),]$Group.1
    ann.summary$Model_organism = factor(ann.summary$Model_organism, levels = Model_organism.order)
    
    # pdf("images/nat_neuromorpho_principals_volume_over_cable.pdf", width = 10, height = 7)
    # ann.summary = subset(ann.summary, branchpoints/cable.length < 0.02)
    ggplot(ann.summary, aes(x=Model_organism, y=bbx, color=Model_organism)) +
      geom_boxplot(outlier.shape = NA, position=position_dodge(0.8), col = "black")+
      geom_jitter(width=0.25)+
      theme_pubr() +
      ylim(0,5e7) +
      scale_color_brewer(palette="Set2") +
      theme(text = element_text(size=10),
            axis.text.x = element_text(angle=45, hjust=1),
            legend.position="none")
  })
  
  output$VolPlot <- renderPlotly({
    print(ggplotly(VolPlot()))
  })
  
  output$downloadPlot10 <- downloadHandler(
    filename = function(){paste('Bounding_box_volume', '.pdf', sep = '')},
    
    content = function(file){
      ggsave(file,width=7,height=4,VolPlot())
    },
    
    contentType = "application/pdf"
  )
  
  
  ShollPlot <- reactive({
    load('groupsdf.Rdata')
    load('nb.Rdata')
    load('annrnlist.Rdata')
    
    nmannrnlist <- subset(annrnlist,rval="names")
    ann.summary = summary(annrnlist)
    annrnlist[,]
    
    annids <- data.frame(ids=sapply(strsplit(nmannrnlist,'/'), "[", 8))
    groups <- merge(annids,unique(groupsdf[,c(1,5:13)]),by='ids',all.x=T)
    
    ann.summary$Model_organism = groups$Model_organism
    annrnlist[,"Model_organism"] = groups$Model_organism
    # annrnlist[,"Age"] = groups$Age
    # annrnlist[,"Brain_region"] = groups$Brain_region
    # annrnlist[,"Cell_type"] = groups$Cell_type
    # annrnlist[,"Labeling_general"] = groups$Labeling_general
    # annrnlist[,"Labeling_acronym"] = groups$Labeling_acronym
    # annrnlist[,"Microscopy"] = groups$Microscopy
    # annrnlist[,"group"] = "Annotated"
    ann.summary$bbx = sapply(annrnlist[rownames(ann.summary)], function(x) prod(abs(boundingbox(x)[1,]-boundingbox(x)[2,])))
    a = aggregate(ann.summary$branchpoints/ann.summary$cable.length, list(ann.summary$Model_organism), mean)
    Model_organism.order = a[order(a$x, decreasing = TRUE),]$Group.1
    ann.summary$Model_organism = factor(ann.summary$Model_organism, levels = Model_organism.order)
    
    # Calculate sholl analysis
    # annrnlist <- annrnlist[1:20]
    # ann.sholl = nlapply(annrnlist, sholl_analysis)
    # save(ann.sholl,file="ann_sholl.rda")
    # ann.sholl.bak <- ann.sholl
    load("ann_sholl.rda")
    df = data.frame()
    for(i in 1:length(ann.sholl)){
      ps = ann.sholl[[i]]
      ps = cbind(ps,ann.sholl[i,1])
      df = rbind(df, ps)
    }
    df$Model_organism <- df$`ann.sholl[i,1]`
    colnames(df)[3] = "Model_organism"
    a = aggregate(df$intersection, list(df$Model_organism, df$radii), mean)
    b = aggregate(df$intersection, list(df$Model_organism, df$radii), sd)
    colnames(a) = c("Model_organism","radii","intersections")
    a$sd = b$x
    
    # Plot
    a$Model_organism = factor(a$Model_organism, levels = Model_organism.order)
    # pdf("images/nat_neuromorpho_principals_sholl.pdf", width = 10, height = 7)
    ggplot(a, aes(x=radii, y=intersections, color=Model_organism)) +
      geom_line()+
      scale_color_brewer(palette="Set2") +
      xlim(0,750) +
     theme_pubr()
  })
  
  output$ShollPlot <- renderPlotly({
    print(ggplotly(ShollPlot()))
  })
  
  output$downloadPlot8 <- downloadHandler(
    filename = function(){paste('Sholl', '.pdf', sep = '')},
    
    content = function(file){
      ggsave(file,width=7,height=4,ShollPlot())
    },
    
    contentType = "application/pdf"
  )
  
  observe({
    updateSelectInput(
      session,
      "xlabtmdmap",
      choices=c("ids","dataset","Planar","group","algorithm","Model_organism","Age","Brain_region","Cell_type","Labeling_general","Labeling_acronym"),
      selected="ids"
    )
    updateSelectInput(
      session,
      "ylabtmdmap",
      choices=c("ids","dataset","Planar","group","algorithm","Model_organism","Age","Brain_region","Cell_type","Labeling_general","Labeling_acronym"),
      selected="ids"
    )
  })
  
  output$ClusterMap <- renderPlotly({
    print(gc())
    mdist <- read.csv("TMD_dists.csv",header=F)
    colnames(mdist) <- as.character(unlist(mdist[1,]))
    mdist = mdist[-1, ]
    rownames(mdist) <- as.character(unlist(mdist[,1]))
    mdist = mdist[,-1]
     
    metadata <- read.csv("metadatags.csv")
    colrows <- data.frame(ids = colnames(mdist))

    colrows <- merge(colrows,metadata, by="ids",sort=F)

    idrnames <- rownames(mdist)
    idcnames <- colnames(mdist)
    # rownames(mdist) <- colrows[,which(names(colrows) == input$xlabtmdmap)]
    # colnames(mdist) <- colrows[,which(names(colrows) == input$ylabtmdmap)]
    if(input$xlabtmdmap!="ids" & input$ylabtmdmap!="ids"){
      labcols <- paste0(idcnames,"_",colrows[,which(names(colrows) == input$xlabtmdmap)])
      labrows <- paste0(idrnames,"_",colrows[,which(names(colrows) == input$ylabtmdmap)])
    }
    else if(input$xlabtmdmap!="ids" & input$ylabtmdmap=="ids"){
      labcols <- paste0(idcnames,"_",colrows[,which(names(colrows) == input$xlabtmdmap)])
      labrows <- idrnames
    }
    else if(input$xlabtmdmap=="ids" & input$ylabtmdmap!="ids"){
      labrows <- paste0(idrnames,"_",colrows[,which(names(colrows) == input$ylabtmdmap)])
      labrows <- paste0(idrnames,"_",colrows[,which(names(colrows) == 'Model_organism')],
                        "_",colrows[,which(names(colrows) == 'Cell_type')])
      labcols <- idcnames
    }
    else{
      labcols <- idcnames
      labrows <- idrnames
    }
    
    # library("WeightedCluster")
    # hc <- hclust(as.dist(mdist))
    # hcRange <- as.clustrange(hc, diss=as.dist(mdist), ncluster=20)
    # summary(hcRange)
    # plot(hcRange, stat=c("ASWw", "HG", "PBC", "HC"), norm="zscore", lwd = 2)
    
    print(gc())
    p <- heatmaply(mdist,
              k_col = 11,
              k_row = 11,
              dist_method = "euclidean", hclust_method ="complete",
              # dist_method = "manhattan", hclust_method ="complete",
              labCol=labcols,
              labRow=labrows,
              column_text_angle = 60,
              fontsize_row = 5,
              fontsize_col = 5)
              # fontsize_col = 5,file='PL_matrix.pdf',width=1000,height=1200)
    print(gc())
    p
  })
  
  output$Neuronnm1 <- renderText({
    ids <- read.csv("clustered_rn.csv")
    ids <- rev(ids$x)
    
    click_event <- event_data("plotly_click",source="A")
    print(click_event)
    
    nid <- ids[click_event$x]
    if(is.null(click_event) == T){
      nid <-"1"
    }
    paste0("Neuron ",nid)
  })
  
  output$Neuronnm2 <- renderText({
    ids <- read.csv("clustered_rn.csv")
    ids <- rev(ids$x)
    
    click_event <- event_data("plotly_click",source="A")
    print(click_event)
    
    nid <- ids[click_event$y]
    if(is.null(click_event) == T){
      nid <-"5"
    }
    paste0("Neuron ",nid)
  })
  
  output$DiagramPlot <- renderPlot({
    
    ids <- read.csv("clustered_rn.csv")
    ids <- rev(ids$x)
    # mdist <- read.csv("TMD_dists.csv",header=F)
    # colnames(mdist) <- as.character(unlist(mdist[1,]))
    # mdist = mdist[-1, ]
    # rownames(mdist) <- as.character(unlist(mdist[,1]))
    # mdist = mdist[,-1]
    # ids <- as.character(unlist(mdist[1,]))
    # 
    # d <- dist(mdist, method = "euclidean")
    # dend <- as.dendrogram(hclust(d, method = "complete"))
    # dend <- dendextend::seriate_dendrogram(dend, d)
    # mdist <- mdist[order.dendrogram(dend),]
    # 
    click_event <- event_data("plotly_click",source="A")
    print(click_event)
    
    nid <- ids[click_event$x]
    if(is.null(click_event) == T){
      nid <-"1"
    }
    
    im <- readJPEG(paste0("TMD_diagrams/",nid,"Figure.jpg"),native=T)
    grid.raster(im)
    im <- NULL
    print(gc()) 
  })
  
  output$DiagramPlot2 <- renderPlot({
    
    ids <- read.csv("clustered_rn.csv")
    ids <- rev(ids$x)
    
    click_event <- event_data("plotly_click",source="A")
    print(click_event)
    
    nid <- ids[click_event$y]
    if(is.null(click_event) == T){
      nid <-"5"
    }
    
    im <- readJPEG(paste0("TMD_diagrams/",nid,"Figure.jpg"),native=T)
    grid.raster(im)
    im <- NULL
    print(gc()) 
  })
  
  output$BarcodePlot <- renderPlot({
    
    ids <- read.csv("clustered_rn.csv")
    ids <- rev(ids$x)
    
    click_event <- event_data("plotly_click",source="A")
    print(click_event)
    
    nid <- ids[click_event$x]
    if(is.null(click_event) == T){
      nid <-"1"
    }
    
    im <- readJPEG(paste0("TMD_barcodes/",nid,"Figure.jpg"),native=T)
    grid.raster(im)
    im <- NULL
    print(gc()) 
  })
  
  output$BarcodePlot2 <- renderPlot({
    
    ids <- read.csv("clustered_rn.csv")
    ids <- rev(ids$x)
    
    click_event <- event_data("plotly_click",source="A")
    print(click_event)
    
    nid <- ids[click_event$y]
    if(is.null(click_event) == T){
      nid <-"5"
    }
    
    im <- readJPEG(paste0("TMD_barcodes/",nid,"Figure.jpg"),native=T)
    grid.raster(im)
    im <- NULL
    print(gc()) 
  })
  
  output$ImagePlot <- renderPlot({
    
    ids <- read.csv("clustered_rn.csv")
    ids <- rev(ids$x)
    
    click_event <- event_data("plotly_click",source="A")
    print(click_event)
    
    nid <- ids[click_event$x]
    if(is.null(click_event) == T){
      nid <-"1"
    }
    
    im <- readJPEG(paste0("TMD_imgs/",nid,"Figure.jpg"),native=T)
    grid.raster(im)
    im <- NULL
    print(gc()) 
  })
  
  output$ImagePlot2 <- renderPlot({
    
    ids <- read.csv("clustered_rn.csv")
    ids <- rev(ids$x)
    
    click_event <- event_data("plotly_click",source="A")
    print(click_event)
    
    nid <- ids[click_event$y]
    if(is.null(click_event) == T){
      nid <-"5"
    }
    
    im <- readJPEG(paste0("TMD_imgs/",nid,"Figure.jpg"),native=T)
    grid.raster(im)
    im <- NULL
    print(gc()) 
  })
  
  output$TreePlot <- renderRglwidget({

    ids <- read.csv("clustered_rn.csv")
    ids <- rev(ids$x)

    click_event <- event_data("plotly_click",source="A")
    print(click_event)

    nid <- ids[click_event$x]
    if(is.null(click_event) == T){
      nid <-"1"
    }

    load(file.path("annrnlist_isotropic.Rdata"))

    nmnrnlist <- subset(annrnlist,rval="names")
    annids <- sapply(strsplit(nmnrnlist,'/'), "[", 8)

    rgl.open(useNULL=T)
    rgl.bg(color = "white")
    axes3d(color = "grey50")
    plot3d(annrnlist[annids == nid],main='',PlotAxes=c("XY", "YZ", "XZ", "ZY"))
    rglwidget()
  })

  output$TreePlot2 <- renderRglwidget({

    ids <- read.csv("clustered_rn.csv")
    ids <- rev(ids$x)

    click_event <- event_data("plotly_click",source="A")
    print(click_event)

    nid <- ids[click_event$y]
    if(is.null(click_event) == T){
      nid <-"5"
    }

    load(file.path("annrnlist_isotropic.Rdata"))

    nmnrnlist <- subset(annrnlist,rval="names")
    annids <- sapply(strsplit(nmnrnlist,'/'), "[", 8)

    rgl.open(useNULL=T)
    rgl.bg(color = "white")
    axes3d(color = "grey50")
    plot3d(annrnlist[annids == nid],main='',PlotAxes=c("XY", "YZ", "XZ", "ZY"))
    rglwidget()
  })
  
  output$TreesMetadata <- renderTable({
    metadata <- read.csv("metadatags.csv")
    
    ids <- read.csv("clustered_rn.csv")
    ids <- rev(ids$x)
    
    click_event <- event_data("plotly_click",source="A")
    print(click_event)
    
    nidx <- ids[click_event$x]
    nidy <- ids[click_event$y]
    if(is.null(click_event) == T){
      nidx <-"1"
      nidy <-"5"
    }
    
    metadata <- metadata[metadata$ids %in% c(nidx,nidy),-1]
    metadata$group <- NULL
    metadata$algorithm <- NULL
    metadata$paths <- NULL
    metadata
  })
  
  output$DendPlot2 <- renderPlot({
    load(file.path("nrnlists",paste0("nrnlist_",as.character(input$dendplot),".rdata")))
    
    nmnrnlist <- subset(nrnlist,rval="names")
    annids <- sapply(strsplit(nmnrnlist,'/'), "[", 8)
    annalg <- sapply(strsplit(nmnrnlist,'/'), "[", 10)
    annalg <- sapply(strsplit(annalg,'[.]'), "[", 2)
    annalg <- substring(annalg,8)
    anngroup <- sapply(strsplit(nmnrnlist,'/'), "[", 9)
    annalg[anngroup=="consensus.eswc_resampled.swc"] <- "Consensus"
    anngroup[anngroup=="consensus.eswc_resampled.swc"] <- "Consensus"
    anngroup[anngroup=="auto_recons"] <- "auto"
    anngroup[anngroup!="auto"&anngroup!="processed"&anngroup!="Consensus"] <- "Annotated"
    annalg[is.na(annalg)] <- "Annotated"
    annalg <- gsub(glob2rx('x*_y*_z*_*'),'',annalg)
    
    plot(nrnlist[annids %in% input$dendplot & anngroup %in% input$recgroup & annalg %in% input$recalg],main='')
  })
    
  output$ImPlot <- renderPlot({
    # im <- load(paste0('gold166_wids/',input$dendplot,'/',input$dendplot,'.tif'))
    # filename <- normalizePath(file.path('./gold166_wids',
    #                                     paste(as.character(input$dendplot),'/', as.character(input$dendplot), '.tif', sep='')))
    filename <- file.path('./gold166_wids',
                          paste(as.character(input$dendplot),'/', as.character(input$dendplot), '.tif', sep=''))
    # print(filename)
    rotate <- function(x) t(apply(x, 2, rev))
    
    im <- readTIFF(filename)
    im <- rotate(im)
    x <- 1:nrow(im)
    y <- 1:ncol(im)
    image(x,y,im,col=gray.colors(255))
  })
  #   print(filename)
  #   list(src = filename,
  #        width = 400,
  #        height = 300,
  #        contentType = "image/tiff")
  # }, deleteFile = FALSE)
  
  ########################
  # output$Distances <- renderPlotly({
  plotDist <- reactive({
    
    dat <- upDataorig()
    # dat <- subset(upDataorig(),select = c(input$variablemorph,input$variabledist,input$variableiq))
    filtdat <- dat[,which(names(dat) %in% input$filtermetrics)]
    # subset(upDataorig(),select = c(input$variablemorph,input$variabledist,input$variableiq))[,which(names(dat)==input$filtermetrics)]
    dat <- dat[input$range[1] <= filtdat*100/max(filtdat) &
                 filtdat*100/max(filtdat) <= input$range[2],]

    grps <- upDatagroups()[(upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
                                             filtdat*100/max(filtdat) <= input$range[2],3] %in% input$variablealg) &
                              (upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
                                               filtdat*100/max(filtdat) <= input$range[2],1] %in% input$variabledat),]
    # grps <- upDatagroups()[(upDatagroups()[,3] %in% input$variablealg & upDatagroups()[,1] %in% input$variabledat),]
    # dat <- upDataorig()
    # dat <- subset(upDataorig(),select = c(input$variablemorph,input$variabledist,input$variableiq))
    # # dat <- subset(dat,select = c(input$variablemorph,input$variabledist,input$variableiq))
    # filtdat <- dat[,which(names(dat) %in% input$filtermetrics)]
    # dat <- dat[input$range[1] <= filtdat*100/max(filtdat) &
    #              filtdat*100/max(filtdat) <= input$range[2],]
    # 
    # grps <- upDatagroups()[input$range[1] <= filtdat*100/max(filtdat) &
    #                              filtdat*100/max(filtdat) <= input$range[2] &
    #                              upDatagroups()[,3] %in% input$variablealg &
    #                              upDatagroups()[,1] %in% input$variabledat ,]
     
    print(length(upData()[,1]))
    print(length(grps[,1]))
    
    pdata <- cbind(upData(),grps)
    # pdata <- arrange(pdata,pdata[,which(names(pdata)=='average of bi-directional entire-structure-averages')])
    pdata$dists <- pdata[,which(names(pdata)=='average.of.bi.directional.entire.structure.averages')]
    # pdata$dists <- pdata[,which(names(pdata)=='percent.of.different.structure')]
    # pdata$dists <- pdata[,which(names(pdata)=='entire.structure.average..from.neuron.1.to.2.')]
    
    
    # pdata <- subset(pdata,pdata$algorithm!='Annotated')
    pdata <- subset(pdata,pdata$algorithm=='Consensus' | pdata$algorithm==input$inputalg | pdata$group==input$recgroup2)
    # print(pdata)
    
    print("nonclust done")
    
    if(input$variableclust3=='clusters_dend' | input$variableclust3=='clusters_IQ' | input$variableclust3=='clusters_both'){
      
      
      if(input$variableclust3=='clusters_dend'){
        load("clusters_dend.Rdata")
        print(pdata$ids)
        pdata <- pdata[pdata$ids %in% idsclusts$ids[idsclusts$clusters_dend %in% as.numeric(input$distclust)],]
      }else if(input$variableclust3=='clusters_IQ'){
        load("clusters_iq.Rdata")
        pdata <- pdata[pdata$ids %in% idsclusts$ids[idsclusts$clusters_IQ %in% as.numeric(input$distclust)],]
      }
      else{
        load("clusters_both.Rdata")
        pdata <- pdata[pdata$ids %in% idsclusts$ids[idsclusts$clusters_both %in% as.numeric(input$distclust)],]
      }
    }
    
    npdata <-  data.frame(algorithm=pdata$algorithm)
    npdata$N <- NULL
    for(i in 1:length(pdata$algorithm)){
      npdata$N[i] <- sum(pdata$algorithm %in% pdata$algorithm[i])
    }
    # aggregate(pdata$dists, list(algorithm = pdata$algorithm), length)
    npdata$lab <- paste0(pdata$algorithm,' (N=',npdata$N,')')
    # npdata$dists <-NULL
    pdata <-  merge(pdata,npdata,by='algorithm')
    pdata$algorithm <- pdata$lab

    ppdata <- aggregate(pdata$dists, list(pdata$algorithm), mean)
    
    
    # ggplotly(
      # ggplot(pdata,aes(reorder(algorithm,pdata[,which(names(pdata)=='average of bi-directional entire-structure-averages')]),
      #                  weight=pdata[,which(names(pdata)=='average of bi-directional entire-structure-averages')])) +
      #   geom_bar(aes(fill=algorithm),
      #            width=0.8,position = position_dodge()) +
      #   coord_flip() +
      #   geom_errorbar()
      ggbarplot(pdata, x = "algorithm", y = "dists",
                order = arrange(ppdata,ppdata$x)$Group.1,
                # order = arrange(ppdata,desc(ppdata$x))$Group.1,
                # orientation = "horiz",
                # fill = "algorithm",
                fill="steelblue",
                # order = as.character(unique(arrange(pdata,pdata$dists)$algorithm)),
                # order = reorder(algorithm,dists),
                ylab= 'average of bi-directional entire-structure-average distances',
                # ylab= 'percent of different structure',
                # ylab= 'entire structure average from neuron 1 to 2',
                add = "mean_se") +
        theme(text = element_text(size=8),
              axis.text.x = element_text(angle=45, hjust=1),
              legend.position="none")
    # )
  })
  
  output$Distances <- renderPlotly({
    print(plotDist())
  })
  
  output$downloadPlot6 <- downloadHandler(
    filename = function(){paste('Distances', '.pdf', sep = '')},
    
    content = function(file){
      ggsave(file,width=8,height=4,device=cairo_pdf,ggpar(plotDist(),legend="right"))
    },
    
    contentType = "application/pdf"
  )
})

# library(rsconnect)
# rsconnect::deployApp('./',appName = 'BigNeuron_Gold166')
