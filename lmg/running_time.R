
times <- read.csv('running_time.csv',header=F)
ids <- sapply(strsplit(as.character(times$V1),'[.]'), "[", 1)
algs <- sapply(strsplit(as.character(times$V1),'[.]'), "[", 2)
algs <- sapply(strsplit(algs,'_'), "[", 2)
ts <- sapply(strsplit(as.character(times$V1),' '), "[", 2)

idsgold <- dir("gold_163_all_soma_sort_s1_pixelsize/")[1:163]

totaltime <- sum(as.numeric(ts[ids %in% idsgold]))/1000
totaltime/60 #minutes
totaltime/3600 #hours

totaltime <- sum(as.numeric(ts))/1000
totaltime/60 #minutes
totaltime/3600 #hours

