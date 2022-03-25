require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

ml2 <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
with(ml2, table(ses, prog))
with(ml2, do.call(rbind, tapply(write, prog, function(x) c(M = mean(x), SD = sd(x)))))