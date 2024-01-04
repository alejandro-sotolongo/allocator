library(xts)
setwd('~/allocator/')
files <- dir('~/allocator/R')
for (i in 1:length(files)) {
  source(paste0('~/allocator/R/', files[i]))
}
rm(files, i)

load('test.RData')
