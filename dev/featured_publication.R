
seed <- as.integer(Sys.Date())
set.seed(seed)
sample_publication <- sample.int(n=length(publications), size = 1)
featured_pub <- publications[sample_publication]