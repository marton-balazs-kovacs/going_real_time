library(microbenchmark)

url <- read_lines("real_time_reporting/gs_url.txt")

ss <- gs_url(url, visibility = "public")

read_or_registration <- microbenchmark(gs_url(url, visibility = "public"),
                                       gs_read(ss))

gs_reads <- microbenchmark(gs_read(ss),
                           gs_read_csv(ss),
                           gs_read_cellfeed(ss),
                           gs_read_listfeed(ss))
