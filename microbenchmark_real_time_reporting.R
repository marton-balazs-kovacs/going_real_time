library(microbenchmark)

url <- read_lines("real_time_reporting/gs_url.txt")

ss <- gs_url(url, visibility = "public")

data_or_registration <- microbenchmark(gs_url(url, visibility = "public"),
                                       gs_read(ss))


