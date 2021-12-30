library(BatchGetSymbols)

# set dates
first.date <- Sys.Date() - 1
last.date <- Sys.Date()+1
freq.data <- 'daily'
# set tickers
tickers <- c('FB','GOOGL')

l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) # cache in tempdir()

print(l.out$df.control)
