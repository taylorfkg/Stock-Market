library(rvest)
library(tidyverse)
library(data.table)
library(progress)

companies <- fread("s&p companies.csv")

companies$page <- paste0("https://www.wsj.com/market-data/quotes/",companies$Symbol,"/company-people")


page <- read_html("https://www.wsj.com/market-data/quotes/IMKTA/company-people")

page %>% html_nodes(".txtBody") %>% html_text()
page %>% html_nodes(".data_data") %>% html_text() %>% .[9]
page %>% html_nodes(".data_data") %>% html_text() %>% .[11]

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = nrow(companies), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

n <- length(companies$Symbol)
pb <- progress_bar$new(total = 100)

c <- 0
stock_profile_catcher <- c()

for (i in companies$Symbol){
  tryCatch({
    
    Sys.sleep(sample(1:15, 1))
    
    c <- c+1
    #print(i)
    
    symb <- i 
    page <- read_html(paste0("https://www.wsj.com/market-data/quotes/",i,"/company-people"))
    
    description <- page %>% html_nodes(".txtBody") %>% html_text()
    sector <- page %>% html_nodes(".data_data") %>% html_text() %>% .[9]
    industry <- page %>% html_nodes(".data_data") %>% html_text() %>% .[11]
    
    stock_profile_catcher_temp <- data.frame(cbind(symb,description,sector,industry))
    
    stock_profile_catcher <- rbind(stock_profile_catcher,stock_profile_catcher_temp)
    
    print(c)
    # print(stock_profile_catcher_temp)
    # cat("*")
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
}

fwrite(stock_profile_catcher,"profile_wsj.csv")

#View(stock_profile_catcher)
















# for (i in companies$Symbol){
#     tryCatch({
#     c <- c+1
#     #print(i)
#     pb$tick()
#     # 
#     # width <- options()$width
#     # cat(paste0(rep('=', c/n * width), collapse = ''))
#     # 
#     #cat(paste0(round(c/length(companies$Symbol) * 100), '% completed'))
#     
#     # extra <- nchar('||100%')
#     #width <- options()$width
#     # step <- round(c / n * (width - extra))
#     # text <- sprintf('|%s%s|% 3s%%', strrep('=', step),
#     #                 strrep(' ', width - step - extra), round(c / n * 100))
#     # cat(text)
#     #Sys.sleep(0.1)
#     
#     #lapply(dfarray, function(i) hist(i))
#     
#     #obj <- get(i)
#     # symb <- i  
#     # adjusted <- eval(parse(text = paste0(i,"$",i,".Adjusted")))
#     # close <- eval(parse(text = paste0(i,"$",i,".Close")))
#     #adjusted <- paste0(i,".Adjusted")
#     #print(adjusted)
#     #data <- get(adjusted)
#     # temp <- data.frame(Ticker=i,adjusted,close,stringsAsFactors = F)
#     # temp$Date <- row.names(temp)
#     # print(temp)
#     # colnames(temp) <- c("Ticker","Adjusted_Price","Close_Price","Date")
#     # prices <- rbind(prices,temp)
#     #print(temp)
#     #setTxtProgressBar(pb, i)
#   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
# }


