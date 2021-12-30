library(rvest)
#library(dplyr)
#library(stringr)
library(XML)
library(urltools)
library(RCurl)
library(V8)
library(curl)
library(data.table)
#library(xlsx)
library(svDialogs)
#library(esquisse)
library(tidyverse)
library(forecast)
library(stringi)
library(DT)
library(lubridate)

#sp_companies <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>% html_node("table") %>%  html_table(1,fill= T)
#nas100_companies <- read_html("https://en.wikipedia.org/wiki/NASDAQ-100") %>% html_nodes("table") %>%  html_table(3,fill= T)
#nas100_companies <- nas100_companies[[3]]

#######GET FINANCIALS FUNCTION-----------------
get_financials <<- function(){
  
  input_ticker <<- dlgInput("Enter ticker")$res %>% str_split(.,",") %>% unlist()
  
  # keep <- askYesNo("Keep Comparison Grid", default = TRUE,prompts = getOption("askYesNo", gettext(c("Yes", "No"))))
  
  keep <- dlg_message("Keep Comparison Grid",c("yesno"),gui = .GUI)$res
  
  keep <- ifelse(keep=="yes",TRUE,FALSE)
  
  if (keep==FALSE){
    comparison_grid <- NULL
  }
  
  if (keep==FALSE){
    qtr_comparison_grid <- NULL
  }
  
  #comparison_grid <- NULL
  
  #qtr_comparison_grid <- NULL
  
  N=0
  
  bond_page <<- read_html("https://www.marketwatch.com/investing/bond/tmubmusd10y?countrycode=bx")
  
  bond <<- as.numeric(bond_page %>% html_node(".u-semi") %>% html_text() %>% gsub("%","",.))/100
  
  for (i in input_ticker){
    
    tryCatch({
    
    ###READ HTML---------------------------  
    Sys.sleep(sample(1:15, 1))
    
    N <- N+1
    
    print(N)
      
    print(i)
    
    #assign(i,input_ticker,envir = .GlobalEnv)
    
    Start_Time <<- Sys.time()
    
    #print(i)
    
    #print(paste0("https://www.marketwatch.com/investing/stock/",i,"/financials"))
    #print(paste0("https://www.marketwatch.com/investing/stock/",i,"/financials/balance-sheet"))
    #print(paste0("https://www.marketwatch.com/investing/stock/",i,"/financials/cash-flow"))
    
    qtr_income_page <<- read_html(paste0("https://www.marketwatch.com/investing/stock/",i,"/financials/income/quarter"))
    
    qtr_header <<- qtr_income_page %>% html_nodes(".overflow__heading .cell__content") %>% html_text()
    
    qtr_header <<- qtr_header[qtr_header !="Item"]
    
    if (grepl("-",qtr_header[5])==FALSE){
      qtr_income_page <<- read_html(paste0("https://www.marketwatch.com/investing/stock/",i,"/financials/income/quarter?countrycode=uk"))
    }
    
    if (grepl("-",qtr_header[5])==FALSE){
      qtr_balance_page <<- read_html(paste0("https://www.marketwatch.com/investing/stock/",i,"/financials/balance-sheet/quarter?countrycode=uk"))
    } else{
      qtr_balance_page <<- read_html(paste0("https://www.marketwatch.com/investing/stock/",i,"/financials/balance-sheet/quarter"))
    }
    
    if (grepl("-",qtr_header[5])==FALSE){
      qtr_cash_page <<- read_html(paste0("https://www.marketwatch.com/investing/stock/",i,"/financials/cash-flow/quarter?countrycode=uk"))
    } else{
      qtr_cash_page <<- read_html(paste0("https://www.marketwatch.com/investing/stock/",i,"/financials/cash-flow/quarter"))
    }
    
    if (grepl("-",qtr_header[5])==FALSE){
      income_page <<- read_html(paste0("https://www.marketwatch.com/investing/stock/",i,"/financials/?countrycode=uk"))
    } else{
      income_page <<- read_html(paste0("https://www.marketwatch.com/investing/stock/",i,"/financials"))
    }
    
    if (grepl("-",qtr_header[5])==FALSE){
      balance_page <<- read_html(paste0("https://www.marketwatch.com/investing/stock/",i,"/financials/balance-sheet?countrycode=uk"))
    } else{
      balance_page <<- read_html(paste0("https://www.marketwatch.com/investing/stock/",i,"/financials/balance-sheet"))
    }
    
    if (grepl("-",qtr_header[5])==FALSE){
      cash_page <<- read_html(paste0("https://www.marketwatch.com/investing/stock/",i,"/financials/cash-flow?countrycode=uk"))
    } else{
      cash_page <<- read_html(paste0("https://www.marketwatch.com/investing/stock/",i,"/financials/cash-flow"))
    }
    
    # summary_page <<- read_html(paste0("https://www.marketwatch.com/investing/stock/",i))
    
    income_data <<- income_page %>% html_nodes(".cell__content span") %>% html_text()
    balance_data <<- balance_page %>% html_nodes(".cell__content span") %>% html_text()
    cash_data <<- cash_page %>% html_nodes(".cell__content span") %>% html_text()
    
    if (length(income_data)==0) {
      next
    }
    
    # stock_page <<- read_html(paste0("https://www.stockanalysis.com/stocks/",i,"/"))
    
    stock_page <<- tryCatch(
      sw_page <- read_html(paste0("https://www.stockanalysis.com/stocks/",i,"/")),
      error=function(e){cat("Message:",conditionMessage(e), "\n")})
    
    if(!is.null(stock_page)){
      Beta <<- stock_page %>% html_node(".top-table+ .top-table tr:nth-child(6) td+ td") %>% html_text()
      analyst <<- stock_page %>% html_node(".top-table+ .top-table tr:nth-child(7) td+ td") %>% html_text()
      price_target <<- stock_page %>% html_node(".top-table+ .top-table tr:nth-child(8) td+ td") %>% html_text()
      earnings_date <<- stock_page %>% html_node(".top-table+ .top-table tr:nth-child(9) td+ td") %>% html_text()
      fwd_pe <<- stock_page %>% html_node(".top-table:nth-child(1) tr:nth-child(7) td+ td") %>% html_text()
      dividend_date <<- stock_page %>% html_node(".top-table:nth-child(1) tr:nth-child(9) td+ td") %>% html_text()
    } else{
      Beta <<- "1"
      analyst <<- "N/A"
      price_target <<- "N/A"
      earnings_date <<- "N/A"
      fwd_pe <<- "N/A"
      dividend_date <<- "N/A"
      
    }
        
        # Beta <<- if_else(is.null(stock_page),"1",
        #                  stock_page %>% html_node(".top-table+ .top-table tr:nth-child(6) td+ td") %>% html_text())
        # 
        # analyst <<- if_else(is.null(stock_page),"N/A",
        #                     stock_page %>% html_node(".top-table+ .top-table tr:nth-child(7) td+ td") %>% html_text())
        # 
        # price_target <<- if_else(is.null(stock_page),"N/A",
        #                          stock_page %>% html_node(".top-table+ .top-table tr:nth-child(8) td+ td") %>% html_text())
        # 
        # earnings_date <<- if_else(is.null(stock_page),"N/A",
        #                           stock_page %>% html_node(".top-table+ .top-table tr:nth-child(9) td+ td") %>% html_text())
        # 
        # fwd_pe <<- if_else(is.null(stock_page),"N/A",
        #                    stock_page %>% html_node(".top-table:nth-child(1) tr:nth-child(7) td+ td") %>% html_text())
        # 
        # dividend_date <<- if_else(is.null(stock_page),"N/A",
        #                           stock_page %>% html_node(".top-table:nth-child(1) tr:nth-child(9) td+ td") %>% html_text())
    
    
    header <<- income_page %>% html_nodes(".overflow__heading .cell__content") %>% html_text() %>% .[.!="Item"&.!="5-year trend"]
    
    ins_name <<- income_page %>% html_nodes(".company__name") %>% html_text()
    
    row_name <<- income_page %>% html_nodes(".fixed--cell") %>% html_text()
    
    income_rows <<- trimws(stri_remove_empty(income_page %>% html_nodes(".fixed--cell") %>% html_text()))
    balance_rows <<- trimws(stri_remove_empty(balance_page %>% html_nodes(".fixed--cell") %>% html_text()))
    cash_rows <<- trimws(stri_remove_empty(cash_page %>% html_nodes(".fixed--cell") %>% html_text()))
    
    income_rows <<- income_rows[income_rows!="Item"]
    balance_rows <<- balance_rows[balance_rows!="Item"]
    cash_rows <<- cash_rows[cash_rows!="Item"]
    
    latest_price <<- income_page %>% 
      html_nodes(".u-semi") %>% 
      html_text() %>% 
      gsub(",","",.) %>% 
      str_extract(.,"[0-9]+\\.[0-9]+") %>% 
      as.numeric()
    
    #latest_price <<- as.numeric(gsub(",","",latest_price))
    
    latest_price_currency <<- income_page %>% 
      html_nodes(".u-semi") %>% 
      html_text() %>% 
      gsub(",","",.) %>% 
      str_extract(.,"[a-z]|\\$|\\€")
    
    latest_price <<- if_else(latest_price_currency=="p",latest_price/100,latest_price)
    
    latest_price_currency <<- if_else(latest_price_currency=="p","GBP",latest_price_currency)
    
    # latest_price_1day_chg <<- income_page %>% html_nodes(".lastpricedetails .bgPercentChange") %>% html_text()
    
    # latest_price_1day_chg <<- summary_page %>% html_nodes(".change--percent--q bg-quote") %>% html_text()
    # 
    # latest_price_5day_chg <<- summary_page %>% html_nodes(".table__row:nth-child(1) .value") %>% html_text()
    # 
    # latest_price_1mnth_chg <<- summary_page %>% html_nodes(".table__row:nth-child(2) .value") %>% html_text()
    # 
    # latest_price_3mnth_chg <<- summary_page %>% html_nodes(".table__row:nth-child(3) .value") %>% html_text()
    # 
    # latest_price_ytd_chg <<- summary_page %>% html_nodes(".table__row:nth-child(4) .value") %>% html_text()
    # 
    # latest_price_1yr_chg <<- summary_page %>% html_nodes(".table__row:nth-child(5) .value") %>% html_text()
    # 
    # Beta <<- summary_page %>% html_nodes(".kv__item:nth-child(7) .kv__primary") %>% html_text()
    
    ins_ticker <<- income_page %>% html_nodes(".company__ticker") %>% html_text() %>% .[1]
    
    units <- cash_page %>% 
    html_nodes(".small") %>% 
      html_text() %>% 
      .[1]
    
    units_split <<- str_locate(units,"[.]")
    
    fy <<- substr(units,16,units_split[1]-1)

    currency <<- substr(units,units_split[1]+13,str_length(units)-1)

    # currency <<- substr(units,units_split[1]+13,units_split[1]+15)

    titles <<- header
    
    #Income Statement------------
    j <- 1
    start_row <- 1
    end_row <- start_row+4
    
    
    while (j <= length(income_rows)){
      
      assign(trimws(income_rows[j]),case_when(
        grepl("%",income_data[start_row:end_row])~as.numeric(gsub("%","",income_data[start_row:end_row]))/100,
        grepl("K",income_data[start_row:end_row])&grepl("[()]",income_data[start_row:end_row])~as.numeric(gsub("K|[()]","",income_data[start_row:end_row]))/-1000000,
        grepl("M",income_data[start_row:end_row])&grepl("[()]",income_data[start_row:end_row])~as.numeric(gsub("M|[()]","",income_data[start_row:end_row]))/-1000,
        grepl("B",income_data[start_row:end_row])&grepl("[()]",income_data[start_row:end_row])~as.numeric(gsub("B|[()]","",income_data[start_row:end_row]))/-1,
        grepl("T",income_data[start_row:end_row])&grepl("[()]",income_data[start_row:end_row])~as.numeric(gsub("T|[()]","",income_data[start_row:end_row]))*-1000,
        grepl("[()]",income_data[start_row:end_row])~as.numeric(gsub("B|[()]","",income_data[start_row:end_row]))/1,
        grepl("K",income_data[start_row:end_row])~as.numeric(gsub("K","",income_data[start_row:end_row]))/1000000,
        grepl("M",income_data[start_row:end_row])~as.numeric(gsub("M","",income_data[start_row:end_row]))/1000,
        grepl("T",income_data[start_row:end_row])~as.numeric(gsub("T","",income_data[start_row:end_row]))*1000,
        grepl("-",income_data[start_row:end_row])~as.numeric(0),
        TRUE ~as.numeric(gsub("B","",income_data[start_row:end_row]))),envir = .GlobalEnv)
      
      j <- j+1
      start_row <- start_row+5
      end_row <- start_row+4
    }
    
    if (length(grep("interest income growth",income_rows,ignore.case = T))){
      `Sales/Revenue` <<- `Net Interest Income`+`Non-Interest Income`}
    
    if (length(grep("interest income growth",income_rows,ignore.case = T))){
      `Sales Growth` <<- `Net Interest Income Growth`}
    
    if (length(grep("total interest expense",income_rows,ignore.case = T))){
      `Cost of Goods Sold (COGS) incl. D&A` <<- `Total Interest Expense`}
    
    if (length(grep("Total Interest Expense Growth",income_rows,ignore.case = T))){
      `COGS Growth` <<- `Total Interest Expense Growth`}
    
    if (length(grep("Interest Expense on Debt",income_rows,ignore.case = T))){
      `Interest Expense` <<- `Interest Expense on Debt`}
    
    if (length(grep("Net Interest Income",income_rows,ignore.case = T))){
      `Gross Income` <<- `Net Interest Income`}
    
    if (length(grep("Net Interest Income Growth",income_rows,ignore.case = T))){
      `Gross Income Growth` <<- `Net Interest Income Growth`}
    
    if (length(grep("Non-Interest Expense",income_rows,ignore.case = T))){
      `SG&A Expense` <- `Non-Interest Expense`}
    
    if (length(grep("Income Taxes",income_rows,ignore.case = T))){
      `Income Tax` <<- `Income Taxes`}
    
    if (length(grep("interest income",income_rows,ignore.case = T))){
      `EBITDA` <<- replace_na(`Net Income`,0)+
        replace_na(`Depreciation & Amortization Expense`,0)+
        replace_na(`Income Tax`+`Interest Expense`,0)+
        replace_na(`Non-Operating Interest Income`,0)}
    
    if (length(grep("interest income",income_rows,ignore.case = T))){
      `EBITDA Margin` <<- EBITDA/`Sales/Revenue`}
    
    `Gross Profit Margin %` <<- round(as.numeric(gsub("B","",`Gross Income`))/as.numeric(gsub("B","",`Sales/Revenue`)),2)
    
    `Net Profit Margin %` <<- round(as.numeric(gsub("B","",`Net Income`))/as.numeric(gsub("B","",`Sales/Revenue`)),2)
    
    `Operating Profit Margin %` <<- round((as.numeric(gsub("B","",`Gross Income`))-as.numeric(gsub("B","",`SG&A Expense`)))/as.numeric(gsub("B","",`Sales/Revenue`)),2)
    
    
    #Balance Sheet-----------------
    j <- 1
    start_row <- 1
    end_row <- start_row+4
    
    
    while (j <= length(balance_rows)){
      
      assign(trimws(balance_rows[j]),case_when(
        grepl("%",balance_data[start_row:end_row])~as.numeric(gsub("%","",balance_data[start_row:end_row]))/100,
        grepl("K",balance_data[start_row:end_row])&grepl("[()]",balance_data[start_row:end_row])~as.numeric(gsub("K|[()]","",balance_data[start_row:end_row]))/-1000000,
        grepl("M",balance_data[start_row:end_row])&grepl("[()]",balance_data[start_row:end_row])~as.numeric(gsub("M|[()]","",balance_data[start_row:end_row]))/-1000,
        grepl("B",balance_data[start_row:end_row])&grepl("[()]",balance_data[start_row:end_row])~as.numeric(gsub("B|[()]","",balance_data[start_row:end_row]))/-1,
        grepl("T",balance_data[start_row:end_row])&grepl("[()]",balance_data[start_row:end_row])~as.numeric(gsub("T|[()]","",balance_data[start_row:end_row]))*-1000,
        grepl("[()]",balance_data[start_row:end_row])~as.numeric(gsub("B|[()]","",balance_data[start_row:end_row]))/1,
        grepl("K",balance_data[start_row:end_row])~as.numeric(gsub("K","",balance_data[start_row:end_row]))/1000000,
        grepl("M",balance_data[start_row:end_row])~as.numeric(gsub("M","",balance_data[start_row:end_row]))/1000,
        grepl("T",balance_data[start_row:end_row])~as.numeric(gsub("T","",balance_data[start_row:end_row]))*1000,
        grepl("-",balance_data[start_row:end_row])~as.numeric(0),
        TRUE ~as.numeric(gsub("B","",balance_data[start_row:end_row]))),envir = .GlobalEnv)
      
      j <- j+1
      start_row <- start_row+5
      end_row <- start_row+4
    }
    
    if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
      `Cash Only` <- `Total Cash & Due from Banks`}
    
    if (length(grep("Investments - Total",balance_rows,ignore.case = T))){
      `Short-Term Investments` <- `Investments - Total`}
    
    if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
      `Cash & Short Term Investments` <- `Cash Only`+`Short-Term Investments`}
    
    if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
      `Total Current Assets` <- `Cash Only`+`Short-Term Investments`+`Net Loans`}
    
    `Total Debt` <<- replace_na(`ST Debt & Current Portion LT Debt`,0)+
      replace_na(`Long-Term Debt`,0)
    
    `Debt To Equity` <<- round(`Total Debt` / `Total Shareholders' Equity`,2)
    
    `Total Liabilities To Equity` <<- round(as.numeric(`Total Liabilities`)/as.numeric(`Total Shareholders' Equity`),2)
    
    `Current Ratio` <<- if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
      round(`Total Assets`/`Total Liabilities`,2)} else {round(`Total Current Assets`/`Total Current Liabilities`,2)}
    
    `Quick Ratio` <<- if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
      round(`Total Assets`/`Total Liabilities`,2)} else {round((`Total Current Assets`-`Inventories`)/`Total Current Liabilities`,2)}
    
    
    #Cash Flow--------------------
    j <- 1
    start_row <- 1
    end_row <- start_row+4
    
    
    while (j <= length(cash_rows)){
      
      assign(trimws(cash_rows[j]),case_when(
        grepl("%",cash_data[start_row:end_row])~as.numeric(gsub("%","",cash_data[start_row:end_row]))/100,
        grepl("K",cash_data[start_row:end_row])&grepl("[()]",cash_data[start_row:end_row])~as.numeric(gsub("K|[()]","",cash_data[start_row:end_row]))/-1000000,
        grepl("M",cash_data[start_row:end_row])&grepl("[()]",cash_data[start_row:end_row])~as.numeric(gsub("M|[()]","",cash_data[start_row:end_row]))/-1000,
        grepl("B",cash_data[start_row:end_row])&grepl("[()]",cash_data[start_row:end_row])~as.numeric(gsub("B|[()]","",cash_data[start_row:end_row]))/-1,
        grepl("T",cash_data[start_row:end_row])&grepl("[()]",cash_data[start_row:end_row])~as.numeric(gsub("T|[()]","",cash_data[start_row:end_row]))*-1000,
        grepl("[()]",cash_data[start_row:end_row])~as.numeric(gsub("B|[()]","",cash_data[start_row:end_row]))/1,
        grepl("K",cash_data[start_row:end_row])~as.numeric(gsub("K","",cash_data[start_row:end_row]))/1000000,
        grepl("M",cash_data[start_row:end_row])~as.numeric(gsub("M","",cash_data[start_row:end_row]))/1000,
        grepl("T",cash_data[start_row:end_row])~as.numeric(gsub("T","",cash_data[start_row:end_row]))*1000,
        grepl("-",cash_data[start_row:end_row])~as.numeric(0),
        TRUE ~as.numeric(gsub("B","",cash_data[start_row:end_row]))),envir = .GlobalEnv)
      
      j <- j+1
      start_row <- start_row+5
      end_row <- start_row+4
      #print(j)
    }
    
    if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
      `Net Income before Extraordinaries` <- `Net Income`}
    
    `Cash flow coverage ratio` <<- round(`Net Operating Cash Flow`/`Total Debt`,2)
    
    `Cash flow margin ratio` <<- round(`Net Operating Cash Flow`/`Sales/Revenue`,2)
    
    `Current liability coverage ratio` <<- round(`Net Operating Cash Flow`/`Total Current Liabilities`,2)
    
    `Price to cash flow ratio` <<- round((latest_price/`Net Operating Cash Flow`)/`Basic Shares Outstanding`,2)
    
    `Cash flow to net income` <<- round(`Net Operating Cash Flow`/`Net Income`,2)
    
    `Return On Equity` <<- round(`Net Income`/`Total Equity`,4)
    
    ##KPIs--------------
    `P/E` <<- round(latest_price/`EPS (Diluted)`[5],2)
    
    `CAPE` <<- round(latest_price/mean(`EPS (Diluted)`),2)
    
    `Price To Sales` <<- round(latest_price/(`Sales/Revenue`/`Diluted Shares Outstanding`),2)
    
    `Price To Book` <<- round(latest_price/(`Total Equity`/`Diluted Shares Outstanding`),2)
    
    `Return On Assets` <<- round(`Net Income`/`Total Assets`,4)
    
    `Dividends Per Share` <<- abs(round(`Cash Dividends Paid - Total`/`Diluted Shares Outstanding`,2))
    
    `Dividends Yield` <<- round((`Dividends Per Share`/latest_price),4)
    
    `Extract Date` <<- as.character(Sys.time())
    
    `10 YR Price Estimate` <<- round(`EPS (Diluted)`[5]*
                                       (
                                         (1+
                                            min(0.1,
                                                (median(`EPS (Diluted) Growth`[2:5])/100)
                                            ))
                                         ^10)*
                                       `P/E`,2)
    
    `Fair Value Price` <<- round((`10 YR Price Estimate`/(1+0.1)^10),2)
    
    `Buy Price` <<- round(`Fair Value Price`*0.75,2)
    
    `Fair Value Gap %` <<- round(((latest_price/`Fair Value Price`)-1)*100,2)
    
    `Book Value PS` <<- round(`Total Equity`/`Diluted Shares Outstanding`,2)
    
    `Book Value PS CAGR` <<- round(((`Book Value PS`[5]/`Book Value PS`[1])^(1/4)-1),4)
    
    `EPS CAGR` <<- round(((`EPS (Diluted)`[5]/`EPS (Diluted)`[1])^(1/4)-1),4)
    
    `FCF PS` <<- round(`Free Cash Flow`/`Diluted Shares Outstanding`,2)
    
    `FCF CAGR` <<- round(((`Free Cash Flow`[5]/`Free Cash Flow`[1])^(1/4)-1),4)
    
    `Net Income CAGR` <<- round(((`Net Income`[5]/`Net Income`[1])^(1/4)-1),4)
    
    `Sales/Revenue CAGR` <<- round(((`Sales/Revenue`[5]/`Sales/Revenue`[1])^(1/4)-1),4)
    
    `Enterprise Value` <<- (latest_price * `Diluted Shares Outstanding`[5])+`Total Debt`[5]-`Cash & Short Term Investments`[5]
    
    `EV/EBITDA` <<- round(`Enterprise Value`/EBITDA[5],2)
    
    `Interest %` <<- `Interest Expense`/`Total Debt`
    
    `Tax Rate` <<- `Income Tax`/`Pretax Income`
    
    `Debt Cost` <<- `Interest %`*(1-`Tax Rate`)
    
    #print(paste0("https://www.marketwatch.com/investing/stock/",i,"/financials/income/quarter"))
    #print(paste0("https://www.marketwatch.com/investing/stock/",i,"/financials/balance-sheet/quarter"))
    #print(paste0("https://www.marketwatch.com/investing/stock/",i,"/financials/cash-flow/quarter"))
    
    

    #QTR---------------------
    
    
    qtr_income_data <<- qtr_income_page %>% html_nodes(".cell__content span") %>% html_text()
    
    qtr_balance_data <<- qtr_balance_page %>% html_nodes(".cell__content span") %>% html_text()
    
    qtr_cash_data <<- qtr_cash_page %>% html_nodes(".cell__content span") %>% html_text()
    
    qtr_income_rows <<- paste0("qtr_",income_rows)
    qtr_balance_rows <<- paste0("qtr_",balance_rows)
    qtr_cash_rows <<- paste0("qtr_",cash_rows)
    
    qtr_header <<- qtr_income_page %>% html_nodes(".overflow__heading .cell__content") %>% html_text()
    
    qtr_header <<- qtr_header[qtr_header !="Item"] 
    
    qtr_fy <<- NA
    
    qtr_titles <<- qtr_header[1:5]
    
    qtr_currency <<- currency
    
    #Income Statement QTR--------------
    j <- 1
    start_row <- 1
    end_row <- start_row+4
    
    while (j <= length(qtr_income_rows)){
      
      assign(qtr_income_rows[j],case_when(
        grepl("%",qtr_income_data[start_row:end_row])~as.numeric(gsub("%","",qtr_income_data[start_row:end_row]))/100,
        grepl("K",qtr_income_data[start_row:end_row])&grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("K|[()]","",qtr_income_data[start_row:end_row]))/-1000000,
        grepl("M",qtr_income_data[start_row:end_row])&grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("M|[()]","",qtr_income_data[start_row:end_row]))/-1000,
        grepl("B",qtr_income_data[start_row:end_row])&grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_income_data[start_row:end_row]))/-1,
        grepl("T",qtr_income_data[start_row:end_row])&grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("T|[()]","",qtr_income_data[start_row:end_row]))*-1000,
        grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_income_data[start_row:end_row]))/1,
        grepl("K",qtr_income_data[start_row:end_row])~as.numeric(gsub("K","",qtr_income_data[start_row:end_row]))/1000000,
        grepl("M",qtr_income_data[start_row:end_row])~as.numeric(gsub("M","",qtr_income_data[start_row:end_row]))/1000,
        grepl("T",qtr_income_data[start_row:end_row])~as.numeric(gsub("T","",qtr_income_data[start_row:end_row]))*1000,
        grepl("-",qtr_income_data[start_row:end_row])~as.numeric(0),
        TRUE ~as.numeric(gsub("B","",qtr_income_data[start_row:end_row]))),envir = .GlobalEnv)
      
      j <- j+1
      start_row <- start_row+5
      end_row <- start_row+4
    }
    
    if (length(grep("qtr_interest income growth",qtr_income_rows,ignore.case = T))){
      `qtr_Sales/Revenue` <<- `qtr_Net Interest Income`+`qtr_Non-Interest Income`}
    
    if (length(grep("qtr_interest income growth",qtr_income_rows,ignore.case = T))){
      `qtr_Sales Growth` <<- `qtr_Net Interest Income Growth`}
    
    if (length(grep("qtr_total interest expense",qtr_income_rows,ignore.case = T))){
      `qtr_Cost of Goods Sold (COGS) incl. D&A` <<- `qtr_Total Interest Expense`}
    
    if (length(grep("qtr_Total Interest Expense Growth",qtr_income_rows,ignore.case = T))){
      `qtr_COGS Growth` <<- `qtr_Total Interest Expense Growth`}
    
    if (length(grep("qtr_Interest Expense on Debt",qtr_income_rows,ignore.case = T))){
      `qtr_Interest Expense` <<- `qtr_Interest Expense on Debt`}
    
    if (length(grep("qtr_Net Interest Income",qtr_income_rows,ignore.case = T))){
      `qtr_Gross Income` <<- `qtr_Net Interest Income`}
    
    if (length(grep("qtr_Net Interest Income Growth",qtr_income_rows,ignore.case = T))){
      `qtr_Gross Income Growth` <<- `qtr_Net Interest Income Growth`}
    
    if (length(grep("qtr_Non-Interest Expense",qtr_income_rows,ignore.case = T))){
      `qtr_SG&A Expense` <<- `qtr_Non-Interest Expense`}
    
    if (length(grep("qtr_Income Taxes",qtr_income_rows,ignore.case = T))){
      `qtr_Income Tax` <<- `qtr_Income Taxes`}
    
    if (length(grep("qtr_interest income",qtr_income_rows,ignore.case = T))){
      `qtr_EBITDA` <<- replace_na(`qtr_Net Income`,0)+
        replace_na(`qtr_Depreciation & Amortization Expense`,0)+
        replace_na(`qtr_Income Tax`+`qtr_Interest Expense`,0)+
        replace_na(`qtr_Non-Operating Interest Income`,0)}
    
    if (length(grep("qtr_interest income",qtr_income_rows,ignore.case = T))){
      `qtr_EBITDA Margin` <<- qtr_EBITDA/`qtr_Sales/Revenue`}
    
    `qtr_Gross Profit Margin %` <<- round(`qtr_Gross Income`/`qtr_Sales/Revenue`,2)
    
    `qtr_Net Profit Margin %` <<- round(`qtr_Net Income`/`qtr_Sales/Revenue`,2)
    
    `qtr_Operating Profit Margin %` <<- round(`qtr_Gross Income`-`qtr_SG&A Expense`/`qtr_Sales/Revenue`,2)
    
    #Balance Sheet QTR------------------
    j <- 1
    start_row <- 1
    end_row <- start_row+4
    
    
    while (j <= length(balance_rows)){
      
      assign(paste0("qtr_",trimws(balance_rows[j])),case_when(
        grepl("%",qtr_balance_data[start_row:end_row])~as.numeric(gsub("%","",qtr_balance_data[start_row:end_row]))/100,
        grepl("K",qtr_balance_data[start_row:end_row])&grepl("[()]",qtr_balance_data[start_row:end_row])~as.numeric(gsub("K|[()]","",qtr_balance_data[start_row:end_row]))/-1000000,
        grepl("M",qtr_balance_data[start_row:end_row])&grepl("[()]",qtr_balance_data[start_row:end_row])~as.numeric(gsub("M|[()]","",qtr_balance_data[start_row:end_row]))/-1000,
        grepl("B",qtr_balance_data[start_row:end_row])&grepl("[()]",qtr_balance_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_balance_data[start_row:end_row]))/-1,
        grepl("T",qtr_balance_data[start_row:end_row])&grepl("[()]",qtr_balance_data[start_row:end_row])~as.numeric(gsub("T|[()]","",qtr_balance_data[start_row:end_row]))*-1000,
        grepl("[()]",qtr_balance_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_balance_data[start_row:end_row]))/1,
        grepl("K",qtr_balance_data[start_row:end_row])~as.numeric(gsub("K","",qtr_balance_data[start_row:end_row]))/1000000,
        grepl("M",qtr_balance_data[start_row:end_row])~as.numeric(gsub("M","",qtr_balance_data[start_row:end_row]))/1000,
        grepl("T",qtr_balance_data[start_row:end_row])~as.numeric(gsub("T","",qtr_balance_data[start_row:end_row]))*1000,
        grepl("-",qtr_balance_data[start_row:end_row])~as.numeric(0),
        TRUE ~as.numeric(gsub("B","",qtr_balance_data[start_row:end_row]))),envir = .GlobalEnv)
      
      j <- j+1
      start_row <- start_row+5
      end_row <- start_row+4
    }
    
    if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
      `qtr_Cash Only` <- `qtr_Total Cash & Due from Banks`}
    
    if (length(grep("Investments - Total",balance_rows,ignore.case = T))){
      `qtr_Short-Term Investments` <- `qtr_Investments - Total`}
    
    if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
      `qtr_Cash & Short Term Investments` <- `qtr_Cash Only`+`qtr_Short-Term Investments`}
    
    if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
      `qtr_Total Current Assets` <- `qtr_Cash Only`+`qtr_Short-Term Investments`+`qtr_Net Loans`}
    
    `qtr_Total Debt` <<- replace_na(`qtr_ST Debt & Current Portion LT Debt`,0)+
      replace_na(`qtr_Long-Term Debt`,0)
    
    `qtr_Debt To Equity` <<- round(`qtr_Total Debt` / `qtr_Total Shareholders' Equity`,2)
    
    `qtr_Total Liabilities To Equity` <<- round(as.numeric(`qtr_Total Liabilities`)/as.numeric(`qtr_Total Shareholders' Equity`),2)
    
    `qtr_Current Ratio` <<- if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
      round(`qtr_Total Assets`/`qtr_Total Liabilities`,2)} else {round(`qtr_Total Current Assets`/`qtr_Total Current Liabilities`,2)}
    
    `qtr_Quick Ratio` <<- if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
      round(`qtr_Total Assets`/`qtr_Total Liabilities`,2)} else {round((`qtr_Total Current Assets`-`qtr_Inventories`)/`qtr_Total Current Liabilities`,2)}
    
    
    #Cash Flow QTR-----------------
    j <- 1
    start_row <- 1
    end_row <- start_row+4
    
    qtr_col_counts <- qtr_cash_page %>% html_nodes(".overflow__heading .cell__content") %>% html_text() %>% str_subset("^[0-9]{2}") %>% unique() %>% length()
    
    while (j <= length(cash_rows)){
      
      assign(paste0("qtr_",trimws(cash_rows[j])),case_when(
        grepl("%",qtr_cash_data[start_row:end_row])~as.numeric(gsub("%","",qtr_cash_data[start_row:end_row]))/100,
        grepl("K",qtr_cash_data[start_row:end_row])&grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("K|[()]","",qtr_cash_data[start_row:end_row]))/-1000000,
        grepl("M",qtr_cash_data[start_row:end_row])&grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("M|[()]","",qtr_cash_data[start_row:end_row]))/-1000,
        grepl("B",qtr_cash_data[start_row:end_row])&grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_cash_data[start_row:end_row]))/-1,
        grepl("T",qtr_cash_data[start_row:end_row])&grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("T|[()]","",qtr_cash_data[start_row:end_row]))*-1000,
        grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_cash_data[start_row:end_row]))/1,
        grepl("K",qtr_cash_data[start_row:end_row])~as.numeric(gsub("K","",qtr_cash_data[start_row:end_row]))/1000000,
        grepl("M",qtr_cash_data[start_row:end_row])~as.numeric(gsub("M","",qtr_cash_data[start_row:end_row]))/1000,
        grepl("T",qtr_cash_data[start_row:end_row])~as.numeric(gsub("T","",qtr_cash_data[start_row:end_row]))*1000,
        grepl("-",qtr_cash_data[start_row:end_row])~as.numeric(0),
        TRUE ~as.numeric(gsub("B","",qtr_cash_data[start_row:end_row]))),envir = .GlobalEnv)
      
      j <- j+1
      start_row <- start_row+5
      end_row <- start_row+4
    }
    
    if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
      `qtr_Net Income before Extraordinaries` <- `qtr_Net Income`}
    
    `qtr_Cash flow coverage ratio` <<- round(`qtr_Net Operating Cash Flow`/`qtr_Total Debt`,2)
    
    `qtr_Cash flow margin ratio` <<- round(`qtr_Net Operating Cash Flow`/`qtr_Sales/Revenue`,2)
    
    `qtr_Current liability coverage ratio` <<- round(`qtr_Net Operating Cash Flow`/`qtr_Total Current Liabilities`,2)
    
    `qtr_FCF PS` <<- round(`qtr_Free Cash Flow`/`qtr_Diluted Shares Outstanding`,2)
    
    `qtr_Price to cash flow ratio` <<- round((latest_price/`qtr_Net Operating Cash Flow`)/`qtr_Diluted Shares Outstanding`,2)
    
    `qtr_Cash flow to net income` <<- round(`qtr_Net Operating Cash Flow`/`qtr_Net Income`,2)
    
    `qtr_Return On Equity` <<- round(`qtr_Net Income`/`qtr_Total Equity`,4)
    
    #FY Statement------------------------
    financial_statement <<- rbind(ins_name,fy,currency,ins_ticker,latest_price,
                                  # Beta,latest_price_1day_chg,latest_price_5day_chg,latest_price_1mnth_chg,latest_price_3mnth_chg,latest_price_ytd_chg,latest_price_1yr_chg,
                                  `10 YR Price Estimate`,`Fair Value Price`,`Fair Value Gap %`,`Buy Price`,latest_price_currency,`Extract Date`,`P/E`,CAPE,`EV/EBITDA`,`EPS (Diluted)`,`EPS (Diluted) Growth`,`EPS CAGR`,`FCF PS`,`Current Ratio`,`Quick Ratio`,
                                  `Dividends Per Share`,`Dividends Yield`,`Debt To Equity`,`Book Value PS`,`Book Value PS CAGR`,`Return On Equity`,`Return On Assets`,`Gross Profit Margin %`,`Net Profit Margin %`,`Operating Profit Margin %`,
                                  `Sales/Revenue`,`Sales/Revenue CAGR`,`Net Income`,`Net Income CAGR`,`Free Cash Flow`,`FCF CAGR`,`Total Equity`,`Total Debt`,`Net Operating Cash Flow`,`Net Investing Cash Flow`,`Cash Dividends Paid - Total`,`Repurchase of Common & Preferred Stk.`,`Net Financing Cash Flow`,
                                  `Enterprise Value`,`Diluted Shares Outstanding`,`Price To Sales`,`Price To Book`,`Price to cash flow ratio`,
                                  `Total Liabilities To Equity`,`Cash flow coverage ratio`,`Cash flow margin ratio`,`Current liability coverage ratio`,`Cash flow to net income`,
                                  
                                  ins_name,fy,currency,latest_price,
                                  `Sales/Revenue`,`Sales Growth`,`Cost of Goods Sold (COGS) incl. D&A`,`COGS Growth`,`COGS excluding D&A`,`Depreciation & Amortization Expense`,`Depreciation`,`Amortization of Intangibles`,`Gross Income`,`Gross Income Growth`,`Gross Profit Margin`,`SG&A Expense`,`SGA Growth`,`Research & Development`,`Other SG&A`,`Other Operating Expense`,`Unusual Expense`,`EBIT after Unusual Expense`,`Non Operating Income/Expense`,`Non-Operating Interest Income`,`Equity in Affiliates (Pretax)`,`Interest Expense`,`Interest Expense Growth`,`Gross Interest Expense`,`Interest Capitalized`,`Pretax Income`,`Pretax Income Growth`,`Pretax Margin`,`Income Tax`,`Income Tax - Current Domestic`,`Income Tax - Current Foreign`,`Income Tax - Deferred Domestic`,`Income Tax - Deferred Foreign`,`Income Tax Credits`,`Equity in Affiliates`,`Other After Tax Income (Expense)`,`Consolidated Net Income`,`Minority Interest Expense`,`Net Income`,`Net Income Growth`,`Net Margin Growth`,`Extraordinaries & Discontinued Operations`,`Extra Items & Gain/Loss Sale Of Assets`,`Cumulative Effect - Accounting Chg`,`Discontinued Operations`,`Net Income After Extraordinaries`,`Preferred Dividends`,`Net Income Available to Common`,`EPS (Basic)`,`EPS (Basic) Growth`,`Basic Shares Outstanding`,`EPS (Diluted)`,`EPS (Diluted) Growth`,`Diluted Shares Outstanding`,`EBITDA`,`EBITDA Growth`,`EBITDA Margin`,
                                  
                                  ins_name,fy,currency,latest_price,
                                  `Cash & Short Term Investments`,`Cash & Short Term Investments Growth`,`Cash Only`,`Short-Term Investments`,`Cash & ST Investments / Total Assets`,`Total Accounts Receivable`,`Total Accounts Receivable Growth`,`Accounts Receivables, Net`,`Accounts Receivables, Gross`,`Bad Debt/Doubtful Accounts`,`Other Receivable`,`Accounts Receivable Turnover`,`Inventories`,`Finished Goods`,`Work in Progress`,`Raw Materials`,`Progress Payments & Other`,`Other Current Assets`,`Miscellaneous Current Assets`,`Total Current Assets`,`Net Property, Plant & Equipment`,`Property, Plant & Equipment - Gross`,`Buildings`,`Land & Improvements`,`Computer Software and Equipment`,`Other Property, Plant & Equipment`,`Accumulated Depreciation`,`Total Investments and Advances`,`Other Long-Term Investments`,`Long-Term Note Receivables`,`Intangible Assets`,`Net Goodwill`,`Net Other Intangibles`,`Other Assets`,`Total Assets`,`Total Assets Growth`,`ST Debt & Current Portion LT Debt`,`Short Term Debt`,`Current Portion of Long Term Debt`,`Accounts Payable`,`Accounts Payable Growth`,`Income Tax Payable`,`Other Current Liabilities`,`Dividends Payable`,`Accrued Payroll`,`Miscellaneous Current Liabilities`,`Total Current Liabilities`,`Long-Term Debt`,`Long-Term Debt excl. Capitalized Leases`,`Non-Convertible Debt`,`Convertible Debt`,`Capitalized Lease Obligations`,`Provision for Risks & Charges`,`Deferred Taxes`,`Deferred Taxes - Credits`,`Deferred Taxes - Debit`,`Other Liabilities`,`Other Liabilities (excl. Deferred Income)`,`Deferred Income`,`Total Liabilities`,`Non-Equity Reserves`,`Total Liabilities / Total Assets`,`Preferred Stock (Carrying Value)`,`Redeemable Preferred Stock`,`Non-Redeemable Preferred Stock`,`Common Equity (Total)`,`Common Equity / Total Assets`,`Common Stock Par/Carry Value`,`Retained Earnings`,`ESOP Debt Guarantee`,`Cumulative Translation Adjustment/Unrealized For. Exch. Gain`,`Unrealized Gain/Loss Marketable Securities`,`Revaluation Reserves`,`Treasury Stock`,`Total Shareholders' Equity`,`Total Shareholders' Equity / Total Assets`,`Accumulated Minority Interest`,`Total Equity`,`Liabilities & Shareholders' Equity`,
                                  
                                  ins_name,fy,currency,latest_price,
                                  `Net Income before Extraordinaries`,`Net Income Growth`,`Depreciation, Depletion & Amortization`,`Depreciation and Depletion`,`Amortization of Intangible Assets`,`Deferred Taxes & Investment Tax Credit`,`Deferred Taxes`,`Investment Tax Credit`,`Other Funds`,`Funds from Operations`,`Extraordinaries`,`Changes in Working Capital`,`Receivables`,`Accounts Payable`,`Other Assets/Liabilities`,`Net Operating Cash Flow`,`Net Operating Cash Flow Growth`,`Net Operating Cash Flow / Sales`,`Capital Expenditures`,`Capital Expenditures Growth`,`Capital Expenditures / Sales`,`Capital Expenditures (Fixed Assets)`,`Capital Expenditures (Other Assets)`,`Net Assets from Acquisitions`,`Sale of Fixed Assets & Businesses`,`Purchase/Sale of Investments`,`Purchase of Investments`,`Sale/Maturity of Investments`,`Other Uses`,`Other Sources`,`Net Investing Cash Flow`,`Net Investing Cash Flow Growth`,`Net Investing Cash Flow / Sales`,`Cash Dividends Paid - Total`,`Common Dividends`,`Preferred Dividends`,`Change in Capital Stock`,`Repurchase of Common & Preferred Stk.`,`Sale of Common & Preferred Stock`,`Proceeds from Stock Options`,`Other Proceeds from Sale of Stock`,`Issuance/Reduction of Debt, Net`,`Change in Current Debt`,`Change in Long-Term Debt`,`Issuance of Long-Term Debt`,`Reduction in Long-Term Debt`,`Other Funds`,`Other Uses`,`Other Sources`,`Net Financing Cash Flow`,`Net Financing Cash Flow Growth`,`Net Financing Cash Flow / Sales`,`Exchange Rate Effect`,`Miscellaneous Funds`,`Net Change in Cash`,`Free Cash Flow`,`Free Cash Flow Growth`,`Free Cash Flow Yield`
                                  
    )
    
    # colnames(financial_statement) <- c("2015","2016","2017","2018","2019")
    
    # colnames(financial_statement) <<- c(header[2],header[3],header[4],header[5],header[6])
    
    colnames(financial_statement) <<- c(header)
    
    name <<- paste0(i,"_financial_statements")
    
    #print(name)
    
    #assign(name,financial_statement,envir = .GlobalEnv)
    
    # View(financial_statement)
    
    # return(financial_statement)
   
    #Income Statement TTM--------------
    j <- 1
    start_row <- 1
    end_row <- start_row+4
    
    qtr_days <<- as.numeric(as.Date(qtr_header[5],c("%d-%B-%Y")) - as.Date(qtr_header[4],c("%d-%B-%Y")))
    
    if (qtr_days<100){
      while (j <= length(income_rows)){
        assign(paste0("TTM ",trimws(income_rows[j])),sum(case_when(
          grepl("%",qtr_income_data[start_row:end_row])~as.numeric(gsub("%","",qtr_income_data[start_row:end_row]))/100,
          grepl("K",qtr_income_data[start_row:end_row])&grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("K|[()]","",qtr_income_data[start_row:end_row]))/-1000000,
          grepl("M",qtr_income_data[start_row:end_row])&grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("M|[()]","",qtr_income_data[start_row:end_row]))/-1000,
          grepl("B",qtr_income_data[start_row:end_row])&grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_income_data[start_row:end_row]))/-1,
          grepl("T",qtr_income_data[start_row:end_row])&grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("T|[()]","",qtr_income_data[start_row:end_row]))*-1000,
          grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_income_data[start_row:end_row]))/1,
          grepl("K",qtr_income_data[start_row:end_row])~as.numeric(gsub("K","",qtr_income_data[start_row:end_row]))/1000000,
          grepl("M",qtr_income_data[start_row:end_row])~as.numeric(gsub("M","",qtr_income_data[start_row:end_row]))/1000,
          grepl("T",qtr_income_data[start_row:end_row])~as.numeric(gsub("T","",qtr_income_data[start_row:end_row]))*1000,
          grepl("-",qtr_income_data[start_row:end_row])~as.numeric(0),
          TRUE ~as.numeric(gsub("B","",qtr_income_data[start_row:end_row])))[2:5],na.rm = T),envir = .GlobalEnv)
        
        j <- j+1
        start_row <- start_row+5
        end_row <- start_row+4
      }
    } else{
      while (j <= length(income_rows)){
        assign(paste0("TTM ",trimws(income_rows[j])),sum(case_when(
          grepl("%",qtr_income_data[start_row:end_row])~as.numeric(gsub("%","",qtr_income_data[start_row:end_row]))/100,
          grepl("K",qtr_income_data[start_row:end_row])&grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("K|[()]","",qtr_income_data[start_row:end_row]))/-1000000,
          grepl("M",qtr_income_data[start_row:end_row])&grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("M|[()]","",qtr_income_data[start_row:end_row]))/-1000,
          grepl("B",qtr_income_data[start_row:end_row])&grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_income_data[start_row:end_row]))/-1,
          grepl("T",qtr_income_data[start_row:end_row])&grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("T|[()]","",qtr_income_data[start_row:end_row]))*-1000,
          grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_income_data[start_row:end_row]))/1,
          grepl("K",qtr_income_data[start_row:end_row])~as.numeric(gsub("K","",qtr_income_data[start_row:end_row]))/1000000,
          grepl("M",qtr_income_data[start_row:end_row])~as.numeric(gsub("M","",qtr_income_data[start_row:end_row]))/1000,
          grepl("T",qtr_income_data[start_row:end_row])~as.numeric(gsub("T","",qtr_income_data[start_row:end_row]))*1000,
          grepl("-",qtr_income_data[start_row:end_row])~as.numeric(0),
          TRUE ~as.numeric(gsub("B","",qtr_income_data[start_row:end_row])))[4:5],na.rm = T),envir = .GlobalEnv)
        
        j <- j+1
        start_row <- start_row+5
        end_row <- start_row+4
      }
    }
    
    if (length(grep("qtr_interest income",qtr_income_rows,ignore.case = T))){
      `TTM Sales/Revenue` <<- `TTM Net Interest Income`+`TTM Non-Interest Income`}
    
    if (length(grep("qtr_interest income growth",qtr_income_rows,ignore.case = T))){
      `TTM Sales Growth` <<- `TTM Net Interest Income Growth`}
    
    if (length(grep("qtr_total interest expense",qtr_income_rows,ignore.case = T))){
      `TTM Cost of Goods Sold (COGS) incl. D&A` <<- `TTM Total Interest Expense`}
    
    if (length(grep("qtr_Total Interest Expense Growth",qtr_income_rows,ignore.case = T))){
      `TTM COGS Growth` <<- `TTM Total Interest Expense Growth`}
    
    if (length(grep("qtr_Interest Expense on Debt",qtr_income_rows,ignore.case = T))){
      `TTM Interest Expense` <<- `TTM Interest Expense on Debt`}
    
    if (length(grep("qtr_Net Interest Income",qtr_income_rows,ignore.case = T))){
      `TTM Gross Income` <<- `TTM Net Interest Income`}
    
    if (length(grep("qtr_Net Interest Income Growth",qtr_income_rows,ignore.case = T))){
      `TTM Gross Income Growth` <<- `TTM Net Interest Income Growth`}
    
    if (length(grep("qtr_Non-Interest Expense",qtr_income_rows,ignore.case = T))){
      `TTM SG&A Expense` <<- `TTM Non-Interest Expense`}
    
    if (length(grep("qtr_Income Taxes",qtr_income_rows,ignore.case = T))){
      `TTM Income Tax` <<- `TTM Income Taxes`}
    
    if (length(grep("qtr_interest income",qtr_income_rows,ignore.case = T))){
      `TTM EBITDA` <<- replace_na(`TTM Net Income`,0)+
        replace_na(`TTM Depreciation & Amortization Expense`,0)+
        replace_na(`TTM Income Tax`+`TTM Interest Expense`,0)+
        replace_na(`TTM Non-Operating Interest Income`,0)}
    
    `TTM Gross Profit Margin %` <<- round(`TTM Gross Income`/`TTM Sales/Revenue`,2)
    
    `TTM Net Profit Margin %` <<- round(`TTM Net Income`/`TTM Sales/Revenue`,2)
    
    `TTM Operating Profit Margin %` <<- round(`TTM Gross Income`-`TTM SG&A Expense`/`TTM Sales/Revenue`,2)
    
    if (length(grep("qtr_interest income",qtr_income_rows,ignore.case = T))){
      `TTM EBITDA Margin` <<- `TTM EBITDA`/`TTM Sales/Revenue`}
    
    `TTM Basic Shares Outstanding` <<- case_when(`qtr_Basic Shares Outstanding`[5]==0|is.na(`qtr_Basic Shares Outstanding`[5])~as.numeric(financial_statement["Basic Shares Outstanding",][5]),TRUE~`qtr_Basic Shares Outstanding`[5])
    
    `TTM Diluted Shares Outstanding` <<- case_when(`qtr_Diluted Shares Outstanding`[5]==0|is.na(`qtr_Diluted Shares Outstanding`[5])~as.numeric(financial_statement["Diluted Shares Outstanding",][5]),TRUE~`qtr_Diluted Shares Outstanding`[5])
    
    `TTM Sales Growth` <<- round((as.numeric(`TTM Sales/Revenue`)/as.numeric(financial_statement["Sales/Revenue",][5])-1),4)
    
    `TTM COGS Growth` <<- round((as.numeric(`TTM Cost of Goods Sold (COGS) incl. D&A`)/as.numeric(financial_statement["Cost of Goods Sold (COGS) incl. D&A",][5])-1),2)
    
    `TTM Gross Income Growth` <<- round((as.numeric(`TTM Gross Income`)/as.numeric(financial_statement["Gross Income",][5])-1),2)
    
    `TTM Gross Profit Margin` <<- round((as.numeric(`TTM Gross Income`)/as.numeric(financial_statement["Sales/Revenue",][5])),2)
    
    `TTM SGA Growth` <<- round((as.numeric(`TTM SG&A Expense`)/as.numeric(financial_statement["SG&A Expense",][5])-1),2)
    
    `TTM Interest Expense Growth` <<- round((as.numeric(`TTM Interest Expense`)/as.numeric(financial_statement["Interest Expense",][5])-1),2)
    
    `TTM Pretax Income Growth` <<- round((as.numeric(`TTM Pretax Income`)/as.numeric(financial_statement["Pretax Income",][5])-1),2)
    `TTM Pretax Margin` <<- round((as.numeric(`TTM Pretax Income`)/as.numeric(financial_statement["Sales/Revenue",][5])),2)
    
    `TTM Net Income Growth` <<- round((as.numeric(`TTM Net Income`)/as.numeric(financial_statement["Net Income",][5])-1),2)
    `TTM Net Margin Growth` <<- round((as.numeric(`TTM Net Income`)/as.numeric(financial_statement["Sales/Revenue",][5])),2)
    
    
    `TTM EPS (Basic) Growth` <<- round((as.numeric(`TTM EPS (Basic)`)/as.numeric(financial_statement["EPS (Basic)",][5])-1),2)
    
    `TTM EPS (Diluted) Growth` <<- round((as.numeric(`TTM EPS (Diluted)`)/as.numeric(financial_statement["EPS (Diluted)",][5])-1),2)
    
    `TTM EBITDA Growth` <<- round((as.numeric(`TTM EBITDA`)/as.numeric(financial_statement["EBITDA",][5])-1),2)
    `TTM EBITDA Margin` <<- round((as.numeric(`TTM EBITDA`)/as.numeric(financial_statement["Sales/Revenue",][5])),2)
    
    #Balance Sheet TTM-------------
    j <- 1
    start_row <- 1
    end_row <- start_row+4
    
    while (j <= length(balance_rows)){
      
      assign(paste0("TTM ",trimws(balance_rows[j])),sum(case_when(
        grepl("%",qtr_balance_data[start_row:end_row])~as.numeric(gsub("%","",qtr_balance_data[start_row:end_row]))/100,
        grepl("K",qtr_balance_data[start_row:end_row])&grepl("[()]",qtr_balance_data[start_row:end_row])~as.numeric(gsub("K|[()]","",qtr_balance_data[start_row:end_row]))/-1000000,
        grepl("M",qtr_balance_data[start_row:end_row])&grepl("[()]",qtr_balance_data[start_row:end_row])~as.numeric(gsub("M|[()]","",qtr_balance_data[start_row:end_row]))/-1000,
        grepl("B",qtr_balance_data[start_row:end_row])&grepl("[()]",qtr_balance_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_balance_data[start_row:end_row]))/-1,
        grepl("T",qtr_balance_data[start_row:end_row])&grepl("[()]",qtr_balance_data[start_row:end_row])~as.numeric(gsub("T|[()]","",qtr_balance_data[start_row:end_row]))*-1000,
        grepl("[()]",qtr_balance_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_balance_data[start_row:end_row]))/1,
        grepl("K",qtr_balance_data[start_row:end_row])~as.numeric(gsub("K","",qtr_balance_data[start_row:end_row]))/1000000,
        grepl("M",qtr_balance_data[start_row:end_row])~as.numeric(gsub("M","",qtr_balance_data[start_row:end_row]))/1000,
        grepl("T",qtr_balance_data[start_row:end_row])~as.numeric(gsub("T","",qtr_balance_data[start_row:end_row]))*1000,
        grepl("-",qtr_balance_data[start_row:end_row])~as.numeric(0),
        TRUE ~as.numeric(gsub("B","",qtr_balance_data[start_row:end_row])))[5],na.rm = T),envir = .GlobalEnv)
      
      j <- j+1
      start_row <- start_row+5
      end_row <- start_row+4
    }
    
    if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
      `TTM Cash Only` <- `qtr_Total Cash & Due from Banks`[5]}
    
    if (length(grep("Investments - Total",balance_rows,ignore.case = T))){
      `TTM Short-Term Investments` <- `qtr_Investments - Total`[5]}
    
    if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
      `TTM Cash & Short Term Investments` <- `qtr_Cash Only`[5]+`qtr_Short-Term Investments`[5]}
    
    if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
      `TTM Total Current Assets` <- `qtr_Cash Only`[5]+`qtr_Short-Term Investments`[5]+`qtr_Net Loans`[5]}
    
    
    `TTM Cash & Short Term Investments Growth` <<- round((as.numeric(`TTM Cash & Short Term Investments`)/as.numeric(financial_statement["Cash & Short Term Investments",][5])-1),2)
    
    `TTM Accounts Receivable Growth` <<- round((as.numeric(`TTM Total Accounts Receivable`)/as.numeric(financial_statement["Total Accounts Receivable",][5])-1),2)
    
    `TTM Cash & ST Investments / Total Assets` <<- round(`TTM Cash & Short Term Investments`/`TTM Total Assets`,2)
    
    `TTM Assets - Total - Growth` <<- paste0(round((as.numeric(`TTM Total Assets`)/as.numeric(financial_statement["Total Assets",][5])-1)*100,2),"%")
    
    `TTM Accounts Payable Growth` <<- paste0(round((as.numeric(`TTM Accounts Payable`)/as.numeric(financial_statement["Accounts Payable",][5])-1)*100,2),"%")
    
    `TTM Total Debt` <<- replace_na(`TTM ST Debt & Current Portion LT Debt`,0)+
      replace_na(`TTM Long-Term Debt`,0)
    
    `TTM Debt To Equity` <<- round(`TTM Total Debt` / `TTM Total Shareholders' Equity`,2)
    
    `TTM Total Liabilities To Equity` <<- round(as.numeric(gsub("B","",`TTM Total Liabilities`))/as.numeric(gsub("B","",`TTM Total Shareholders' Equity`)),2)
    
    `TTM Current Ratio` <<- round(as.numeric(gsub("B","",`TTM Total Current Assets`))/as.numeric(gsub("B","",`TTM Total Current Liabilities`)),2)
    
    `TTM Quick Ratio` <<- round((as.numeric(gsub("B","",`TTM Total Current Assets`))-as.numeric(gsub("B","",`TTM Inventories`)))/as.numeric(gsub("B","",`TTM Total Current Liabilities`)),2)
    
    #Cash Flow TTM----------------
    
    j <- 1
    start_row <- 1
    end_row <- start_row+4
    
    if (qtr_days<100){
      while (j <= length(cash_rows)){
        assign(paste0("TTM ",trimws(cash_rows[j])),sum(case_when(
          grepl("%",qtr_cash_data[start_row:end_row])~as.numeric(gsub("%","",qtr_cash_data[start_row:end_row]))/100,
          grepl("K",qtr_cash_data[start_row:end_row])&grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("K|[()]","",qtr_cash_data[start_row:end_row]))/-1000000,
          grepl("M",qtr_cash_data[start_row:end_row])&grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("M|[()]","",qtr_cash_data[start_row:end_row]))/-1000,
          grepl("B",qtr_cash_data[start_row:end_row])&grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_cash_data[start_row:end_row]))/-1,
          grepl("T",qtr_cash_data[start_row:end_row])&grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("T|[()]","",qtr_cash_data[start_row:end_row]))*-1000,
          grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_cash_data[start_row:end_row]))/1,
          grepl("K",qtr_cash_data[start_row:end_row])~as.numeric(gsub("K","",qtr_cash_data[start_row:end_row]))/1000000,
          grepl("M",qtr_cash_data[start_row:end_row])~as.numeric(gsub("M","",qtr_cash_data[start_row:end_row]))/1000,
          grepl("T",qtr_cash_data[start_row:end_row])~as.numeric(gsub("T","",qtr_cash_data[start_row:end_row]))*1000,
          grepl("-",qtr_cash_data[start_row:end_row])~as.numeric(0),
          TRUE ~as.numeric(gsub("B","",qtr_cash_data[start_row:end_row])))[2:5],na.rm = T),envir = .GlobalEnv)
        
        j <- j+1
        start_row <- start_row+5
        end_row <- start_row+4
      }
    } else{
      while (j <= length(cash_rows)){
        assign(paste0("TTM ",trimws(cash_rows[j])),sum(case_when(
          grepl("%",qtr_cash_data[start_row:end_row])~as.numeric(gsub("%","",qtr_cash_data[start_row:end_row]))/100,
          grepl("K",qtr_cash_data[start_row:end_row])&grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("K|[()]","",qtr_cash_data[start_row:end_row]))/-1000000,
          grepl("M",qtr_cash_data[start_row:end_row])&grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("M|[()]","",qtr_cash_data[start_row:end_row]))/-1000,
          grepl("B",qtr_cash_data[start_row:end_row])&grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_cash_data[start_row:end_row]))/-1,
          grepl("T",qtr_cash_data[start_row:end_row])&grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("T|[()]","",qtr_cash_data[start_row:end_row]))*-1000,
          grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_cash_data[start_row:end_row]))/1,
          grepl("K",qtr_cash_data[start_row:end_row])~as.numeric(gsub("K","",qtr_cash_data[start_row:end_row]))/1000000,
          grepl("M",qtr_cash_data[start_row:end_row])~as.numeric(gsub("M","",qtr_cash_data[start_row:end_row]))/1000,
          grepl("T",qtr_cash_data[start_row:end_row])~as.numeric(gsub("T","",qtr_cash_data[start_row:end_row]))*1000,
          grepl("-",qtr_cash_data[start_row:end_row])~as.numeric(0),
          TRUE ~as.numeric(gsub("B","",qtr_cash_data[start_row:end_row])))[4:5],na.rm = T),envir = .GlobalEnv)
        
        j <- j+1
        start_row <- start_row+5
        end_row <- start_row+4
      }
    }
    
    if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
      `TTM Net Income before Extraordinaries` <- `TTM Net Income`}
    
    `TTM Net Income before Extraordinaries Growth` <<- round((as.numeric(`TTM Net Income before Extraordinaries`)/as.numeric(financial_statement["Net Income before Extraordinaries",][5])-1),2)
    
    
    `TTM Net Operating Cash Flow Growth` <- round((as.numeric(`TTM Net Operating Cash Flow`)/as.numeric(financial_statement["Net Operating Cash Flow",][5])-1),2)
    `TTM Net Operating Cash Flow / Sales` <- round(`TTM Net Operating Cash Flow`/`TTM Sales/Revenue`,2)
    
    `TTM Capital Expenditures Growth` <- round((as.numeric(`TTM Capital Expenditures`)/as.numeric(financial_statement["Capital Expenditures",][5])-1),2)
    `TTM Capital Expenditures / Sales` <- round(`TTM Capital Expenditures`/`TTM Sales/Revenue`,2)
    
    `TTM Net Investing Cash Flow Growth` <<- round((as.numeric(`TTM Net Investing Cash Flow`)/as.numeric(financial_statement["Net Investing Cash Flow",][5])-1),2)
    `TTM Net Investing Cash Flow / Sales` <<- round(`TTM Net Investing Cash Flow`/`TTM Sales/Revenue`,2)
    
    
    `TTM Net Financing Cash Flow Growth` <<- round((as.numeric(`TTM Net Financing Cash Flow`)/as.numeric(financial_statement["Net Financing Cash Flow",][5])-1),2)
    `TTM Net Financing Cash Flow / Sales` <<- round(`TTM Net Financing Cash Flow`/`TTM Sales/Revenue`,2)
    `TTM Exchange Rate Effect` <<- NA
    
    `TTM Free Cash Flow` <<- case_when(`TTM Free Cash Flow`==0~financial_statement["Free Cash Flow",][5],TRUE~as.character(`TTM Free Cash Flow`))
    `TTM Free Cash Flow Growth` <<- round((as.numeric(`TTM Free Cash Flow`)/as.numeric(financial_statement["Free Cash Flow",][5])-1),2)
    `TTM Free Cash Flow Yield` <<- NA
    
    #TTM Ratios--------------------
    
    `TTM Gross Profit Margin %` <<- round(as.numeric(gsub("B","",`TTM Gross Income`))/as.numeric(gsub("B","",`TTM Sales/Revenue`)),4)
    
    `TTM Net Profit Margin %` <<- round(as.numeric(gsub("B","",`TTM Net Income`))/as.numeric(gsub("B","",`TTM Sales/Revenue`)),4)
    
    `TTM Operating Profit Margin %` <<- round((as.numeric(gsub("B","",`TTM Gross Income`))-as.numeric(gsub("B","",`TTM SG&A Expense`)))/as.numeric(gsub("B","",`TTM Sales/Revenue`)),4)
    
    `TTM Cash flow coverage ratio` <<- round(as.numeric(gsub("B","",`TTM Net Operating Cash Flow`))/as.numeric(gsub("B","",`TTM Total Debt`)),2)
    
    `TTM Cash flow margin ratio` <<- round(as.numeric(gsub("B","",`TTM Net Operating Cash Flow`))/as.numeric(gsub("B","",`TTM Sales/Revenue`)),2)
    
    `TTM Current liability coverage ratio` <<- round(as.numeric(gsub("B","",`TTM Net Operating Cash Flow`))/as.numeric(gsub("B","",`TTM Total Current Liabilities`)),2)
    
    `TTM Price to cash flow ratio` <<- round(as.numeric(gsub(",","",latest_price))/(as.numeric(gsub("B","",`TTM Net Operating Cash Flow`))/`TTM Basic Shares Outstanding`),2)
    
    `TTM Cash flow to net income` <<- round(as.numeric(gsub("B","",`TTM Net Operating Cash Flow`))/as.numeric(gsub("B","",`TTM Net Income`)),2)
    
    `TTM Return On Equity` <<- round(as.numeric(gsub("B","",`TTM Net Income`))/as.numeric(gsub("B","",`TTM Total Equity`)),4)
    
    `TTM P/E` <<- round(as.numeric(gsub(",","",latest_price))/as.numeric(`TTM EPS (Diluted)`),2)
    
    `TTM CAPE` <<- round(as.numeric(gsub(",","",latest_price))/mean(c(as.numeric(financial_statement["EPS (Diluted)",][1:5]),as.numeric(`TTM EPS (Diluted)`))),2)
    
    `TTM Price To Sales` <<- round(as.numeric(gsub(",","",latest_price))/(as.numeric(gsub("B","",`TTM Sales/Revenue`))/`TTM Basic Shares Outstanding`),2)
    
    `TTM Price To Book` <<- round(as.numeric(gsub(",","",latest_price))/(as.numeric(gsub("B","",`TTM Total Equity`))/`TTM Basic Shares Outstanding`),2)
    
    `TTM Return On Assets` <<- round(as.numeric(gsub("B","",`TTM Net Income`))/as.numeric(gsub("B","",`TTM Total Assets`)),4)
    
    `TTM Dividends Per Share` <<- round(as.numeric(gsub("B|[()]","",abs(`TTM Cash Dividends Paid - Total`)))/`TTM Diluted Shares Outstanding`,2)
    
    `TTM Dividends Yield` <<- round((`TTM Dividends Per Share`/as.numeric(gsub(",","",latest_price))),4)
    
    `TTM units` <<- qtr_header[1]
    
    `TTM units_split` <<- str_locate(`TTM units`,"[.]")
    
    `TTM fy` <<- if (qtr_days<100){paste0(qtr_header[2],"|",qtr_header[5])} else{paste0(qtr_header[4],"|",qtr_header[5])}
    
    `TTM currency` <<- substr(`TTM units`,13,15)
    
    `TTM currency` <<- currency
    
    `TTM 10 YR Price Estimate` <<- round(as.numeric(`TTM EPS (Diluted)`)*
      (
        (1+
         min(0.1,
             (median(as.numeric(gsub("%","",c(financial_statement["EPS (Diluted) Growth",][2:5],`TTM EPS (Diluted) Growth`))))/100)
      ))
      ^10
      )*`TTM P/E`,2)
    
    `TTM Fair Value Price` <<- round((`TTM 10 YR Price Estimate`/(1+0.1)^10),2)
    
    `TTM Fair Value Gap %` <<- round(((as.numeric(gsub(",","",latest_price))/`TTM Fair Value Price`)-1)*100,4)
    
    `TTM Book Value PS` <<- round(`TTM Total Equity`/`TTM Diluted Shares Outstanding`,2)
    
    `TTM Book Value PS CAGR` <<- round(((`TTM Book Value PS`/as.numeric(financial_statement["Book Value PS",][1]))^(1/5)-1),4)
    
    `TTM EPS CAGR` <<- round(((`TTM EPS (Basic)`/as.numeric(financial_statement["EPS (Diluted)",][1]))^(1/5)-1),4)
    
    `TTM FCF CAGR` <<- round(((as.numeric(`TTM Free Cash Flow`)/as.numeric(financial_statement["Free Cash Flow",][1]))^(1/5)-1),4)
    
    `TTM FCF PS` <<- round(as.numeric(`TTM Free Cash Flow`)/`TTM Diluted Shares Outstanding`,2)
    
    `TTM Net Income CAGR` <<- round(((`TTM Net Income`/as.numeric(financial_statement["Net Income",][1]))^(1/5)-1),4)
    
    `TTM Sales/Revenue CAGR` <<- round(((`TTM Sales/Revenue`/as.numeric(financial_statement["Sales/Revenue",][1]))^(1/5)-1),4)
    
    `TTM Enterprise Value` <<- (as.numeric(latest_price) * `TTM Diluted Shares Outstanding`)+`TTM Total Debt`-`TTM Cash & Short Term Investments`
    
    `TTM EV/EBITDA` <<- round(`TTM Enterprise Value`/`TTM EBITDA`,2)
    
    `TTM Diluted Shares Outstanding CAGR` <<- round(((`TTM Diluted Shares Outstanding`/as.numeric(financial_statement["Diluted Shares Outstanding",][1]))^(1/5)-1),4)
    
    #latetst_qtr
    #Income Statement LQ--------------
    j <- 1
    start_row <- 1
    end_row <- start_row+4
    
    while (j <= length(income_rows)){
      
      assign(paste0("LT_QTR ",trimws(income_rows[j])),sum(case_when(
        grepl("%",qtr_income_data[start_row:end_row])~as.numeric(gsub("%","",qtr_income_data[start_row:end_row]))/100,
        grepl("K",qtr_income_data[start_row:end_row])&grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("K|[()]","",qtr_income_data[start_row:end_row]))/-1000000,
        grepl("M",qtr_income_data[start_row:end_row])&grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("M|[()]","",qtr_income_data[start_row:end_row]))/-1000,
        grepl("B",qtr_income_data[start_row:end_row])&grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_income_data[start_row:end_row]))/-1,
        grepl("T",qtr_income_data[start_row:end_row])&grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("T|[()]","",qtr_income_data[start_row:end_row]))*-1000,
        grepl("[()]",qtr_income_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_income_data[start_row:end_row]))/1,
        grepl("K",qtr_income_data[start_row:end_row])~as.numeric(gsub("K","",qtr_income_data[start_row:end_row]))/1000000,
        grepl("M",qtr_income_data[start_row:end_row])~as.numeric(gsub("M","",qtr_income_data[start_row:end_row]))/1000,
        grepl("T",qtr_income_data[start_row:end_row])~as.numeric(gsub("T","",qtr_income_data[start_row:end_row]))*1000,
        grepl("-",qtr_income_data[start_row:end_row])~as.numeric(0),
        TRUE ~as.numeric(gsub("B","",qtr_income_data[start_row:end_row])))[5],na.rm = T),envir = .GlobalEnv)
      
      j <- j+1
      start_row <- start_row+5
      end_row <- start_row+4
    }
    
    if (length(grep("qtr_interest income growth",qtr_income_rows,ignore.case = T))){
      `LT_QTR Sales/Revenue` <<- `LT_QTR Net Interest Income`[5]+`LT_QTR Non-Interest Income`[5]}
    
    if (length(grep("qtr_interest income growth",qtr_income_rows,ignore.case = T))){
      `LT_QTR Sales Growth` <<- `LT_QTR Net Interest Income Growth`[5]}
    
    if (length(grep("qtr_total interest expense",qtr_income_rows,ignore.case = T))){
      `LT_QTR Cost of Goods Sold (COGS) incl. D&A` <<- `LT_QTR Total Interest Expense`[5]}
    
    if (length(grep("qtr_Total Interest Expense Growth",qtr_income_rows,ignore.case = T))){
      `LT_QTR COGS Growth` <<- `LT_QTR Total Interest Expense Growth`[5]}
    
    if (length(grep("qtr_Interest Expense on Debt",qtr_income_rows,ignore.case = T))){
      `LT_QTR Interest Expense` <<- `LT_QTR Interest Expense on Debt`[5]}
    
    if (length(grep("qtr_Net Interest Income",qtr_income_rows,ignore.case = T))){
      `LT_QTR Gross Income` <<- `LT_QTR Net Interest Income`[5]}
    
    if (length(grep("qtr_Net Interest Income Growth",qtr_income_rows,ignore.case = T))){
      `LT_QTR Gross Income Growth` <<- `LT_QTR Net Interest Income Growth`[5]}
    
    if (length(grep("qtr_Non-Interest Expense",qtr_income_rows,ignore.case = T))){
      `LT_QTR SG&A Expense` <<- `LT_QTR Non-Interest Expense`[5]}
    
    if (length(grep("qtr_Income Taxes",qtr_income_rows,ignore.case = T))){
      `LT_QTR Income Tax` <<- `LT_QTR Income Taxes`[5]}
    
    if (length(grep("qtr_interest income",qtr_income_rows,ignore.case = T))){
      `LT_QTR EBITDA` <<- replace_na(`LT_QTR Net Income`[5],0)+
        replace_na(`LT_QTR Depreciation & Amortization Expense`[5],0)+
        replace_na(`LT_QTR Income Tax`+`LT_QTR Interest Expense`[5],0)+
        replace_na(`LT_QTR Non-Operating Interest Income`[5],0)}
    
    `LT_QTR Sales Growth` <- round(((`qtr_Sales/Revenue`[5]/`qtr_Sales/Revenue`[1])-1),2)
    
    `LT_QTR COGS Growth` <- round(((`qtr_Cost of Goods Sold (COGS) incl. D&A`[5]/`qtr_Cost of Goods Sold (COGS) incl. D&A`[1])-1),2)
    
    `LT_QTR Gross Income Growth` <- round(((`qtr_Gross Income`[5]/`qtr_Gross Income`[1])-1),2)
    
    `LT_QTR Gross Profit Margin` <- round((`qtr_Gross Income`[5]/`qtr_Sales/Revenue`[5]),2)
    
    `LT_QTR SGA Growth` <- round(((`qtr_SG&A Expense`[5]/`qtr_SG&A Expense`[1])-1),2)
    
    `LT_QTR Interest Expense Growth` <- round(((`qtr_Interest Expense`[5]/`qtr_Interest Expense`[1])-1),2)
    
    `LT_QTR Pretax Income Growth` <- round(((`qtr_Pretax Income`[5]/`qtr_Pretax Income`[1])-1),2)
    `LT_QTR Pretax Margin` <- round((`qtr_Pretax Income`[5]/`qtr_Sales/Revenue`[5]),2)
    
    `LT_QTR Net Income Growth` <- round(((`qtr_Net Income`[5]/`qtr_Net Income`[1])-1),2)
    `LT_QTR Net Margin Growth` <- round((`qtr_Net Income`[5]/`qtr_Sales/Revenue`[5])-((`qtr_Net Income`[1]/`qtr_Sales/Revenue`[1])),2)
    
    `LT_QTR EPS (Basic) Growth` <- round((`qtr_EPS (Basic)`[5]/`qtr_EPS (Basic)`[1]-1),2)
    
    `LT_QTR EPS (Diluted) Growth` <- round((`qtr_EPS (Diluted)`[5]/`qtr_EPS (Diluted)`[1]-1),2)
    
    `LT_QTR EBITDA Growth` <- round((`qtr_EBITDA`[5]/`qtr_EBITDA`[1]-1),2)
    `LT_QTR EBITDA Margin` <- round((`qtr_EBITDA`[5]/`qtr_Sales/Revenue`[5]),2)

#Balance Sheet LQ--------------------
j <- 1
start_row <- 1
end_row <- start_row+4

while (j <= length(balance_rows)){
  
  assign(paste0("LT_QTR ",trimws(balance_rows[j])),sum(case_when(
    grepl("%",qtr_balance_data[start_row:end_row])~as.numeric(gsub("%","",qtr_balance_data[start_row:end_row]))/100,
    grepl("K",qtr_balance_data[start_row:end_row])&grepl("[()]",qtr_balance_data[start_row:end_row])~as.numeric(gsub("K|[()]","",qtr_balance_data[start_row:end_row]))/-1000000,
    grepl("M",qtr_balance_data[start_row:end_row])&grepl("[()]",qtr_balance_data[start_row:end_row])~as.numeric(gsub("M|[()]","",qtr_balance_data[start_row:end_row]))/-1000,
    grepl("B",qtr_balance_data[start_row:end_row])&grepl("[()]",qtr_balance_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_balance_data[start_row:end_row]))/-1,
    grepl("T",qtr_balance_data[start_row:end_row])&grepl("[()]",qtr_balance_data[start_row:end_row])~as.numeric(gsub("T|[()]","",qtr_balance_data[start_row:end_row]))*-1000,
    grepl("[()]",qtr_balance_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_balance_data[start_row:end_row]))/1,
    grepl("K",qtr_balance_data[start_row:end_row])~as.numeric(gsub("K","",qtr_balance_data[start_row:end_row]))/1000000,
    grepl("M",qtr_balance_data[start_row:end_row])~as.numeric(gsub("M","",qtr_balance_data[start_row:end_row]))/1000,
    grepl("T",qtr_balance_data[start_row:end_row])~as.numeric(gsub("T","",qtr_balance_data[start_row:end_row]))*1000,
    grepl("-",qtr_balance_data[start_row:end_row])~as.numeric(0),
    TRUE ~as.numeric(gsub("B","",qtr_balance_data[start_row:end_row])))[5],na.rm = T),envir = .GlobalEnv)
  
  j <- j+1
  start_row <- start_row+5
  end_row <- start_row+4
}

if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
  `LT_QTR Cash Only` <- `qtr_Total Cash & Due from Banks`[5]}

if (length(grep("Investments - Total",balance_rows,ignore.case = T))){
  `LT_QTR Short-Term Investments` <- `qtr_Investments - Total`[5]}

if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
  `LT_QTR Cash & Short Term Investments` <- `qtr_Cash Only`[5]+`qtr_Short-Term Investments`[5]}

if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
  `LT_QTR Total Current Assets` <- `qtr_Cash Only`[5]+`qtr_Short-Term Investments`[5]+`qtr_Net Loans`[5]}


`LT_QTR Cash & Short Term Investments Growth` <<- round((as.numeric(`LT_QTR Cash & Short Term Investments`)/as.numeric(financial_statement["Cash & Short Term Investments",][5])-1),2)

`LT_QTR Accounts Receivable Growth` <<- round((`qtr_Total Accounts Receivable`[5]/`qtr_Total Accounts Receivable`[1]-1),2)

`LT_QTR Cash & ST Investments / Total Assets` <<- round(`LT_QTR Cash & Short Term Investments`/`LT_QTR Total Assets`,2)

`LT_QTR Assets - Total - Growth` <<- round((as.numeric(`LT_QTR Total Assets`)/`qtr_Total Assets`[1]-1),2)

`LT_QTR Accounts Payable Growth` <<- round((as.numeric(`LT_QTR Accounts Payable`)/(`qtr_Accounts Payable`[1])-1),2)

`LT_QTR Total Debt` <<- replace_na(`LT_QTR ST Debt & Current Portion LT Debt`,0)+
  replace_na(`LT_QTR Long-Term Debt`,0)

`LT_QTR Debt To Equity` <<- round(`LT_QTR Total Debt` / `LT_QTR Total Shareholders' Equity`,2)

`LT_QTR Total Liabilities To Equity` <<- round(as.numeric(gsub("B","",`LT_QTR Total Liabilities`))/as.numeric(gsub("B","",`LT_QTR Total Shareholders' Equity`)),2)

`LT_QTR Current Ratio` <<- round(as.numeric(gsub("B","",`LT_QTR Total Current Assets`))/as.numeric(gsub("B","",`LT_QTR Total Current Liabilities`)),2)

`LT_QTR Quick Ratio` <<- round((as.numeric(gsub("B","",`LT_QTR Total Current Assets`))-as.numeric(gsub("B","",`LT_QTR Inventories`)))/as.numeric(gsub("B","",`LT_QTR Total Current Liabilities`)),2)

#Cash Flow LQ ------------------
j <- 1
start_row <- 1
end_row <- start_row+4

while (j <= length(cash_rows)){
  
  assign(paste0("LT_QTR ",trimws(cash_rows[j])),sum(case_when(
    grepl("%",qtr_cash_data[start_row:end_row])~as.numeric(gsub("%","",qtr_cash_data[start_row:end_row]))/100,
    grepl("K",qtr_cash_data[start_row:end_row])&grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("K|[()]","",qtr_cash_data[start_row:end_row]))/-1000000,
    grepl("M",qtr_cash_data[start_row:end_row])&grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("M|[()]","",qtr_cash_data[start_row:end_row]))/-1000,
    grepl("B",qtr_cash_data[start_row:end_row])&grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_cash_data[start_row:end_row]))/-1,
    grepl("T",qtr_cash_data[start_row:end_row])&grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("T|[()]","",qtr_cash_data[start_row:end_row]))*-1000,
    grepl("[()]",qtr_cash_data[start_row:end_row])~as.numeric(gsub("B|[()]","",qtr_cash_data[start_row:end_row]))/1,
    grepl("K",qtr_cash_data[start_row:end_row])~as.numeric(gsub("K","",qtr_cash_data[start_row:end_row]))/1000000,
    grepl("M",qtr_cash_data[start_row:end_row])~as.numeric(gsub("M","",qtr_cash_data[start_row:end_row]))/1000,
    grepl("T",qtr_cash_data[start_row:end_row])~as.numeric(gsub("T","",qtr_cash_data[start_row:end_row]))*1000,
    grepl("-",qtr_cash_data[start_row:end_row])~as.numeric(0),
    TRUE ~as.numeric(gsub("B","",qtr_cash_data[start_row:end_row])))[5],na.rm = T),envir = .GlobalEnv)
  
  j <- j+1
  start_row <- start_row+5
  end_row <- start_row+4
}

if (length(grep("Total Cash & Due from Banks",balance_rows,ignore.case = T))){
  `LT_QTR Net Income before Extraordinaries` <- `qtr_Net Income`[5]}

`LT_QTR Net Income before Extraordinaries Growth` <<- round((as.numeric(`LT_QTR Net Income before Extraordinaries`)/`qtr_Net Income before Extraordinaries`[1]-1),2)

`LT_QTR Net Operating Cash Flow Growth` <- round((`qtr_Net Operating Cash Flow`[5]/`qtr_Net Operating Cash Flow`[1]-1),2)
`LT_QTR Net Operating Cash Flow / Sales` <- round(`qtr_Net Operating Cash Flow`[5]/`qtr_Sales/Revenue`[5],2)

`LT_QTR Capital Expenditures Growth` <- round((`qtr_Capital Expenditures`[5]/`qtr_Capital Expenditures`[1]-1),2)
`LT_QTR Capital Expenditures / Sales` <- round(`qtr_Capital Expenditures`[5]/`qtr_Sales/Revenue`[5],2)

`LT_QTR Net Investing Cash Flow Growth` <<- round((as.numeric(`LT_QTR Net Investing Cash Flow`)/as.numeric(`qtr_Net Investing Cash Flow`[1])-1),2)
`LT_QTR Net Investing Cash Flow / Sales` <<- round(`LT_QTR Net Investing Cash Flow`/`LT_QTR Sales/Revenue`,2)

`LT_QTR Net Financing Cash Flow Growth` <<- round((as.numeric(`LT_QTR Net Financing Cash Flow`)/as.numeric(`qtr_Net Financing Cash Flow`[1])-1),2)
`LT_QTR Net Financing Cash Flow / Sales` <<- round(`LT_QTR Net Financing Cash Flow`/`LT_QTR Sales/Revenue`,2)
`LT_QTR Exchange Rate Effect` <<- NA

`LT_QTR Free Cash Flow Growth` <<- round((as.numeric(`LT_QTR Free Cash Flow`)/as.numeric(`qtr_Free Cash Flow`[1])-1),2)
`LT_QTR Free Cash Flow Yield` <<- NA
`LT_QTR FCF PS` <<- round(as.numeric(`LT_QTR Free Cash Flow`)/`qtr_Diluted Shares Outstanding`[5],2)

#Ratio LQ-----------------

`LT_QTR fy` <<- if (qtr_days<100){paste0(qtr_header[1],"|",qtr_header[5])} else{paste0(qtr_header[3],"|",qtr_header[5])}

`LT_QTR Gross Profit Margin %` <<- round(as.numeric(gsub("B","",`LT_QTR Gross Income`))/as.numeric(gsub("B","",`LT_QTR Sales/Revenue`)),4)

`LT_QTR Net Profit Margin %` <<- round(as.numeric(gsub("B","",`LT_QTR Net Income`))/as.numeric(gsub("B","",`LT_QTR Sales/Revenue`)),4)

`LT_QTR Operating Profit Margin %` <<- round((as.numeric(gsub("B","",`LT_QTR Gross Income`))-as.numeric(gsub("B","",`LT_QTR SG&A Expense`)))/as.numeric(gsub("B","",`LT_QTR Sales/Revenue`)),4)

`LT_QTR Cash flow coverage ratio` <<- round(as.numeric(gsub("B","",`LT_QTR Net Operating Cash Flow`))/as.numeric(gsub("B","",`LT_QTR Total Debt`)),2)

`LT_QTR Cash flow margin ratio` <<- round(as.numeric(gsub("B","",`LT_QTR Net Operating Cash Flow`))/as.numeric(gsub("B","",`LT_QTR Sales/Revenue`)),2)

`LT_QTR Current liability coverage ratio` <<- round(as.numeric(gsub("B","",`LT_QTR Net Operating Cash Flow`))/as.numeric(gsub("B","",`LT_QTR Total Current Liabilities`)),2)

`LT_QTR Price to cash flow ratio` <<- round(as.numeric(gsub(",","",latest_price))/(as.numeric(gsub("B","",`LT_QTR Net Operating Cash Flow`))/`LT_QTR Basic Shares Outstanding`),2)

`LT_QTR Cash flow to net income` <<- round(as.numeric(gsub("B","",`LT_QTR Net Operating Cash Flow`))/as.numeric(gsub("B","",`LT_QTR Net Income`)),2)

`LT_QTR Return On Equity` <<- round(as.numeric(gsub("B","",`LT_QTR Net Income`))/as.numeric(gsub("B","",`LT_QTR Total Equity`)),4)

##DCF-----------------                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                

sales_growth <<- c(`Sales Growth`,`TTM Sales Growth`)

sales_growth <<- sales_growth[sales_growth!=0]

sales_growth_estimate <<- as.numeric(
  quantile(
  c(
    mean(sales_growth,na.rm = T),
  as.numeric(quantile(sales_growth,0.5,na.rm = T)),
  as.numeric(quantile(sales_growth,0.25,na.rm = T)),
  `Sales/Revenue CAGR`
  ),
  0.5,na.rm = T)
  )

gross_margin_estimate <<-as.numeric(
  quantile(
    c(
      mean(c(`Gross Profit Margin %`,`TTM Gross Profit Margin %`),na.rm = T),
      as.numeric(quantile(c(`Gross Profit Margin %`,`TTM Gross Profit Margin %`),0.5,na.rm = T))
      # as.numeric(quantile(c(`Gross Profit Margin %`,`TTM Gross Profit Margin %`),0.25,na.rm = T))
      ),
    0.5,na.rm = T)
  )

gross_net_margin <<- as.numeric(c(`Net Income`/`Gross Income`,`TTM Net Income`/`TTM Gross Income`))

gross_net_margin <<- gross_net_margin[gross_net_margin>0&gross_net_margin<=1]

gross_net_margin_estimate <<-as.numeric(
  quantile(
    c(
      mean(gross_net_margin,na.rm = T),
      as.numeric(quantile(gross_net_margin,0.5,na.rm = T))
      # as.numeric(quantile(gross_net_margin,0.25,na.rm = T))
    ),
    0.5,na.rm = T)
)

ni_ocf_rate <<- as.numeric(c(`Net Operating Cash Flow`,`TTM Net Operating Cash Flow`)/c(`Net Income`,`TTM Net Income`))

ni_ocf_rate <<- ni_ocf_rate[ni_ocf_rate>0]

ni_ocf_estimate <<-as.numeric(
  quantile(
    c(
      mean(ni_ocf_rate,na.rm = T),
      as.numeric(quantile(ni_ocf_rate,0.5,na.rm = T)),
      as.numeric(quantile(ni_ocf_rate,0.25,na.rm = T))
    ),
    0.5,na.rm = T)
)

capex_rate <<- abs(c(`Capital Expenditures`,`TTM Capital Expenditures`))/c(`Sales/Revenue`,`TTM Sales/Revenue`)

capex_estimate <<-as.numeric(
  quantile(
    c(
      mean(capex_rate,na.rm = T),
      as.numeric(quantile(capex_rate,0.5,na.rm = T)),
      as.numeric(quantile(capex_rate,0.25,na.rm = T))
    ),
    0.5,na.rm = T)
)

debt_repayment_rate <<- (c(`Issuance/Reduction of Debt, Net`,`TTM Issuance/Reduction of Debt, Net`)*-1)/(c(`Total Debt`,`TTM Total Debt`)+(c(`Issuance/Reduction of Debt, Net`,`TTM Issuance/Reduction of Debt, Net`)*-1))

debt_repayment_rate <<- debt_repayment_rate[debt_repayment_rate>=0]

debt_repayment_estimate <<-as.numeric(
  quantile(
    c(
      mean(debt_repayment_rate,na.rm = T),
      as.numeric(quantile(debt_repayment_rate,0.5,na.rm = T)),
      as.numeric(quantile(debt_repayment_rate,0.25,na.rm = T))
    ),
    0.5,na.rm = T)
)

debt_repayment_estimate[is.na(debt_repayment_estimate)] <- 0

debt_repayment_rate[is.na(debt_repayment_rate)] <- 0

interest_rate <<- as.numeric(c(`Interest Expense`,`TTM Interest Expense`)/c(`Total Debt`,`TTM Total Debt`))

interest_rate_estimate <<-as.numeric(
  quantile(
    c(
      mean(interest_rate,na.rm = T),
      as.numeric(quantile(interest_rate,0.5,na.rm = T)),
      as.numeric(quantile(interest_rate,0.25,na.rm = T))
    ),
    0.5,na.rm = T)
)

interest_rate_estimate[is.na(interest_rate_estimate)] <- 0

interest_rate[is.na(interest_rate)] <- 0

# net_income_estimate <<- as.numeric(quantile(c(mean(c(`Net Income`/`Sales/Revenue`,`TTM Net Income`/`TTM Sales/Revenue`)[c(`Net Income`/`Sales/Revenue`,`TTM Net Income`/`TTM Sales/Revenue`)>0],na.rm = T),
#                                               as.numeric(quantile(c(`Net Income`/`Sales/Revenue`,`TTM Net Income`/`TTM Sales/Revenue`)[c(`Net Income`/`Sales/Revenue`,`TTM Net Income`/`TTM Sales/Revenue`)>0],0.5,na.rm = T))),0.5))
# 
# fcf_estimate <<- as.numeric(quantile(c(mean(c(`Free Cash Flow`/`Net Income`,as.numeric(`TTM Free Cash Flow`)/`TTM Net Income`)[c(`Free Cash Flow`/`Net Income`,as.numeric(`TTM Free Cash Flow`)/`TTM Net Income`)>0],na.rm = T),
#                            as.numeric(quantile(c(`Free Cash Flow`/`Net Income`,as.numeric(`TTM Free Cash Flow`)/`TTM Net Income`)[c(`Free Cash Flow`/`Net Income`,as.numeric(`TTM Free Cash Flow`)/`TTM Net Income`)>0],0.5,na.rm = T))),0.5))

sales1 <<- as.numeric(`TTM Sales/Revenue`)*(1+sales_growth_estimate)

sales2 <<- sales1*(1+sales_growth_estimate)

sales3 <<- sales2*(1+sales_growth_estimate)

sales4 <<- sales3*(1+sales_growth_estimate)

sales5 <<- sales4*(1+sales_growth_estimate)

gross_income1 <<- sales1*gross_margin_estimate

gross_income2 <<- sales2*gross_margin_estimate

gross_income3 <<- sales3*gross_margin_estimate

gross_income4 <<- sales4*gross_margin_estimate

gross_income5 <<- sales5*gross_margin_estimate

# net_income1 <<- sales1*net_income_estimate
# 
# net_income2 <<- sales2*net_income_estimate
# 
# net_income3 <<- sales3*net_income_estimate
# 
# net_income4 <<- sales4*net_income_estimate
# 
# net_income5 <<- sales5*net_income_estimate

net_income1 <<- gross_income1*gross_net_margin_estimate

net_income2 <<- gross_income2*gross_net_margin_estimate

net_income3 <<- gross_income3*gross_net_margin_estimate

net_income4 <<- gross_income4*gross_net_margin_estimate

net_income5 <<- gross_income5*gross_net_margin_estimate

ocf1 <<- net_income1*ni_ocf_estimate

ocf2 <<- net_income2*ni_ocf_estimate

ocf3 <<- net_income3*ni_ocf_estimate

ocf4 <<- net_income4*ni_ocf_estimate

ocf5 <<- net_income5*ni_ocf_estimate

nb1 <<- (`TTM Total Debt`*debt_repayment_estimate)*-1

debt1 <<- `TTM Total Debt`+nb1

nb2 <<- (debt1*debt_repayment_estimate)*-1

debt2 <<- debt1+nb2

nb3 <<- (debt2*debt_repayment_estimate)*-1

debt3 <<- debt2+nb3

nb4 <<- (debt3*debt_repayment_estimate)*-1

debt4 <<- debt3+nb4

nb5 <<- (debt4*debt_repayment_estimate)*-1

debt5 <<- debt4+nb5

interest1 <<- `TTM Total Debt`*interest_rate_estimate

interest2 <<- debt1*interest_rate_estimate

interest3 <<- debt2*interest_rate_estimate

interest4 <<- debt3*interest_rate_estimate

interest5 <<- debt4*interest_rate_estimate

capex1 <<- sales1*capex_estimate

capex2 <<- sales2*capex_estimate

capex3 <<- sales3*capex_estimate

capex4 <<- sales4*capex_estimate

capex5 <<- sales5*capex_estimate

`Risk Free` <<- bond

`TTM Interest %` <<- `TTM Interest Expense`/`TTM Total Debt`

`TTM Interest %` <<- if_else(is.nan(`TTM Interest %`),0,`TTM Interest %`)

`TTM Tax Rate` <<- `TTM Income Tax`/`TTM Pretax Income`

`TTM Tax Rate` <<- if_else(is.nan(`TTM Tax Rate`),0,`TTM Tax Rate`)

`TTM Debt Cost` <<- `TTM Interest %`*(1-`TTM Tax Rate`)

`TTM Debt Cost` <<- if_else(is.nan(`TTM Debt Cost`),0,`TTM Debt Cost`)

`Expected Market Growth` <<- 0.1

Beta <<- as.numeric(Beta)

CAPM <<- `Risk Free`+(Beta*(`Expected Market Growth`- `Risk Free`))

`Debt Cost Weight` <<- if_else(`TTM Total Debt`/(`TTM Enterprise Value`)>1,0.25,`TTM Total Debt`/(`TTM Enterprise Value`))

`Equity Weight` <<- 1-`Debt Cost Weight`

WACC <<- (`Debt Cost Weight`*`TTM Debt Cost`)+(`Equity Weight`*CAPM)  

WACC <<- if_else(is.nan(WACC),0,WACC)

fcff_discount_rate <<- WACC

fcfe_discount_rate <<- CAPM

#discount_rate <<- WACC

#terminal_cagr <<- case_when(`FCF CAGR`>0.2&`LT_QTR Sales Growth`>0&`LT_QTR EBITDA Growth`>0~0.025,`FCF CAGR`>0.1~0.015,`FCF CAGR`>0.05~0.005,`FCF CAGR`>0~0.0025,TRUE~0.001)

terminal_cagr <<- if_else((sales_growth_estimate*0.1)>0.02,0.02,sales_growth_estimate*0.1)

terminal_cagr <<- max(terminal_cagr,0.001)

#terminal_cagr <<- case_when(FCF_Growth>=0.2~0.03,FCF_Growth>=0.1~0.025,FCF_Growth>=0.05~0.015,FCF_Growth>0.0025~0.010,TRUE~0.001)

sales_terminal <<- (sales5*(1+terminal_cagr))/(fcff_discount_rate - terminal_cagr)

gross_margin_terminal <<- sales_terminal*gross_margin_estimate

net_income_terminal <<- gross_margin_terminal*gross_net_margin_estimate

ocf_terminal <<-  net_income_terminal * ni_ocf_estimate

debt_terminal <<- debt5+nb5 

interest_terminal <<- debt_terminal*interest_rate_estimate

capex_terminal <<- sales_terminal*capex_estimate 

nb_terminal <<- (debt_terminal*debt_repayment_estimate)*-1

FCFF <<- round(as.numeric(c(`Net Operating Cash Flow`,`TTM Net Operating Cash Flow`))+
                 (as.numeric(c(`Interest Expense`,`TTM Interest Expense`)))-
                 (as.numeric(c(`Capital Expenditures`,`TTM Capital Expenditures`))*-1),2)

FCFF1 <<- ocf1+(interest1*(1-`TTM Tax Rate`))-capex1

FCFF2 <<- ocf2+(interest2*(1-`TTM Tax Rate`))-capex2

FCFF3 <<- ocf3+(interest3*(1-`TTM Tax Rate`))-capex3

FCFF4 <<- ocf4+(interest4*(1-`TTM Tax Rate`))-capex4

FCFF5 <<- ocf5+(interest5*(1-`TTM Tax Rate`))-capex5

FCFF_Terminal_Multiple <<- 1/(fcff_discount_rate - terminal_cagr)
    
    FCFF_Terminal <<- (FCFF5*FCFF_Terminal_Multiple)

    FCFF_Sum <<- FCFF1+FCFF2+FCFF3+FCFF4+FCFF5+FCFF_Terminal
    
    sales_terminal <<- (sales5*(1+terminal_cagr))/(fcff_discount_rate - terminal_cagr)    
        
    fcff_discount1 <<- (1+fcff_discount_rate)^1 
    
    fcff_discount2 <<- (1+fcff_discount_rate)^2 
    
    fcff_discount3 <<- (1+fcff_discount_rate)^3 
    
    fcff_discount4 <<- (1+fcff_discount_rate)^4 
    
    fcff_discount5 <<- (1+fcff_discount_rate)^5
    
    PFCFF1 <<- FCFF1/fcff_discount1
    
    PFCFF2 <<- FCFF2/fcff_discount2
    
    PFCFF3 <<- FCFF3/fcff_discount3
    
    PFCFF4 <<- FCFF4/fcff_discount4
    
    PFCFF5 <<- FCFF5/fcff_discount5
    
    PFCFF_Terminal <<- FCFF_Terminal/fcff_discount5
    
    PFCFF_Sum <<- PFCFF1+PFCFF2+PFCFF3+PFCFF4+PFCFF5+PFCFF_Terminal
    
    PFCFF_Total <<- PFCFF_Sum
    
    PFCFF_Equity_Total <<- PFCFF_Total+`TTM Cash & Short Term Investments`-`TTM Total Debt`
    
    `Intrinsic Value Price` <<- round(PFCFF_Equity_Total/`TTM Diluted Shares Outstanding`,2)
    
    `Intrinsic Value Gap %` <<- round(((as.numeric(latest_price)/`Intrinsic Value Price`)-1)*100,2)
    
    `TTM Buy Price` <<- round(`Intrinsic Value Price`*0.75,2)

    FCFE <<- round(as.numeric(c(`Net Operating Cash Flow`,`TTM Net Operating Cash Flow`))-
                     (as.numeric(c(`Capital Expenditures`,`TTM Capital Expenditures`))*-1)+
                     (as.numeric(c(`Issuance/Reduction of Debt, Net`,`TTM Issuance/Reduction of Debt, Net`))),2)
    
    FCFE1 <<- ocf1-capex1+nb1
    
    FCFE2 <<- ocf2-capex2+nb2
    
    FCFE3 <<- ocf3-capex3+nb3
    
    FCFE4 <<- ocf4+-capex4+nb4
    
    FCFE5 <<- ocf5+-capex5+nb5
    
    FCFE_Terminal_Multiple <<- 1/(fcfe_discount_rate - terminal_cagr)
    
    FCFE_Terminal <<- (FCFE5*FCFE_Terminal_Multiple)
    
    FCFE_Sum <<- FCFE1+FCFE2+FCFE3+FCFE4+FCFE5+FCFE_Terminal
    
    fcfe_discount1 <<- (1+fcfe_discount_rate)^1 
    
    fcfe_discount2 <<- (1+fcfe_discount_rate)^2 
    
    fcfe_discount3 <<- (1+fcfe_discount_rate)^3 
    
    fcfe_discount4 <<- (1+fcfe_discount_rate)^4 
    
    fcfe_discount5 <<- (1+fcfe_discount_rate)^5
    
    PFCFE1 <<- FCFE1/fcfe_discount1
    
    PFCFE2 <<- FCFE2/fcfe_discount2
    
    PFCFE3 <<- FCFE3/fcfe_discount3
    
    PFCFE4 <<- FCFE4/fcfe_discount4
    
    PFCFE5 <<- FCFE5/fcfe_discount5
    
    PFCFE_Terminal <<- FCFE_Terminal/fcfe_discount5
    
    PFCFE_Sum <<- PFCFE1+PFCFE2+PFCFE3+PFCFE4+PFCFE5+PFCFE_Terminal
    
    PFCFE_Total <<- PFCFE_Sum
    
    `FCFE Intrinsic Value Price` <<- round(PFCFE_Total/`TTM Diluted Shares Outstanding`,2)
    
    `FCFE Buy Price` <<- round(`FCFE Intrinsic Value Price`*0.75,2)

    #QTR Statement------------------------    
    qtr_financial_statement <<- rbind(ins_name,fy,currency,ins_ticker,latest_price,
                                      
                                      `Intrinsic Value Price`,`Intrinsic Value Gap %`,`TTM 10 YR Price Estimate`,`TTM Fair Value Price`,`TTM Fair Value Gap %`,`TTM Buy Price`,latest_price_currency,`Extract Date`,`qtr_Net Income`,`TTM Net Income CAGR`,`P/E`,CAPE,`qtr_Sales/Revenue`,`TTM Sales/Revenue CAGR`,`qtr_Total Equity`,`qtr_Total Debt`,`qtr_Net Operating Cash Flow`,`qtr_Net Investing Cash Flow`,`qtr_Cash Dividends Paid - Total`,`qtr_Repurchase of Common & Preferred Stk.`,`qtr_Net Financing Cash Flow`,`qtr_Free Cash Flow`,`TTM FCF CAGR`,
                                      `Enterprise Value`,`EV/EBITDA`,`qtr_Basic Shares Outstanding`,`qtr_EPS (Basic)`,`qtr_EPS (Basic) Growth`,`TTM EPS CAGR`,`Dividends Per Share`,`Dividends Yield`,
                                      `Price To Sales`,`Price To Book`,`Price to cash flow ratio`,`Debt To Equity`,`Book Value PS`,`TTM Book Value PS CAGR`,`Return On Equity`,`Return On Assets`,`Gross Profit Margin %`,`Net Profit Margin %`,`Operating Profit Margin %`,
                                      `Total Liabilities To Equity`,`Current Ratio`,`Quick Ratio`,`Cash flow coverage ratio`,`Cash flow margin ratio`,`Current liability coverage ratio`,`Cash flow to net income`,
                                      
                                      ins_name,fy,currency,latest_price,
                                      `qtr_Sales/Revenue`,`qtr_Sales Growth`,`qtr_Cost of Goods Sold (COGS) incl. D&A`,`qtr_COGS Growth`,`qtr_COGS excluding D&A`,`qtr_Depreciation & Amortization Expense`,`qtr_Depreciation`,`qtr_Amortization of Intangibles`,`qtr_Gross Income`,`qtr_Gross Income Growth`,`qtr_Gross Profit Margin`,`qtr_SG&A Expense`,`qtr_SGA Growth`,`qtr_Research & Development`,`qtr_Other SG&A`,`qtr_Other Operating Expense`,`qtr_Unusual Expense`,`qtr_EBIT after Unusual Expense`,`qtr_Non Operating Income/Expense`,`qtr_Non-Operating Interest Income`,`qtr_Equity in Affiliates (Pretax)`,`qtr_Interest Expense`,`qtr_Interest Expense Growth`,`qtr_Gross Interest Expense`,`qtr_Interest Capitalized`,`qtr_Pretax Income`,`qtr_Pretax Income Growth`,`qtr_Pretax Margin`,`qtr_Income Tax`,`qtr_Income Tax - Current Domestic`,`qtr_Income Tax - Current Foreign`,`qtr_Income Tax - Deferred Domestic`,`qtr_Income Tax - Deferred Foreign`,`qtr_Income Tax Credits`,`qtr_Equity in Affiliates`,`qtr_Other After Tax Income (Expense)`,`qtr_Consolidated Net Income`,`qtr_Minority Interest Expense`,`qtr_Net Income`,`qtr_Net Income Growth`,`qtr_Net Margin Growth`,`qtr_Extraordinaries & Discontinued Operations`,`qtr_Extra Items & Gain/Loss Sale Of Assets`,`qtr_Cumulative Effect - Accounting Chg`,`qtr_Discontinued Operations`,`qtr_Net Income After Extraordinaries`,`qtr_Preferred Dividends`,`qtr_Net Income Available to Common`,`qtr_EPS (Basic)`,`qtr_EPS (Basic) Growth`,`qtr_Basic Shares Outstanding`,`qtr_EPS (Diluted)`,`qtr_EPS (Diluted) Growth`,`qtr_Diluted Shares Outstanding`,`qtr_EBITDA`,`qtr_EBITDA Growth`,`qtr_EBITDA Margin`,
                                      
                                      ins_name,fy,currency,latest_price,
                                      `qtr_Cash & Short Term Investments`,`qtr_Cash & Short Term Investments Growth`,`qtr_Cash Only`,`qtr_Short-Term Investments`,`qtr_Cash & ST Investments / Total Assets`,`qtr_Total Accounts Receivable`,`qtr_Total Accounts Receivable Growth`,`qtr_Accounts Receivables, Net`,`qtr_Accounts Receivables, Gross`,`qtr_Bad Debt/Doubtful Accounts`,`qtr_Other Receivable`,`qtr_Accounts Receivable Turnover`,`qtr_Inventories`,`qtr_Finished Goods`,`qtr_Work in Progress`,`qtr_Raw Materials`,`qtr_Progress Payments & Other`,`qtr_Other Current Assets`,`qtr_Miscellaneous Current Assets`,`qtr_Total Current Assets`,`qtr_Net Property, Plant & Equipment`,`qtr_Property, Plant & Equipment - Gross`,`qtr_Buildings`,`qtr_Land & Improvements`,`qtr_Computer Software and Equipment`,`qtr_Other Property, Plant & Equipment`,`qtr_Accumulated Depreciation`,`qtr_Total Investments and Advances`,`qtr_Other Long-Term Investments`,`qtr_Long-Term Note Receivables`,`qtr_Intangible Assets`,`qtr_Net Goodwill`,`qtr_Net Other Intangibles`,`qtr_Other Assets`,`qtr_Total Assets`,`qtr_Total Assets Growth`,`qtr_ST Debt & Current Portion LT Debt`,`qtr_Short Term Debt`,`qtr_Current Portion of Long Term Debt`,`qtr_Accounts Payable`,`qtr_Accounts Payable Growth`,`qtr_Income Tax Payable`,`qtr_Other Current Liabilities`,`qtr_Dividends Payable`,`qtr_Accrued Payroll`,`qtr_Miscellaneous Current Liabilities`,`qtr_Total Current Liabilities`,`qtr_Long-Term Debt`,`qtr_Long-Term Debt excl. Capitalized Leases`,`qtr_Non-Convertible Debt`,`qtr_Convertible Debt`,`qtr_Capitalized Lease Obligations`,`qtr_Provision for Risks & Charges`,`qtr_Deferred Taxes`,`qtr_Deferred Taxes - Credits`,`qtr_Deferred Taxes - Debit`,`qtr_Other Liabilities`,`qtr_Other Liabilities (excl. Deferred Income)`,`qtr_Deferred Income`,`qtr_Total Liabilities`,`qtr_Non-Equity Reserves`,`qtr_Total Liabilities / Total Assets`,`qtr_Preferred Stock (Carrying Value)`,`qtr_Redeemable Preferred Stock`,`qtr_Non-Redeemable Preferred Stock`,`qtr_Common Equity (Total)`,`qtr_Common Equity / Total Assets`,`qtr_Common Stock Par/Carry Value`,`qtr_Retained Earnings`,`qtr_ESOP Debt Guarantee`,`qtr_Cumulative Translation Adjustment/Unrealized For. Exch. Gain`,`qtr_Unrealized Gain/Loss Marketable Securities`,`qtr_Revaluation Reserves`,`qtr_Treasury Stock`,`qtr_Total Shareholders' Equity`,`qtr_Total Shareholders' Equity / Total Assets`,`qtr_Accumulated Minority Interest`,`qtr_Total Equity`,`qtr_Liabilities & Shareholders' Equity`,
                                      
                                      ins_name,fy,currency,latest_price,
                                      `qtr_Net Income before Extraordinaries`,`qtr_Net Income Growth`,`qtr_Depreciation, Depletion & Amortization`,`qtr_Depreciation and Depletion`,`qtr_Amortization of Intangible Assets`,`qtr_Deferred Taxes & Investment Tax Credit`,`qtr_Deferred Taxes`,`qtr_Investment Tax Credit`,`qtr_Other Funds`,`qtr_Funds from Operations`,`qtr_Extraordinaries`,`qtr_Changes in Working Capital`,`qtr_Receivables`,`qtr_Accounts Payable`,`qtr_Other Assets/Liabilities`,`qtr_Net Operating Cash Flow`,`qtr_Net Operating Cash Flow Growth`,`qtr_Net Operating Cash Flow / Sales`,`qtr_Capital Expenditures`,`qtr_Capital Expenditures Growth`,`qtr_Capital Expenditures / Sales`,`qtr_Capital Expenditures (Fixed Assets)`,`qtr_Capital Expenditures (Other Assets)`,`qtr_Net Assets from Acquisitions`,`qtr_Sale of Fixed Assets & Businesses`,`qtr_Purchase/Sale of Investments`,`qtr_Purchase of Investments`,`qtr_Sale/Maturity of Investments`,`qtr_Other Uses`,`qtr_Other Sources`,`qtr_Net Investing Cash Flow`,`qtr_Net Investing Cash Flow Growth`,`qtr_Net Investing Cash Flow / Sales`,`qtr_Cash Dividends Paid - Total`,`qtr_Common Dividends`,`qtr_Preferred Dividends`,`qtr_Change in Capital Stock`,`qtr_Repurchase of Common & Preferred Stk.`,`qtr_Sale of Common & Preferred Stock`,`qtr_Proceeds from Stock Options`,`qtr_Other Proceeds from Sale of Stock`,`qtr_Issuance/Reduction of Debt, Net`,`qtr_Change in Current Debt`,`qtr_Change in Long-Term Debt`,`qtr_Issuance of Long-Term Debt`,`qtr_Reduction in Long-Term Debt`,`qtr_Other Funds`,`qtr_Other Uses`,`qtr_Other Sources`,`qtr_Net Financing Cash Flow`,`qtr_Net Financing Cash Flow Growth`,`qtr_Net Financing Cash Flow / Sales`,`qtr_Exchange Rate Effect`,`qtr_Miscellaneous Funds`,`qtr_Net Change in Cash`,`qtr_Free Cash Flow`,`qtr_Free Cash Flow Growth`,`qtr_Free Cash Flow Yield`
                                      
    )
    
    #colnames(qtr_financial_statement) <- c(qtr_header[2],qtr_header[3],qtr_header[4],qtr_header[5],qtr_header[6])
    
    colnames(qtr_financial_statement) <- c(qtr_header[1:5])
    
    
    name <<- paste0(i,"_qtr_financial_statements")
    
    #print(name)
    
    # assign(name,qtr_financial_statement,envir = .GlobalEnv)
    
    # View(qtr_financial_statement)
    
    # return(qtr_financial_statement)
    
    #FY Statement(Incl DCF)------------------------
    
    financial_statement <<- rbind(ins_name,fy,currency,ins_ticker,latest_price,

                                  `Intrinsic Value Price`,`Intrinsic Value Gap %`,`TTM 10 YR Price Estimate`,`TTM Fair Value Price`,`TTM Fair Value Gap %`,`TTM Buy Price`,latest_price_currency,`Extract Date`,`P/E`,CAPE,`EV/EBITDA`,`EPS (Diluted)`,`EPS (Diluted) Growth`,`TTM EPS CAGR`,`FCF PS`,`Current Ratio`,`Quick Ratio`,
                                  `Dividends Per Share`,`Dividends Yield`,`Debt To Equity`,`Book Value PS`,`TTM Book Value PS CAGR`,`Return On Equity`,`Return On Assets`,`Gross Profit Margin %`,`Net Profit Margin %`,`Operating Profit Margin %`,
                                  `Sales/Revenue`,`TTM Sales/Revenue CAGR`,`Net Income`,`TTM Net Income CAGR`,`Free Cash Flow`,`TTM FCF CAGR`,`Total Equity`,`Total Debt`,`Net Operating Cash Flow`,`Net Investing Cash Flow`,`Cash Dividends Paid - Total`,`Repurchase of Common & Preferred Stk.`,`Net Financing Cash Flow`,
                                  `Enterprise Value`,`Diluted Shares Outstanding`,
                                  `Price To Sales`,`Price To Book`,`Price to cash flow ratio`,
                                  `Total Liabilities To Equity`,`Cash flow coverage ratio`,`Cash flow margin ratio`,`Current liability coverage ratio`,`Cash flow to net income`,
                                  
                                  ins_name,fy,currency,latest_price,
                                  `Sales/Revenue`,`Sales Growth`,`Cost of Goods Sold (COGS) incl. D&A`,`COGS Growth`,`COGS excluding D&A`,`Depreciation & Amortization Expense`,`Depreciation`,`Amortization of Intangibles`,`Gross Income`,`Gross Income Growth`,`Gross Profit Margin`,`SG&A Expense`,`SGA Growth`,`Research & Development`,`Other SG&A`,`Other Operating Expense`,`Unusual Expense`,`EBIT after Unusual Expense`,`Non Operating Income/Expense`,`Non-Operating Interest Income`,`Equity in Affiliates (Pretax)`,`Interest Expense`,`Interest Expense Growth`,`Gross Interest Expense`,`Interest Capitalized`,`Pretax Income`,`Pretax Income Growth`,`Pretax Margin`,`Income Tax`,`Income Tax - Current Domestic`,`Income Tax - Current Foreign`,`Income Tax - Deferred Domestic`,`Income Tax - Deferred Foreign`,`Income Tax Credits`,`Equity in Affiliates`,`Other After Tax Income (Expense)`,`Consolidated Net Income`,`Minority Interest Expense`,`Net Income`,`Net Income Growth`,`Net Margin Growth`,`Extraordinaries & Discontinued Operations`,`Extra Items & Gain/Loss Sale Of Assets`,`Cumulative Effect - Accounting Chg`,`Discontinued Operations`,`Net Income After Extraordinaries`,`Preferred Dividends`,`Net Income Available to Common`,`EPS (Basic)`,`EPS (Basic) Growth`,`Basic Shares Outstanding`,`EPS (Diluted)`,`EPS (Diluted) Growth`,`Diluted Shares Outstanding`,`EBITDA`,`EBITDA Growth`,`EBITDA Margin`,
                                  
                                  ins_name,fy,currency,latest_price,
                                  `Cash & Short Term Investments`,`Cash & Short Term Investments Growth`,`Cash Only`,`Short-Term Investments`,`Cash & ST Investments / Total Assets`,`Total Accounts Receivable`,`Total Accounts Receivable Growth`,`Accounts Receivables, Net`,`Accounts Receivables, Gross`,`Bad Debt/Doubtful Accounts`,`Other Receivable`,`Accounts Receivable Turnover`,`Inventories`,`Finished Goods`,`Work in Progress`,`Raw Materials`,`Progress Payments & Other`,`Other Current Assets`,`Miscellaneous Current Assets`,`Total Current Assets`,`Net Property, Plant & Equipment`,`Property, Plant & Equipment - Gross`,`Buildings`,`Land & Improvements`,`Computer Software and Equipment`,`Other Property, Plant & Equipment`,`Accumulated Depreciation`,`Total Investments and Advances`,`Other Long-Term Investments`,`Long-Term Note Receivables`,`Intangible Assets`,`Net Goodwill`,`Net Other Intangibles`,`Other Assets`,`Total Assets`,`Total Assets Growth`,`ST Debt & Current Portion LT Debt`,`Short Term Debt`,`Current Portion of Long Term Debt`,`Accounts Payable`,`Accounts Payable Growth`,`Income Tax Payable`,`Other Current Liabilities`,`Dividends Payable`,`Accrued Payroll`,`Miscellaneous Current Liabilities`,`Total Current Liabilities`,`Long-Term Debt`,`Long-Term Debt excl. Capitalized Leases`,`Non-Convertible Debt`,`Convertible Debt`,`Capitalized Lease Obligations`,`Provision for Risks & Charges`,`Deferred Taxes`,`Deferred Taxes - Credits`,`Deferred Taxes - Debit`,`Other Liabilities`,`Other Liabilities (excl. Deferred Income)`,`Deferred Income`,`Total Liabilities`,`Non-Equity Reserves`,`Total Liabilities / Total Assets`,`Preferred Stock (Carrying Value)`,`Redeemable Preferred Stock`,`Non-Redeemable Preferred Stock`,`Common Equity (Total)`,`Common Equity / Total Assets`,`Common Stock Par/Carry Value`,`Retained Earnings`,`ESOP Debt Guarantee`,`Cumulative Translation Adjustment/Unrealized For. Exch. Gain`,`Unrealized Gain/Loss Marketable Securities`,`Revaluation Reserves`,`Treasury Stock`,`Total Shareholders' Equity`,`Total Shareholders' Equity / Total Assets`,`Accumulated Minority Interest`,`Total Equity`,`Liabilities & Shareholders' Equity`,
                                  
                                  ins_name,fy,currency,latest_price,
                                  `Net Income before Extraordinaries`,`Net Income Growth`,`Depreciation, Depletion & Amortization`,`Depreciation and Depletion`,`Amortization of Intangible Assets`,`Deferred Taxes & Investment Tax Credit`,`Deferred Taxes`,`Investment Tax Credit`,`Other Funds`,`Funds from Operations`,`Extraordinaries`,`Changes in Working Capital`,`Receivables`,`Accounts Payable`,`Other Assets/Liabilities`,`Net Operating Cash Flow`,`Net Operating Cash Flow Growth`,`Net Operating Cash Flow / Sales`,`Capital Expenditures`,`Capital Expenditures Growth`,`Capital Expenditures / Sales`,`Capital Expenditures (Fixed Assets)`,`Capital Expenditures (Other Assets)`,`Net Assets from Acquisitions`,`Sale of Fixed Assets & Businesses`,`Purchase/Sale of Investments`,`Purchase of Investments`,`Sale/Maturity of Investments`,`Other Uses`,`Other Sources`,`Net Investing Cash Flow`,`Net Investing Cash Flow Growth`,`Net Investing Cash Flow / Sales`,`Cash Dividends Paid - Total`,`Common Dividends`,`Preferred Dividends`,`Change in Capital Stock`,`Repurchase of Common & Preferred Stk.`,`Sale of Common & Preferred Stock`,`Proceeds from Stock Options`,`Other Proceeds from Sale of Stock`,`Issuance/Reduction of Debt, Net`,`Change in Current Debt`,`Change in Long-Term Debt`,`Issuance of Long-Term Debt`,`Reduction in Long-Term Debt`,`Other Funds`,`Other Uses`,`Other Sources`,`Net Financing Cash Flow`,`Net Financing Cash Flow Growth`,`Net Financing Cash Flow / Sales`,`Exchange Rate Effect`,`Miscellaneous Funds`,`Net Change in Cash`,`Free Cash Flow`,`Free Cash Flow Growth`,`Free Cash Flow Yield`
                                  
    )


    colnames(financial_statement) <<- c(header)

    name <<- paste0(i,"_financial_statements")

    #print(name)

    # assign(name,financial_statement,envir = .GlobalEnv)
    
    
    
    #TTM Statement------------------------
    
    ttm_financial_statement <<- rbind(
      ins_name,`TTM fy`,`TTM currency`,ins_ticker,latest_price,
      
      `Intrinsic Value Price`,`Intrinsic Value Gap %`,`TTM 10 YR Price Estimate`,`TTM Fair Value Price`,`TTM Fair Value Gap %`,`TTM Buy Price`,
      latest_price_currency,`Extract Date`,`TTM P/E`,CAPE,`TTM EV/EBITDA`,`TTM EPS (Diluted)`,`TTM EPS (Diluted) Growth`,`TTM EPS CAGR`,`TTM FCF PS`,`TTM Current Ratio`,`TTM Quick Ratio`,
      `TTM Dividends Per Share`,`TTM Dividends Yield`,`TTM Debt To Equity`,`TTM Book Value PS`,`TTM Book Value PS CAGR`,`TTM Return On Equity`,`TTM Return On Assets`,`TTM Gross Profit Margin %`,`TTM Net Profit Margin %`,`TTM Operating Profit Margin %`,
      `TTM Sales/Revenue`,`TTM Sales/Revenue CAGR`,`TTM Net Income`,`TTM Net Income CAGR`,`TTM Free Cash Flow`,`TTM FCF CAGR`,`TTM Total Equity`,`TTM Total Debt`,`TTM Net Operating Cash Flow`,`TTM Net Investing Cash Flow`,`TTM Cash Dividends Paid - Total`,
      `TTM Repurchase of Common & Preferred Stk.`,`TTM Net Financing Cash Flow`,
      `TTM Enterprise Value`,`TTM Diluted Shares Outstanding`,
      `TTM Price To Sales`,`TTM Price To Book`,`TTM Price to cash flow ratio`,
      `TTM Total Liabilities To Equity`,`TTM Cash flow coverage ratio`,`TTM Cash flow margin ratio`,`TTM Current liability coverage ratio`,`TTM Cash flow to net income`,
      
      ins_name,`TTM fy`,`TTM currency`,latest_price,
      `TTM Sales/Revenue`,`TTM Sales Growth`,`TTM Cost of Goods Sold (COGS) incl. D&A`,`TTM COGS Growth`,`TTM COGS excluding D&A`,`TTM Depreciation & Amortization Expense`,`TTM Depreciation`,`TTM Amortization of Intangibles`,`TTM Gross Income`,`TTM Gross Income Growth`,`TTM Gross Profit Margin`,`TTM SG&A Expense`,`TTM SGA Growth`,`TTM Research & Development`,`TTM Other SG&A`,`TTM Other Operating Expense`,`TTM Unusual Expense`,`TTM EBIT after Unusual Expense`,`TTM Non Operating Income/Expense`,`TTM Non-Operating Interest Income`,`TTM Equity in Affiliates (Pretax)`,`TTM Interest Expense`,`TTM Interest Expense Growth`,`TTM Gross Interest Expense`,`TTM Interest Capitalized`,`TTM Pretax Income`,`TTM Pretax Income Growth`,`TTM Pretax Margin`,`TTM Income Tax`,`TTM Income Tax - Current Domestic`,`TTM Income Tax - Current Foreign`,`TTM Income Tax - Deferred Domestic`,`TTM Income Tax - Deferred Foreign`,`TTM Income Tax Credits`,`TTM Equity in Affiliates`,`TTM Other After Tax Income (Expense)`,`TTM Consolidated Net Income`,`TTM Minority Interest Expense`,`TTM Net Income`,`TTM Net Income Growth`,`TTM Net Margin Growth`,`TTM Extraordinaries & Discontinued Operations`,`TTM Extra Items & Gain/Loss Sale Of Assets`,`TTM Cumulative Effect - Accounting Chg`,`TTM Discontinued Operations`,`TTM Net Income After Extraordinaries`,`TTM Preferred Dividends`,`TTM Net Income Available to Common`,`TTM EPS (Basic)`,`TTM EPS (Basic) Growth`,`TTM Basic Shares Outstanding`,`TTM EPS (Diluted)`,`TTM EPS (Diluted) Growth`,`TTM Diluted Shares Outstanding`,`TTM EBITDA`,`TTM EBITDA Growth`,`TTM EBITDA Margin`,
      
      ins_name,`TTM fy`,`TTM currency`,latest_price,
      `TTM Cash & Short Term Investments`,`TTM Cash & Short Term Investments Growth`,`TTM Cash Only`,`TTM Short-Term Investments`,`TTM Cash & ST Investments / Total Assets`,`TTM Total Accounts Receivable`,`TTM Total Accounts Receivable Growth`,`TTM Accounts Receivables, Net`,`TTM Accounts Receivables, Gross`,`TTM Bad Debt/Doubtful Accounts`,`TTM Other Receivable`,`TTM Accounts Receivable Turnover`,`TTM Inventories`,`TTM Finished Goods`,`TTM Work in Progress`,`TTM Raw Materials`,`TTM Progress Payments & Other`,`TTM Other Current Assets`,`TTM Miscellaneous Current Assets`,`TTM Total Current Assets`,`TTM Net Property, Plant & Equipment`,`TTM Property, Plant & Equipment - Gross`,`TTM Buildings`,`TTM Land & Improvements`,`TTM Computer Software and Equipment`,`TTM Other Property, Plant & Equipment`,`TTM Accumulated Depreciation`,`TTM Total Investments and Advances`,`TTM Other Long-Term Investments`,`TTM Long-Term Note Receivables`,`TTM Intangible Assets`,`TTM Net Goodwill`,`TTM Net Other Intangibles`,`TTM Other Assets`,`TTM Total Assets`,`TTM Total Assets Growth`,`TTM ST Debt & Current Portion LT Debt`,`TTM Short Term Debt`,`TTM Current Portion of Long Term Debt`,`TTM Accounts Payable`,`TTM Accounts Payable Growth`,`TTM Income Tax Payable`,`TTM Other Current Liabilities`,`TTM Dividends Payable`,`TTM Accrued Payroll`,`TTM Miscellaneous Current Liabilities`,`TTM Total Current Liabilities`,`TTM Long-Term Debt`,`TTM Long-Term Debt excl. Capitalized Leases`,`TTM Non-Convertible Debt`,`TTM Convertible Debt`,`TTM Capitalized Lease Obligations`,`TTM Provision for Risks & Charges`,`TTM Deferred Taxes`,`TTM Deferred Taxes - Credits`,`TTM Deferred Taxes - Debit`,`TTM Other Liabilities`,`TTM Other Liabilities (excl. Deferred Income)`,`TTM Deferred Income`,`TTM Total Liabilities`,`TTM Non-Equity Reserves`,`TTM Total Liabilities / Total Assets`,`TTM Preferred Stock (Carrying Value)`,`TTM Redeemable Preferred Stock`,`TTM Non-Redeemable Preferred Stock`,`TTM Common Equity (Total)`,`TTM Common Equity / Total Assets`,`TTM Common Stock Par/Carry Value`,`TTM Retained Earnings`,`TTM ESOP Debt Guarantee`,`TTM Cumulative Translation Adjustment/Unrealized For. Exch. Gain`,`TTM Unrealized Gain/Loss Marketable Securities`,`TTM Revaluation Reserves`,`TTM Treasury Stock`,`TTM Total Shareholders' Equity`,`TTM Total Shareholders' Equity / Total Assets`,`TTM Accumulated Minority Interest`,`TTM Total Equity`,`TTM Liabilities & Shareholders' Equity`,
      
      ins_name,`TTM fy`,`TTM currency`,latest_price,
      `TTM Net Income before Extraordinaries`,`TTM Net Income Growth`,`TTM Depreciation, Depletion & Amortization`,`TTM Depreciation and Depletion`,`TTM Amortization of Intangible Assets`,`TTM Deferred Taxes & Investment Tax Credit`,`TTM Deferred Taxes`,`TTM Investment Tax Credit`,`TTM Other Funds`,`TTM Funds from Operations`,`TTM Extraordinaries`,`TTM Changes in Working Capital`,`TTM Receivables`,`TTM Accounts Payable`,`TTM Other Assets/Liabilities`,`TTM Net Operating Cash Flow`,`TTM Net Operating Cash Flow Growth`,`TTM Net Operating Cash Flow / Sales`,`TTM Capital Expenditures`,`TTM Capital Expenditures Growth`,`TTM Capital Expenditures / Sales`,`TTM Capital Expenditures (Fixed Assets)`,`TTM Capital Expenditures (Other Assets)`,`TTM Net Assets from Acquisitions`,`TTM Sale of Fixed Assets & Businesses`,`TTM Purchase/Sale of Investments`,`TTM Purchase of Investments`,`TTM Sale/Maturity of Investments`,`TTM Other Uses`,`TTM Other Sources`,`TTM Net Investing Cash Flow`,`TTM Net Investing Cash Flow Growth`,`TTM Net Investing Cash Flow / Sales`,`TTM Cash Dividends Paid - Total`,`TTM Common Dividends`,`TTM Preferred Dividends`,`TTM Change in Capital Stock`,`TTM Repurchase of Common & Preferred Stk.`,`TTM Sale of Common & Preferred Stock`,`TTM Proceeds from Stock Options`,`TTM Other Proceeds from Sale of Stock`,`TTM Issuance/Reduction of Debt, Net`,`TTM Change in Current Debt`,`TTM Change in Long-Term Debt`,`TTM Issuance of Long-Term Debt`,`TTM Reduction in Long-Term Debt`,`TTM Other Funds`,`TTM Other Uses`,`TTM Other Sources`,`TTM Net Financing Cash Flow`,`TTM Net Financing Cash Flow Growth`,`TTM Net Financing Cash Flow / Sales`,`TTM Exchange Rate Effect`,`TTM Miscellaneous Funds`,`TTM Net Change in Cash`,`TTM Free Cash Flow`,`TTM Free Cash Flow Growth`,`TTM Free Cash Flow Yield`
      
    )
    
    colnames(ttm_financial_statement) <<- ttm_financial_statement[2]
    
    name <<- paste0(i,"_ttm_financial_statements")
    
    #print(name)
    
    # assign(name,ttm_financial_statement,envir = .GlobalEnv)
    
    #LT QTR Statement------------------------
    
    LT_QTR_financial_statement <<- rbind(
      ins_name,`LT_QTR fy`,`TTM currency`,ins_ticker,latest_price,
      
      `Intrinsic Value Price`,`Intrinsic Value Gap %`,`TTM 10 YR Price Estimate`,`TTM Fair Value Price`,`TTM Fair Value Gap %`,`TTM Buy Price`,`latest_price_currency`,
      `Extract Date`,`TTM P/E`,`TTM CAPE`,`TTM EV/EBITDA`,`LT_QTR EPS (Diluted)`,`LT_QTR EPS (Diluted) Growth`,`TTM EPS CAGR`,`LT_QTR FCF PS`,`TTM Current Ratio`,`TTM Quick Ratio`,
      `TTM Dividends Per Share`,`TTM Dividends Yield`,`TTM Debt To Equity`,`TTM Book Value PS`,`TTM Book Value PS CAGR`,`TTM Return On Equity`,`TTM Return On Assets`,
      `LT_QTR Gross Profit Margin %`,`LT_QTR Net Profit Margin %`,`LT_QTR Operating Profit Margin %`,`LT_QTR Sales/Revenue`,`TTM Sales/Revenue CAGR`,`LT_QTR Net Income`,
      `TTM Net Income CAGR`,`LT_QTR Free Cash Flow`,`TTM FCF CAGR`,`LT_QTR Total Equity`,`LT_QTR Total Debt`,`LT_QTR Net Operating Cash Flow`,`LT_QTR Net Investing Cash Flow`,
      `LT_QTR Cash Dividends Paid - Total`,`LT_QTR Repurchase of Common & Preferred Stk.`,`LT_QTR Net Financing Cash Flow`,`TTM Enterprise Value`,`LT_QTR Diluted Shares Outstanding`,
      `TTM Price To Sales`,`TTM Price To Book`,`TTM Price to cash flow ratio`,`TTM Total Liabilities To Equity`,`TTM Cash flow coverage ratio`,`TTM Cash flow margin ratio`,
      `TTM Current liability coverage ratio`,`TTM Cash flow to net income`,
      
      ins_name,`LT_QTR fy`,`TTM currency`,latest_price,
      `LT_QTR Sales/Revenue`,`LT_QTR Sales Growth`,`LT_QTR Cost of Goods Sold (COGS) incl. D&A`,`LT_QTR COGS Growth`,`LT_QTR COGS excluding D&A`,`LT_QTR Depreciation & Amortization Expense`,`LT_QTR Depreciation`,`LT_QTR Amortization of Intangibles`,`LT_QTR Gross Income`,`LT_QTR Gross Income Growth`,`LT_QTR Gross Profit Margin`,`LT_QTR SG&A Expense`,`LT_QTR SGA Growth`,`LT_QTR Research & Development`,`LT_QTR Other SG&A`,`LT_QTR Other Operating Expense`,`LT_QTR Unusual Expense`,`LT_QTR EBIT after Unusual Expense`,`LT_QTR Non Operating Income/Expense`,`LT_QTR Non-Operating Interest Income`,`LT_QTR Equity in Affiliates (Pretax)`,`LT_QTR Interest Expense`,`LT_QTR Interest Expense Growth`,`LT_QTR Gross Interest Expense`,`LT_QTR Interest Capitalized`,`LT_QTR Pretax Income`,`LT_QTR Pretax Income Growth`,`LT_QTR Pretax Margin`,`LT_QTR Income Tax`,`LT_QTR Income Tax - Current Domestic`,`LT_QTR Income Tax - Current Foreign`,`LT_QTR Income Tax - Deferred Domestic`,`LT_QTR Income Tax - Deferred Foreign`,`LT_QTR Income Tax Credits`,`LT_QTR Equity in Affiliates`,`LT_QTR Other After Tax Income (Expense)`,`LT_QTR Consolidated Net Income`,`LT_QTR Minority Interest Expense`,`LT_QTR Net Income`,`LT_QTR Net Income Growth`,`LT_QTR Net Margin Growth`,`LT_QTR Extraordinaries & Discontinued Operations`,`LT_QTR Extra Items & Gain/Loss Sale Of Assets`,`LT_QTR Cumulative Effect - Accounting Chg`,`LT_QTR Discontinued Operations`,`LT_QTR Net Income After Extraordinaries`,`LT_QTR Preferred Dividends`,`LT_QTR Net Income Available to Common`,`LT_QTR EPS (Basic)`,`LT_QTR EPS (Basic) Growth`,`LT_QTR Basic Shares Outstanding`,`LT_QTR EPS (Diluted)`,`LT_QTR EPS (Diluted) Growth`,`LT_QTR Diluted Shares Outstanding`,`LT_QTR EBITDA`,`LT_QTR EBITDA Growth`,`LT_QTR EBITDA Margin`,

      ins_name,`LT_QTR fy`,`TTM currency`,latest_price,
      `LT_QTR Cash & Short Term Investments`,`LT_QTR Cash & Short Term Investments Growth`,`LT_QTR Cash Only`,`LT_QTR Short-Term Investments`,`LT_QTR Cash & ST Investments / Total Assets`,`LT_QTR Total Accounts Receivable`,`LT_QTR Total Accounts Receivable Growth`,`LT_QTR Accounts Receivables, Net`,`LT_QTR Accounts Receivables, Gross`,`LT_QTR Bad Debt/Doubtful Accounts`,`LT_QTR Other Receivable`,`LT_QTR Accounts Receivable Turnover`,`LT_QTR Inventories`,`LT_QTR Finished Goods`,`LT_QTR Work in Progress`,`LT_QTR Raw Materials`,`LT_QTR Progress Payments & Other`,`LT_QTR Other Current Assets`,`LT_QTR Miscellaneous Current Assets`,`LT_QTR Total Current Assets`,`LT_QTR Net Property, Plant & Equipment`,`LT_QTR Property, Plant & Equipment - Gross`,`LT_QTR Buildings`,`LT_QTR Land & Improvements`,`LT_QTR Computer Software and Equipment`,`LT_QTR Other Property, Plant & Equipment`,`LT_QTR Accumulated Depreciation`,`LT_QTR Total Investments and Advances`,`LT_QTR Other Long-Term Investments`,`LT_QTR Long-Term Note Receivables`,`LT_QTR Intangible Assets`,`LT_QTR Net Goodwill`,`LT_QTR Net Other Intangibles`,`LT_QTR Other Assets`,`LT_QTR Total Assets`,`LT_QTR Total Assets Growth`,`LT_QTR ST Debt & Current Portion LT Debt`,`LT_QTR Short Term Debt`,`LT_QTR Current Portion of Long Term Debt`,`LT_QTR Accounts Payable`,`LT_QTR Accounts Payable Growth`,`LT_QTR Income Tax Payable`,`LT_QTR Other Current Liabilities`,`LT_QTR Dividends Payable`,`LT_QTR Accrued Payroll`,`LT_QTR Miscellaneous Current Liabilities`,`LT_QTR Total Current Liabilities`,`LT_QTR Long-Term Debt`,`LT_QTR Long-Term Debt excl. Capitalized Leases`,`LT_QTR Non-Convertible Debt`,`LT_QTR Convertible Debt`,`LT_QTR Capitalized Lease Obligations`,`LT_QTR Provision for Risks & Charges`,`LT_QTR Deferred Taxes`,`LT_QTR Deferred Taxes - Credits`,`LT_QTR Deferred Taxes - Debit`,`LT_QTR Other Liabilities`,`LT_QTR Other Liabilities (excl. Deferred Income)`,`LT_QTR Deferred Income`,`LT_QTR Total Liabilities`,`LT_QTR Non-Equity Reserves`,`LT_QTR Total Liabilities / Total Assets`,`LT_QTR Preferred Stock (Carrying Value)`,`LT_QTR Redeemable Preferred Stock`,`LT_QTR Non-Redeemable Preferred Stock`,`LT_QTR Common Equity (Total)`,`LT_QTR Common Equity / Total Assets`,`LT_QTR Common Stock Par/Carry Value`,`LT_QTR Retained Earnings`,`LT_QTR ESOP Debt Guarantee`,`LT_QTR Cumulative Translation Adjustment/Unrealized For. Exch. Gain`,`LT_QTR Unrealized Gain/Loss Marketable Securities`,`LT_QTR Revaluation Reserves`,`LT_QTR Treasury Stock`,`LT_QTR Total Shareholders' Equity`,`LT_QTR Total Shareholders' Equity / Total Assets`,`LT_QTR Accumulated Minority Interest`,`LT_QTR Total Equity`,`LT_QTR Liabilities & Shareholders' Equity`,

      ins_name,`LT_QTR fy`,`TTM currency`,latest_price,
      `LT_QTR Net Income before Extraordinaries`,`LT_QTR Net Income Growth`,`LT_QTR Depreciation, Depletion & Amortization`,`LT_QTR Depreciation and Depletion`,`LT_QTR Amortization of Intangible Assets`,`LT_QTR Deferred Taxes & Investment Tax Credit`,`LT_QTR Deferred Taxes`,`LT_QTR Investment Tax Credit`,`LT_QTR Other Funds`,`LT_QTR Funds from Operations`,`LT_QTR Extraordinaries`,`LT_QTR Changes in Working Capital`,`LT_QTR Receivables`,`LT_QTR Accounts Payable`,`LT_QTR Other Assets/Liabilities`,`LT_QTR Net Operating Cash Flow`,`LT_QTR Net Operating Cash Flow Growth`,`LT_QTR Net Operating Cash Flow / Sales`,`LT_QTR Capital Expenditures`,`LT_QTR Capital Expenditures Growth`,`LT_QTR Capital Expenditures / Sales`,`LT_QTR Capital Expenditures (Fixed Assets)`,`LT_QTR Capital Expenditures (Other Assets)`,`LT_QTR Net Assets from Acquisitions`,`LT_QTR Sale of Fixed Assets & Businesses`,`LT_QTR Purchase/Sale of Investments`,`LT_QTR Purchase of Investments`,`LT_QTR Sale/Maturity of Investments`,`LT_QTR Other Uses`,`LT_QTR Other Sources`,`LT_QTR Net Investing Cash Flow`,`LT_QTR Net Investing Cash Flow Growth`,`LT_QTR Net Investing Cash Flow / Sales`,`LT_QTR Cash Dividends Paid - Total`,`LT_QTR Common Dividends`,`LT_QTR Preferred Dividends`,`LT_QTR Change in Capital Stock`,`LT_QTR Repurchase of Common & Preferred Stk.`,`LT_QTR Sale of Common & Preferred Stock`,`LT_QTR Proceeds from Stock Options`,`LT_QTR Other Proceeds from Sale of Stock`,`LT_QTR Issuance/Reduction of Debt, Net`,`LT_QTR Change in Current Debt`,`LT_QTR Change in Long-Term Debt`,`LT_QTR Issuance of Long-Term Debt`,`LT_QTR Reduction in Long-Term Debt`,`LT_QTR Other Funds`,`LT_QTR Other Uses`,`LT_QTR Other Sources`,`LT_QTR Net Financing Cash Flow`,`LT_QTR Net Financing Cash Flow Growth`,`LT_QTR Net Financing Cash Flow / Sales`,`LT_QTR Exchange Rate Effect`,`LT_QTR Miscellaneous Funds`,`LT_QTR Net Change in Cash`,`LT_QTR Free Cash Flow`,`LT_QTR Free Cash Flow Growth`,`LT_QTR Free Cash Flow Yield`
    )
    
    colnames(LT_QTR_financial_statement) <<- LT_QTR_financial_statement[2]
    
    name <<- paste0(i,"_LT_QTR_financial_statements")
    
    #print(name)
    
    # assign(name,LT_QTR_financial_statement,envir = .GlobalEnv)
    
    ##RBIND--------------
    financial_statement <<- cbind(financial_statement,ttm_financial_statement,LT_QTR_financial_statement)
    
    #View(financial_statement)
    
    #name <<- paste0(i,"_financial_statements")
    
    #print(name)
    
    #assign(name,financial_statement,envir = .GlobalEnv)
    
    #Comparison Grid/DCF Grid------------------------
    
    if (!exists("comparison_grid")){
      comparison_grid <- c()
    }
    
    comparison_grid <- cbind(comparison_grid,ttm_financial_statement)
    
    colnames(comparison_grid) <- comparison_grid[1,]
    
    assign("comparison_grid",comparison_grid,envir = .GlobalEnv)
    
    if (!exists("qtr_comparison_grid")){
      qtr_comparison_grid <- c()
    }
    
    qtr_comparison_grid <- cbind(qtr_comparison_grid,LT_QTR_financial_statement)
    
    colnames(qtr_comparison_grid) <- qtr_comparison_grid[1,]
    
    assign("qtr_comparison_grid",qtr_comparison_grid,envir = .GlobalEnv)
    
    if (!exists("dcf_statement")){
      dcf_statement <- c()
    }
    
    if (!exists("dcf_statement2")){
      dcf_statement2 <- c()
    }
    
    dcf_statement <<- rbind(
      ins_name,`TTM currency`,ins_ticker,
      Sales= financial_statement["Sales/Revenue",][1:6],
      Sales_Growth_Pct= round(c(`Sales Growth`,`TTM Sales Growth`)*100,2),
      Gross_Income=round(as.numeric(financial_statement["Gross Income",][1:6]),2),
      Gross_Income_Pct = round(c(`Gross Income`/`Sales/Revenue`,`TTM Gross Income`/`TTM Sales/Revenue`)*100,2),
      Net_Income=round(as.numeric(financial_statement["Net Income",][1:6]),2),
      Gross_Net_Income_Pct=round(c(`Net Income`/`Gross Income`,`TTM Net Income`/`TTM Gross Income`)*100,2),
      Net_Income_Pct= round(c(`Net Income`/`Sales/Revenue`,`TTM Net Income`/`TTM Sales/Revenue`)*100,2),
      OCF=round(as.numeric(c(`Net Operating Cash Flow`,`TTM Net Operating Cash Flow`)),2),
      OCF_NI_Pct=round(c(`Net Operating Cash Flow`,`TTM Net Operating Cash Flow`)/c(`Net Income`,`TTM Net Income`)*100,2),
      Total_Debt=round(as.numeric(c(`Total Debt`,`TTM Total Debt`)),2),
      Net_Borrowings=round(as.numeric(c(`Issuance/Reduction of Debt, Net`,`TTM Issuance/Reduction of Debt, Net`)),2),
      Debt_Repayment_Pct=round((c(`Issuance/Reduction of Debt, Net`,`TTM Issuance/Reduction of Debt, Net`)*-1)/(c(`Total Debt`,`TTM Total Debt`)+(c(`Issuance/Reduction of Debt, Net`,`TTM Issuance/Reduction of Debt, Net`)*-1))*100,2),
      Interest_Expense=round(as.numeric(financial_statement["Interest Expense",][1:6]),2),
      Interest_Expense_Pct=round(interest_rate*100,2),
      CAPEX=round(as.numeric(financial_statement["Capital Expenditures",][1:6])*-1,2),
      CAPEX_Sales_Pct=round(capex_rate*100,2),
      FCFF=FCFF,
      FCFE=FCFE,
      FCFF_Discount_Rate=1,
      FCFE_Discount_Rate=1,
      PFCFF=FCFF,
      PFCFE=FCFE
    )
    
    dcf_statement2 <<- rbind(
      ins_name,`TTM currency`,ins_ticker,
      Sales= round(c(sales1,sales2,sales3,sales4,sales5,sales_terminal),2),
      Sales_Growth_Pct= round(sales_growth_estimate*100,2),
      Gross_Income=round(c(gross_income1,gross_income2,gross_income3,gross_income4,gross_income5,gross_margin_terminal),2),
      Gross_Income_Pct = round(gross_margin_estimate*100,2),
      Net_Income=round(c(net_income1,net_income2,net_income3,net_income4,net_income5,net_income_terminal),2),
      Gross_Net_Income_Pct=round(gross_net_margin_estimate*100,2),
      Net_Income_Pct= round((c(net_income1,net_income2,net_income3,net_income4,net_income5,net_income_terminal)/c(sales1,sales2,sales3,sales4,sales5,sales_terminal))*100,2),
      OCF=round(c(ocf1,ocf2,ocf3,ocf4,ocf5,ocf_terminal),2),
      OCF_NI_Pct=round(ni_ocf_estimate*100,2),
      Total_Debt=round(c(debt1,debt2,debt3,debt4,debt5,debt_terminal),2),
      Net_Borrowings=round(c(nb1,nb2,nb3,nb4,nb5,nb_terminal),2),
      Debt_Repayment_Pct=round(debt_repayment_estimate*100,2),
      Interest_Expense=round(c(interest1,interest2,interest3,interest4,interest5,interest_terminal),2),
      Interest_Expense_Pct=round(interest_rate_estimate*100,2),
      CAPEX=round(c(capex1,capex2,capex3,capex4,capex5,capex_terminal),2),
      CAPEX_Sales_Pct=round(capex_estimate*100,2),
      FCFF=round(c(FCFF1,FCFF2,FCFF3,FCFF4,FCFF5,FCFF_Terminal),2),
      FCFE=round(c(FCFE1,FCFE2,FCFE3,FCFE4,FCFE5,FCFE_Terminal),2),
      FCFF_Discount_Rate=round(c(fcff_discount1,fcff_discount2,fcff_discount3,fcff_discount4,fcff_discount5,fcff_discount5),2),
      FCFE_Discount_Rate=round(c(fcfe_discount1,fcfe_discount2,fcfe_discount3,fcfe_discount4,fcfe_discount5,fcfe_discount5),2),
      PFCFF=round(c(PFCFF1,PFCFF2,PFCFF3,PFCFF4,PFCFF5,PFCFF_Terminal),2),
      PFCFE=round(c(PFCFE1,PFCFE2,PFCFE3,PFCFE4,PFCFE5,PFCFE_Terminal),2)
    )
    
    dcf_statement2_names <<- c("Y1","Y2","Y3","Y4","Y5","Terminal")
    
    #colnames(dcf_statement2) <<- dcf_statement2_names
    
    colnames(dcf_statement2) <<- c("Y1","Y2","Y3","Y4","Y5","Terminal")
    
    dcf_statement3 <<- cbind(dcf_statement,dcf_statement2)
    
    dcf_statement3 <<- rbind(dcf_statement3,
                           Total_PFCFF= round(PFCFF_Total,2),
                           Total_PFCFF_Equity=round(PFCFF_Equity_Total,2),
                           Total_PFCFE= round(PFCFE_Total,2),
                           Shares=c(`Diluted Shares Outstanding`,`TTM Diluted Shares Outstanding`,`TTM Diluted Shares Outstanding`,`TTM Diluted Shares Outstanding`,`TTM Diluted Shares Outstanding`,`TTM Diluted Shares Outstanding`,`TTM Diluted Shares Outstanding`,`TTM Diluted Shares Outstanding`),
                           FCFF_Intrinsic_Value= `Intrinsic Value Price`,
                           FCFE_Intrinsic_Value= `FCFE Intrinsic Value Price`,
                           Latest_Price= latest_price,
                           Latest_Price_Currency= latest_price_currency,
                           FCFF_Buy_Price = `TTM Buy Price`,
                           FCFE_Buy_Price = `FCFE Buy Price`,
                           Beta=Beta,
                           Equity_Cost = round(CAPM*100,2),
                           Debt_Cost=round(`TTM Debt Cost`*100,2),
                           Capital_Cost=round(WACC*100,2)
                           
    )
    
    dcf_statement4 <<- rbind(dcf_statement3,
                             EPS=round(as.numeric(dcf_statement3["Net_Income",])/as.numeric(dcf_statement3["Shares",]),2)
    )
    
    assign("dcf_statement",dcf_statement4,envir = .GlobalEnv)
    
    name <<- paste0(i,"_dcf_statement")
    
    #print(name)
    
    # assign(name,dcf_statement4,envir = .GlobalEnv)
    
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
  End_Time <<- Sys.time()
  
  print(End_Time-Start_Time)
  View(financial_statement)
  View(comparison_grid)
  
}

get_financials()

write.csv(dcf_statement,"LRCX_dcf.csv",row.names = TRUE)

write.csv(financial_statement,"BATS_Financial.csv",row.names = TRUE,na = "")

##Manual Comparison Grid
#######################################################################

#comparison_grid <- NULL

comparison_grid_backup <- comparison_grid

if (!exists("comparison_grid")){
  comparison_grid <- c()} else {comparison_grid <- NULL}

for (j in tickers){
  tryCatch({
    
    print(j)
    
    #lapply(dfarray, function(j) hist(j))
    
    #obj <- get(j)
    symb <- j  
    ttm <- eval(parse(text = paste0(i,"_ttm_financial_statements")))
    #adjusted <- paste0(i,".Adjusted")
    #print(adjusted)
    #data <- get(adjusted)
    #temp <- data.frame(ttm,stringsAsFactors = F)
    temp <- ttm
    # temp$Date <- row.names(temp)
    print(temp)
    #colnames(temp) <- c("Ticker","Adjusted_Price","Date")
    comparison_grid <- cbind(comparison_grid,temp)
    colnames(comparison_grid) <- comparison_grid[1,]
    
    #print(temp)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
}
colnames(comparison_grid) <- comparison_grid[1,]
View(comparison_grid)

#prices <- prices %>% distinct()


######################################################################

write.csv(dcf_statement,"SNA_DCF.csv",row.names = TRUE,na = "")

# comparison_grid <- cbind(ttm_financial_statement)

# comparison_grid <- comparison_grid[,1]

viz_data <- data.frame(t(comparison_grid)) %>% distinct(ins_ticker,.keep_all = T)

viz_data_transposed <- data.frame(t(comparison_grid)) %>% distinct(ins_ticker,.keep_all = T)

viz_data_qtr_transposed <- data.frame(t(qtr_comparison_grid)) %>% distinct(ins_ticker,.keep_all = T)

fwrite(viz_data,"sp500_data.csv")

fwrite(viz_data_transposed,"sp500_data_transposed.csv")

fwrite(viz_data_qtr_transposed,"vertical_comparison_grid_qtr.csv")

write.csv(comparison_grid,"horizontal_comparison_grid.csv",row.names = TRUE)


quality_list <- viz_data %>%
  mutate(net_debt=as.numeric(TTM.Total.Debt)-as.numeric(TTM.Cash...Short.Term.Investments),
         net_debt_to_equity=net_debt/as.numeric(TTM.Total.Shareholders..Equity),
         invested_capital=net_debt+as.numeric(TTM.Total.Shareholders..Equity),
         roic=as.numeric(TTM.Net.Income)/invested_capital,
         sales_to_invested_capital=as.numeric(TTM.Sales.Revenue)/invested_capital
         ) %>% 
  filter(TTM.Gross.Profit.Margin>=0.3,TTM.Net.Profit.Margin..>=0.1,
         net_debt_to_equity>=0&net_debt_to_equity<=1.5,TTM.Return.On.Equity>=0.1,
         TTM.Current.Ratio>=1.5,TTM.Free.Cash.Flow>0,TTM.Sales.Revenue.CAGR>0,
         TTM.Cash.Dividends.Paid...Total<0|TTM.Repurchase.of.Common...Preferred.Stk.<0,
         !is.na(as.numeric(TTM.Net.Profit.Margin..)),!is.nan(as.numeric(TTM.Net.Profit.Margin..)),
         !is.infinite(as.numeric(TTM.Net.Profit.Margin..)),TTM.Sales.Growth>0,roic>=0.1,sales_to_invested_capital>=0.5
         ) %>%
  arrange(as.numeric(TTM.Net.Profit.Margin..),as.numeric(Intrinsic.Value.Gap..)) %>% 
  select(ins_name,ins_ticker,latest_price_currency,latest_price,Intrinsic.Value.Price,TTM.currency,TTM.Sales.Revenue,TTM.Sales.Growth,TTM.Gross.Income,TTM.Gross.Income.Growth,
         TTM.Gross.Profit.Margin,TTM.Net.Income,TTM.Net.Income.Growth,TTM.Net.Profit.Margin..,everything())

quality_list %>% View()

quality_list %>% 
  select(sales_to_invested_capital) %>% 
  ggplot(aes(sales_to_invested_capital))+
  geom_histogram()


get_financials()

plot(dcf_statement["FCFF",][1:11])



# fwrite(sp_companies,"s&p companies.csv")

# input_ticker <<- dlgInput("Enter ticker")$res %>% str_split(.,",") %>% unlist()

# comparison_grid <- NULL

# comparison_grid_tech_sector <- comparison_grid

semiconductor_df <- viz_data %>% 
  filter(ins_ticker %in% c("INTC","AMD","NVDA","QCOM","MU","TSM","AVGO","TXN","SWKS","QRVO","NXPI","SYNA","HIMX") ) %>%
  t()

semiconductor_df <- comparison_grid

write.csv(semiconductor_df,"semiconductor_data.csv",row.names = TRUE)

write.csv(intc_financial_statements,"intc.csv",row.names = TRUE)

plot(financial_statement["Free Cash Flow",])

###---------------------
tsdata <- ts(as.numeric(financial_statement["Sales/Revenue",][1:6]),start=1,frequency=1)

plot(tsdata)

autoarima1 <- auto.arima(tsdata)

linearfcst <- tslm(tsdata~trend)

forecast1 <- forecast(autoarima1,h=6)

forecast2 <- forecast(linearfcst,h=6)

plot(forecast1)

plot(forecast2)

fore_data <- ts(c(financial_statement["Sales/Revenue",][1],financial_statement["Sales/Revenue",][2],
                  financial_statement["Sales/Revenue",][3],financial_statement["Sales/Revenue",][4],
                  financial_statement["Sales/Revenue",][5]),start=1,frequency=1)

plot(fore_data)
hw <- HoltWinters(fore_data,beta = FALSE, gamma = FALSE)

plot(hw)


# test_page <<- read_html("https://www.marketwatch.com/investing/stock/FB")
# 
# latest_price_1day_chg <<- test_page %>% html_nodes(".change--percent--q bg-quote") %>% html_text()
# 
# latest_price_5day_chg <<- test_page %>% html_nodes(".table__row:nth-child(1) .value") %>% html_text()
# 
# latest_price_1mnth_chg <<- test_page %>% html_nodes(".table__row:nth-child(2) .value") %>% html_text()
# 
# latest_price_3mnth_chg <<- test_page %>% html_nodes(".table__row:nth-child(3) .value") %>% html_text()
# 
# latest_price_ytd_chg <<- test_page %>% html_nodes(".table__row:nth-child(4) .value") %>% html_text()
# 
# latest_price_1yr_chg <<- test_page %>% html_nodes(".table__row:nth-child(5) .value") %>% html_text()

###-------------------

get("Sales/Revenue")
test <- as.matrix(rbind(mget(income_rows_x)))

View(cash_rows)

comparison_grid_backup <- comparison_grid

CERN_dcf_statement %>% as.tibble(rownames = "rowname")



write.csv(GRMN_financial_statements,"financial_statement.csv",row.names = TRUE)

write.csv(comparison_grid,"comparison_grid.csv",row.names = TRUE)

write.csv(STMP_qtr_financial_statements,"STMP_qtr_financials.csv",row.names = TRUE)


fwrite(matrix(c(row.names(financial_statement),
          row.names(ttm_financial_statement),
          row.names(LT_QTR_financial_statement))),"names.csv")

income_page %>% 
  html_nodes(".u-semi") %>% 
  html_text() %>% 
  gsub(",","",.) %>% 
  str_extract(.,"[a-z]|\\$|\\€")

income_page %>% 
  html_nodes(".u-semi") %>% 
  html_text() %>% 
  #gsub(",","",.) %>% 
  str_extract(.,"\\d")


#keep <- askYesNo("Keep Comparison Grid", default = TRUE,prompts = getOption("askYesNo", gettext(c("Yes", "No"))))
