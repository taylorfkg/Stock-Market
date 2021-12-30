library(rvest)
library(dplyr)
library(stringr)
library(XML)
library(urltools)
library(RCurl)
library(V8)
library(curl)
library(data.table)
library(xlsx)

# main_page <- read_html("https://uk.investing.com/indices/uk-indices?&primarySectors=on")

main_page <- read_html("https://www.investing.com/indices/uk-indices?&primarySectors=on&otherIndices=on")
 
# sector_table <- main_page %>% html_node("table") %>% html_table(fill = T)
# 
# View(sector_table[[4]])

all_links <- main_page %>% html_nodes("a") %>% html_attr("href")

# target_links_raw <- all_links[329:365]

target_links_raw <- all_links[343:379]

target_links <- paste0("https://uk.investing.com",target_links_raw)

sector_df <- as.data.frame(target_links)

title_node <- ".relativeAttr"

const_node <- "#quotes_summary_current_data div~ div+ div a"

change_node <- ".inlineblock:nth-child(7) .bold"

# sector_page <- read_html("https://uk.investing.com/indices/ftse-350-health-care-eq.---serv.")
# 
# sector_name <- sector_page %>% html_node(title_node) %>% html_text()
# 
# sector_constituents <- sector_page %>% html_node(const_node) %>% html_text()
# 
# sector_growth <- sector_page %>% html_node(change_node) %>% html_text()
# 
# sector_name
# sector_constituents
# sector_growth

sector_text_catcher <-  data.frame(i=character(),sector_name=character(),sector_constituents=character(),sector_growth=character(),stringsAsFactors = F)

start_time=Sys.time()

for (i in sector_df$target_links) {
  tryCatch({
    Sys.sleep(sample(1:10, 1))
    sector_page <- read_html(i)
    sector_name <- sector_page %>% html_node(title_node) %>% html_text()
    sector_constituents <- sector_page %>% html_node(const_node) %>% html_text()
    sector_growth <- sector_page %>% html_node(change_node) %>% html_text()
    
    sector_text_catcher_temp <- as.data.frame(cbind(i,sector_name,sector_constituents,sector_growth))
    
    sector_text_catcher <- rbind(sector_text_catcher,sector_text_catcher_temp)
    
    print(sector_text_catcher_temp)
    cat("*")
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

End_Time=Sys.time()

Scrape_Duration = End_Time - start_time

Scrape_Duration



components <- data.frame(sector_df[1:36,],stringsAsFactors = F)

components$target_link <- paste0(components$sector_df.1.36...,"-components")

start_time=Sys.time()

for (i in components$target_link) {
  tryCatch({
    Sys.sleep(sample(1:10, 1))

component_page <- read_html(i)

component_table <- component_page %>% html_node("table") %>% html_table()


# component_links <- data.frame(component_links) %>% filter(str_detect(component_links,"/equities/"))

# component_table <- component_table[,2:9]

component_title <- component_page %>% html_node(title_node) %>% html_text()

tmp_table <- as.data.frame(cbind(component_table,component_title))

tmp_links <- data.frame(link=read_html(i) %>% html_nodes("a") %>% html_attr("href"))

constituents_table <- rbind(constituents_table,tmp_table)

component_links <- rbind(component_links,tmp_links)

print(tmp_table)
cat("*")
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

End_Time=Sys.time()

Scrape_Duration = End_Time - start_time

Scrape_Duration

constituents_table <- constituents_table %>% distinct()

fwrite(sector_text_catcher,"ftse_350_sector_performance.csv")

fwrite(constituents_table,"ftse_350_constituents.csv")


prev_close_node <- ".inlineblock:nth-child(1) .float_lang_base_2"

open_node <- ".inlineblock:nth-child(4) .bold"

volume_node <- ".inlineblock:nth-child(7) .bold"

avg_volume_node <- ".inlineblock:nth-child(10) .bold"

yr_change_node <- ".inlineblock:nth-child(13) .bold"

day_range_node <- ".inlineblock:nth-child(2) .bold"

yr_wk_range_node <- ".inlineblock:nth-child(5) .bold"

market_cap_node <- ".inlineblock:nth-child(8) .bold"

pe_ratio_node <- ".inlineblock:nth-child(11) .bold"

shares_node <- ".inlineblock:nth-child(14) .bold"

rev_node <- ".inlineblock:nth-child(3) .bold"

eps_node <- ".inlineblock:nth-child(6) .bold"

dividend_node <- ".inlineblock:nth-child(9) .bold"

beta_node <- ".inlineblock:nth-child(12) .bold"

earnings_date_node <- ".inlineblock a"

profile_node <- "#profile-fullStory-showhide"

industry_node <- ".companyProfileHeader div:nth-child(1) a"

sector_node <- ".companyProfileHeader div+ div a"

employee_node <- ".companyProfileHeader div:nth-child(3) .bold"

index_node <- "#DropdownBtn .bold"

isin_node <- "div:nth-child(3) .elp"

sedol_node <- "div:nth-child(4) .elp"

company_page <- read_html("https://uk.investing.com/equities/glaxosmithkline")

company_page %>% html_table()

component_links <- component_links %>% distinct()%>% filter(str_detect(link,"/equities/"))

component_links <- component_links[15:338,]

component_links$target_link <- paste0("https://uk.investing.com",component_links$link)

company_text_catcher <-  data.frame(i=character(),company_name=character(),prev_close=character(),open=character(),
                                    volume=character(),avg_volume=character(),yr_change=character(),
                                    # day_range=character(),
                                    yr_wk_range=character(),market_cap=character(),pe_ratio=character(),shares=character(),
                                    rev=character(),eps=character(),dividend=character(),beta=character(),
                                    earnings_date=character(),profile=character(),industry=character(),sector=character(),
                                    employee=character(),index=character(),isin=character(),sedol=character(),
                                    stringsAsFactors = F)

start_time=Sys.time()

for (i in component_links$target_link) {
  tryCatch({
    Sys.sleep(sample(1:10, 1))
    
    company_page <- read_html(i)
    

company_name <- company_page %>% html_nodes(title_node) %>% html_text()
prev_close <- company_page %>% html_nodes(prev_close_node) %>% html_text()
open <- company_page %>% html_nodes(open_node) %>% html_text()
volume <- company_page %>% html_nodes(volume_node) %>% html_text()
avg_volume <- company_page %>% html_nodes(avg_volume_node) %>% html_text()
yr_change <- company_page %>% html_nodes(yr_change_node) %>% html_text()
# day_range <- company_page %>% html_nodes(day_range_node) %>% html_text()
yr_wk_range <- company_page %>% html_nodes(yr_wk_range_node) %>% html_text()
market_cap <- company_page %>% html_nodes(market_cap_node) %>% html_text()
pe_ratio <- company_page %>% html_nodes(pe_ratio_node) %>% html_text()
shares <- company_page %>% html_nodes(shares_node) %>% html_text()
rev <- company_page %>% html_nodes(rev_node) %>% html_text()
eps <- company_page %>% html_nodes(eps_node) %>% html_text()
dividend <- company_page %>% html_nodes(dividend_node) %>% html_text()
beta <- company_page %>% html_nodes(beta_node) %>% html_text()
earnings_date <- company_page %>% html_nodes(earnings_date_node) %>% html_text()
profile <- company_page %>% html_nodes(profile_node) %>% html_text()
industry <- company_page %>% html_nodes(industry_node) %>% html_text()
sector <- company_page %>% html_nodes(sector_node) %>% html_text()
employee <- company_page %>% html_nodes(employee_node) %>% html_text()
index <- company_page %>% html_nodes(index_node) %>% html_text()
isin <- company_page %>% html_nodes(isin_node) %>% html_text()
sedol <- company_page %>% html_nodes(sedol_node) %>% html_text()



company_text_catcher_tmp <-  data.frame(cbind(i,company_name=company_name[1],prev_close=prev_close[1],open,
                                    volume,avg_volume,yr_change,
                                    # day_range[6],
                                    yr_wk_range,market_cap,pe_ratio,shares,
                                    rev,eps,dividend,beta,
                                    earnings_date=earnings_date[36],profile,industry,sector,
                                    employee,index,isin,sedol),
                                    stringsAsFactors = F)

company_text_catcher <- rbind(company_text_catcher,company_text_catcher_tmp)

print(company_text_catcher_tmp)
cat("*")
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

End_Time=Sys.time()

Scrape_Duration = End_Time - start_time

Scrape_Duration

fwrite(company_text_catcher,"company_stocks_performance.csv")


