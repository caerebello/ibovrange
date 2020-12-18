if(require(tidyverse)) library(tidyverse)
if(require(lubridate)) library(lubridate)
if(require(rvest)) library(rvest)

#tidy ibov
ibov <- as.data.frame(read_csv('data/ibov.csv'))
ibov <- ibov %>% rename(date = Data, close = Último, open = Abertura, high = Máxima, low = Mínima, volM = Vol., var = `Var%`)
ibov$date <- dmy(ibov$date)
ibov <- ibov %>% mutate(year = year(date), month = month(date), month_day = day(date), week_day = ifelse(wday(date) == 1, 2, 
      ifelse(wday(date) == 7, 6, wday(date))), quarter = quarter(date), close = close*1000, 
     open = open*1000, high = high*1000, low = low*1000, var = as.numeric(var %>% str_replace(",", ".") %>% str_replace("%", "")), 
     volM = as.numeric(volM %>% str_replace(",", ".") %>% str_replace("M", "")))

#tidy interest(DI & SELIC)
interest <- as.data.frame(read_csv('data/interest.csv'))
interest$date <- dmy(interest$date)

#tidy FED interest rate
fed <- as.data.frame(read_csv('data/fed-rates.csv'))

#tidy DAX 30
dax <- as.data.frame(read_csv('data/dax.csv'))
dax_lates <- as.data.frame(read_csv('data/dax-lates.csv'))
dax <- bind_rows(dax, dax_lates)
dax <- dax %>% rename(date = Data, dax = Abertura) %>% select(date, dax)
dax$date <- dmy(dax$date)

#tidy Euronext 100
euronext <- as.data.frame(read_csv('data/euronext.csv'))
euronext_lates <- as.data.frame(read_csv('data/euronext-lates.csv'))
euronext <- bind_rows(euronext, euronext_lates)
euronext <- euronext %>% rename(date = Data, euronext = Abertura) %>% select(date, euronext)
euronext$date <- dmy(euronext$date)

#tidy Dow-Jones
dj <- as.data.frame(read_csv('data/dow-jones.csv'))
dj <- dj %>% rename(date = Data, dj = Abertura) %>% select(date, dj)
dj$date <- dmy(dj$date)

#tidy LSE
lse <- as.data.frame(read_csv('data/lse.csv'))
lse <- lse %>% rename(date = Data, lse = Abertura) %>% select(date, lse)
lse$date <- dmy(lse$date)

#tidy Nasdaq
nasdaq <- as.data.frame(read_csv('data/nasdaq.csv'))
nasdaq_lates <- as.data.frame(read_csv('data/nasdaq-lates.csv'))
nasdaq <- bind_rows(nasdaq, nasdaq_lates)
nasdaq <- nasdaq %>% rename(date = Data, nasdaq = Abertura) %>% select(date, nasdaq)
nasdaq$date <- dmy(nasdaq$date)

#tidy S&P 500
sp500 <- as.data.frame(read_csv('data/sp500.csv'))
sp500_lates <- as.data.frame(read_csv('data/sp500-lates.csv'))
sp500 <- bind_rows(sp500, sp500_lates)
sp500 <- sp500 %>% rename(date = Data, sp500 = Abertura) %>% select(date, sp500)
sp500$date <- dmy(sp500$date)

#tidy Nikkei
nikkei <- as.data.frame(read_csv('data/nikkei.csv'))
nikkei <- nikkei %>% rename(date = Data, nikkei = Último) %>% select(date, nikkei)
nikkei$date <- dmy(nikkei$date)

#tidy SSE 100
sse <- as.data.frame(read_csv('data/sse.csv'))
sse <- sse %>% rename(date = Data, sse = Último) %>% select(date, sse)
sse$date <- dmy(sse$date)

#tidy USD BRL exchange rate
usd <- as.data.frame(read_csv('data/usd.csv'))
usd_lates <- as.data.frame(read_csv('data/usd-lates.csv'))
usd <- bind_rows(usd, usd_lates)
usd <- usd %>% rename(date = Data, usd = Abertura) %>% select(date, usd)
usd$usd <- usd$usd/10000
usd$date <- dmy(usd$date)

#tidy EUR BRL exchange rate
eur <- as.data.frame(read_csv('data/eur.csv'))
eur_lates <- as.data.frame(read_csv('data/eur-lates.csv'))
eur <- bind_rows(eur, eur_lates)
eur <- eur %>% rename(date = Data, eur = Abertura) %>% select(date, eur)
eur$eur <- eur$eur/10000
eur$date <- dmy(eur$date)

#tidy WTI Future Oil
wti <- as.data.frame(read_csv('data/wti.csv'))
wti_lates <- as.data.frame(read_csv('data/wti-lates.csv'))
wti <- bind_rows(wti, wti_lates)
wti <- wti %>% rename(date = Data, wti = Abertura) %>% select(date, wti)
wti$wti <- sqrt((wti$wti/100)^2)
wti$date <- dmy(wti$date)

#tidy iron ore
iron <- as.data.frame(read_csv('data/iron.csv'))
iron <- iron %>% rename(date = Data, iron = Abertura) %>% select(date, iron)
iron$iron <- iron$iron/100
iron$date <- dmy(iron$date)

#tidy ieci (industrial entrepreneur confidence index) - Fonte CNI
ieci <- as.data.frame(read_csv('data/icei.csv'))
ieci <- ieci %>% rename(ieci = icei) %>% select(date, ieci)
ieci <- ieci %>% mutate(year = as.numeric(str_extract(str_extract(date, "[.]\\d{4}[;]"), "\\d{4}"))) %>%
  mutate(quarter = as.numeric(str_extract(str_extract(date, "\\d{1}[?]"), "\\d{1}"))) %>% select(-date)

#tidy cci (consummer confidence index) - Fonte Fecomercio
cci <- as.data.frame(read_csv('data/icc.csv'))
cci <- cci %>% mutate(year = as.numeric(str_extract(str_extract(date, "[/]\\d{4}[;]"), "\\d{4}"))) %>%
  mutate(month = as.numeric(str_extract(str_extract(date, "\\d{2}[/]"), "\\d{2}")), 
         cci = as.numeric(str_extract(str_extract(date, "[;]\\d{2,3}"), "\\d{2,3}"))) %>% mutate(cci = cci + decimal/100) %>% 
  select(-date, - decimal)

#tidy economic indicators - Fonte IBGE (INPC, IPCA, industry activity)
ei <- as.data.frame(read_csv('data/indicadores.csv'))
ei <- ei %>% mutate(year = as.numeric(str_extract(str_extract(date, "[/]\\d{4}[;]"), "\\d{4}")), 
         month = as.numeric(str_extract(str_extract(date, "\\d{2}[/]"), "\\d{2}")), 
         ipca = as.numeric(str_extract(str_extract(INPC, "[;](\\d{1,2}|[-]\\d{1,2})"), "(\\d{1,2}|[-]\\d{1,2})")) + 
           as.numeric(str_extract(str_extract(IPCA, "\\d{2}"), "\\d{2}"))/100) %>%
  mutate(ipca = ifelse(str_detect(INPC, "[;][-]\\d{1,2}"), ipca * (-1), ipca), 
         industry_activity = as.numeric(str_extract(str_extract(`Índice de Condições Econômicas Atuais`, "[;]\\d{1,3}"), "\\d{1,3}")) + 
              `Indicadores da produção (2012=100) - Geral`/10) %>% select(year, month,  ipca, industry_activity)
ipca_acc <- as.data.frame(read_csv('data/ipca-acc.csv'))

#unemployment rate - Fonte IBGE
unemployment <- as.data.frame(read_csv('data/unemployment.csv'))
unemployment <- unemployment %>% mutate(year = as.numeric(str_extract(str_extract(date, "\\d{4}[.]"), "\\d{4}")), 
     month = as.numeric(str_extract(str_extract(date, "[.]\\d{2}"), "\\d{2}")), 
     unemployment = desocupação) %>% select(year, month, unemployment)
unemployment_lates <- as.data.frame(read_csv('data/unemployment-lates.csv'))
unemployment_lates <- unemployment_lates %>% mutate(year = as.numeric(str_extract(str_extract(date, "[/]\\d{4}[;]"), "\\d{4}")), 
    month = as.numeric(str_extract(str_extract(date, "\\d{2}[/]"), "\\d{2}")), 
    unemployment = as.numeric(str_extract(str_extract(date, "[;]\\d{1,2}"), "\\d{1,2}")) + desecupação/10) %>% 
  select(year, month, unemployment)
unemployment <- bind_rows(unemployment, unemployment_lates)

#IGPM
url <- paste0("https://www.indiceseindicadores.com.br/igp-m/")
igpm <- read_html(url) %>% html_nodes("table")
igpm <- igpm[[1]] %>% html_table %>% t()
cnames <- igpm[1,]
rnames <- igpm[,1]
igpm <- igpm[-1,]
igpm <- igpm[,-1]
colnames(igpm) <- cnames[-1]
rownames(igpm) <- rnames[-1]
igpm <- gather(as.data.frame(igpm)) 
igpm <- igpm %>% rename(year = key, igpm = value) %>% mutate(year = as.numeric(year)) %>% filter(year >= 2000) %>% group_by(year) %>% 
  mutate(month = c(1:13), igpm = as.numeric(igpm %>% str_replace(",", "."))) %>% filter(month != 13)
igpm_acc <- as.data.frame(read_csv('data/igpm-acc.csv'))

#join tables
ibov <- ibov %>% left_join(interest, by = "date") %>% left_join(fed, by = "date") %>% left_join(dax, by = "date") %>% 
  left_join(euronext, by = "date") %>% left_join(dj, by = "date") %>% left_join(lse, by = "date") %>% left_join(nasdaq, by = "date") %>%
  left_join(sp500, by = "date") %>% left_join(nikkei, by = "date") %>% left_join(sse, by = "date") %>% left_join(usd, by = "date") %>%
  left_join(eur, by = "date") %>% left_join(wti, by = "date") %>% left_join(iron, by = "date") %>% left_join(ieci, by = c("year", "quarter")) %>%
  left_join(cci, by = c("year", "month")) %>% left_join(ei, by = c("year", "month")) %>% left_join(unemployment, by = c("year", "month")) %>% 
  left_join(igpm, by = c("year", "month")) %>% left_join(igpm_acc, by = "date") %>% left_join(ipca_acc, by = "date")
      
#save data set as Rda file
save(ibov, file="rdas/ibov.rda")

close <- ibov %>% select(date, close) %>% mutate(type = "Ibovespa", value = scale(close)) %>% select(-close)
dax <- ibov %>% select(date, dax) %>% mutate(type = "DAX", value = scale(dax)) %>% select(-dax)
euronext <- ibov %>% select(date, euronext) %>% mutate(type = "Euronext", value = scale(euronext)) %>% select(-euronext)
dj <- ibov %>% select(date, dj) %>% mutate(type = "Dow-Jones", value = scale(dj)) %>% select(-dj)
lse <- ibov %>% select(date, lse) %>% mutate(type = "LSE", value = scale(lse)) %>% select(-lse)
nasdaq <- ibov %>% select(date, nasdaq) %>% mutate(type = "Nasdaq", value = scale(nasdaq)) %>% select(-nasdaq)
sp500 <- ibov %>% select(date, sp500) %>% mutate(type = "S&P 500", value = scale(sp500)) %>% select(-sp500)
nikkei <- ibov %>% select(date, nikkei) %>% mutate(type = "Nikkei", value = scale(nikkei)) %>% select(-nikkei)
sse <- ibov %>% select(date, sse) %>% mutate(type = "SSE", value = scale(sse)) %>% select(-sse)
fed <- ibov %>% select(date, fed_rate) %>% mutate(type = "FED Rate", value = scale(fed_rate)) %>% select(-fed_rate)
industry_activity <- ibov %>% select(date, industry_activity) %>% mutate(type = "Industry Activity", value = scale(industry_activity)) %>% select(-industry_activity)
ieci <- ibov %>% select(date, ieci) %>% mutate(type = "IECI", value = scale(ieci)) %>% select(-ieci)
cci <- ibov %>% select(date, cci) %>% mutate(type = "CCI", value = scale(cci)) %>% select(-cci)
unemployment <- ibov %>% select(date, unemployment) %>% mutate(type = "Unemployment", value = scale(unemployment)) %>% select(-unemployment)
igpm_acc <- ibov %>% mutate(type = "IGP-M", value = scale(igpm_acc)) %>% select(date, value, type)
ipca_acc <- ibov %>% mutate(type = "IPCA", value = scale(ipca_acc)) %>% select(date, value, type)
eur <- ibov %>% select(date, eur) %>% mutate(type = "EUR", value = scale(eur)) %>% select(-eur)
usd <- ibov %>% select(date, usd) %>% mutate(type = "USD", value = scale(usd)) %>% select(-usd)
iron <- ibov %>% select(date, iron) %>% mutate(type = "Iron", value = scale(iron)) %>% select(-iron)
wti <- ibov %>% select(date, wti) %>% mutate(type = "Oil", value = scale(wti)) %>% select(-wti)

ibov2 <- bind_rows(close, dax, euronext, dj, lse, nasdaq, sp500, nikkei, sse, fed, industry_activity, ieci, cci, unemployment, igpm_acc, ipca_acc, usd, eur, iron, wti)

save(ibov2, file="rdas/ibov2.rda")

ict <- read_csv("data/ibovespa-composition.csv")[,-1]
save(ict, file="rdas/ict.rda")

