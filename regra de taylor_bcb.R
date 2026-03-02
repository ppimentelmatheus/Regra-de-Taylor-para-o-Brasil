###########################################################################
###################### Regra de Taylor ##################################

## Author: Matheus Porto Pimenttel
## Date: 09/04/2024


# Carregamento de pacotes e coleta de pacotes ----------------------------

library(tidyverse)
library(readr)
library(readxl)
library(zoo)
library(meedr)
library(rbcb)
library(GetBCBData)
library(dplyr)
library(magrittr)
library(forecast)

# Taxa selic --------------------------------------------------------------

selic_raw = GetBCBData::gbcbd_get_series(
  id = 432,
  first.date = '1999-01-01',
  use.memoise = FALSE
)

selic_t = selic_raw |> dplyr::mutate(
  date_quarter = tsibble::yearquarter(ref.date)
) |>
  dplyr::select(date_quarter, 'selic' = value) |>
  as_tibble() |>
  dplyr::group_by(date_quarter) |>
  dplyr::summarise(selic = last(selic)) |>
  dplyr::ungroup()

selic_t |>
  dplyr::filter(as.Date(date_quarter) >= '2000-01-01') |>
  ggplot(aes(x=as.Date(date_quarter), y=selic))+
  geom_line()+
  theme_light()+

  labs(title = "Interest Rate (Selic)",
       caption = "Own elaboration,
       Source: Central Bank of Brazil",
       x="", y='Selic')



# Swap DI 360 -------------------------------------------------------------
raw_swap_di = ipeadatar::ipeadata(code = "BMF12_SWAPDI36012")

swap_di = raw_swap_di |> 
  dplyr::select(date, juros = value) |> 
  dplyr::mutate(date = tsibble::yearmonth(date)) |> 
  dplyr::mutate(date_quarter = tsibble::yearquarter(date)) |> 
  dplyr::group_by(date_quarter) |> 
  dplyr::summarise(selic = mean(juros)) |> 
  dplyr::ungroup()


selic_raw = GetBCBData::gbcbd_get_series(
  id = 432,
  first.date = '1999-01-01',
  use.memoise = FALSE
)

selic_t = selic_raw |> dplyr::mutate(
  date_quarter = tsibble::yearquarter(ref.date)
) |> 
  dplyr::select(date_quarter, 'selic' = value) |> 
  as_tibble() |> 
  dplyr::group_by(date_quarter) |> 
  dplyr::summarise(selic = last(selic)) |> 
  dplyr::ungroup()

selic_t |> 
  dplyr::filter(as.Date(date_quarter) >= '2000-01-01') |> 
  ggplot(aes(x=as.Date(date_quarter), y=selic))+
  geom_line()+
  theme_light()+
  scale_x_date(date_breaks = "3 year",
               date_labels = "%Y")+
  labs(x="", y='Selic')
# Meta de Inflação --------------------------------------------------------

target = readxl::read_xlsx(
  path = "meta.xlsx")


# Hiato do Produto --------------------------------------------------------

hiato_bcb = read_xlsx(
  path = "ri202312anp.xlsx",
  sheet = "Graf 2.2.4",
  skip=8,
  col_names=T
) |> drop_na()

hiato_bcb = hiato_bcb |>
  dplyr::mutate(date_quarter = tsibble::yearquarter(Trimestre)) |>
  dplyr::select(date_quarter, hiato_bcb = Hiato)


# Inflação ----------------------------------------------------------------

ipca = read_xlsx(
  path = "ipca.xlsx",
  col_names=T
) |> as_tibble() |>
  dplyr::mutate(date = tsibble::yearquarter(date)) |>
  dplyr::rename(date_quarter = date,
                ipca = value)


# Juro Neutro -------------------------------------------------------------

## Expectativa de inflação
ipca_e_t4_raw = rbcb::get_market_expectations(
  type       = "annual",
  indic      = "IPCA"
)

## Realizar o filtro para a data de referência 3 anos a frente da obs
ipca_e_t4 = ipca_e_t4_raw |>
  filter(ipca_e_t4_raw$DataReferencia == year(Data)+4) |>
  dplyr::select(date = Data, ipca_e = Mediana) |>
  dplyr::mutate(date = tsibble::yearmonth(date)) |>
                  dplyr::group_by(date) |>
                  dplyr::summarise(ipca_e_t4 = mean(ipca_e))


selic_e_raw = meedr::get_annual(
  indicator = "Selic",
  first_date = '1999-01-01',
  use_memoise = FALSE
) |>
  dplyr::select(date, reference_date, median)

selic_e_t4 = selic_e_raw |>
  dplyr::filter(selic_e_raw$reference_date == year(date)+4) |>
  dplyr::select(date = date, selic_e = median) |>
  dplyr::mutate(date = tsibble::yearmonth(date)) |>
  dplyr::group_by(date) |>
  dplyr::summarise(selic_e_t4 = mean(selic_e))

# Juntar os dados em um data frame
proxy_neutro_t4 = dplyr::inner_join(
  x = ipca_e_t4,
  y = selic_e_t4,
  by = "date"
)

# Cria o juro neutro e trimestraliza

proxy_neutro_t4 = proxy_neutro_t4 |>
  dplyr::mutate(neutro = (((1+(selic_e_t4/100))/(1+(ipca_e_t4/100)))-1)*100)

proxy_neutro_t4 = proxy_neutro_t4 |>
  dplyr::mutate(date_quarter = tsibble::yearquarter(date)) |>
  dplyr::group_by(date_quarter) |>
  dplyr::summarise(neutro = mean(neutro)) |>
  ungroup()

# Filtro HP

# Converte dados para classe ts
proxy_neutro_t4_ts <- ts(# função para criar um objeto de classe ts
  data  = proxy_neutro_t4$neutro,# vetor da série
  start = c(                            # vetor com dois elementos: ano e trimestre de início da série
    lubridate::year(min(proxy_neutro_t4$date_quarter)),   # ano (formato AAAA)
    lubridate::quarter(min(proxy_neutro_t4$date_quarter)) # trimestre (formato T)
  ),
  frequency = 4# frequência da série (trimestral = 4)
)

# Calcula o filtro HP
filtro_hp <- mFilter::hpfilter(x = proxy_neutro_t4_ts, type = "lambda", freq = 1600)

# Salva a tendência calculada
neutro_trend = filtro_hp$trend

neutro_trend = as_tibble(neutro_trend)
proxy_neutro_t4 %<>% dplyr::bind_cols(neutro_trend)

proxy_neutro_t4 = proxy_neutro_t4 |>
  dplyr::rename("neutro_trend" = `Series 1`)

# Juntar os dados ---------------------------------------------------------

dfs = list(swap_di, hiato_bcb, ipca, proxy_neutro_t4)

dados_reg = purrr::reduce(
  .x = dfs,
  .f = dplyr::left_join,
  by="date_quarter"
) %>%
  drop_na() |>
  tsibble::as_tsibble(index=date_quarter)
target = target[1:81,]
dados_reg %<>% dplyr::bind_cols(target)

# Expandir os dados com defasagens ----------------------------------------
dados_reg = dados_reg %>%
  dplyr::mutate("selic_lag1" = lag(selic, 1),
                "selic_lag2" = lag(selic, 2),
                "selic_lag3" = lag(selic, 3),
                "selic_lag4" = lag(selic, 4),
                #desvio da meta
                "desvio_ipca" = ipca - meta,
                # defasagem hiato
                "hiato_lag1" = lag(hiato_bcb, 1)
  ) |> drop_na() |> tsibble::as_tsibble(index = date_quarter)


# Modelos -----------------------------------------------------------------

## Modelo OLS Simples

eq_taylor_simples = lm(
  formula = selic ~ 1 + desvio_ipca + hiato_bcb,
  data = dados_reg)

## Modelo OLS BCB

eq_taylor_bcb = lm(
  formula = selic ~ -1 + selic_lag1 + selic_lag2 + desvio_ipca + hiato_lag1,
  data = dados_reg
)

dados_reg = dados_reg |> 
  as_tibble(index = none) |>
  dplyr::select(selic, selic_lag1, selic_lag2, desvio_ipca, hiato_lag1)

## Modelo com parametros
parameters = eq_taylor_bcb$coefficients
theta1 = parameters[[1]]
theta2 = parameters[[2]]
theta3 = parameters[[3]]
theta4 = parameters[[4]]
resid = eq_taylor_bcb$residuals

eq_taylor = tibble(
  quarter = dados_reg$date_quarter,
  eq_taylor1 = theta1*dados_reg$selic_lag1 + theta2*dados_reg$selic_lag2,
  eq_taylor2 = (1- theta1 - theta2)*
    (dados_reg$neutro_trend + dados_reg$meta
  + theta3*dados_reg$desvio_ipca + theta4*dados_reg$hiato_lag1),
  eq_taylor = eq_taylor1 + eq_taylor2 + resid)

## Juntar modelos
dados_reg = dados_reg %>%
  mutate(modelo = fitted(eq_taylor_bcb),
         residuos = resid(eq_taylor_bcb),
         eq_taylor_suavizada = eq_taylor$eq_taylor)

# Gráfico Ajuste ----------------------------------------------------------

ggplot(dados_reg, aes(x=as.Date(date_quarter)))+
  geom_line(aes(y = selic, color = 'Selic'), linewidth=.8)+
  geom_line(aes(y = modelo, color = 'modelo'), linewidth=.8)+
  scale_color_manual('', values = c('Selic' = 'red',
                                    'modelo' = 'darkblue'))+
  labs(title = "Selic vs Modelo",
       x = 'Quarter', y = '')+
  theme_light()+
  theme(legend.position = 'bottom')

ggplot(dados_reg, aes(as.Date(date_quarter)))+
  geom_line(aes(y = selic, color = 'Selic'), linewidth=.8)+
  geom_line(aes(y = eq_taylor_suavizada, color = 'eq_taylor_suavizada'), linewidth=.8)+
  scale_color_manual('', values = c('Selic' = 'red',
                                    'eq_taylor_suavizada' = 'darkblue'))+
  labs(title = "Selic vs Modelo",
       x = 'Quarter', y = '')+
  theme_light()+
  theme(legend.position = 'bottom')



# Exportar dados ----------------------------------------------------------

googlesheets4::gs4_create(
  name = "taylor_bcb",
  sheets = dados_reg[-1]
)


# Modelo com parâmetros variantes no tempo (TVP) ------------------------------
library(dlm)



  















