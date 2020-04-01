#install.packages("remotes")
#install.packages("tidyverse")
#install.packages("padr")
#remotes::install_github("GuangchuangYu/nCov2019")
library(nCov2019)
library(tidyverse)
library(padr)
library(lubridate)

setwd('/home/paulo/Documents/dados/covid/relatório/')

# Obtendo os dados históricos por país
corona_global = load_nCov2019(lang = "en") %>% 
  pluck(3) %>% 
  as_tibble() %>%
  mutate(cum_dead = ifelse(country == "Brazil" & time == strptime('2020-03-19', format= '%Y-%m-%d'), 3, cum_dead)) %>%
  mutate(cum_dead = ifelse(country == "Brazil" & time == strptime('2020-03-20', format= '%Y-%m-%d'), 7, cum_dead))  %>%
  group_by(country) %>%
  mutate(mindata = min(time)) %>%
  ungroup() %>%
  pad(interval = "day", by = "time", group = "country") %>%
  fill(cum_confirm, cum_heal, cum_dead) %>%
  transform(elapsed = as.numeric(time - mindata, units = "days"))

# Data da primeira morte
first_death = corona_global %>%
  filter(cum_dead > 0) %>%
  group_by(country) %>%
  summarise(firstdeath = min(time) + 3)

corona_global = corona_global %>% left_join(first_death, by = "country")

corona_global %>% filter(country == 'Brazil') %>% View()

paises = c('Italy', 'Spain', 'United States', 'China', 'Brazil')

# Gráfico
g = ggplot(data = corona_global %>% filter(country %in% paises), aes(x = elapsed, y = log(cum_confirm), color = factor(country)))
g + geom_line() + geom_point() +ylab("Log(casos confirmados)") + xlab("Dias desde o primeiro caso") +
guides(col=guide_legend(title="País"))

# Calculando taxa de crescimento
corona_global = corona_global %>% group_by(country) %>% mutate(growth = (cum_confirm - lag(cum_confirm)) / lag(cum_confirm), growthd = ifelse(lag(cum_dead) == 0, NA, (cum_dead - lag(cum_dead))/lag(cum_dead)))

# Gráfico crescimento
data.labels = list(data.frame(pais = 'China', quarentine = 53,
  x = 75,
  y = 2.5,
  label = 'Quarentena começa em Wuhan',
  ymax = 100
),
data.frame(pais = 'Brasil', quarentine = 25,
  x =20,
  y = 1.5,
  label = 'Quarentena começa em São Paulo',
  ymax = 100
),
data.frame(pais = 'Itália', quarentine = 32,
  x =25,
  y = .7,
  label = 'Quarentena em toda a Itália',
  ymax = 2.5
),
data.frame(pais = 'EUA', quarentine = c(54, 62),
  x =c(45, 50),
  y = c(1.5, 1),
  label = c('Trump declara apoio à quarentena de 15 dias', 'Trump manifesta dúvidas sobre a quarentena'),
  ymax = 100
),
data.frame(pais = 'Reino Unido', quarentine = 46,
                            x =35,
                            y = 1,
                            label = c('Quarentena começa na Inglaterra'),
           ymax = 100
))


paises = c('China', 'Brazil', 'Italy', 'United States', 'United Kingdom')

for(i in 1:length(paises)) {
  
  data.label = data.labels[[i]]
  pais = paises[i]
  
  if(i == 4) {
    g = ggplot(data = corona_global %>% filter(country == pais & growth < data.label$ymax[1]), aes(x = elapsed, y = growth))
    g + geom_line(lty = 'dashed') + geom_point() + xlab("Dias desde o primeiro caso") + ylab("Taxa de crescimento") +
      ggtitle(paste0(data.label$pais, '- crescimento diário dos casos confirmados'))+ 
      #geom_hline(yintercept = as.numeric((corona_global %>% filter(country == pais) %>% summarise(mean(growth, na.rm = T)))[2]), lty = 'dashed', color= 'red') +
      geom_vline(xintercept = data.label$quarentine[1], color = 'blue', lty = 'dashed')  +
      geom_text(data = data.label, aes(x = x[1], y = y[1], label = label[1]), color = 'blue') +
      geom_vline(xintercept = data.label$quarentine[2], color = 'blue', lty = 'dashed') +
      geom_text(data = data.label, aes(x = x[2], y = y[2], label = label[2]), color = 'blue')  
  } else {
    g = ggplot(data = corona_global %>% filter(country == pais & growth < data.label$ymax[1]), aes(x = elapsed, y = growth))
    g + geom_line(lty = 'dashed') + geom_point() + xlab("Dias desde o primeiro caso") + ylab("Taxa de crescimento") +
      ggtitle(paste0(data.label$pais, '- crescimento diário dos casos confirmados'))+ 
      #geom_hline(yintercept = as.numeric((corona_global %>% filter(country == pais) %>% summarise(mean(growth, na.rm = T)))[2]), lty = 'dashed', color= 'red') +
      geom_vline(xintercept = data.label$quarentine[1], color = 'blue', lty = 'dashed')  +
      geom_text(data = data.label, aes(x = x[1], y = y[1], label = label[1]), color = 'blue') 
  }      
  ggsave(paste0('confirmed_', pais, '.png'))
  
  if(i == 4) {
    g = ggplot(data = corona_global %>% filter(country == pais & time > firstdeath), aes(x = elapsed, y = growthd))
    g + geom_line(lty = 'dashed') + geom_point() + xlab("Dias desde o primeiro caso") + ylab("Taxa de crescimento") +
      ggtitle(paste0(data.label$pais, '- crescimento das mortes'))+ 
      #geom_hline(yintercept = as.numeric((corona_global %>% filter(country == pais) %>% summarise(mean(growthd, na.rm = T)))[2]), lty = 'dashed', color= 'red') +
      geom_vline(xintercept = data.label$quarentine[1], color = 'blue', lty = 'dashed') +
      geom_vline(xintercept = data.label$quarentine[2], color = 'blue', lty = 'dashed')
  } else {
    g = ggplot(data = corona_global %>% filter(country == pais & time > firstdeath), aes(x = elapsed, y = growthd))
    g + geom_line(lty = 'dashed') + geom_point() + xlab("Dias desde o primeiro caso") + ylab("Taxa de crescimento") +
      ggtitle(paste0(data.label$pais, '- crescimento das mortes')) + 
      #geom_hline(yintercept = as.numeric((corona_global %>% filter(country == pais) %>% summarise(mean(growthd, na.rm = T)))[2]), lty = 'dashed', color= 'red') +
      geom_vline(xintercept = data.label$quarentine[1], color = 'blue', lty = 'dashed')
  }
  ggsave(paste0('dead_', pais, '.png'))
  
  # Crescimento casos x crescimento mortes
  dftemp = corona_global %>% filter(country == pais) %>% ungroup() %>%select(c(elapsed, growth, growthd)) %>% 
    pivot_longer(-elapsed, names_to = 'type', values_to = 'rate') %>%
    mutate(type = recode(type, growth = 'Casos', growthd = 'Mortes'))
  
  g = ggplot(data = dftemp, aes(x = elapsed, y = rate, colour = type))
  g + geom_line(lty = 4) +
    geom_point() +
    geom_line(lty = 2) +
    guides(col=guide_legend(title="")) +
    xlab("Dias desde o primeiro caso") +
    ylab("Taxa de crescimento") + 
    ggtitle(paste0(data.label$pais, ' - taxa de crescimento diário'))
  ggsave(paste0('both_', pais, '.png'))
  
}

pais = 'Brazil'
corona_global %>% filter(country == pais) %>% View()


pais = 'Spain'
# Crescimento casos x crescimento mortes
dftemp = corona_global %>% filter(country == pais) %>% ungroup() %>%select(c(elapsed, growth, growthd)) %>% 
  pivot_longer(-elapsed, names_to = 'type', values_to = 'rate') %>%
  mutate(type = recode(type, growth = 'Casos', growthd = 'Mortes'))

g = ggplot(data = dftemp, aes(x = elapsed, y = rate, colour = type))
g + geom_line(lty = 4) +
  geom_point() +
  geom_line(lty = 2) +
  guides(col=guide_legend(title="")) +
  xlab("Dias desde o primeiro caso") +
  ylab("Taxa de crescimento") + 
  ggtitle(paste0(data.label$pais, ' - taxa de crescimento diário'))
