## ----setup2, include=F-------------------------------------------------------------------------

knitr::opts_chunk$set(echo=FALSE,fig.align='center',dpi=1200,dev=c("pdf","png"),warning = F,message = F)



## ----tikz_issues, include=FALSE----------------------------------------------------------------
# piece of code to figure out how to compile tikz
if(knitr::is_latex_output()) {
  tikz_fig <- 'pdf'
} else if(knitr::is_html_output()) {
  tikz_fig <- 'svg'
} else {
  tikz_fig <- 'svg'
}
knitr::opts_chunk$set(echo=FALSE,fig.align='center')


## 

## 

## \scalebox{1.5}{\tikzfig{causal_diagram_success_events}}

## 

## 


# Figure 2.2 --------------------------------------------------------------
## ----fdi, fig.cap="Egypt Development Statistics",fig.align='center',out.width="\\linewidth"----

require(tidyverse)
require(ggplot2)
require(ggthemes)

# World Bank Development Indicators

egypt_data <- read_csv("data/egypt_indicators.csv")

egypt_data %>% 
  filter(`Series Name` %in% c("Natural gas rents (% of GDP)",
                              "Oil rents (% of GDP)",
                              "Manufacturing, value added (% of GDP)",
                              "General government final consumption expenditure (% of GDP)",
                              "Agriculture, forestry, and fishing, value added (% of GDP)",
                              "Services, value added (% of GDP)",
                              "Customs and other import duties (% of tax revenue)",
                              "Cost of business start-up procedures (% of GNI per capita)")) %>% 
  mutate(`Series Name`=factor(`Series Name`,
                              levels=c("Natural gas rents (% of GDP)",
                              "Oil rents (% of GDP)",
                              "Manufacturing, value added (% of GDP)",
                              "Agriculture, forestry, and fishing, value added (% of GDP)",
                              "Services, value added (% of GDP)",
                              "General government final consumption expenditure (% of GDP)",
                              "Customs and other import duties (% of tax revenue)",
                              "Cost of business start-up procedures (% of GNI per capita)"),
                              labels=c("Natural gas rents",
                              "Oil rents",
                              "Manufacturing, value added",
                              "Agriculture, value added",
                              "Services, value added",
                              "Government Expenditure",
                              "Customs % of tax revenue",
                              "Cost of business\nstart-up procedures"))) %>% 
  ggplot(aes(y=Value, 
             x=Time)) +
  geom_line() +
  theme_tufte() +
  theme(text=element_text(family="")) +
  facet_wrap(~`Series Name`,scales="free_y",ncol = 3) +
    labs(y="% of GDP",x="",caption="Data from the World Bank Development Indicators.")

ggsave("figures/figure_2_2.pdf")


# Figure 2.3 --------------------------------------------------------------
## ----egyptdem,fig.align='center',fig.cap='Varieties of Democracy Values for Egypt',out.width="\\linewidth",echo=F,warning=F,message=F----

require(dplyr)
require(ggplot2)
readr::read_csv("data/Egypt_vdem.csv") %>% 
  mutate_all(as.numeric) %>% 
  ggplot(aes(y=`Electoral Democracy Index`,x=Year)) +
  geom_ribbon(aes(ymin=`Electoral Democracy Index CI (Low)`,
              ymax=`Electoral Democracy Index CI (High)`),alpha=0.5) + 
  geom_line() +
  theme_minimal() +
  xlab("") +
  ylim(c(0,1)) +
  ylab("VDEM Democracy Score (0 to 1)")

ggsave("figures/figure_2_3.pdf")


## ----owndem,fig.cap="Average Affect Towards Democracy Among Business Employees and Managers in Egypt, Online Survey in Summer 2018",warning=FALSE,message=FALSE----

require(dplyr)
require(qualtRics)
require(tidyr)
require(lubridate)
require(stringr)
require(ggplot2)

all_data <- readr::read_csv('data/egypt_mil_survey.csv') %>% 
  mutate(Q99=factor(Q99,levels=c('Registered as a domestic company',
                                 'Unregistered domestic company',
                                 "Other",
                                 'State-owned enterprise',
                                 '100% foreign-owned enterprise',
                                 'Joint-venture with an ${e://Field/country_adj}  private enterprise',
                                 'Joint-venture with an ${e://Field/country_adj} state-owned enterprise'),
                    labels=c('Domestic\nEnterprise',
                             'Informal\nEnterprise',
                             'Informal\nEnterprise',
                             'State-owned\nEnterprise',
                             'Foreign-owned\nEnterprise',
                             'Foreign\nJoint Venture',
                             'Foreign\nJoint Venture')),
         Q95=factor(Q95,levels=c('Less than 5',
                               'From 5 to 9',
                               'From 10 to 19',
                               'From 20 to 50',
                               'From 51 to 100',
                               'From 101 to 250',
                               'From 251 to 500',
                               'From 501 to 1000',
                               '1001 and over'))) %>% 
  filter(!is.na(Q99),!is.na(Q101_1),country=="Egypt",Finished)

all_data %>% 
  ggplot(aes(y=Q101_1,x=Q99)) + stat_summary(fun.data = mean_cl_normal) +
  theme_minimal() +
  theme(panel.grid=element_blank()) +
  ggtitle("On a scale of 1 to 10, how strongly does your company's owner\nsupport democracy?",
          subtitle = "Online survey of 1,303 Egyptian employees and managers") + 
  ylab('Scale of 1 (weakest) to 10 (strongest)') +
  ylim(c(1,10)) +
  xlab("")

ggsave("figures/figure_2_4.pdf")



## ----actfirm, fig.cap="Political Activities of Firms as Reported by Employees",out.width="\\linewidth"----
#knitr::include_graphics('data/polact-1.pdf')

# this file included as figures/figure_2_5.pdf

