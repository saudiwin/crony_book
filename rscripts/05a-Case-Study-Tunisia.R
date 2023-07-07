## ----tikz_issues2, include=FALSE---------------------------------------------------------------
# piece of code to figure out how to compile tikz
if(knitr::is_latex_output()) {
  tikz_fig <- 'pdf'
} else if(knitr::is_html_output()) {
  tikz_fig <- 'svg'
} else {
  tikz_fig <- 'svg'
}
knitr::opts_chunk$set(echo=FALSE,fig.align='center',dpi=1200,dev=c("pdf","png"))


## 

## 

## \scalebox{1.5}{\tikzfig{causal_diagram_failure_events}}

## 

## 


# Figure 3.2 --------------------------------------------------------------

## ----tundem,fig.align='center',fig.cap='Varieties of Democracy Values for Tunisia',out.width="\\linewidth",echo=F,warning=F,message=F----
require(dplyr)
require(ggplot2)
readr::read_csv("data/vdem_all.csv") %>% 
  filter(country_name=="Tunisia") %>% 
  select(country_name, year, v2x_polyarchy, v2x_polyarchy_codehigh,
         v2x_polyarchy_codelow) %>% 
  ggplot(aes(y=v2x_polyarchy,x=year)) +
  geom_ribbon(aes(ymin=`v2x_polyarchy_codelow`,
              ymax=`v2x_polyarchy_codehigh`),alpha=0.5) + 
  geom_line() +
  theme_minimal() +
  xlab("") +
  ylim(c(0,1)) +
  ylab("VDEM Democracy Score (0 to 1)")

ggsave("figures/figure_3_2.pdf")


# Table 3.2 ---------------------------------------------------------------



## ----inttab,message=FALSE,warning=FALSE--------------------------------------------------------

require(dplyr)

mytab <- tibble::tribble(
   ~col1,                                                                                                                                                            ~col2,
   "INTERVIEWER",
   "Yeah. Has the, I mean, have people in the customs tried to collect bribes and stuff from you? We've had a lot of people complain about that.",
   
           "INTERVIEWEE",                                                                                                                                                                                                                                                                                                     "Yes.",
  "INTERVIEWER",                                                                                                                                                                                                                             "But it doesn't seem like it really matters. It's slow, it's slow, it's slow.",
           "INTERVIEWEE",                                                                                                                                                                        "(In English) Because before the revolution, it was simple. You had to pay one person and you'll have exactly what was the amount.",
  "INTERVIEWER",                                                                                                                                                                                                                                                                                            "And now it's…",
           "INTERVIEWEE",   "Now, it's a lot of people! And you never know the amount, and in this company, we do not choose this process, but I know that other companies don't have another choice. Now the problem is, you pay someone, and when you go to pick the parcel, you find someone else and you have to pay them, too."
  )

mytab %>% 
  knitr::kable(col.names=NULL,booktabs=T,caption="Interview Transcription, 2016") %>% 
  kableExtra::kable_styling(latex_options="hold_position") %>% 
  kableExtra::column_spec(2, width = "30em")


# Figure 3.3 --------------------------------------------------------------

## ----owndem1,fig.cap="Average Affect Towards Democracy Among Business Employees and Managers in Tunisia, Online Survey in Summer 2018",warning=FALSE,message=FALSE----

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
                             'Foreign Joint Venture',
                             'Foreign Joint Venture')),
         Q95=factor(Q95,levels=c('Less than 5',
                               'From 5 to 9',
                               'From 10 to 19',
                               'From 20 to 50',
                               'From 51 to 100',
                               'From 101 to 250',
                               'From 251 to 500',
                               'From 501 to 1000',
                               '1001 and over'))) %>% 
  filter(!is.na(Q99),!is.na(Q101_1),country=="Tunisia",Finished)

all_data %>% 
  ggplot(aes(y=Q101_1,x=Q99)) + stat_summary(fun.data = mean_cl_normal) +
  theme_minimal() +
  theme(panel.grid=element_blank()) +
  ggtitle("On a scale of 1 to 10, how strongly does your company's owner\nsupport democracy?",
          subtitle = "Online survey of 344 Tunisian employees and managers") + 
  ylim(c(0,10)) +
  ylab('Scale of 1 (weakest) to 10 (strongest)') +
  xlab("")

ggsave("figures/figure_3_3.pdf")


# Figure 3.4 --------------------------------------------------------------

## ----irtoned, fig.cap="Latent Positions of Tunisian MPs, 2014 - 2016",out.width="\\linewidth"----
#knitr::include_graphics('images/all_arp_custom.pdf')

# included as figures/figure3_4.pdf


# Figure 3.5 --------------------------------------------------------------

## ----conglom,fig.cap="Comparison of Top 3 Tunisian Conglomerate Revenue Growth versus Individual Firms",out.width="\\linewidth",message=F,warning=F----

# load and plot data from groupes

require(googlesheets4)
require(tidyr)
require(dplyr)
require(lubridate)
require(ggplot2)

tunis_comp <- read_csv("data/tunis_comp.csv") %>% 
  mutate(Conglomerate=unlist(Conglomerate)) %>% 
  fill(Number,`Company Name`,Public,Conglomerate,.direction="down")

tunis_comp %>% 
    mutate(Revenue=as.numeric(Revenue),
           Year=as.numeric(Year),
           Big=as.numeric(grepl(x=`Company Name`,pattern="MABROUK|ELLOUMI|POULINA"))) %>% 
    filter(Public!="1") %>% 
    # Conglomerate=factor(Conglomerate, labels=c("Independent","Conglomerate"),
    #                     levels=c(0,1))) %>% 
    group_by(Big,Year) %>% 
    summarize(Revenue=mean(Revenue,na.rm=T)) %>% 
    mutate(Big=factor(Big,levels=c(0,1),labels=c("Other Companies","Big 3"))) %>% 
  ggplot(aes(y=Revenue,x=Year)) +
  geom_line(aes(colour=Big,linetype=Big),size=1) + 
  guides(colour=guide_legend(title=""),
         linetype=guide_legend(title="")) +
  scale_color_grey() +
  labs(x="Tunisian Dinar") +
  ggthemes::theme_tufte() +
  theme(text=element_text(family=""))
  #scale_y_log10(labels=scales::dollar_format(prefix="TND")) +

ggsave("figures/figure_3_5.pdf")
