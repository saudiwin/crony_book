## ----setup5, include=FALSE---------------------------------------------------------------------
require(haven)
require(forcats)
require(cjoint)
require(dplyr)
require(tidyr)
require(binom)
require(ggplot2)
require(readr)
require(brms)
require(kableExtra)
require(stringr)
require(boot)
require(ggthemes)
require(mirt)

options(readr.default_locale=readr::locale(tz="Asia/Dubai"))

# parallelsugar is only available on Github
# to install, use the command devtools::install_github('nathanvan/parallelsugar')
require(parallel)

source('scripts/helper_func.R')



orig_data <- read_csv('data/surveys/egypt_tunisia_2017.csv')
# run recode script
orig_data <- recode_vars(orig_data)

# Egypt/Tunisia Surveys

all_imp_dist_eg_tn <- readRDS('data/surveys/all_imp_dist_eg_tn.rds') %>% 
  mutate(ResponseId=as.character(ResponseId),
         Country=recode(Country,Egypt="Egypt I")) %>% 
  filter(imputed %in% c("1","2","3","4","5"))
all_impute_eg_tn <- readRDS('data/surveys/all_impute_eg_tn.rds')
all_impute_eg <- lapply(all_impute_eg_tn,filter,Country=='Egypt')
all_impute_eg_mil <- lapply(all_impute_eg_tn,filter,Country=='Egypt',actor_type %in% c("Military","Government"))
all_impute_alg <- lapply(all_impute_eg_tn,filter,Country=='Algeria')
num_cores <- 4

# Morocco/Jordan surveys

all_impute_m_jn <- readRDS('data/surveys/all_impute_jn_m.rds') 

# Venezuela/Ukraine/Egypt II surveys

all_impute_eg_vn <- readRDS("data/surveys/all_impute_eg_vn.rds")

run_code <- F

knitr::opts_chunk$set(echo=F,warning=F,message=F,fig.align='center',dpi=1200,dev=c("pdf","png"))


## ----loaddata2,include=F-----------------------------------------------------------------------

# Egypt 2018 survey

data2018 <- readr::read_csv('data/surveys/egypt_mil.csv') %>% 
  filter(Finished) %>% 
  mutate(rank_eg=ordered(rank_eg,levels=c('Private',
                                          'Corporal',
                                          'Sergeant',
                                          'Staff Sergeant',
                                          'Warrant Officer Class 2',
                                          'Chief Warrant Officer',
                                          'Second Lieutenant',
                                          'Lieutenant',
                                          'Captain',
                                          'Major',
                                          'Lieutenant Colonel',
                                          'Colonel',
                                          'Brigadier',
                                          'Major General',
                                          'Lieutenant General',
                                          'Field Marshall')),
         Q99=factor(Q99,levels=c('Registered as a domestic company',
                                 'Unregistered domestic company',
                                 'State-owned enterprise',
                                 '100% foreign-owned enterprise',
                                 'Joint-venture with an ${e://Field/country_adj}  private enterprise',
                                 'Joint-venture with an ${e://Field/country_adj} state-owned enterprise'),
                    labels=c('Domestic\nEnterprise',
                             'Informal\nEnterprise',
                             'State-owned\nEnterprise',
                             'Foreign-owned\nEnterprise',
                             'JV Domestic',
                             'JV SOE')),
         Q95=factor(Q95,levels=c('Less than 5',
                               'From 5 to 9',
                               'From 10 to 19',
                               'From 20 to 50',
                               'From 51 to 100',
                               'From 101 to 250',
                               'From 251 to 500',
                               'From 501 to 1000',
                               '1001 and over'))) %>% 
  filter(country %in% c('Egypt','Tunisia')) %>% 
  mutate(Country=recode(country,`Egypt`="Egypt Military",
                        `Tunisia`="Tunisia Military"))

# long form

all_imp_dist_m_jn <- bind_rows(all_impute_m_jn,.id="imputed") %>% 
    dplyr::select(Q52_1,
         Q52_2,
         Q52_3,
         Q52_4,
         Q33_1,
         Q13,
         Q8,
         Q8_1,
         Q30_2,
         Q25,
         Q26,
         Q27,
         Q37,
         Q21_2,
         Q28_2,
         Q74,
         Q38,
         Country,
         Q14,
         Q9,
         ResponseId,imputed,
         actor,actor_type,appeal,appeal_type,DV1,DV2,DV3)

all_imp_dist_eg_vn <- bind_rows(all_impute_eg_vn,.id="imputed") %>% 
  dplyr::select(Q14="registration",
                firm_pol,
                Q21_2="perf_2",
                matches("inspect"),
                Q74="ceo",
                Q13="sector_1",
                Q38="bribe_increase",
                Q30_2="supply_2",
                Q28_2="cust_2",
                Q8="firm_size",
                ceo_party_eg,
                Q8_1="conglomerate",
                Q37="bribe_income",
                Country="country",
                Q9="position",
                ResponseId,imputed,
                Appeal1,Appeal2, Actor1, Actor2,matches("out1|out2")) %>% 
  mutate(Q52_1=as.numeric(grepl(x=firm_pol,pattern="Contributed")),
         Q52_2=as.numeric(grepl(x=firm_pol,pattern="Distributed")),
         Q52_3=as.numeric(grepl(x=firm_pol,pattern="Instructed")),
         Q52_4=as.numeric(grepl(x=firm_pol,pattern="Hosted")),
         Q33_1 = as.numeric(inspect_1_1) + as.numeric(inspect_2_1) + 
           as.numeric(inspect_3_1) + as.numeric(inspect_4_1),
         Q33_1=ifelse(Q33_1>100,100,Q33_1),
         Q13=fct_collapse(factor(Q13),
                        `Service/Commerce`=c("OTHER SERVICE ACTIVITIES",
                                             "WHOLESALE AND RETAIL TRADE; REPAIR OF MOTOR VEHICLES AND MOTORCYCLES",
                                             "PUBLIC ADMINISTRATION AND DEFENCE; COMPULSORY SOCIAL SECURITY",
                                             "ACCOMMODATION AND FOOD SERVICE ACTIVITIES",
                                             "ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS; UNDIFFERENTIATED GOODS- AND SERVICES-PRODUCING ACTIVITIES OF HOUSEHOLDS FOR OWN USE",
                                             "PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES",
                                             "EDUCATION",
                                             "ACTIVITIES OF EXTRATERRITORIAL ORGANISATIONS AND BODIES",
                                             "ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITIES",
                                             "INFORMATION AND COMMUNICATION",
                                             "ARTS, ENTERTAINMENT AND RECREATION",
                                             "ACCOMMODATION AND FOOD SERVICE ACTIVITIES",
                                             "HUMAN HEALTH AND SOCIAL WORK ACTIVITIES",
                        "TRANSPORTATION AND STORAGE"),
                        `Finance/Banking/Insurance`=c("FINANCIAL AND INSURANCE ACTIVITIES"),
                        `Construction/Investment in Infrastructure Construction`=c("CONSTRUCTION","REAL ESTATE ACTIVITIES"),
                        `Industry/Manufacturing`=c("WATER SUPPLY; SEWERAGE, WASTE MANAGEMENT AND REMEDIATION ACTIVITIES",
                                                   "ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY",
                       "MANUFACTURING"),
                       `Agriculture/Forestry/Aquaculture`=c("AGRICULTURE, FORESTRY AND FISHING"),
                       `Mining`=c("MINING AND QUARRYING")),
         Country=recode(Country, Egypt="Egypt II")) %>% 
  mutate_at(vars(Q52_1:Q52_4),~ifelse(is.na(firm_pol),NA_real_,.)) %>% 
  mutate_at(vars(Q28_2,Q30_2),as.numeric) %>% 
  mutate(Q33_1=factor(Q33_1)) %>% 
  dplyr::select(-firm_pol)

# reshape and add in treatment info

all_imp_dist_eg_vn <- all_imp_dist_eg_vn %>% 
    gather(key="type",value="treatment",Appeal1, Appeal2,Actor1, Actor2) %>%
    separate(type,into=c("treat_type","task"),sep=-1) %>% 
  spread(key="treat_type",value="treatment") %>% 
  gather(key="DV_type",value="DV",matches("out")) %>% 
  separate(DV_type,into = c("task2","DV_type")) %>%
  mutate(DV_type=paste0("DV",DV_type),
         task2=as.numeric(str_remove(task2,"out")),
         task=as.numeric(task)) %>%
  filter(task2==task) %>% mutate(DV=as.numeric(str_extract(DV,"[0-9]+"))) %>%
  spread(key="DV_type",value="DV") %>% 
  mutate_at(vars(matches('Actor')),
            fct_collapse,`Prime Minister`=c("le premier ministre",
                                     "prime minister",
                                     "رئيس الوزراء"),
            MOI=c("le Ministère de l'Intérieur",
                  "Министерство Внутренних Дел",
                  "el ministro del Interior",
                  "Ministry of Interior",
                  "وزارة الداخلية"),
            MOJ=c("le Ministère de la Justice",
                  "Министерство Юстиции",
                  "el ministro de Justicia",
                  "Ministry of Justice",
                  "وزراة العدل"),
            Government=c("le gouvernement",
                         "государство",
                         "el gobierno",
                         "government",
                         "الحكومة"),
            Parliament=c("le parlement",
                         "парламент",
                         "el parlamento",
                         "parliament",
                         "البرلمان"),
            President=c("le président",
                        "президент",
                        "el presidente",
                                     "president",
                                     "الرئيس"),
            Municipality=c("la municipalité",
                           "муниципалитет",
                           "la municipalidad",
                           "municipality",
                           "المحليات"),
            Military=c("l'armée",
                       "القوات المسلحة",
                       "military",
                       "военные",
                       "los militares")) %>% 
  mutate_at(vars(matches('Appeal')),
            funs(fct_collapse(.,Reforms=c("mette en place des réformes qui vont stimuler la croissance et réduire le chômage",
                                          "ستقوم\\سيقوم بالاصلاحات اللازمة لتشجيع النمو الاقتصادى و تقليل مستوى البطالة",
                                          "will implement reforms that will encourage economic growth and lower unemployment",
                                          "va a implementar reformas que estimularán el crecimiento económico y disminuirán el desempleo",
                                          "будет воплощать реформы, которые будут способствовать экономическому росту и снижению безработицы"),
                              Contracts=c("aide votre entreprise à gagner des appels d'offre publics",
                                          "ayuda a su empresa a conseguir contratos del gobierno",
                                          "помогает вашей компании получать государственные контракты",
                                          "helps your company secure government contracts",
                                          "ستساعد\\سيساعد شركتكم فى الحصول على عقود حكومية"),
                              Confiscate=c("ne tente pas de confisquer les bénéfices de votre firme",
                                           "لن تحاول\\يحاول أن تأخذ\\يأخذ أرباح شركتكم",
                                           "does not try to take your firm's profits",
                                           "не пытается изъять прибыль компании",
                                           "no trata de quitarle las utilidades de su empresa"),
                              Export=c("aide votre entreprise à exporter facilement ses produits et services",
                                       "ستساعد\\سيساعد شركتكم فى تصدير السلع و الخدمات",
                                       "helps your company export its goods and services",
                                       "ayuda a su empresa a exportar sus productos y servicios",
                                       "помогает вашей компании с экспортом услуг и товаров"),
                              Licenses=c("aide votre firme à garantir des licences (agréments) d'activité de la part des régulateurs",
                                         "ستساعد\\سيساعد شركتكم فى الحصول على التصاريح اللازمة لتشغيل الأعمال",
                                         "helps your company secure permits from regulators to do business",
                                         "помогает вашей компании с получением разрешений регулирующих органов для ведения бизнеса",
                                         "ayuda a su empresa a conseguir los permisos de los reguladores para hacer negocios"),
                              Import=c("aide votre firme à importer le matériel nécessaire",
                                       "ستساعد\\سيساعد شركتكم فى استيراد المواد اللازمة",
                                       "helps your company import necessary materials",
                                       "помогает вашей компании с импортом необходимых материалов",
                                       "ayuda a su empresa a importar los insumos necesarios"),
                              Control=c("ne tente pas de s'emparer ou de contrôler votre business",
                                        "لن تحاول\\يحاول أن تسيطر\\يسيطر على شركتكم",
                                        "does not try to take control of your firm",
                                        "не пытается получить контроль над вашей фирмой",
                                        "no trata de tomar el control de su empresa"),
                              `NA`=c('Default','helps your company secure contracts to supply goods')))) %>% 
  select(actor_type="Actor",
         appeal_type="Appeal",
         treat="task",everything()) %>% 
  mutate(actor=paste0("Actor",treat),
         appeal=paste0("Appeal",treat))

# combine long forms

combine_long_form <- bind_rows(all_imp_dist_eg_tn,
                               all_imp_dist_m_jn,
                               all_imp_dist_eg_vn) %>% 
  mutate(actor_type=na_if(actor_type,"Default"),
         appeal_type=na_if(appeal_type,"Default"),
         actor_type=factor(actor_type),
         appeal_type=factor(appeal_type))

new_scale <- mirt(select(combine_long_form,DV1,DV2,DV3),model=1,itemtype="graded")

new_scores <- fscores(new_scale)

combine_long_form$new_scale <- new_scores[,1]



## ----econ,fig.cap="World Bank Development Indicator Statistics"--------------------------------

require(WDI)
require(vdemdata)
require(patchwork)
data(vdem)
wdi_data <- WDI(country=c("DZA","EGY","TUN","VEN","JOR","MAR","UKR"),
                indicator=c(GDP="NY.GDP.PCAP.KD",
                             Unemployment="SL.UEM.TOTL.ZS",
                             FDI="BN.KLT.DINV.CD.ZS")) %>% 
  gather(key="series",value="value",-year,-iso2c,-country) %>% 
  mutate(country=recode(country,
                        `Egypt, Arab Rep.`="Egypt",
                        `Venezuela, RB`="Venezuela"))

vdem_data <- filter(vdem, country_text_id %in% c("DZA","EGY","TUN","VEN","JOR","MAR","UKR"),
                    year>1949)

fdi <- wdi_data %>% 
  filter(!is.na(value),series=="FDI") %>% 
  ggplot(aes(y=value,x=year)) +
  geom_line() +
  theme_tufte() +
  theme(text=element_text(family="")) +
  ggtitle("Foreign Direct Investment  (% GDP)") +
  facet_wrap(~country,nrow=1) + 
  labs(y ="",x="") +
  scale_y_continuous(breaks=c(2.5,7.5),
                     labels=scales::percent_format(accuracy=1,scale=1))

gdp <- wdi_data %>% 
  filter(!is.na(value),series=="GDP") %>% 
  ggplot(aes(y=value,x=year)) +
  geom_line() +
  theme_tufte() +
  theme(text=element_text(family="")) +
  ggtitle("Gross Domestic Product (2015 USD)") +
  facet_wrap(~country,nrow=1) + 
  labs(y ="",x="") + 
  scale_y_continuous(labels=scales::dollar_format(accuracy=1),breaks=c(2000,4000)) +
  scale_x_continuous(breaks=c(1960,1990,2010))

unemp <- wdi_data %>% 
  filter(!is.na(value),series=="Unemployment") %>% 
  ggplot(aes(y=value,x=year)) +
  geom_line() +
  theme_tufte() +
  theme(text=element_text(family="")) +
  ggtitle("Unemployment (%)") +
  facet_wrap(~country,nrow=1) + 
  labs(y ="",x="") + 
  geom_hline(yintercept=5, linetype=2) +
  scale_y_log10(labels=scales::percent_format(accuracy=1,scale=1)) +
  scale_x_continuous(breaks=c(1992,2010))

fdi / gdp / unemp + plot_annotation(caption = "Only unemployment series are available for Venezuela, and only GDP per capita and unemployment for the Ukraine.",
                                    theme=theme_tufte()) & theme(text=element_text(family=""),
                                                                 axis.text = element_text(size=7))

ggsave("wb_data.pdf",dpi=1200)



## ----vdem,fig.cap="Varieties of Democracy Political Indicators by Country"---------------------

# get a bunch of political statistics

vdem_data %>% 
  dplyr::select(country_name,year,`Democracy\nIndex`="v2x_polyarchy",
                `Party\nEntry`="v2psbars",
         `Executive\nCorruption`="v2exbribe",
          `Judicial\nCorruption`="v2jucorrdc") %>% 
  gather(key="series",value="value",-year,-country_name) %>% 
  filter(year>1949) %>% 
  mutate(series=factor(series,
                       levels=c("Democracy\nIndex",
                                "Party\nEntry",
                                "Executive\nCorruption",
                                "Judicial\nCorruption"))) %>% 
  ggplot(aes(y=value,x=year)) +
  geom_line() +
  theme_tufte() +
  theme(text=element_text(family=""),
        strip.text.y.left = element_text(angle = 0),
        strip.text = element_text(size=7)) +
  labs(y="",x="",
       caption="Index values derived from a statistical model of country expert ratings. Higher values indicate more\ndemocracy and more party entry for the top two indices and less corruption for the corruption indices.") +
  scale_y_continuous(position="right") +
  scale_x_continuous(breaks=c(1960,2010)) +
  facet_grid(rows=vars(series),cols=vars(country_name),
             scales="free",switch = "y")

ggsave("vdem_data.pdf",dpi=1200)



## ----regpol2,fig.cap='Reported Firm Political Activity in Regional Comparison'-----------------

# need to calculate relative proportions 

combine_long_prop <- combine_long_form %>% 
  distinct(Country,Q9,ResponseId,Q8) %>% 
  group_by(Country,Q9,Q8) %>% 
  summarize(count_resp=n()) %>% 
  group_by(Country) %>% 
  mutate(prop=count_resp/sum(count_resp))
  
egypt_prop <- ungroup(combine_long_prop) %>% 
  filter(Country=="Egypt I") %>% 
  select(orig_prop="prop",Q9,Q8) %>% 
  distinct

combine_long_prop <- left_join(combine_long_prop,egypt_prop,by=c("Q9","Q8"))

combine_long_form <- left_join(combine_long_form,combine_long_prop,by=c("Country","Q9","Q8"))


estimates <- combine_long_form %>% 
  dplyr::select(matches('Q52'),Country,imputed,ResponseId,Q14,prop,orig_prop,Q8) %>% 
  gather(activity,value,-Country,-imputed,-ResponseId,-Q14,-prop,-orig_prop,-Q8) %>% 
  filter(activity!='Q52_5',
         Q14=='Registered as a domestic company') %>% 
  mutate(activity=fct_recode(factor(activity),
                             `Contributed\n funds to\n campaigns`='Q52_1',
                             `Distributed\n campaign\n information\n to employees`='Q52_2',
                             `Instructed\n employees\n to vote for a\n certain candidate`='Q52_3',
                             `Hosted party\n rallies`='Q52_4'),
         Country=fct_relevel(Country,c("Morocco","Jordan",'Egypt II',"Egypt I",'Algeria','Tunisia',
                                       "Ukraine","Venezuela"))) %>% 
  ungroup

## bootstrapped percentile confidence interval
# standard bootstrap function required by boot
boot_fn <- function(d, i , myweights=NULL) {
  mean(d[i]*myweights)
}

estimates_sum <- parallel::mclapply(unique(estimates$Country), function(c) {
  
      lapply(unique(estimates$imputed), function(i) {
        
        lapply(unique(estimates$activity), function(a) {
          
              this_data <- filter(estimates,Country==c,imputed==i,activity==a)

          tibble(out_est=as.numeric(boot(this_data$value, boot_fn, R=1001, myweights=this_data$orig_prop/this_data$prop)$t),
               Country=c,
               imputed=i,
               activity=a)
          
        }) %>% bind_rows
        
      }) %>% bind_rows
  
},mc.cores=3) %>% bind_rows

library(ggrepel)

estimates_sum %>% 
  group_by(Country,activity) %>% 
  summarize(prop_est=median(out_est),
            upper=quantile(out_est,.95),
         lower=quantile(out_est,.05)) %>% 
  ungroup %>% 
  mutate(Country=recode(Country, `Venezuela, Bolivarian Republic of...`= "Venezuela")) %>% 
  ggplot(aes(y=prop_est,x=Country)) + 
  geom_text(aes(label=Country),size=2.2,vjust="top") +
  geom_linerange(aes(ymin=lower,ymax=upper),
                  position = position_dodge(width=.5),size=.8,alpha=.5,colour="red") +
 theme_minimal() +
  theme(panel.grid.major.x=element_blank(),
        axis.text = element_text(face='bold',size=6),
        axis.text.x = element_blank(),
        legend.text=element_text(face='bold',size=9),
        legend.position = 'bottom') +
  scale_y_continuous(labels=scales::percent) + xlab("") + 
  ylab("") + scale_colour_brewer(name='',palette='Dark2') +
  facet_wrap(~activity,scales="free_y") +
  scale_shape(name='') +
  labs(caption = str_wrap("Estimates weighted by firm size and proportion of managers vs. employees to match Egypt I sample. Uncertainty derived from bootstrapping over imputed datasets.",width = 100))


## ----prepare2,include=F------------------------------------------------------------------------

# need to calculate relative proportions 

estimates <- combine_long_form %>% 
  dplyr::select(matches('Q52'),Country,imputed,ResponseId,Q14,prop,orig_prop,Q8,
                Q36,Q38,Q33_1) %>% 
  gather(activity,value,-Country,-imputed,-ResponseId,-Q14,-prop,-orig_prop,-Q8,
         -Q36,-Q38,-Q33_1) %>% 
  filter(activity!='Q52_5',
         Q14=='Registered as a domestic company') %>% 
  mutate(activity=fct_recode(factor(activity),
                             `Contributed\n funds to\n campaigns`='Q52_1',
                             `Distributed\n campaign\n information\n to employees`='Q52_2',
                             `Instructed\n employees\n to vote for a\n certain candidate`='Q52_3',
                             `Hosted party\n rallies`='Q52_4'),
         Q36=factor(Q36,levels=c("Much less likely",
                                 "Less likely",
                                 "No change",
                                 "More likely",
                                 "Much more likely")),
         Q38=factor(Q38,levels=c("Large decrease",
                                 "Decrease",
                                 "No change",
                                 "Increase",
                                 "Large Increase")),
         Q33_1=as.numeric(as.character(Q33_1))) %>% 
  ungroup

## bootstrapped percentile confidence interval
# standard bootstrap function required by boot
boot_fn <- function(d, i , myweights=NULL) {
  mean(d[i]*myweights,na.rm=T)
}

estimates_extract <- parallel::mclapply(unique(estimates$Country), function(c) {
  
      lapply(unique(estimates$imputed), function(i) {
        
        lapply(unique(estimates$activity), function(a) {
          
          lapply(unique(estimates$Q36), function(v) {
          
              this_data <- filter(estimates,Country==c,imputed==i,activity==a,
                                  Q36==v)
              
              if(nrow(this_data)==0) {
                return(tibble(Country=c,
               imputed=i,
               value=v,
               activity=a,
               out_est=0))
                
              } else {
                return(tibble(out_est=as.numeric(boot(this_data$value, boot_fn, R=1001, myweights=this_data$orig_prop/this_data$prop)$t),
               Country=c,
               imputed=i,
               value=v,
               activity=a))
              }

          }) %>% bind_rows
          
        }) %>% bind_rows
        
      }) %>% bind_rows
  
},mc.cores=3) %>% bind_rows

estimates_bribe <- parallel::mclapply(unique(estimates$Country), function(c) {
  
      lapply(unique(estimates$imputed), function(i) {
        
        lapply(unique(estimates$activity), function(a) {
          
          lapply(unique(estimates$Q38), function(v) {
          
              this_data <- filter(estimates,Country==c,imputed==i,activity==a,
                                  Q38==v)
              
              if(nrow(this_data)==0) {
                return(tibble(Country=c,
               imputed=i,
               value=v,
               activity=a,
               out_est=0))
                
              } else {
                return(tibble(out_est=as.numeric(boot(this_data$value, boot_fn, R=1001, myweights=this_data$orig_prop/this_data$prop)$t),
               Country=c,
               imputed=i,
               value=v,
               activity=a))
              }

          }) %>% bind_rows
          
        }) %>% bind_rows
        
      }) %>% bind_rows
  
},mc.cores=3) %>% bind_rows

estimates_inspect <- parallel::mclapply(unique(estimates$Country), function(c) {
  
      lapply(unique(estimates$imputed), function(i) {
        
        lapply(unique(estimates$activity), function(a) {
          
          lapply(unique(estimates$value), function(v) {
          
              this_data <- filter(estimates,Country==c,imputed==i,activity==a,
                                  value==v)
              
              if(nrow(this_data)==0) {
                return(tibble(Country=c,
               imputed=i,
               value=v,
               activity=a,
               out_est=0))
                
              } else {
                return(tibble(out_est=as.numeric(boot(this_data$Q33_1, boot_fn, R=1001, myweights=this_data$orig_prop/this_data$prop)$t),
               Country=c,
               imputed=i,
               value=v,
               activity=a))
              }

          }) %>% bind_rows
          
        }) %>% bind_rows
        
      }) %>% bind_rows
  
},mc.cores=3) %>% bind_rows





## ----extract2,fig.cap="Political Activities by Whether Government Officials' Exploitation of Regulations Increased In Last Five Years",include=F----

estimates_extract %>% 
  group_by(activity) %>% 
  mutate(mean_est=mean(out_est),
         Country=recode(Country,
                        `Venezuela, Bolivarian Republic of...`="Venezuela")) %>% 
  filter(!is.na(value)) %>% 
  group_by(Country,activity,value) %>% 
  summarize(prop_est=median(out_est),
            upper=quantile(out_est,.95),
         lower=quantile(out_est,.05),
         mean_est=mean_est[1]) %>% 
  ggplot(aes(y=prop_est,x=value)) + 
  geom_pointrange(aes(ymin=lower,ymax=upper),size=.1) +
  #geom_hline(aes(yintercept=mean_est),linetype=3) +
    theme_tufte() +
  scale_color_viridis_d(option="C",name="") +
  theme(panel.grid.major.x=element_blank(),
        axis.text = element_text(face='bold',size=6),
        axis.text.x = element_text(angle=90),
        legend.text=element_text(face='bold',size=9),
        legend.position = 'bottom',
        text=element_text(family="")) +
  scale_y_continuous(labels=scales::percent) + xlab("") + 
  ylab("") + 
  facet_grid(rows=vars(activity),cols=vars(Country),scales="free_y") +
  scale_shape(name='') +
  labs(caption = str_wrap("Dotted line shows average level of corporate political activity per category across countries. Estimates weighted by firm size and proportion of managers vs. employees to match Egypt I sample. Uncertainty derived from bootstrapping over imputed datasets.",width = 100))



## ----bribes2,fig.cap="Political Activities by Answers to Percentage Paid of Sales in Bribes Increased Since the Arab Spring?"----

require(ggrepel)

estimates_bribe %>% 
  group_by(value,activity) %>% 
  mutate(mean_est=mean(out_est),
         Country=recode(Country,
                        `Venezuela, Bolivarian Republic of...`="Venezuela"),
         country_lab=ifelse(Country %in% c("Egypt I","Egypt II"),Country,"Other Countries")) %>% 
  group_by(Country,activity,value) %>% 
  summarize(prop_est=median(out_est),
            upper=quantile(out_est,.95),
         lower=quantile(out_est,.05),
         mean_est=mean_est[1],
         country_lab=country_lab[1]) %>% 
  ggplot(aes(y=prop_est,x=value)) + 
  geom_line(aes(group=Country),alpha=0.3) +
  # geom_pointrange(aes(ymin=lower,ymax=upper,colour=Country,shape=Country),
  #                 position = position_dodge(width=.5),size=.5) +
  geom_line(aes(y=mean_est,group=Country),colour="black",linetype=2) +
    theme_tufte() +
  scale_color_grey() +
  theme(panel.grid.major.x=element_blank(),
        axis.text = element_text(face='bold',size=6),
        axis.text.x = element_text(angle=90),
        legend.text=element_text(face='bold',size=9),
        legend.position = 'bottom') +
  theme(text=element_text(family="")) +
  scale_y_continuous(labels=scales::percent) + xlab("") + 
  #scale_x_discrete(expand=expansion(add=c(0,3))) +
  #geom_text_repel(aes(label=country_lab),size=2) +
  ylab("") + 
  facet_wrap(~activity,scales="free_y") +
  scale_shape(name='') +
  labs(caption = str_wrap("Dotted line shows average level of corporate political activity per category across countries. Estimates weighted by firm size and proportion of managers vs. employees to match Egypt I sample. Uncertainty derived from bootstrapping over imputed datasets.",width = 100))



## ----inspect2,fig.cap="Difference in Number of Inspections from Regulators for Politically-Active versus Politically-Inactive Companies"----

estimates_inspect %>% 
  mutate(value=factor(value,labels=c("Did Not Participate","Participated")),
         Country=recode(Country, 
                        `Venezuela, Bolivarian Republic of...`="Venezuela")) %>% 
  group_by(activity) %>% 
  mutate(mean_est=mean(out_est)) %>% 
  group_by(Country,activity) %>% 
  summarize(prop_est=median(out_est[value=="Participated"] - out_est[value=="Did Not Participate"]),
            upper=quantile(out_est[value=="Participated"] - out_est[value=="Did Not Participate"],.95),
         lower=quantile(out_est[value=="Participated"] - out_est[value=="Did Not Participate"],.05),
         mean_est=mean_est[1]) %>% 
  ggplot(aes(y=prop_est,x=Country)) + 
  geom_pointrange(aes(ymin=lower,ymax=upper,colour=Country),
                  position = position_dodge(width=.5),size=.25) +
  geom_hline(aes(yintercept=0),linetype=3) +
    theme_tufte() +
  scale_color_grey() +
  theme(panel.grid.major.x=element_blank(),
        axis.text = element_text(face='bold',size=6),
        axis.text.x = element_text(angle=90),
        legend.text=element_text(face='bold',size=9),
        legend.position = 'bottom') +
  theme(text=element_text(family="")) +
  scale_y_continuous(labels=scales::number_format(accuracy=1)) + xlab("") + 
  ylab("") + 
  facet_wrap(~activity,scales="free_y") +
  scale_shape(name='') +
  guides(color="none") +
  labs(caption = str_wrap("Dotted line shows zero, or no difference between politically active and inactive companies. Estimates weighted by firm size and proportion of managers vs. employees to match Egypt I sample. Uncertainty derived from bootstrapping over imputed datasets.",width = 100))



## ----data_edit2,include=F----------------------------------------------------------------------

combine_long_form <- mutate(combine_long_form,
                          Q33_1 = as.numeric(Q33_1),
                          Q8=as.numeric(factor(Q8,
                                                    levels=c("Less than 5",
                                                             "From 5 to 9",
                                                             "From 10 to 19",
                                                             "From 20 to 50",
                                                             "From 51 to 100",
                                                             "From 101 to 250",
                                                             "From 251 to 500",
                                                             "From 501 to 1000",
                                                             "1001 and over"),
                                                    labels=c("2.5",
                                                             "7.5",
                                                             "15",
                                                             "35",
                                                             "75",
                                                             "175",
                                                             "375",
                                                             "750",
                                                             "2000"))),
                          Q21_2=as.numeric(as.character(factor(Q21_2,
                                                  levels=c("More than 20% loss",
                                                           "Between 10 and 20% loss",
                                                           "Between 10 and 5% loss",
                                                           "Between 5 and 0% loss",
                                                           "Broke even",
                                                           "Between 0 and 5% profit margin",
                                                           "Between 5 and 10% profit margin",
                                                           "Between 10 and 20% profit margin",
                                                           "More than 20% profit margin"),
                                                  labels=c("-30",
                                                           "-15",
                                                           "-7.5",
                                                           "-2.5",
                                                           "0",
                                                           "2.5",
                                                           "7.5",
                                                           "15",
                                                           "30")))),
                          Q30_2=ordered(Q30_2),
                          Q28_2=ordered(Q28_2),
                          Q37=as.numeric(as.character(factor(Q37,levels=c("0%",
                                                             "Less than 1%",
                                                             "From 2% to less than 5%",
                                                             "From 5% to less than 10%",
                                                             "From 10% to less than 20%",
                                                             "From 20% to less than 30%",
                                                             "Over 30%"),
                                                labels=c("0","0.5","3.5","7.5","15","25","50")))),
                          Q74=ifelse(Q9=="Employee","No",as.character(Q74)),
                          Q38=ordered(Q38,levels=c("Large decrease",
                                                   "Decrease",
                                                   "No change",
                                                   "Increase",
                                                   "Large Increase")))



## ----logitPartisan2----------------------------------------------------------------------------

if(run_code) {

# fit one brms model so we don't have to always re-compile

fit_mod1 <- brm(formula=bf(new_scale ~ Q8 + Q13 + Q21_2 + Q8_1 + 
                           Q33_1  + mo(Q38)  + Q9,
                          decomp="QR"),
               data=distinct(select(filter(combine_long_form,imputed=="1",
                                                                                 Country=="Egypt I",
                                                                 Q14=="Registered as a domestic company"),
                                                                 Q8,Q13,Q21_2,Q8_1,
                                                                 Q30_2,Q28_2,Q33_1,Q37,Q9,new_scale,
                                                                 Q38)),
               chains=1,iter=1000,
               family="Normal",
               file="fit_mod1_brms.rds")

new_scale <- lapply(unique(combine_long_form$Country),
                         function(c) {
                           
  # load fitted model
  
  this_fit <- readRDS("fit_mod1_brms.rds")              
  
  
  over_imps1 <- parallel::mclapply(unique(combine_long_form$imputed), function(i) {
    
    
    this_fit <- update(this_fit,newdata=distinct(select(filter(combine_long_form,imputed==i,
                                                                                 Country==c,
                                                                 Q14=="Registered as a domestic company"),
                                                                 Q8,Q13,Q21_2,Q8_1,new_scale,
                                                                 Q30_2,Q28_2,Q33_1,Q37,Q9,outcome1="Q52_1",
                                                                 Q38)))
  },mc.cores=3)
  
  
  
  
  
  over_imps_sum1 <- lapply(over_imps1, function(d) {
      this_c <- as.matrix(d)
      out_d <- as_tibble(this_c)
      out_d$Country <- c
      out_d$R2 <- as.numeric(bayes_R2(d,summary=F))
      out_d$`N Obs` <- nrow(d$data)
      
      # get predictions and bind

      pred <- fitted(d,scale="linear",summary=F)
      colnames(pred) <- paste0(1:ncol(pred),"_outcome")

      out_d <- bind_cols(out_d,as_tibble(pred))
      
      return(out_d)
    }) %>% bind_rows
  
  return(over_imps_sum1)

  })

new_sum <- parallel::mclapply(new_scale, function(d) {
  
  d %>% 
  gather(key="Variable",value="Estimate",-Country,-matches("outcome")) %>% 
  rowwise(Country,Variable,Estimate) %>% 
  summarize(pred_dlogis=ifelse(!(Variable %in% c("lp__","R2","N Obs")),
                                 mean(Estimate*dlogis(c_across(matches("outcome"))),
                             na.rm=T),
                             Estimate)) %>% 
group_by(Country,Variable) %>% 
  summarize(estimate=round(mean(pred_dlogis,na.rm=T),3),
            High=round(quantile(pred_dlogis,.95,na.rm=T),3),
            Low=round(quantile(pred_dlogis,.05,na.rm=T),3)) %>% 
  mutate(interval=paste0("(",Low,", ",High,")"),
         estimate=ifelse(sign(High)==sign(Low),paste0(estimate,"*"),estimate)) %>% 
  select(-High,-Low) 
  
},mc.cores=3)
  
 new_sum <- new_sum %>%  
   bind_rows %>% 
  gather(key="Type",value="estimate",-Country,-Variable) %>% 
  spread(key = "Country",value="estimate") %>% 
  filter(!(grepl(x=Variable,pattern="simo|lp|E3"))) %>% 
  mutate(Variable=recode(Variable,
                         `b_Intercept`="Intercept",
                         `R2`="R$^2$",
                         `b_IQ37E2`="Percent Bribes Paid$^2$",
                         `b_Q13ConstructionDInvestmentinInfrastructureConstruction`="Construction",
                         `b_Q13FinanceDBankingDInsurance`="Finance",
                         `b_Q13IndustryDManufacturing`="Manufacturing",
                         `b_Q13Mining`="Mining",
                         `b_Q13ServiceDCommerce`="Services",
                         `b_Q21_2`="Firm Performance",
                         `b_Q26Yes`="Islamic Loans",
                         `bsp_moQ28_2`="Rank Military Customer",
                         `bsp_moQ30_2`="Rank Military Supplier",
                         `b_Q33_1`="No. Times Inspected",
                         `b_IQ33_1E2`="No. Times Inspected$^2$",
                         `b_Q37`="Percent Bribes Paid",
                         `b_Q8`="No. Firm Employees",
                         `b_Q8_1Yes`="Conglomerate",
                         `b_Q9Manager`="Manager",
                         bsp_moQ38="Bribes Increase"),
         Variable=factor(Variable,levels=c("Rank Military Customer",
                                           "Rank Military Supplier",
                                           "No. Times Inspected",
                                           "No. Times Inspected$^2$",
                                           "Percent Bribes Paid",
                                           "Percent Bribes Paid$^2$",
                                           "Bribes Increase",
                                           "Islamic Loans",
                                           "Conglomerate",
                                           "Firm Performance",
                                           "No. Firm Employees",
                                           "Manager",
                                           "Construction",
                                           "Finance",
                                           "Manufacturing",
                                           "Mining",
                                           "Services",
                                           "Intercept",
                                           "R$^2$",
                                           "N Obs"))) %>% 
  arrange(Variable,Type)

saveRDS(new_sum,'data/new_sum.rds')

} else {
  new_sum <- readRDS("data/new_sum.rds")
}

new_sum %>%  
  rename(Venezuela="Venezuela, Bolivarian Republic of...") %>% 
  select(-Type) %>% 
kable(booktabs = T,align = c("l","c","c","c"),longtable=T,
      caption="Covariates Predicting Historical Corporate Political Engagement",
      linesep = "",
      escape=F) %>%
  kable_styling(latex_options = c("hold_position"),
                font_size = 7) %>% 
    collapse_rows(columns=1:2,latex_hline="major") %>% 
  #row_spec(seq(from=1,to=nrow(fund_sum)*3-1,by=2),hline_after = F) %>% 
  row_spec(seq(from=2,to=nrow(new_sum),by=2),font_size = 6,italic = T,bold=T) %>% 
  #row_spec(nrow(fund_sum)-6,hline_after=T) %>% 
  footnote(general=c("Estimation of Bayesian logistic regression using Markov Chain Monte Carlo with Stan to handle issues of perfect separation. 5% - 95% quantile intervals in parentheses. The coefficients are sample average marginal effects expressed as the increase in probability of the given outcome for a 1-unit change in the regressor. The number of observations has an uncertainty interval due to imputation uncertainty in the number of domestic firms."),threeparttable = T)
  


## ----logitBribes2,include=F,eval=F-------------------------------------------------------------
## 
## if(run_code) {
## 
## # fit one brms model so we don't have to always re-compile
## 
## try(file.remove("fit_mod2.rds"))
## 
## fit_mod <- brm(formula=bf(Q38 ~ Q8 + Q13 + Q21_2 + Q8_1 +
##                                     mo(Q30_2) +  mo(Q28_2) +
##                            Q33_1 + I(Q33_1^2) + Q37+ I(Q37^2)   + Q9,
##                           decomp="QR"),
##                data=distinct(select(filter(combine_long_form,imputed==1,
##                                                                                  Country=="Egypt II",
##                                                                  Q14=="Registered as a domestic company"),
##                                                                  Q8,Q13,Q21_2,Q8_1,Q26,
##                                                                  Q30_2,Q28_2,Q33_1,Q37,Q9,outcome="Q52_3",
##                                                                  Q38)),
##                chains=1,iter=1000,
##                family=cumulative(threshold="flexible"),
##                file="fit_mod2.rds")
## 
## bribes <- lapply(unique(combine_long_form$Country),
##                          function(col) {
## 
##   # load fitted model
## 
##   this_fit <- readRDS("fit_mod2.rds")
## 
## 
##   over_imps <- lapply(unique(combine_long_form$imputed), function(i) {
## 
## 
##     this_fit <- update(this_fit,newdata=distinct(select(filter(combine_long_form,imputed==i,
##                                                                                  Country==col,
##                                                                  Q14=="Registered as a domestic company"),
##                                                                  Q8,Q13,Q21_2,Q8_1,Q26,
##                                                                  Q30_2,Q28_2,Q33_1,Q37,Q9,outcome="Q52_3",
##                                                                  Q38)))
##   })
## 
##   over_imps_sum <- lapply(over_imps, function(d) {
##       this_c <- as.matrix(d)
##       out_d <- as_tibble(this_c)
##       out_d$Country <- col
##       out_d$R2 <- as.numeric(bayes_R2(d,summary=F))
##       out_d$`N Obs` <- nrow(d$data)
##       return(out_d)
##     }) %>% bind_rows
## 
##   return(over_imps_sum)
## 
##   })
## 
## saveRDS(bribes,'data/obs_bribes_fit.rds')
## } else {
##   bribes <- readRDS("data/obs_bribes_fit.rds")
## }
## 
## bribes_sum <- bind_rows(bribes) %>%
##   gather(key="Variable",value="Estimate",-Country) %>%
##   group_by(Country,Variable) %>%
##   summarize(estimate=round(mean(Estimate),3),
##             High=round(quantile(Estimate,.95),3),
##             Low=round(quantile(Estimate,.05),3)) %>%
##   mutate(interval=paste0("(",Low,", ",High,")"),
##          estimate=ifelse(sign(High)==sign(Low),paste0(estimate,"*"),estimate)) %>%
##   select(-High,-Low) %>%
##   gather(key="Type",value="estimate",-Country,-Variable) %>%
##   spread(key = "Country",value="estimate") %>%
##   filter(!(grepl(x=Variable,pattern="simo|lp|E3|bsp|disc|Intercept"))) %>%
##   mutate(Variable=recode(Variable,
##                          `R2`="R$^2$",
##                          `b_IQ37E2`="Percent Bribes Paid$^2$",
##                          `b_Q13ConstructionDInvestmentinInfrastructureConstruction`="Construction",
##                          `b_Q13FinanceDBankingDInsurance`="Finance",
##                          `b_Q13IndustryDManufacturing`="Manufacturing",
##                          `b_Q13Mining`="Mining",
##                          `b_Q13ServiceDCommerce`="Services",
##                          `b_Q21_2`="Firm Performance",
##                          `b_Q26Yes`="Islamic Loans",
##                          `bsp_moQ28_2`="Rank Military Customer",
##                          `bsp_moQ30_2`="Rank Military Supplier",
##                          `b_Q33_1`="No. Times Inspected",
##                          `b_IQ33_1E2`="No. Times Inspected$^2$",
##                          `b_Q37`="Percent Bribes Paid",
##                          `b_Q8`="No. Firm Employees",
##                          `b_Q8_1Yes`="Conglomerate",
##                          `b_Q9Manager`="Manager"),
##          Variable=factor(Variable,levels=c("Rank Military Customer",
##                                            "Rank Military Supplier",
##                                            "No. Times Inspected",
##                                            "No. Times Inspected$^2$",
##                                            "Percent Bribes Paid",
##                                            "Percent Bribes Paid$^2$",
##                                            "Islamic Loans",
##                                            "Conglomerate",
##                                            "Firm Performance",
##                                            "No. Firm Employees",
##                                            "Manager",
##                                            "Construction",
##                                            "Finance",
##                                            "Manufacturing",
##                                            "Mining",
##                                            "Services",
##                                            "Intercept",
##                                            "R$^2$",
##                                            "N Obs"))) %>%
##   arrange(Variable,Type) %>%
##   select(-Type) %>%
##   group_by(Variable) %>%
##   mutate(Variable2=c(as.character(Variable[1]),"")) %>%
##   ungroup %>%
##   select(-Variable) %>%
##   select(Variable="Variable2",everything())
## 
## kable(bribes_sum,  booktabs = T,align = c("l","c","c","c"),
##       caption="Model Predicting Increase in Bribe Payments by Country",
##       linesep = "",
##       escape=F) %>%
##   kable_styling(latex_options = c("hold_position"),
##                 font_size = 8) %>%
##   row_spec(seq(from=1,to=nrow(bribes_sum)-1,by=2),hline_after = F) %>%
##   row_spec(seq(from=2,to=nrow(bribes_sum),by=2),font_size = 6,italic = T,bold=T) %>%
##   row_spec(nrow(bribes_sum)-7,hline_after=T) %>%
##   footnote(general=c("Estimation of Bayesian ordinal logistic regression using Markov Chain Monte Carlo with Stan to handle issues of perfect separation. 5% - 95% HPD intervals in parentheses. The number of observations has an uncertainty interval due to imputation uncertainty in the number of domestic firms."),threeparttable = T)
## 


## ----milsec2,fig.cap='Ties between Military-Linked Firms and Companies by Country'-------------


estimates <- combine_long_form %>% 
  filter(Q14=='Registered as a domestic company') %>% 
  mutate(Country=recode(Country,
                        `Venezuela, Bolivarian Republic of...`="Venezuela")) %>% 
  dplyr::select(matches('Q52'),Country,imputed,ResponseId,prop,orig_prop,Q13,Q28_2,Q30_2) %>% 
  mutate(Q28_2=as.numeric(Q28_2),
         Q30_2=as.numeric(Q30_2))

estimates_sum <- parallel::mclapply(unique(estimates$Country), function(col) {
  
      lapply(unique(estimates$imputed), function(i) {
        
        lapply(unique(estimates$Q13), function(a) {
          
              this_data <- filter(estimates,Country==col,imputed==i,Q13==a)

          tibble(out_est_cust=as.numeric(boot(this_data$Q28_2, boot_fn, R=1001, myweights=rep(1,nrow(this_data)))$t),
                 out_est_supp=as.numeric(boot(this_data$Q30_2, boot_fn, R=1001, myweights=rep(1,nrow(this_data)))$t),
               Country=col,
               imputed=i,
               sector=a)
          
        }) %>% bind_rows
        
      }) %>% bind_rows
  
},mc.cores=3) %>% bind_rows

estimates_sum %>% 
  gather(Relationship,value="Ranking",out_est_supp,out_est_cust) %>% 
  mutate(Relationship=recode(Relationship,out_est_supp="Supplier",
                     out_est_cust="Customer")) %>% 
  group_by(Country,sector,Relationship) %>% 
  summarize(prop_est=median(Ranking),
            upper=quantile(Ranking,.95),
         lower=quantile(Ranking,.05)) %>% 
  group_by(sector) %>% 
  mutate(mean_sec=mean(prop_est)) %>% 
  ungroup %>% 
  mutate(sector=fct_recode(sector,
                                         Service='Service/Commerce',
                                         Mining='Mining',
                                         Industry='Industry/Manufacturing',
                                         Finance='Finance/Banking/Insurance',
                                         Construction='Construction/Investment in Infrastructure Construction',
                                         Agriculture='Agriculture/Forestry/Aquaculture')) %>% 
  ggplot(aes(y=prop_est,x=reorder(Country,-prop_est))) + 
  geom_pointrange(size=.8,aes(ymin=lower,
                             ymax=upper,colour=Relationship,shape=Relationship),
                  position=position_dodge(width=.5)) +
  theme_minimal() +
  coord_flip() +
  scale_color_grey(name='Relationship') +
  scale_shape(name='Relationship') +
  ylab('Average Rank from 1 (Most Important) to 6 (Least Important)') +
  xlab('Firm Sector') +
  geom_hline(aes(yintercept=mean_sec),linetype=2) +
  #ggtitle('Ties between Military-Linked Firms and North African Companies') +
  theme(panel.grid.major.x= element_blank(),
        panel.grid.minor.x=element_blank(),
        strip.text = element_text(face='bold'),
        legend.position = 'bottom') +
  facet_wrap(~sector)


## ----amceRent2,fig.cap="Estimates for Rent Treatments (Appeal Types) by Dependent Variable"----

all_impute <- combine_long_form %>% 
  filter(!is.na(DV1) & !is.na(DV2) & !is.na(DV3)) %>% 
  mutate(Country=factor(Country),
         Country=recode(Country,
                        `Venezuela, Bolivarian Republic of...`="Venezuela"),
         actor_type=fct_collapse(actor_type,
                                 `Head of State`=c("President","Prime Minister"))) %>% 
  split(combine_long_form$imputed) 

if(run_code) {
  
  #need to modify all_impute to drop a few missing appeal type records

   Estimates_DV1 <- lapply(all_impute, mutate,appeal_type=na_if(appeal_type,"NA")) %>%
            lapply(filter,!is.na(appeal_type)) %>%
      impute_amce(num_cores=num_cores,
                      formula=new_scale~appeal_type*Country + Q9 + Q13 + Q14,
                     respondent.id = 'ResponseId',
                  respondent.varying="Country",
       baselines=list(appeal_type='Reforms',
                      Country='Tunisia'),
       keep_vars=c('new_scale','appeal_type','ResponseId','Country',"Q9","Q13","Q14"))

country_levels <- levels(all_impute[[1]]$Country)
all_appeals <-bind_rows(summary(Estimates_DV1)[3:10],.id='Country') %>% 
  mutate(upper=Estimate+`Std. Err`*1.96,
         lower=Estimate-`Std. Err`*1.96,
         Country=factor(Country,labels=summary(Estimates_DV1)$table_values_amce[,3]))

saveRDS(all_appeals,"data/country_appeals.rds")
} else {
  all_appeals <- readRDS("data/country_appeals.rds")
}

# all_appeals %>% create_table(caption='Conditional $AMCE$ for Figure 6: Country Heterogeneity in Effect of Institutions Offering Rent to Firm',
#                                       test_name='mil_country_int_actor')

all_appeals %>% 
    mutate(Level=recode(Level,Confiscate="Confiscate Income",
                            Control="Confiscate Company")) %>% 
   ggplot(aes(y=Estimate,
             x=reorder(Country,Estimate),
             ymin=lower,
             ymax=upper)) +
  geom_pointrange(position=position_dodge(width=.5),size=.3,alpha=.6) +
  theme_minimal() +
  ggtitle(str_wrap('On a Scale of 1 to 10, How Likely Do You Think It Is That Your CEO Will...',width=50)) +
  xlab('Offer Made to Company') +
  ylab("") +
  scale_y_continuous(breaks=c(-.5,-0.2,0,0.2,0.5),labels=c('Less\nLikely','-0.2','0','0.2','More\nLikely')) +
  scale_color_brewer(name='',palette='Dark2') +
  scale_shape(name='')  +
   theme(panel.grid=element_blank(),
         strip.text = element_text(face='bold'),
         legend.position = 'bottom',
         axis.text = element_text(size=8,face="bold")) +
  #ggtitle('Country Heterogeneity in Effect of Institutions Offering Rent to Firm') +
  facet_wrap(~Level) +
  geom_hline(yintercept=0,linetype=3) +
    coord_flip()

ggsave("country_all_appeal.png")



## ----amceInst2,fig.cap="Estimates for Political Actor Treatments by Dependent Variable"--------

if(run_code) {
  
  # need to modify all_impute to drop a few missing appeal type records
  
   Estimates_DV1 <- lapply(all_impute, mutate,actor_type=na_if(actor_type,"NA")) %>%
            lapply(filter,!is.na(actor_type)) %>%
      impute_amce(num_cores=num_cores,
                      formula=new_scale~actor_type*Country + Q9 + Q13 + Q14,
                     respondent.id = 'ResponseId',
                  respondent.varying="Country",
       baselines=list(actor_type='Government',
                      Country='Tunisia'),
       keep_vars=c('new_scale','actor_type','ResponseId','Country',"Q9","Q13","Q14"))

country_levels <- levels(all_impute[[1]]$Country)
all_actors <-bind_rows(summary(Estimates_DV1)[3:10],.id='Country') %>% 
  mutate(upper=Estimate+`Std. Err`*1.96,
         lower=Estimate-`Std. Err`*1.96,
         Country=factor(Country,labels=summary(Estimates_DV1)$table_values_amce[,3]))

saveRDS(all_actors,"data/country_actors.rds")
} else {
  all_actors <- readRDS("data/country_actors.rds")
}

# all_appeals %>% create_table(caption='Conditional $AMCE$ for Figure 6: Country Heterogeneity in Effect of Institutions Offering Rent to Firm',
#                                       test_name='mil_country_int_actor')

all_actors %>% 
   ggplot(aes(y=Estimate,
             x=reorder(Country,Estimate),
             ymin=lower,
             ymax=upper)) +
  geom_pointrange(position=position_dodge(width=.5),size=.3,alpha=.6) +
  theme_minimal() +
  ylab('') +
  xlab('Conjoint Treatment') +
  scale_y_continuous(breaks=c(-.2,-0.07,0,0.07,.2),labels=c('Less\nLikely','-0.05','0','0.05','More\nLikely')) +
  scale_color_brewer(name='',palette='Dark2') +
  scale_shape(name='')  +
   theme(panel.grid=element_blank(),
         strip.text = element_text(face='bold'),
         legend.position = 'bottom',
             axis.text=element_text(size=8,face="bold")) +
  ggtitle(str_wrap('On a Scale of 1 to 10, How Likely Do You Think It Is That Your CEO Will...',width=50)) +  facet_wrap(~DV) +
  geom_hline(yintercept=0,linetype=3) +
    coord_flip() +
  facet_wrap(~Level)

ggsave("country_actors.png")



## ----amceforeign,fig.cap="Rent Treatments by Firm Type"----------------------------------------

all_impute <- lapply(all_impute,mutate,export_rank=Q28_8,
                     foreign_firm_rank=Q28_5) %>% 
  lapply(mutate,foreign_markets=factor(as.numeric(foreign_firm_rank<3 | export_rank<3),
                                       labels=c("Non-exporter","Exporter")),
         Q14=factor(recode(Q14,
                    `Foreign firm in a joint-venture with an ${e://Field/country_adj}  private enterprise`="JV",
                    `Foreign firm in a joint-venture with an ${e://Field/country_adj} state-owned enterprise`="JV",
                    `Joint-venture with an ${e://Field/country_adj}  private enterprise`="JV",
                    `Joint-venture with an ${e://Field/country_adj} state-owned enterprise`="JV",
                    Other="Informal Domestic",
                    `Registered as a domestic company`="Formal Domestic",
                    `100% foreign-owned enterprise`="Foreign-owned",
                    `Unregistered domestic company`="Informal Domestic"))) 
  

if(run_code) {
  
  # need to modify all_impute to drop a few missing appeal type records
  
Estimates_DV1 <- all_impute %>% 
      lapply(filter,!is.na(Q14),appeal_type!="NA") %>% 
      impute_amce(num_cores=num_cores,
                      formula=new_scale~appeal_type*Q14,
                     respondent.id = 'ResponseId',
                  respondent.varying="Q14",
       baselines=list(appeal_type="Reforms"),
       keep_vars=c('new_scale','appeal_type','ResponseId','Country',"Q9","Q13","Q14",
                   "foreign_firm_rank"))

country_levels <- levels(all_impute[[1]]$Country)
all_actors <- bind_rows(summary(Estimates_DV1)[3:7],.id="Type") %>% 
  mutate(upper=Estimate+`Std. Err`*1.96,
         lower=Estimate-`Std. Err`*1.96,
         Type=factor(Type,labels=summary(Estimates_DV1)$table_values_amce[,3]))

saveRDS(all_actors,"data/type_int.rds")
} else {
  all_actors <- readRDS("data/type_int.rds")
}

# all_appeals %>% create_table(caption='Conditional $AMCE$ for Figure 6: Country Heterogeneity in Effect of Institutions Offering Rent to Firm',
#                                       test_name='mil_country_int_actor')

all_actors %>% 
    mutate(Level=recode(Level,Confiscate="Confiscate Income",
                            Control="Confiscate Company")) %>% 
   ggplot(aes(y=Estimate,
             x=reorder(Type,Estimate),
             ymin=lower,
             ymax=upper)) +
  geom_pointrange(position=position_dodge(width=.5),size=.3,alpha=.6) +
  theme_minimal() +
  ggtitle(str_wrap('On a Scale of 1 to 10, How Likely Do You Think It Is That Your CEO Will...',width=50)) +
  ylab("") +
  xlab('') +
  scale_y_continuous(breaks=c(-.5,-0.2,0,0.2,.5),labels=c('Less\nLikely','-0.2','0','0.2','More\nLikely')) +
  scale_color_brewer(name='',palette='Dark2') +
  scale_shape(name='')  +
   theme(panel.grid=element_blank(),
         strip.text = element_text(face='bold'),
         legend.position = 'bottom',
             axis.text=element_text(size=8,face="bold")) +
  #ggtitle('Country Heterogeneity in Effect of Institutions Offering Rent to Firm') +
  facet_wrap(~Level) +
  geom_hline(yintercept=0,linetype=3) +
    coord_flip()

ggsave("all_appeals_type.png")



## ----Estimatesector,fig.cap="Estimates for Military Actor Treatments by Beliefs About Corruption"----

all_impute <- all_impute %>% 
  lapply(mutate,
         Q13=factor(recode(Q13,
                    `Agriculture/Forestry/Aquaculture`="Agriculture",
                    `Construction/Investment in Infrastructure Construction`="Construction",
                    `Finance/Banking/Insurance`="Finance",
                    `Industry/Manufacturing`="Manufacturing",
                    `Service/Commerce`="Services"))) 
  

if(run_code) {
  
  # need to modify all_impute to drop a few missing appeal type records
  
Estimates_DV1 <- all_impute %>% 
      lapply(filter,!is.na(Q35),appeal_type!="NA") %>% 
      impute_amce(num_cores=num_cores,
                      formula=new_scale~appeal_type*Q35,
                     respondent.id = 'ResponseId',
                  respondent.varying="Q35",
       baselines=list(appeal_type="Reforms"),
       keep_vars=c('new_scale','appeal_type','ResponseId','Country',"Q9","Q13","Q14","Q35",
                   "foreign_firm_rank"))

country_levels <- levels(all_impute[[1]]$Country)
all_actors <- bind_rows(summary(Estimates_DV1)[3:6],.id="Type") %>% 
  mutate(upper=Estimate+`Std. Err`*1.96,
         lower=Estimate-`Std. Err`*1.96,
         Type=factor(Type,labels=summary(Estimates_DV1)$table_values_amce[,3]))

saveRDS(all_actors,"data/extract_int.rds")
} else {
  all_actors <- readRDS("data/extract_int.rds")
}

# all_appeals %>% create_table(caption='Conditional $AMCE$ for Figure 6: Country Heterogeneity in Effect of Institutions Offering Rent to Firm',
#                                       test_name='mil_country_int_actor')

all_actors %>% 
  mutate(Type=factor(Type,levels=c("Strongly Disagree","Disagree","Agree","Strongly Agree"))) %>% 
    mutate(Level=recode(Level,Confiscate="Confiscate Income",
                            Control="Confiscate Company")) %>% 
   ggplot(aes(y=Estimate,
             x=Type,
             ymin=lower,
             ymax=upper)) +
  geom_pointrange(position=position_dodge(width=.5),size=.3,alpha=.6) +
  theme_minimal() +
  ggtitle(str_wrap('Do you agree with the following statement? "Government officials use compliance with local regulations to extract informal payments from businesses like ours."',width=50)) +
  ylab("") +
  xlab('') +
  scale_y_continuous(breaks=c(-.5,-0.2,0,0.2,.5),labels=c('Less\nLikely','-0.2','0','0.2','More\nLikely')) +
  scale_color_brewer(name='',palette='Dark2') +
  scale_shape(name='')  +
   theme(panel.grid=element_blank(),
         strip.text = element_text(face='bold'),
         legend.position = 'bottom',
             axis.text=element_text(size=8,face="bold")) +
  #ggtitle('Country Heterogeneity in Effect of Institutions Offering Rent to Firm') +
  facet_wrap(~Level) +
  geom_hline(yintercept=0,linetype=3) +
    coord_flip()

ggsave("all_appeals_extract.png")



## ----countryint2,fig.cap="Country-level Intercepts Across All Three Experimental Outcomes",fig.height=7----

if(run_code) {
  Estimates_DV1 <- all_impute %>% 
    lapply(filter,!is.na(actor_type)) %>% 
    impute_amce(
                         num_cores=num_cores,
                      formula=new_scale~actor_type +  appeal_type + Country + Q9 + Q13 + Q14,
                     respondent.id = 'ResponseId',
       baselines=list(appeal_type='Reforms',
                      actor_type='Government',
                      Country='Tunisia'),
       keep_vars=c('DV1','actor_type','new_scale','appeal_type','ResponseId','Country',"Q9","Q13","Q14"))

all_countryint <- summary(Estimates_DV1)$amce %>% 
  mutate(upper=Estimate+`Std. Err`*1.96,
         lower=Estimate-`Std. Err`*1.96,
         Level=fct_relevel(factor(Level),
                                        c('Reforms',
                                          'Confiscate',
                                          'Control',
                                          'Export',
                                          'Import',
                                          'Contracts',
                                          'Licenses')))
  saveRDS(all_countryint,"data/country_across.rds")
} else {
  all_countryint <- readRDS("data/country_across.rds")
}

all_countryint %>%
  filter(Attribute=='Country') %>% 
  ggplot(aes(y=Estimate,
             x=reorder(Level,Estimate),
             ymin=lower,
             ymax=upper)) +
  geom_pointrange(position=position_dodge(width=.5)) +
  theme_minimal() +
  ylab('On a Scale of 1 to 10, How Likely Do You Think It Is That Your CEO Will...') +
  scale_color_brewer(palette='Dark2') +
  scale_y_continuous(breaks=c(-.5,-0.25,0,0.5,.75),labels=c('Less\nLikely','-0.25','0','0.5','More\nLikely')) +
   theme(panel.grid=element_blank(),
         strip.text = element_text(face='bold'),
         axis.text = element_text(face='bold'),
         legend.position = 'bottom') +
  xlab("") +
  #ggtitle("Country-level Intercepts Across All Three Experimental Outcomes") +
  geom_hline(yintercept=0,linetype=3) +
  coord_flip() +
  annotate('text',x=1.5,y=-.015,label='Baseline:   Tunisia')


