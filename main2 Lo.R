

library(rworldmap)
library(sp)
##############################
##############################
#### CARICAMENTO FUNZIONI ####
##############################
##############################

#source(file.choose())

###########################
###########################
#### IMPORTAZIONE DATI ####
###########################
###########################

# Dataset relativo ai terremoti dal 1970 al 2025
# Fonte: https://earthquake.usgs.gov/earthquakes/search/

# Variabili (https://earthquake.usgs.gov/data/comcat/#event-terms)
#   - time: istante in cui si è verificato l'evento
#   - latitude: latitudine dell'evento
#   - longitude: longitudine dell'evento
#   - depth: profondità dell'evento in km
#   - mag: magnitudo dell'evento (è una misura logaritmica)
#   - magType: metodo utilizzato per calcolare la magnitudo dell'evento (https://www.usgs.gov/programs/earthquake-hazards/magnitude-types)
#   - nst: numero di stazioni sismiche utilizzate per determinare la posizione dell'evento
#   - gap: il più grande divario azimutale, in gradi, tra stazioni azimutalmente adiacenti (se >180 gradi, allora grande incertezza su profondità e posizione rilevate)
#   - dmin: distanza dell'epicentro dalla stazione sismica più vicina (valore più piccolo, maggiore affidabilità nella misurazione della profondità)
#   - rms: tempo di percorrenza in secondi (misura che fornisce l'adattamento dei tempi di arrivo effettivi a quelli previsti; più è picccola, migliore è l'adattamento)
#   - net: identifica il network "preferito" come fonte di dati
#   - id: codice univoco che identifica l'evento
#   - updated: istante più recente in cui l'evento è stato aggiornato
#   - place: descrizione testuale della regione geografica più vicina all'evento
#   - type: tipo di evento sismico
#   - horizontalError: incertezza sulla posizione segnalata in km
#   - depthError: incertezza sulla profondità segnalata in km
#   - magError: incertezza sulla magnitudo segnalata (l'errore standard stimato della magnitudo)
#   - magNst: numero di stazioni sismiche utilizzate per calcolare la magnitudo dell'evento
#   - status: indica se la misurazione dell'evento è stata rivista da un essere umano
#   - locationSource: network che ha originariamente creato il luogo segnalato dell'evento
#   - magSource: network che ha originariamente creato la magnitudo segnalata dell'evento

#db = read.table(file.choose(), header=T, sep=",", dec=".")
#attach(db)

################################
################################
#### STUDIO INIZIALE DEL DB ####
################################
################################

dim(db); colnames(db); rownames(db)[1:10]
head(db); tail(db)
str(db)

#### Tipologia delle variabili e numero di missing ####
cbind(tipo = sapply(db, function (x){class(x)}),
      n_missing = sapply(db, function (x){sum(is.na(x))}))
#colSums(is.na(db))

#### Distribuzione di frequenze (assolute e percentuali) delle variabili categoriche ####
sapply(db[,c("magType","net","type","status","locationSource","magSource")], function (x){table(x)})
sapply(db[,c("magType","net","type","status","locationSource","magSource")], function (x){round(prop.table(table(x)),6)*100})

#### Statistiche descrittive delle variabili quantitative ####
summary(db[,sapply(db, function (x){class(x)})!="character"])
library(psych)
describe(db[,sapply(db, function (x){class(x)})!="character"])

############################
############################
#### MANIPOLAZIONE DATI ####
############################
############################

#### Creazione di nuove variabili ####
library(dplyr)
library(lubridate)
db_analysis = mutate(db, date_event = as.Date(substr(time,1,10)),
                         year_event = year(date_event),
                         magType_group = ifelse(magType=="mb","mb",
                                          ifelse(magType=="mw", "mw",
                                            ifelse(magType=="mwc", "mwc",
                                              ifelse(magType=="mww", "mww", "other")))),
                         nst_class = factor(ifelse(nst>=0 & nst<=100, 1,
                                              ifelse(nst>=101 & nst<=200, 2, 3)),
                                            labels=c("0-100","101-200",">200")),
                         magNst_class = factor(ifelse(magNst>=0 & magNst<=20, 1,
                                                ifelse(magNst>=21 & magNst<=50, 2, 3)),
                                               labels=c("0-20","21-50",">50"))
                     )
db_analysis$continent = coordinates_continent(data.frame(as.integer(db$longitude),as.integer(db$latitude)))
db_analysis$continent = recode(db_analysis$continent,"South America and the Caribbean"="South America")
db_analysis$country = coordinates_country(data.frame(as.integer(db$longitude),as.integer(db$latitude)))

#### Eliminazione delle variabili non di interesse ####
db_analysis = select(db_analysis,-c(time,net,locationSource,magSource,rms,updated))

#### Ridenominazione delle variabili ####
#db_analysis = rename(db_analysis, new_name = old_name)

#### Selezione di un sottoinsieme di osservazioni ####
db_analysis = filter(db_analysis, type=="earthquake" & continent!="<NA>" & continent!="Antarctica")

#############################
#############################
#### ANALISI DESCRITTIVA ####
#############################
#############################

##########################
# Variabili quantitative #
##########################
stat_quant = db_analysis %>% select(c("depth","mag","nst","magNst","gap","dmin","horizontalError","depthError","magError"))

cbind(n = sapply(stat_quant,function(x){sum(!is.na(x))}),
      n_missing = sapply(stat_quant,function(x){sum(is.na(x))}),
      min = apply(stat_quant,MARGIN=2,min,na.rm=T),
      max = apply(stat_quant,MARGIN=2,max,na.rm=T),
      mean = apply(stat_quant,MARGIN=2,mean,na.rm=T),
      median = apply(stat_quant,MARGIN=2,median,na.rm=T),
      sd = apply(stat_quant,MARGIN=2,sd,na.rm=T))

describe(stat_quant)[,c("n","mean","sd","median","min","max","range")]
#summary(stat_quant)

cbind(pearson = cor(stat_quant$depth, stat_quant$mag, method="pearson"),
      kendall = cor(stat_quant$depth, stat_quant$mag, method="kendall"),
      spearman = cor(stat_quant$depth, stat_quant$mag, method="spearman"))
round(cor(stat_quant, method="pearson", use="pairwise.complete.obs"),4)
round(cor(stat_quant, method="pearson", use="complete.obs"),4)

rm(stat_quant)

#########################
# Variabili categoriche #
#########################
stat_cat = db_analysis %>% select(c("status","magType_group","continent","nst_class","magNst_class"))

cbind(n = sapply(stat_cat,function(x){sum(!is.na(x))}),
      n_missing = sapply(stat_cat,function(x){sum(is.na(x))}))

arrange(as.data.frame(cbind(freq_ass = table(stat_cat$status),
                            freq_perc = round(prop.table(table(stat_cat$status)),4)*100)),
        desc(freq_ass))

arrange(as.data.frame(cbind(freq_ass = table(stat_cat$magType_group),
                            freq_perc = round(prop.table(table(stat_cat$magType_group)),4)*100)),
        desc(freq_ass))

arrange(as.data.frame(cbind(freq_ass = table(droplevels(stat_cat$continent)),
                            freq_perc = round(prop.table(table(droplevels(stat_cat$continent))),4)*100)),
        desc(freq_ass))

cbind(freq_ass = table(stat_cat$nst_class),
      freq_perc = round(prop.table(table(stat_cat$nst_class)),4)*100)

cbind(freq_ass = table(stat_cat$magNst_class),
      freq_perc = round(prop.table(table(stat_cat$magNst)),4)*100)

rm(stat_cat)

########################################
# Variabili quantitative e categoriche #
########################################
#db_analysis %>% group_by(status) %>% summarise(min=min(mag), max=max(mag), mean=mean(mag), median=median(mag), sd=sd(mag))
db_analysis %>% group_by(magType_group) %>% summarise(min=min(mag), max=max(mag), mean=mean(mag), median=median(mag), sd=sd(mag))
db_analysis %>% group_by(continent) %>% summarise(min=min(mag), max=max(mag), mean=mean(mag), median=median(mag), sd=sd(mag))
db_analysis %>% group_by(nst_class) %>% summarise(min=min(mag), max=max(mag), mean=mean(mag), median=median(mag), sd=sd(mag))
db_analysis %>% group_by(magNst_class) %>% summarise(min=min(mag), max=max(mag), mean=mean(mag), median=median(mag), sd=sd(mag))

############################
############################
#### GRAFICI UNIVARIATI ####
############################
############################

library(ggplot2)
library(scales)

###########
# Barplot #
###########
data_barplot = db_analysis %>% count(magType_group) %>%
                               mutate(perc = n/sum(n),
                                      perc_l = paste0(round(perc*100,2),"%"))

ggplot(data_barplot, aes(x=reorder(magType_group,perc), y=perc)) + 
  geom_bar(stat="identity", fill="lightblue", color="darkblue") +
  geom_text(aes(label=perc_l), vjust=1.6, size=3.5) +
  scale_y_continuous(labels=scales::percent) +
  theme_minimal() +
  labs(title="BARPLOT per tipo di calcolo della magnitudo",
       caption="https://www.usgs.gov/programs/earthquake-hazards/magnitude-types",
       x="Tipo di calcolo della magnitudo", 
       y="Percentuale")

#ggplot(db_analysis, aes(x=magType_group)) + 
#  geom_bar(fill="lightblue", color="darkblue")

rm(data_barplot)

#############
# Pie chart #
#############
data_piechart = db_analysis %>% count(continent) %>%
                                arrange(desc(continent)) %>%
                                mutate(perc = round(n/sum(n)*100, 2),
                                ypos_l = cumsum(perc)-0.5*perc,
                                perc_l = paste0(round(n/sum(n)*100, 2),"%"))

ggplot(data_piechart, aes(x="", y=perc, fill=continent)) +
  geom_bar(stat="identity", width=1, color="black") +
  geom_text(aes(y=ypos_l, label=perc_l), color="black", size=3.5) +
  coord_polar("y", start=0, direction=-1) +
  theme_void() +
  scale_fill_brewer(palette="Set2") +
  labs(title="PIE CHART di continente",
       fill="Continente")

#data_piechart = round(prop.table(table(droplevels(db_analysis$continent)))*100,2)
#pie(data_piechart, labels=paste(names(data_piechart), " ", data_piechart, "%", sep=""), main="PIE CHART per continente")

rm(data_piechart)

###########
# Treemap #
###########
library(treemapify)
data_treemap = db_analysis %>% count(magType)

ggplot(data_treemap, aes(fill=magType, area=n, label=magType)) +
  geom_treemap() + 
  geom_treemap_text(colour="black", place="centre", size=12) +
  theme(legend.position="none") +
# scale_fill_brewer(palette="Set2") +
  labs(title="TREEMAP per tipo di calcolo della magnitudo")

rm(data_treemap)

##############################################
# Istogramma e Funzione di densità di kernel #
##############################################
hist = ggplot(db_analysis, aes(x=mag)) +
          geom_histogram(fill="lightblue", color="white", bins=40) +
          theme_minimal() +
          labs(title="ISTOGRAMMA di magnitudo",
               subtitle="Numero di bins = 40",
               x="Magnitudo", 
               y="Frequenza")

kernel =  ggplot(db_analysis, aes(x=mag)) +
            geom_density(fill="lightblue", bw=bw.nrd0(db_analysis$mag)) +
            theme_minimal() +
            labs(title="DENSITÀ DI KERNEL di magnitudo",
                 subtitle="Stimata con parametro di smoothing ottimale",
                 x="Magnitudo", 
                 y="Densità")

library(gridExtra)
grid.arrange(hist, kernel, nrow=1)

rm(hist, kernel)

###########
# Dotplot #
###########
# Da non fare perché n troppo grande (consigliato quando n<100)
#ggplot(db_analysis, aes(x=mag)) +
#  geom_dotplot(fill="lightblue", color="darkblue")

###########
# Boxplot #
###########
dev.new()
par(mfrow=c(2,1))

boxplot(db_analysis$mag, col="lightblue", border="darkblue", horizontal=TRUE,
        main="BOXPLOT di magnitudo", xlab="Magnitudo")
abline(v=mean(db_analysis$mag), lty=2, col="darkred")

boxplot(db_analysis$depth, col="lightblue", border="darkblue", horizontal=TRUE,
        main="BOXPLOT di profondità", xlab="Profondità")
abline(v=mean(db_analysis$depth), lty=2, col="darkred")

dev.off()

###########################
###########################
#### GRAFICI BIVARIATI ####
###########################
###########################

###################
# Stacked barplot #
###################
data_stacked = db_analysis %>% group_by(continent, magType_group) %>%
                               summarize(n=n()) %>% 
                               mutate(perc=round(n/sum(n),4), perc_l=scales::percent(perc))

ggplot(data_stacked, aes(x=continent, y=perc, fill=magType_group)) + 
  geom_bar(stat="identity", position="fill") +
  scale_y_continuous(breaks=seq(0, 1, .1), label=percent) +
  geom_text(aes(label=perc_l), size=2.8, position=position_stack(vjust=0.5)) +
  theme_minimal() +
  scale_fill_brewer(palette="PuBuGn") +
  labs(title="STACKED BARPLOT per continente e tipo di calcolo della magnitudo",
       y="Percentuale",
       x="Continente",
       fill="")

#ggplot(db_analysis, aes(x=continent, fill=magType_group)) + 
#  geom_bar(position="stack")

rm(data_stacked)

###################
# Grouped barplot #
###################
data_grouped = db_analysis %>% group_by(continent, magType_group) %>%
                               summarize(n=n()) %>% 
                               mutate(perc=round(n/sum(n),4))

ggplot(data_grouped, aes(x=continent, y=perc, fill=magType_group)) + 
  geom_bar(stat="identity", position=position_dodge(preserve="single")) +
  scale_y_continuous(breaks=seq(0, 1, .1), label=percent) +
  theme_minimal() +
  scale_fill_brewer(palette="Set3") +
  labs(title="GROUPED BARPLOT per continente e tipo di calcolo della magnitudo",
       y="Percentuale",
       x="Continente",
       fill="")

#ggplot(db_analysis, aes(x=continent, fill=magType_group)) + 
#  geom_bar(position=position_dodge(preserve="single"))

rm(data_grouped)

#############
# Line plot #
#############
data_lineplot = db_analysis %>% group_by(year_event) %>%
                                summarise(mean=mean(mag), max=max(mag))

lineplot_mean = ggplot(data_lineplot, aes(x=year_event, y=mean)) +
                  geom_line(linewidth=1.25, color="lightgrey") +
                  geom_point(size=3, color="steelblue") +
                  scale_x_continuous(breaks=seq(min(data_lineplot$year_event), max(data_lineplot$year_event), 5),
                                     limits=c(min(data_lineplot$year_event), max(data_lineplot$year_event))) +
                  theme_minimal() +
                  labs(title="LINE PLOT della magnitudo media",
                       subtitle="1970 - 2024",
                       x="Anno", 
                       y="Magnitudo media")

lineplot_max = ggplot(data_lineplot, aes(x=year_event, y=max)) +
                  geom_line(linewidth=1.25, color="lightgrey") +
                  geom_point(size=3, color="steelblue") +
                  scale_x_continuous(breaks=seq(min(data_lineplot$year_event), max(data_lineplot$year_event), 5),
                                     limits=c(min(data_lineplot$year_event), max(data_lineplot$year_event))) +
                  theme_minimal() +
                  labs(title="LINE PLOT della magnitudo massima",
                       subtitle="1970 - 2024",
                       x="Anno", 
                       y="Magnitudo massima")

grid.arrange(lineplot_mean, lineplot_max, nrow=2)

rm(data_lineplot, lineplot_mean, lineplot_max)

#########################
# Boxplot e Violin plot #
#########################
#### Boxplot ####
ggplot(db_analysis, aes(x=droplevels(continent), y=mag)) +
  geom_boxplot(fill="lightblue", alpha=0.5, notch=F) +
  theme_minimal() +
  labs(title="BOXPLOT di magnitudo per continente",
       x="Continente", 
       y="Magnitudo")

#boxplot(db_analysis$mag~droplevels(db_analysis$continent))

#### Violin plot ####
ggplot(db_analysis, aes(x=droplevels(continent), y=mag)) +
  geom_violin(fill="lightblue", color="darkblue") +
  theme_minimal() +
  labs(title="VIOLIN PLOT di magnitudo per continente",
       x="Continente", 
       y="Magnitudo")

#### Boxplot + Violin plot ####
ggplot(db_analysis, aes(x=droplevels(continent), y=mag)) +
  geom_violin(fill="lightblue", color="darkblue") +
  geom_boxplot(width=0.15, size=0.8, fill="red",
               outlier.color="red", outlier.size=2, outlier.shape=1) +
  theme_minimal() +
  labs(title="BOXPLOT + VIOLIN PLOT di magnitudo per continente",
       x="Continente", 
       y="Magnitudo")

######################
# Beeswarm-syle plot #
######################
# Informazioni simili al Violin plot
#library(ggbeeswarm)
#ggplot(db_analysis, aes(x=droplevels(continent), y=mag)) +
#  geom_quasirandom(color="cornflowerblue", alpha=0.2) +
#  theme_minimal() +
#  labs(title="BEESWARM-SYLE PLOT di magnitudo per continente",
#       x="Magnitudo", 
#       y="Continente") 
  
###################
# Ridgeline graph #
###################
library(ggridges)
ridge_1 = ggplot(db_analysis, aes(x=mag, y=continent, fill=continent)) +
            geom_density_ridges() + 
            theme_ridges() +
            theme(legend.position="none") +
            labs(title="RIDGELINE GRAPH",
                 subtitle="Per continente",
                 x="Magnitudo", 
                 y="Continente")

data_ridge_2a = db_analysis %>% filter(continent=="South America")
tab_ridge_2 = data.frame(table(droplevels(data_ridge_2a$country))) %>% filter(Freq>=30) %>%
                                                                      rename(country=Var1)
data_ridge_2b = inner_join(data_ridge_2a, tab_ridge_2, by="country")

ridge_2 = ggplot(data_ridge_2b, aes(x=mag, y=country, fill=country)) +
  geom_density_ridges() + 
  theme_ridges() +
  theme(legend.position="none") +
  labs(title="",
       subtitle="Per stato del Sudamerica",
       x="Magnitudo", 
       y="Stato")

grid.arrange(ridge_1, ridge_2, nrow=1)

rm(ridge_1, ridge_2, data_ridge_2a, data_ridge_2b, tab_ridge_2)

##############
# Strip plot #
##############
ggplot(db_analysis, aes(y=continent, x=mag)) +
  geom_point(color="cornflowerblue", alpha = 0.2) +
  theme_minimal() +
  labs(title="STRIP PLOT di magnitudo per continente",
       x="Magnitudo", 
       y="Continente")   

##################
# Cleveland plot #
##################
data_cleveland = db_analysis %>% group_by(continent) %>%
                                 summarise(max=max(mag))

cleveland_1 = ggplot(data_cleveland, aes(x=max, y=reorder(continent,max))) +
                geom_point(color="darkblue", size = 2.5) +
                geom_segment(aes(x=min(db_analysis$mag), xend=max,
                                 y=reorder(continent,max), yend=reorder(continent,max)),
                                 color="lightblue") +
                theme_minimal() + 
                theme(panel.grid.major=element_blank(),
                      panel.grid.minor=element_blank()) +
                labs(title="CLEVELAND PLOT di magnitudo massima",
                     subtitle="Per continente",
                     x="Magnitudo", 
                     y="") 

data_cleveland_2a = db_analysis %>% filter(continent=="South America")
tab_cleveland_2 = data.frame(table(droplevels(data_cleveland_2a$country))) %>%
                      filter(Freq>=30) %>% rename(country=Var1)
data_cleveland_2b = inner_join(data_cleveland_2a, tab_cleveland_2, by="country") %>%
                        group_by(country) %>% summarise(max=max(mag))

cleveland_2 = ggplot(data_cleveland_2b, aes(x=max, y=reorder(country,max))) +
                  geom_point(color="darkblue", size = 2.5) +
                  geom_segment(aes(x=min(db_analysis$mag), xend=max,
                                   y=reorder(country,max), yend=reorder(country,max)),
                               color="lightblue") +
                  theme_minimal() + 
                  theme(panel.grid.major=element_blank(),
                        panel.grid.minor=element_blank()) +
                  labs(title="",
                       subtitle="Per stato del Sudamerica",
                       x="Magnitudo", 
                       y="")

grid.arrange(cleveland_1, cleveland_2, nrow=1)

rm(data_cleveland, cleveland_1, cleveland_2, data_cleveland_2a, data_cleveland_2b, tab_cleveland_2)

###############
# Scatterplot #
###############
ggplot(db_analysis, aes(x=depth, y=mag)) +
  geom_point(color="lightblue", alpha=0.8, size=2) +
  theme_minimal() +
  labs(title="SCATTERPLOT",
       subtitle="Profondità vs Magnitudo",
       x="Profondità",
       y="Magnitudo")

ggplot(db_analysis, aes(x=dmin, y=horizontalError)) +
  geom_point(color="lightblue", alpha=0.8, size=2) +
  geom_smooth(se=F, color="cornflowerblue") +
  scale_y_continuous(limits=c(0,20)) +
  scale_x_continuous(breaks=seq(0,23,5), limits=c(0,23)) +
  theme_minimal() +
  labs(title="SCATTERPLOT",
       subtitle="Distanza dalla stazione più vicina vs Errore sul rilevamento della posizione",
       x="Distanza dalla stazione",
       y="Errore posizione")

##############################
##############################
#### GRAFICI MULTIVARIATI ####
##############################
##############################

########################################################################
# Istogramma e Funzione di densità di kernel con variabili categoriche #
########################################################################
#### Istogramma ####
ggplot(db_analysis, aes(x=mag)) +
  geom_histogram(fill="lightblue", color="white", bins=40) +
  theme_minimal() +
  facet_wrap(~continent, ncol=1) +
# facet_grid(continent ~ magType_group) +
  labs(title="ISTOGRAMMI di magnitudo per continente",
       subtitle="Numero di bins = 40",
       x="Magnitudo", 
       y="Frequenza")

#### Funzione di densità di kernel ####
ggplot(db_analysis, aes(x=mag, fill=continent)) +
  geom_density(bw=bw.nrd0(db_analysis$mag), alpha=0.5) +
  theme_minimal() +
  scale_fill_brewer(palette="PuBuGn") +
  labs(title="DENSITÀ DI KERNEL di magnitudo per continente",
       subtitle="Stimata con parametro di smoothing ottimale",
       x="Magnitudo", 
       y="Densità",
       fill="Continente")

############################
# Scatterplot multivariati #
############################
#### Scatterplot 3D (solo variabili quantitative) ####
library(scatterplot3d)
with(db_analysis, {scatterplot3d( x=dmin,
                                  y=gap,
                                  z=horizontalError,
                                  color="lightblue",
                                  pch=19,
                                  type="h",
                                  lty.hplot=2,
                                  main="SCATTERPLOT 3D",
                                  xlab="Distanza dalla stazione",
                                  ylab="Divario azimutale",
                                  zlab="Errore posizione")
                    })

#### Scatterplot con variabili categoriche ####
ggplot(db_analysis, aes(x=dmin,
                        y=horizontalError,
                        color=magType_group
                        #shape=
                        #size=
                        )) +
  geom_point(alpha=0.8, size=2) +
  geom_smooth(method="lm", se=F, color="blue") +
  scale_y_continuous(limits=c(0,20)) +
  scale_x_continuous(breaks=seq(0,23,5), limits=c(0,23)) +
  scale_color_manual(values=c("red","orange","cornflowerblue","purple","green")) +
  theme_minimal() +
  facet_wrap(~continent) +
  labs(title="SCATTERPLOTS per continente",
       x="Distanza dalla stazione",
       y="Errore posizione",
       color="Tipo calcolo magnitudo")

#### Scatterplo matrix ####
#library(GGally)
#data_scatter_m = db_analysis %>% select(c("depth","mag","nst","magNst","gap","dmin","horizontalError","depthError","magError"))
#ggpairs(data_scatter_m) 
#rm(data_scatter_m)

###############
# BUBBLE PLOT #
###############
db_bubble = db_analysis %>% filter(country=="Chile")

ggplot(db_bubble, aes(x=dmin, y=horizontalError, size=nst)) +
  geom_point(alpha=0.5, fill="lightblue", color="darkblue", shape=21) +
  scale_size_continuous(range = c(1, 12)) +
  theme_minimal() +
  labs(title="BUBBLE PLOT",
       x="Distanza dalla stazione",
       y="Errore posizione",
       size="Numero stazioni")

rm(db_bubble)

############
# HEAT MAP #
############
library(superheat)
db_heat = db_analysis %>% select(c("continent","depth","mag","nst","magNst","gap","dmin","horizontalError","depthError","magError"))
db_heat = data.frame(cbind(depth = tapply(db_heat$depth,droplevels(db_heat$continent),mean,na.rm=T),
                           mag = tapply(db_heat$mag,droplevels(db_heat$continent),mean,na.rm=T),
                           nst = tapply(db_heat$nst,droplevels(db_heat$continent),mean,na.rm=T),
                           magNst = tapply(db_heat$magNst,droplevels(db_heat$continent),mean,na.rm=T),
                           gap = tapply(db_heat$gap,droplevels(db_heat$continent),mean,na.rm=T),
                           dmin = tapply(db_heat$dmin,droplevels(db_heat$continent),mean,na.rm=T),
                           horizontalError = tapply(db_heat$horizontalError,droplevels(db_heat$continent),mean,na.rm=T),
                           depthError = tapply(db_heat$depthError,droplevels(db_heat$continent),mean,na.rm=T),
                           magError = tapply(db_heat$magError,droplevels(db_heat$continent),mean,na.rm=T)))
  
superheat(db_heat,
          scale=T,
          left.label.text.size=3,
          bottom.label.text.size=2.3,
          bottom.label.size = 0.08,
          row.dendrogram=F)

rm(db_heat)

####################
# CORRELATION PLOT #
####################
library(ggcorrplot)
data_corrplot = db_analysis %>% select(c("depth","mag","nst","magNst","gap","dmin","horizontalError","depthError","magError"))
#select_if(db_analysis, is.numeric)
matrix_corrplot = round(cor(data_corrplot, method="pearson", use="pairwise.complete.obs"),4)
#matrix_corrplot = round(cor(data_corrplot, method="pearson", use="complete.obs"),4)

ggcorrplot(matrix_corrplot, hc.order=T, type="lower", lab=T)

rm(data_corrplot, matrix_corrplot)

###############################
###############################
#### TIME-DEPENDENT GRAPHS ####
###############################
###############################

############################
# Serie storica univariata #
############################
#data_timegraph_u = db_analysis %>% group_by(date_event) %>% summarise(mean=mean(mag), max=max(mag))
data_timegraph_u = db_analysis %>% group_by(year_event) %>% summarise(mean=mean(mag), max=max(mag))

ggplot(data_timegraph_u, aes(x=year_event, y=max)) +
  geom_line(color="darkblue", size=0.7) +
# geom_smooth(color="red", size=0.7, se=F) +
  theme_minimal() +
  labs(title="Serie storica di magnitudo massima",
       subtitle="1970 - 2024",
       x="",
       y="Magnitudo massima") 

rm(data_timegraph_u)

###########################
# Serie storica bivariata #
###########################
data_timegraph_b = db_analysis %>% group_by(continent, year_event) %>% summarise(mean=mean(mag), max=max(mag))

ggplot(data_timegraph_b, aes(x=year_event, y=max, color=continent)) +
  geom_line(size=0.5) +
  theme_minimal() +
  labs(title="Serie storica di magnitudo massima per continente",
       subtitle="1970 - 2024",
       x="",
       y="Magnitudo massima") 

rm(data_timegraph_b)

###############
# Slope graph #
###############
library(CGPfunctions)
db_slope = db_analysis %>% mutate(year_slope = factor(ifelse(year_event<1980, 1970,
                                                        ifelse(year_event<1990, 1980,
                                                          ifelse(year_event<2000, 1990,
                                                            ifelse(year_event<2010, 2000,
                                                              ifelse(year_event<2020, 2010, 2020))))))
                                  ) %>%
                group_by(continent, year_slope) %>%
                summarise(mean=mean(mag), max=max(mag))

newggslopegraph(db_slope, year_slope, max, continent) +
  labs(title="SLOPE GRAPH", subtitle="Magnitudo massima per continente")

rm(db_slope)

####################
# Basic area chart #
####################
data_basic_area = db_analysis %>% group_by(year_event) %>%
                                  summarise(mean=mean(mag), max=max(mag))

ggplot(data_basic_area, aes(x=year_event, y=max)) +
  geom_area(fill="lightblue", color="darkblue") +
  theme_minimal() +
  labs(title="BASIC AREA CHART di magnitudo massima",
       subtitle="1970 - 2024",
       x="",
       y="Magnitudo massima") 

rm(data_basic_area)

######################
# Stacked area chart #
######################
data_stacked_area = db_analysis %>% group_by(year_event, continent) %>%
                                    summarise(mean=mean(mag), max=max(mag))

ggplot(data_stacked_area, aes(x=year_event, y=max, fill=continent)) +
  geom_area(color="darkblue") +
  theme_minimal() +
  labs(title="STACKED AREA CHART di magnitudo massima per continente",
       subtitle="1970 - 2024",
       x="",
       y="Magnitudo massima") 

rm(data_stacked_area)

###############
###############
#### MAPPE ####
###############
###############

#### Caricamento dei dati dei confini del mondo ####
world_map = map_data("world")

#### Creazione mappa ####
ggplot() +
  #Aggiungere la mappa del mondo
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "lightgray", color = "white") +
  #Aggiungere i dati sul db analizzato
  geom_point(data = db, 
             aes(x = longitude, y = latitude, size = mag, color = mag),
             alpha = 0.7) +
  #Personalizzazioni
  scale_size_continuous(name = "Magnitudo", range = c(1, 10)) +
  scale_color_gradient(low = "yellow", high = "red", name = "Magnitudo") +
  labs(title = "Terremoti nel Mondo", 
       x = "Longitudine", 
       y = "Latitudine") +
  theme_minimal() +
  coord_fixed(1.3) #Per mantenere le proporzioni della mappa
