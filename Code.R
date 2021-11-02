
############################# San Camillo #################################



library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(GGally)
library(ggpubr)
library(caret)
library(writexl)
library(PerformanceAnalytics)
library(corrplot)
library(caret)
library(writexl)
library(PerformanceAnalytics)
library(FSA)
library(ARTool)
library(emmeans)
library(rcompanion)

## Qui importo il database
library(haven)
Database_Final <- read_dta("Dropbox/Data_Stroke_San_Camillo/Database_Final.dta")



#%>%  filter(!is.na(NIHSSout)) 
## Qui mi ordino il database con le variabili che servono
p <- Database_Final %>% mutate(Gruppi_Età = case_when(Age__Groups == 1 ~ "Adults", Age__Groups == 2 ~ "Elderly", Age__Groups == 3 ~ "Very Elderly"))%>% 
  mutate(Outcome = ifelse(Outcome_True == 2,"Good","Poor"))%>% mutate(SEX =ifelse(SEX == "1","M","F"))%>% mutate (FA = ifelse(FA == "1", "FA","No-FA"))%>% mutate (TROMBOLISI = ifelse(TROMBOLISI == "1","Yes","No")) 

p2 <- p %>% mutate(TROMBECTOMIA = ifelse(INDICAZTROMBECT == "0", "No Indication to Thrombectomy", "Endovascular Thrombectomy"))

p2 <- p2 %>% mutate(Aetiology = case_when(TOAST_True == 3 ~ "Large-vessel Atherosclerotic", TOAST_True == 1  ~ "Cardioembolic")) 

p2 <- p2 %>% mutate(Oxford_True = case_when(Oxford_True == 1  ~ "LACS", Oxford_True == 2  ~ "Multifocal", Oxford_True == 3  ~ "TACS",  Oxford_True == 4  ~ "POCS", Oxford_True == 5  ~ "PACS" ))

database <- p2 %>%  mutate(Outcome = factor(Outcome, levels = c("Good","Poor")))%>% mutate(Anticoagulant = ifelse(TAO == 1 | NAO == 1, "Yes","No")) %>%
  mutate(Years = case_when(Intervalli_Anno == 1 ~ "2012-2015", Intervalli_Anno == 2 ~ "2016-2019")) %>% mutate(MAP = PAS - ((PAS -PAD)/3))%>% mutate(Dyslipidemia = ifelse(COLESTTOT > 200, "Yes","No"))%>% 
  mutate(Gruppi_Età = factor(Gruppi_Età), Years = factor(Years), SEX = factor(SEX), TROMBOLISI = factor(TROMBOLISI), TROMBECTOMIA = factor(TROMBECTOMIA, levels = c("No Indication to Thrombectomy","Endovascular Thrombectomy") ), ICH = factor(ICH), DM = factor(DM), 
         IPERTART = factor(IPERTART), FA = factor(FA, levels = c("No-FA","FA")),FUMO = factor(FUMO),
         FAMIGL = factor (FAMIGL), PFO = factor(PFO), Oxford_True = factor(Oxford_True), TOAST_True = factor(TOAST_True),
         Outcome_True = factor(Outcome_True), OBESITA = factor(OBESITA), Dyslipidemia = factor(Dyslipidemia), Aetiology = factor(Aetiology), Anticoagulant = factor(Anticoagulant)) 



database <- database %>% select(Gruppi_Età, FA, Years, SEX, TROMBOLISI, TROMBECTOMIA, MAP, DM, IPERTART, FUMO, FAMIGL, PFO, Oxford_True, TOAST_True, Outcome, OBESITA, GGDEGENZA, Anno,ETA, NIHSS_diff, NIHSSin, NIHSSout, Dyslipidemia, COLESTTOT, LDL, HDL, TRIGL, Aetiology, PAS, PAD, RANKINPRE, Anticoagulant, HBGLIC) %>% filter(NIHSS_diff >= -20)

database <- database %>% mutate(Outcome = ifelse(NIHSS_diff >= 2, "Good","Poor")) %>% mutate(Outcome = factor(Outcome))%>% filter(Years == "2012-2015"|Years == "2016-2019")%>% filter(HBGLIC < 52)
I



#Rimuovi outliers da errore inserimento, molto utile!!!!
range <- 500

database$COLESTTOT[database$COLESTTOT > range] <- NA
database$LDL[database$LDL > range] <- NA

tab_xtab(database$Gruppi_Età, database$Outcome, show.row.prc = TRUE,
)

tab_xtab(database$Gruppi_Età, database$HBGLIC, show.row.prc = TRUE,
)


chisq.test(database$Gruppi_Età, database$Outcome)

# Divido per anni


d2012 <- database %>% filter(Years == "2012-2015")

d2019 <- database %>% filter(Years == "2016-2019")


library(sjPlot)
library(compareGroups)
view_df(database)
summary(database)


# Creo variabile per ASA

database$ASA <- grepl("ASA", database$TERPRECED, fixed = TRUE)



#  Creo Tabelle per statistica Descrittiva

res2012 <- compareGroups(Gruppi_Età ~ SEX + IPERTART + DM + Dyslipidemia + FUMO + FAMIGL + FA + TROMBOLISI  - Years, d2012, subset = Years == "2012-2015", simplify = FALSE)

res2019 <- compareGroups(Gruppi_Età ~ SEX + IPERTART + DM + Dyslipidemia + FUMO + FAMIGL + FA + TROMBOLISI  - Years, d2019, subset = Years == "2016-2019", simplify = FALSE)

restab2012 <- createTable(res2012, hide.no = "no")
restab2019 <- createTable(res2019, hide.no = "no")
restab <- cbind("2012-2015" = restab2012, "2016-2019" = restab2019)

tab_xtab(database$TROMBECTOMIA, database$Anno)
tab_xtab(d2012$Gruppi_Età, d2012$Outcome, show.row.prc = TRUE,
)

tabe <- compareGroups(Gruppi_Età ~ HBGLIC, d2012)
tabe2 <- compareGroups(Gruppi_Età ~ HBGLIC, d2019)


phitabe <- createTable(tabe, hide.no = "no")
phitabe2 <- createTable(tabe2, hide.no = "no")


res.aov <- aov(d2019$HBGLIC ~ d2019$Gruppi_Età, var.equal = TRUE)

export2md(restab)



export2word(restab, file = "table.docx")

# Creo seconda Tabella per statistica descrittiva

tab2012 <- compareGroups(Gruppi_Età ~ NIHSSin + NIHSSout  + COLESTTOT + LDL + HDL + TRIGL + Aetiology + Oxford_True - Years, d2012, subset = Years == "2012-2015", simplify = FALSE)

tab2019 <- compareGroups(Gruppi_Età ~ NIHSSin + NIHSSout + COLESTTOT + LDL + HDL + TRIGL + Aetiology + Oxford_True - Years, d2019, subset = Years == "2016-2019", simplify = FALSE)

phitab2012 <- createTable(tab2012, hide.no = "no")
phitab2019 <- createTable(tab2019, hide.no = "no")
phitab <- cbind("2012-2015" = phitab2012, "2016-2019" = phitab2019)

export2md(phitab)

export2word(phitab, file = "table2.docx")

chisq.test(d2012$Gruppi_Età, d2012$Anticoagulant)
chisq.test(d2019$Gruppi_Età, d2019$Aetiology)

# plot grid
plot_grid(list(p1, p2), tags = TRUE)

p1 <- plot_grpfrq(d2019$Gruppi_Età, d2019$Aetiology, geom.colors = "Pastel", facet.grid = TRUE)
p2 <- plot_grpfrq(d2012$Gruppi_Età, d2012$Aetiology, geom.colors = "Pastel", facet.grid = TRUE)


library(table1)
table1::label(d2012$NIHSSin) <- "NIHSS at ED"
table1::label(d2012$NIHSSout) <- "NIHSS at Discharge"

table1::table1(~NIHSSin + NIHSSout | Gruppi_Età, data = d2012, topclass="Rtable1-zebra")







# Divido per gruppi di età

veryelderly <- database %>% filter(Gruppi_Età == "Very Elderly")

v2012  <- d2012 %>% filter(Gruppi_Età == "Very Elderly")
v2019  <- d2019 %>% filter(Gruppi_Età == "Very Elderly")


elderly <- database %>% filter(Gruppi_Età == "Elderly")

e2012 <- d2012 %>% filter(Gruppi_Età == "Elderly")
e2019 <- d2019 %>% filter(Gruppi_Età == "Elderly")

adults <- database %>% filter(Gruppi_Età == "Adults")

a2012 <- d2012 %>% filter(Gruppi_Età == "Adults")
a2019 <- d2019 %>% filter(Gruppi_Età == "Adults")



## Qualche Plot per capire i dati

database %>% drop_na(TROMBECTOMIA)%>%
  ggviolin(x = "Gruppi_Età", y = "NIHSS_diff", add = "boxplot", add.params = list(fill = "white"), fill = "Gruppi_Età", palette = "nejm")  + facet_grid(. ~ FA)+
  scale_y_continuous(breaks = c(-20,-15,-10,-5,0,5,10,15,20)) + theme_clean() 

a <- database %>% drop_na(Years)%>% drop_na(TROMBECTOMIA)%>%
  ggviolin(x = "Gruppi_Età", y = "NIHSS_diff", add = "boxplot", add.params = list(fill = "white"), fill = "Gruppi_Età", palette = "nejm")  + facet_grid(. ~ TROMBOLISI)+
  scale_y_continuous(breaks = c(-20,-15,-10,-5,0,5,10,15,20)) + theme_clean() + ggsave("testo.eps", units="in", width=7, height=7, dpi=1200, family = "sans")

ggsave(filename = "survival-curveee.eps",
       plot = print(a),dpi = 2400,
       device = cairo_ps)

cairo_ps(filename = "survival-curve.eps",
         width = 12, height = 12, pointsize = 1,
         fallback_resolution = 1200)
print(a)
dev.off()

ggsave("test.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

plot_grpfrq(database$Outcome, database$Gruppi_Età, geom.colors = "gs")

tab_xtab(database$TROMBECTOMIA, database$Gruppi_Et?)


database %>% drop_na(TROMBECTOMIA)%>%
  ggplot(aes(x=TROMBECTOMIA, y = ..count.., fill = Gruppi_Età, palette = "nejm")) +
  geom_bar(col = "black") +
  theme_clean()+theme(legend.title = element_blank())





######IMPORTANTISSIMO TABELLEEEEE UNA SVOLTA!!!!!!

tab_stackfrq(database, title = "Descriptive Statistics")

### Tabella anni DEFINITIVA
tab_xtab(database$Years, database$Gruppi_Età, show.col.prc=TRUE, show.summary = TRUE, file="sjt_des.doc") 



table(veryelderly$Outcome, veryelderly$FA)
table(elderly$Outcome, elderly$FA)
table(adults$Outcome, adults$FA)

tab_xtab()


## Logistic Regression con Outcome, comparison tra gruppi di età in base ad anno

logistic_2012 <- glm(Outcome ~ Gruppi_Età +  TROMBOLISI + FA + SEX + DM + IPERTART + FUMO + FAMIGL + Dyslipidemia + Anticoagulant, data = d2012, family = "binomial"(link = "logit"))
logistic_2019 <- glm(Outcome ~ Gruppi_Età +  TROMBOLISI + TROMBECTOMIA + FA + SEX + DM + IPERTART + FUMO + FAMIGL + Dyslipidemia + Anticoagulant, data = d2019, family = "binomial"(link = "logit"))

tab_model(logistic_2012,logistic_2019)



v_2012 <- glm(Outcome ~ TROMBOLISI + FA + SEX + DM + IPERTART + FAMIGL + Dyslipidemia, data = v2012, family = "binomial"(link = "logit"))
v_2019 <- glm(Outcome ~ TROMBOLISI + TROMBECTOMIA + FA + SEX + DM + IPERTART + FUMO + FAMIGL + Dyslipidemia, data = v2019, family = "binomial"(link = "logit"))

tab_model(v_2012,v_2019)



e_2012 <- glm(Outcome ~ TROMBOLISI + FA + SEX + DM + IPERTART + FUMO + FAMIGL + Dyslipidemia, data = e2012, family = "binomial"(link = "logit"))
e_2019 <- glm(Outcome ~ TROMBOLISI + TROMBECTOMIA + FA + SEX + DM + IPERTART + FUMO + FAMIGL + Dyslipidemia, data = e2019, family = "binomial"(link = "logit"))

tab_model(e_2012,e_2019)



a_2012 <- glm(Outcome ~ TROMBOLISI + FA + SEX + DM + IPERTART + FUMO + FAMIGL + Dyslipidemia, data = a2012, family = "binomial"(link = "logit"))
a_2019 <- glm(Outcome ~ TROMBOLISI + TROMBECTOMIA + FA + SEX + DM + IPERTART + FUMO + FAMIGL + Dyslipidemia, data = a2019, family = "binomial"(link = "logit"))

tab_model(a_2012,a_2019)





library("DescTools")
Cstat(logistic_2012)
Cstat(logistic_2019)


summary(logistic_2012)
plot_model(logistic_2012)


forest_model_format_options(
  colour = "blue",
  color = NULL,
  shape = 15,
  text_size = 5,
  point_size = 5,
  banded = TRUE
)


forest_model(v_2012)
forest_model(v_2019)
forest_model(e_2012)
forest_model(e_2019)
forest_model(a_2012)
forest_model(a_2019)



##Logistic Regression da considerare per il paper

vlogistic <- glm(Outcome ~ TROMBOLISI + TROMBECTOMIA + FA + SEX + DM + IPERTART + FUMO + FAMIGL + Dyslipidemia, data = veryelderly, family = "binomial"(link = "logit"))
elogistic <- glm(Outcome ~ TROMBOLISI + TROMBECTOMIA + FA + SEX + DM + IPERTART + FUMO + FAMIGL + Dyslipidemia, data = elderly, family = "binomial"(link = "logit"))
alogistic <- glm(Outcome ~ TROMBOLISI + TROMBECTOMIA + FA + SEX + DM + IPERTART + FUMO + FAMIGL + Dyslipidemia, data = adults, family = "binomial"(link = "logit"))

tab_model(vlogistic, elogistic, alogistic, file = "logistic_year.doc")
tab_model(v_2012, e_2012, a_2012, file = "2012.doc")
tab_model(v_2019, e_2019, a_2019, file = "2019.doc")

plot_model(v_2012, show.values = TRUE, value.offset = .3)
plot_model(v_2019, show.values = TRUE, value.offset = .3)

plot_model(e_2012, show.values = TRUE, value.offset = .3)
plot_model(e_2019, show.values = TRUE, value.offset = .3)

plot_model(a_2012, show.values = TRUE, value.offset = .3)
plot_model(a_2019, show.values = TRUE, value.offset = .3)


# Multiple Linear Regression

model <- glm(Outcome ~  FA , data = database, na.action = na.exclude)
summary(model)
plot_model(model)
tab_model(model)

FA_NMA <- c(0,1)
contrasts(database$FA)<-cbind(FA_NMA)


# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(model)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(model)
summary(gvmodel)

# Other useful functions
coefficients(model) # model coefficients
confint(model, level=0.95) # CIs for model parameters
fitted(model) # predicted values
residuals(model) # residuals
anova(model) # anova table
vcov(model) # covariance matrix for model parameters
influence(model) # regression diagnostics


# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(model,type=c("lmg","last","first","pratt"),
            rela=TRUE)
plot(calc.relimp(model, rela=TRUE))

# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(model, b = 2, type = c("lmg", "last", "first", "pratt"), rank = TRUE,diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result

# compare models
fit1 <- lm(y ~ x1 + x2 + x3 + x4, data=mydata)
fit2 <- lm(y ~ x1 + x2)
anova(fit1, fit2)


# All Subsets Regression
library(leaps)
leaps<-regsubsets(NIHSS_diff ~ TROMBOLISI + SEX , data = veryelderly,nbest=2)
# view results
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
# plot statistic by subset size
library(car)
subsets(leaps, statistic="rsq")







# figure 1, geom Density per Year

plot <- database %>%  drop_na(Years)%>%
  ggplot(aes(x=ETA, y = ..count.., fill = Gruppi_Età, palette = "jco")) + scale_fill_brewer(palette = "Pastel1")+
  geom_histogram(col = "black") + 
  xlab("Age") +
  theme_pubr()+theme(legend.title = element_blank())+
  facet_grid(.~ Years)
plot

ggsave(
  filename = "figure%03d.tiff",
  plot = plot,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  dpi = 1200,
  limitsize = TRUE,
)


#Figure 2 

plot2 <- database %>%  
  ggplot(aes(x=TROMBOLISI, y = ..count.., fill = Gruppi_Età, palette = "nejm")) +
  geom_histogram() +
  xlab("Thrombectomy") +
  theme_clean()

plot2






# add boxplot with white fill color
p %>% ggviolin(x = "Gruppi_Età", y = "NIHSSin", fill = "Gruppi_Età",
               palette = c("#00AFBB", "#E7B800", "#FC4E07"),
               add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ # Add significance levels
  stat_compare_means(label.y = 50)
p





p %>% ggplot(aes(x = Gruppi_Età, y = NIHSSin, col = Gruppi_Età)) +
  geom_boxplot()+ theme_clean()+theme(legend.title = element_blank())+
  p





# per verificare quanti sono i missing values
library(Amelia)
missmap(Database_Final, main = "Missing values vs observed")

# utilissimo per vedere confronti rapidi con grafici!!!!!!!!
p %>%
  select(Gruppi_Età, NIHSS_diff,MAP, COLESTTOT,GGDEGENZA, TROMBOLISI, INDICAZTROMBECT) %>% filter(!is.na(INDICAZTROMBECT))%>% filter(COLESTTOT < 300) %>%
  ggpairs(aes(color=Gruppi_Età))
p




### Logistic Regression

data <- p2 %>% filter(Gruppi_Età == "Very Elderly")

summary(data)

logistic <- glm(Outcome_True ~ FA + SEX + TROMBOLISI, data=data, family="binomial")

summary(logistic)


model <- p %>% filter(Gruppi_Età == "Very Elderly") %>% glm(Outcome_True ~ FA, family = binomial) %>% stepAIC(trace = TRUE)
summary(model)

####chiama modello con SEP AIKAKE

predicted<-predict(model,prepmod,type = "response")
pred<-as.factor(ifelse (predicted>0.5,1,0))
prepmod$PEGG<-as.factor(prepmod$PEGG)

confusionMatrix(pred,prepmod$PEGG)

exp(model$coefficients)




####   Cambiamento del trend di gruppi di età per anno.


p3 <- database %>% mutate(AgeGroups = case_when(ETA < 18 ~ "under 18", ETA >= 18 & ETA <= 45 ~ "18-45", ETA > 45 & ETA <= 65 ~ "46-65", ETA > 65 & ETA <= 85 ~ "65-85", ETA > 85 ~ "over 85"))%>%
  group_by(Anno, AgeGroups) %>% summarize(num =n())%>% as.data.frame(p3)


ggplot(p3, aes(x = Anno, y = num, fill = AgeGroups)) +
  geom_area(colour = "black",size = .2, alpha = .4) + scale_y_continuous()+ labs(fill = "Age Groups", size = 10) + theme_stata()




# COME sostituire outliers con nan
database$colest_outlier <- ifelse(database$COLESTTOT %in% boxplot.stats(database$COLESTTOT)$out, 1, 0)

database[database$COLESTTOT %in% database$colest_outlier, "COLESTTOT"] = NA




Stats <- Stats %>% mutate (FCD = factor(FCD)) %>% mutate (Time = factor(Time))


fit <- lmer(PAC ~ FCD + (1 | Time), data = Stats)


sjp.lmer(fit)
sjp.lmer(fit)



# load packages
library(sjPlot)
library(sjmisc)
# load sample data set.
data(efc)
set_theme(theme = "forest", 
          geom.label.size = 3, 
          axis.textsize = .9, 
          axis.title.size = .9)



# fit model
library(lme4)
fit <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

# prepare group variable
efc$grp = as.factor(efc$e15relat)
levels(x = efc$grp) <- get_labels(efc$e15relat)
# data frame for fitted model
mydf <- data.frame(neg_c_7 = efc$neg_c_7,
                   sex = to_factor(efc$c161sex),
                   c12hour = efc$c12hour,
                   barthel = efc$barthtot,
                   grp = efc$grp)
# fit 2nd model
fit2 <- lmer(neg_c_7 ~ sex + c12hour + barthel + (1 | grp), data = mydf)


sjp.lmer(fit, y.offset = .4)


plot(fit)


# Visualise random effects 
re.effects <- plot_model(fit, type = "re", show.values = TRUE)

# show summary
summary(fit)


######## Survey SIN ########à

colnames(Survey2)[1] <- "Year_of_Experience"
colnames(Survey2)[4] <- "Workplace"
colnames(Survey2)[7] <- "How_many_AED_for_DRE"

database <- Survey2 %>% mutate(Year_of_Experience = case_when(Year_of_Experience == 1 ~ "< 5", Year_of_Experience == 2 ~ "5-10", Year_of_Experience == 3 ~ "10-20", Year_of_Experience == 4 ~ "> 20")) %>% mutate(Year_of_Experience = factor(Year_of_Experience)) 
database <- database %>% mutate(Workplace = case_when(Workplace == 1 ~ "University", Workplace == 2 ~ "Hospital", Workplace >= 3 ~ "Outpatient")) %>% mutate(Workplace = factor(Workplace, levels = c("University","Hospital","Outpatient")))
database <- database %>% mutate(How_many_AED_for_DRE = factor(How_many_AED_for_DRE))

logistic <- glm(How_many_AED_for_DRE ~ Year_of_Experience + Workplace, data = database, family = "binomial"(link = "logit"))

library("DescTools")
Cstat(logistic)


summary(logistic)
plot_model(logistic)


forest_model_format_options(
  colour = "blue",
  color = NULL,
  shape = 15,
  text_size = 5,
  point_size = 5,
  banded = TRUE
)


forest_model(v_2012)
forest_model(v_2019)
forest_model(e_2012)
forest_model(e_2019)
forest_model(a_2012)
forest_model(a_2019)


#############Multinomial Logistic Regression ###############

database$How_many_AED_for_DRE2 <- relevel(database$How_many_AED_for_DRE, ref = "3")
test <- multinom(How_many_AED_for_DRE2 ~ Year_of_Experience + Workplace, data = database)

summary(test)
