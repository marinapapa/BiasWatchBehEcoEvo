#########################################################################
###### Statistical analysis #############################################
#########################################################################
alldt <- read.csv('../data/data_for_stats.csv')

## Gender factors
alldt$fa_g <- as.factor(as.numeric(alldt$g_f == 'female'))
alldt$la_g <- as.factor(as.numeric(alldt$g_l == 'female'))

## Citation quantiles factors
alldt$cit_q <- ordered(alldt$cit_q, 
                       levels = c("1", "2", "3", "4", "5"))

## Split single authors data
nosingl <- alldt[alldt$N > 1,]
singl <- alldt[alldt$N < 2 ,]


#########################################################################
##_____________________ 1) Citation performance__________________________

##########################
## Model 1: first and last authors

cat1 <- nosingl[!(nosingl$journal %in% c('animal behaviour',
                                         'behavioural ecology',
                                         'behavioural ecology and sociobiology',
                                         'ethology')),]

m1_f <- MASS::polr(cit_q ~ fa_g + la_g + N + refs_log,
                   data = cat1)

generalhoslem::lipsitz.test(m1_f)
generalhoslem::pulkrob.chisq(m1_f, catvars = c('fa_g', 'la_g'))
generalhoslem::pulkrob.deviance(m1_f,catvars = c('fa_g', 'la_g'))

coefficients <- summary(m1_f)$coefficients
exp(coefficients[ ,"Value"])

m1_f <- ordinal::clm(cit_q ~ N  + fa_g + la_g + refs_log , data = cat1)
anova(m1_f)
summary(m1_f) 
stargazer::stargazer(coefficients)

################
## Model 2: Female Ratio & Diversity Index

m2_f <- MASS::polr(cit_q ~ refs_log + div_idx + fr + N, data = cat1)
generalhoslem::lipsitz.test(m2_f)
coefficients <- summary(m2_f)$coefficients
exp(coefficients[ ,"Value"])

m2_f <- ordinal::clm(cit_q ~ refs_log + div_idx + fr + N, data = cat1)
anova(m2_f)
summary(m2_f)
stargazer::stargazer(coefficients)

###############################
## Model 3: Single author
cat2 <- singl[!(singl$journal %in% c('animal behaviour',
                                     'behavioral ecology',
                                     'behavioral ecology and sociobiology',
                                     'ethology'
                                     )),]

m3_f <- MASS::polr(cit_q ~ fa_g + refs_log, data = cat2)
generalhoslem::lipsitz.test(m3_f)
generalhoslem::pulkrob.chisq(m3_f,catvars = c('fa_g'))
generalhoslem::pulkrob.deviance(m3_f,catvars = c('fa_g'))

coefficients <- summary(m3_f)$coefficients
exp(coefficients[ ,"Value"])

m3_f <- ordinal::clm(cit_q ~ fa_g + refs_log, data = cat2)
anova(m3_f)
summary(m3_f) 
stargazer::stargazer(coefficients)

#########################################################################
##_____________________ 2) Last author and diversity ____________________

## Last author
feml <- 1- nosingl$g_idx[nosingl$g_l == 'female']
mall <- 1- nosingl$g_idx[nosingl$g_l == 'male']
median(feml)
median(mall)
feml <- nosingl$div_idx[nosingl$g_l == 'female']
mall <- nosingl$div_idx[nosingl$g_l == 'male']
median(feml)
median(mall)
wilcox.test(feml, mall, alternative = "two.sided")
nosingl$fr <- 1- nosingl$g_idx
nosingl %>% rstatix::wilcox_effsize(fr ~ g_l)
nosingl %>% rstatix::wilcox_effsize(div_idx ~ g_l)
nosingl %>% rstatix::wilcox_test(fr ~ g_l)
nosingl %>% rstatix::wilcox_test(div_idx ~ g_l)

## First author
feml <- nosingl$div_idx[nosingl$g_f == 'female']
mall <- nosingl$div_idx[nosingl$g_f == 'male']
median(feml)
median(mall)
wilcox.test(feml, mall, alternative = "two.sided")
nosingl %>% rstatix::wilcox_effsize(div_idx ~ g_f)
nosingl %>% rstatix::wilcox_test(div_idx ~ g_f)


#########################################################################
##_________________ 3) Repeat 1 for animal behaviour journals ___________

anbeh <- alldt[alldt$journal %in% c('animal behaviour',
                                    'behavioral ecology',
                                    'behavioral ecology and sociobiology',
                                    'ethology'
                                    ),]
anb_nos <- anbeh[anbeh$N > 1,]

##########################
## Model 1: first and last authors
m1_f <- MASS::polr(cit_q ~ fa_g + la_g + N + Nrefs, data = anb_nos)
generalhoslem::lipsitz.test(m1_f)
generalhoslem::pulkrob.chisq(m1_f,catvars = c('fa_g', 'la_g'))
generalhoslem::pulkrob.deviance(m1_f,catvars = c('fa_g', 'la_g'))

coefficients <- summary(m1_f)$coefficients
exp(coefficients[ ,"Value"])

m1_f <- ordinal::clm(cit_q ~ N + Nrefs + fa_g + la_g , data = nosingl)
anova(m1_f)
summary(m1_f) 
stargazer::stargazer(coefficients)

################
## Model 2: Diversity & Female ratio
m3_f <- MASS::polr(cit_q ~ N + Nrefs + fr + div_idx , data = anb_nos)
generalhoslem::lipsitz.test(m3_f)
coefficients <- summary(m3_f)$coefficients
exp(coefficients[ ,"Value"])

m3_f <- ordinal::clm(cit_q ~ N + Nrefs + fr + div_idx, data = anb_nos)
anova(m3_f)
summary(m3_f)
stargazer::stargazer(coefficients)

###############################
## Model 3: Single author
anb_s <- anbeh[anbeh$N < 2,]

m4_f <- MASS::polr(cit_q ~ fa_g + Nrefs, data = anb_s)
generalhoslem::lipsitz.test(m4_f)
generalhoslem::pulkrob.chisq(m4_f,catvars = c('fa_g'))
generalhoslem::pulkrob.deviance(m4_f,catvars = c('fa_g'))

coefficients <- summary(m4_f)$coefficients
exp(coefficients[ ,"Value"])

m3_f <- ordinal::clm(cit_q ~ fa_g + Nrefs, data = anb_s)
anova(m3_f)
summary(m3_f) 
stargazer::stargazer(coefficients)

