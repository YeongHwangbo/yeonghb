

######### Load the data #######

# Load pks to get the participants' demographic data (age)
library(pks)
data("probability")

str(probability)


# Demographic data
table(probability$sex)
# f   m 
# 310 194 

table(probability$mode)
# lab online 
# 26    478

table(probability$age)
age <- na.omit(probability$age) # two NAs age omitted
str(age)  # 502

mean(age)
median(age)
sd(age)
range(age)


# Load probability dataset
library(CDM)
data(data.cdm05)

prb <-  data.cdm05$data
Qmat <-  data.cdm05$q.matrix
skills <- data.cdm05$skills

str(prb) # 504 12Q
# 1-4 single skill
# 5-10 2 skills
# 11,12 3 skills


# Q matrix
(qmat <- as.matrix(Qmat))
head(skills) #descripstions of 4 skills


# Estimate G-DINA as a baseline model
library(GDINA)

fit.GDINA  <- GDINA(prb, Qmat,  mono.constraint = T)
summary(fit.GDINA)

######### Model selection Test-level (Relative) #######

# Fit the model at Test level
fit.GDINA <- GDINA(prb, Qmat,  mono.constraint = T)  # G-DINA
fit.DINA <- GDINA(prb, Qmat, mono.constraint = T, model='DINA')
fit.DINO <- GDINA(prb, Qmat, mono.constraint = T, model='DINO')
fit.ACDM <- GDINA(prb, Qmat, mono.constraint = T, model='ACDM')
fit.LLM <- GDINA(prb, Qmat, mono.constraint = T, model='LLM')
fit.RRUM<- GDINA(prb, Qmat, mono.constraint = T, model='RRUM')

# ACDM, LLM compensatory
# R-RUM non compensatory

anova(fit.GDINA, fit.DINA, fit.DINO, fit.ACDM, fit.LLM, fit.RRUM)


# Table for fit statistics
rel.test <- matrix(NA,6,3)
col <-c('-2LL','AIC','BIC')
colnames(rel.test) <- col
rownames(rel.test) <- c('G-DINA','DINA','DINO','A-CDM','LLM','R-RUM')

rel.test[1,] <- c(4859.65, 4985.65, 5251.67)
rel.test[2,] <- c(4957.88, 5035.88, 5200.56)
rel.test[3,] <- c(5126.47, 5204.47, 5369.15)
rel.test[4,] <- c(4887.02, 4985.02, 5191.93)
rel.test[5,] <- c(4875.41, 4973.41, 5180.32)
rel.test[6,] <- c(4885.06, 4983.06, 5189.97)

rel.test


######### Model selection Test-level (Absolute) #######

# Test level Absolute fit - modelfit() or itemfit()
itemfit(fit.DINO)
modelfit(fit.DINO)


# Table for adsoulte fit statistics
abs.test <- matrix(NA,6,8)
col <-c('Max.z(r)','p-value','Max.z(l)','p-value',
        'M2','p-value','RMSEA2','SRMSR')
colnames(abs.test) <- col
rownames(abs.test) <- c('G-DINA','DINA','DINO','A-CDM','LLM','R-RUM')

abs.test[1,] <- c(3.1441, 0.1100, 2.6740, 0.4947,24.0944, 0.0635, 0.0347, 0.0454)
abs.test[2,] <- c(5.1458, 0.000, 4.9408, 0.000, 79.7155, 0.000, 0.0455, 0.0742)
abs.test[3,] <- c(7.6658, 0.000,  5.1069, 0.000, 113.2815, 0.000, 0.0615, 0.1202)
abs.test[4,] <- c(3.9736, 0.0047, 3.6746, 0.0157, 40.2387, 0.0801, 0.027, 0.0491)
abs.test[5,] <- c(3.8458, 0.0079, 3.2554, 0.0747, 39.8506, 0.0864, 0.0272,0.0477)
abs.test[6,] <- c(3.9205, 0.0058, 3.3115, 0.0612, 39.871, 0.0861, 0.0273, 0.05)

abs.test


######### Model selection Item-level #######

# Relative fit / Item-level (Wald, LRT)
(comp <- modelcomp(fit.GDINA)) # for item level model comparison

# Extract selected reduced models and p-values
extract(comp, 'selected.model')  
extract(comp, 'pvalues')  

# Selected models origianl
# reduced <- c('GDINA','GDINA','GDINA','GDINA',
#              'LLM','LLM','ACDM','DINO',
#              'ACDM','ACDM','RRUM','RRUM')

reduced <- c('GDINA','GDINA','GDINA','GDINA',
             'DINO','LLM','ACDM','DINO',
             'DINO','DINA','DINA','DINA')

# Fit the model with reduced models selected
fit.wald  <- GDINA(prb, Qmat, model=reduced, mono.constraint = T)
summary(fit.wald)


# Get fit statistics from mixed-CDM
anova(fit.GDINA, fit.wald) 

itemfit(fit.wald)
modelfit(fit.wald)

summary(itemfit(fit.wald)) # item-level fit for each item 


######### Comparision of model fit (item, test level) #######

# Absoulte fit statistics of G-DINA, LLM, Mixed-CDM
abs.comp <- abs.test[c(1,5), ]
abs.comp

abs.comp <- rbind(abs.comp, c(3.0545, 0.1488, 3.3672, 0.0501, 65.0771, 0.0029, 0.0388, 0.052))
rownames(abs.comp)[3] <- 'Mixed-CDM'  # model fit from item level


# Relative fit statistics of G-DINA, LLM, Mixed-CDM
rel.comp <- rel.test[c(1,5), ]
rel.comp

rel.comp <- rbind(rel.comp, c(4880.807, 4962.81, 5135.93))
rownames(rel.comp)[3] <- 'Mixed-CDM'  # model fit from item level


# Model comparison of G-DINA, LLM, Mixed-CDM
anova(fit.GDINA, fit.LLM, fit.wald)

# Classification Accuracy 
CA(fit.wald) # MAP by default

CA(fit.GDINA) # .9266. wald higher




