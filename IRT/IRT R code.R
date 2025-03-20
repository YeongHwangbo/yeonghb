
############ a) Data preparaton ###########

# Load the dataset
library(readxl)
tripm.0 <-read_excel("TriPM.xlsx")

# Missing data: 999 to NA
tripm.0[,2:62][tripm.0[,2:62]=='999'] <- NA 
tripm.NA <- tripm.0

# Check the number of missing data for each item
summary(tripm.NA) # the number of NA's

# Check the number of missing data for each participatn
rowSums(is.na(tripm.NA)) # 58 - 61

# Drop the rows that contain NA
tripm <- na.omit(tripm.NA)
rowSums(is.na(tripm))  # No missing values. Dropped 255 rows

nrow(is.na(tripm))  # 1423

# Change the name of columns
(colnames(tripm)[5:62] <- paste0('Item.', 1:58))


# Convert ID, SEX, and PSCH_PROB to fators
tripm$ID <- as.factor(tripm$ID)
tripm$SEX <- as.factor(tripm$SEX)   # 1 male, 2 female
tripm$PSYCH_PROB <- as.factor(tripm$PSYCH_PROB)

# Convert AGE to integer
tripm$AGE <- as.integer(tripm$AGE)

str(tripm)

############ a) Reverse coding, Descriptive statistics ############

# Extract the items
tripm58 <- tripm[,5:62]
summary(tripm58)

# Perform reverse coding
tripm58[,c(1, 3, 5, 6, 7, 8, 9, 12, 13, 14, 15, 17, 18, 19, 
           20, 22, 23, 24, 26, 27, 28, 29, 31, 
           32, 34, 36, 37, 38, 40, 42, 43, 45, 46, 
           48, 49, 51, 53, 54, 55, 56, 58)] <- 5 - tripm58[,c(1, 3, 5, 6, 7, 8, 9, 12, 13, 14, 15, 
                                                              17, 18, 19, 20, 22, 23, 24, 26, 27, 28, 29, 31, 
                                                              32, 34, 36, 37, 38, 40, 42, 43, 45, 46, 
                                                              48, 49, 51, 53, 54, 55, 56, 58)]


# Descriptive statistics
summary(tripm[1:4])

apply(tripm[1:4], 2, table)$AGE   # Number of people by age


# Response pattern
apply(tripm58, 2, table)



############ b) Dichotomization #########

#install.packages("sjmisc")
library(sjmisc)

# Dichotomize the data
tripm.dic <- dicho(tripm58, dich.by=2, append=FALSE, as.num=TRUE)

# Change the name of columns
(colnames(tripm.dic) <- paste0('Item.', 1:58))



############ c) Reliability and Dimensionality #######

library(lavaan)

# 1) Unidimensional 1pl
mod.tri.1dim.1pl <- 'GEN =~ a*Item.1+a*Item.2+a*Item.3+a*Item.4+a*Item.5+a*Item.6+a*Item.7
                            +a*Item.8+a*Item.9+a*Item.10+a*Item.11+a*Item.12+a*Item.13+a*Item.14
                            +a*Item.15+a*Item.16+a*Item.17+a*Item.18+a*Item.19+a*Item.20+a*Item.21
                            +a*Item.22+a*Item.23+a*Item.24+a*Item.25+a*Item.26+a*Item.27+a*Item.28
                            +a*Item.29+a*Item.30+a*Item.31+a*Item.32+a*Item.33+a*Item.34+a*Item.35
                            +a*Item.36+a*Item.37+a*Item.38+a*Item.39+a*Item.40+a*Item.41+a*Item.42
                            +a*Item.43+a*Item.44+a*Item.45+a*Item.46+a*Item.47+a*Item.48+a*Item.49
                            +a*Item.50+a*Item.51+a*Item.52+a*Item.53+a*Item.54+a*Item.55+a*Item.56
                            +a*Item.57+a*Item.58'

tri.1dim.1pl <- cfa(mod.tri.1dim.1pl, data=tripm.dic, std.lv=T, 
                    ordered=colnames(tripm.dic), estimator="WLSMV")

#save(tri.1dim.1pl, file='tri.1dim.1pl.RData')
load('tri.1dim.1pl.RData') 


summary(tri.1dim.1pl, fit.measure=T, standardized=F, rsquare=T)
# default=delta parameterization

fitMeasures(tri.1dim.1pl, c("cfi.scaled", "tli.scaled", "RMSEA.scaled"))
# cfi.scaled tli.scaled 
# 0.372      0.371 

# 2) Unidimensional 2pl
mod.tri.1dim.2pl <- 'GEN =~ Item.1+Item.2+Item.3+Item.4+Item.5+Item.6+Item.7
                            +Item.8+Item.9+Item.10+Item.11+Item.12+Item.13+Item.14
                            +Item.15+Item.16+Item.17+Item.18+Item.19+Item.20+Item.21
                            +Item.22+Item.23+Item.24+Item.25+Item.26+Item.27+Item.28
                            +Item.29+Item.30+Item.31+Item.32+Item.33+Item.34+Item.35
                            +Item.36+Item.37+Item.38+Item.39+Item.40+Item.41+Item.42
                            +Item.43+Item.44+Item.45+Item.46+Item.47+Item.48+Item.49
                            +Item.50+Item.51+Item.52+Item.53+Item.54+Item.55+Item.56
                            +Item.57+Item.58'

tri.1dim.2pl <- cfa(mod.tri.1dim.2pl, data=tripm.dic, std.lv=T, 
                    ordered=colnames(tripm.dic), estimator="WLSMV")

#save(tri.1dim.2pl, file='tri.1dim.2pl.RData')
load('tri.1dim.2pl.RData') 


summary(tri.1dim.2pl, fit.measure=T, standardized=F, rsquare=T)


fitMeasures(tri.1dim.2pl, c("chisq.scaled", "df", "pvalue.scaled"))
fitMeasures(tri.1dim.2pl, c("cfi.scaled", "tli.scaled", "RMSEA.scaled"))
# cfi.scaled tli.scaled 
# 0.587      0.572 


# Model comparison : one factor 1pl vs 2pl
anova(tri.1dim.1pl, tri.1dim.2pl)  # *** 

lavTestLRT(tri.1dim.1pl, tri.1dim.2pl, method="satorra.bentler.2001") # ***

# Delta CFI
fitMeasures(tri.1dim.2pl,"cfi.scaled") - fitMeasures(tri.1dim.1pl,"cfi.scaled")  # 0.215



library(semTools)

# Reliability estimate for unidimensional 1pl model
reliability(tri.1dim.1pl) 

# Reliability estimate for unidimensional 2pl model
reliability(tri.1dim.2pl)



# 3) Three factor structure 2pl
mod.tri.3fac.2pl <- '
Boldness =~ Item.1+Item.4+Item.7 + Item.10+Item.13+Item.16+Item.19
            +Item.22+Item.25+Item.28 + Item.32+Item.35+Item.38
            +Item.41+Item.44+Item.47 + Item.50+Item.54+Item.57

Disinhibition =~ Item.3+Item.5+Item.9 + Item.12+Item.15+Item.18
                 +Item.21+Item.24+Item.27 + Item.30+Item.31+Item.34+Item.37
                 +Item.43+Item.46+Item.49 + Item.51+Item.53+Item.56+Item.58
Callousness =~ Item.2+Item.6+Item.8 + Item.11+Item.14+Item.17
              +Item.20+Item.23+Item.26+Item.29 + Item.33+Item.36+Item.39
              +Item.40+Item.42+Item.45+Item.48 + Item.52+Item.55
'


tri.3fac.2pl <- cfa(mod.tri.3fac.2pl, data=tripm.dic, 
                    std.lv=T,  # orthogonal false
                    ordered=colnames(tripm.dic), estimator="WLSMV")

#save(tri.3fac.2pl, file='tri.3fac.2pl.RData')
load('tri.3fac.2pl.RData') 

summary(tri.3fac.2pl, fit.measure=T, standardized=F, rsquare=T)

fitMeasures(tri.3fac.2pl, c("chisq.scaled", "df", "pvalue.scaled"))
fitMeasures(tri.3fac.2pl, c('cfi.scaled','tli.scaled','RMSEA.scaled'))
# cfi.scaled tli.scaled 
# 0.693      0.681 
# rmsea scaled: 0.053


# Model comparison single-factor model vs three-factor model
anova(tri.3fac.2pl, tri.1dim.2pl) # ***

lavTestLRT(tri.3fac.2pl, tri.1dim.2pl) #, method="satorra.bentler.2010")
# satorra.bentler.2010, 2001 : lavaan WARNING: some estimated ov variances are negative


fitMeasures(tri.3fac.2pl,"cfi.scaled") - fitMeasures(tri.1dim.2pl,"cfi.scaled")
# cfi.scaled 
# 0.106

# Reliability estimate for three-factor model
reliability(tri.3fac.2pl) 




# 4) Bifactor 2pl
mod.tri.bif.2pl <- '
GEN =~ Item.1+Item.2+Item.3+Item.4+Item.5+Item.6+Item.7
      +Item.8+Item.9+Item.10+Item.11+Item.12+Item.13+Item.14
      +Item.15+Item.16+Item.17+Item.18+Item.19+Item.20+Item.21
      +Item.22+Item.23+Item.24+Item.25+Item.26+Item.27+Item.28
      +Item.29+Item.30+Item.31+Item.32+Item.33+Item.34+Item.35
      +Item.36+Item.37+Item.38+Item.39+Item.40+Item.41+Item.42
      +Item.43+Item.44+Item.45+Item.46+Item.47+Item.48+Item.49
      +Item.50+Item.51+Item.52+Item.53+Item.54+Item.55+Item.56+Item.57+Item.58

Boldness =~ Item.1+Item.4+Item.7 + Item.10+Item.13+Item.16+Item.19
            +Item.22+Item.25+Item.28 + Item.32+Item.35+Item.38
            +Item.41+Item.44+Item.47 + Item.50+Item.54+Item.57

Disinhibition =~ Item.3+Item.5+Item.9 + Item.12+Item.15+Item.18
                 +Item.21+Item.24+Item.27 + Item.30+Item.31+Item.34+Item.37
                 +Item.43+Item.46+Item.49 + Item.51+Item.53+Item.56+Item.58
Callousness =~ Item.2+Item.6+Item.8 + Item.11+Item.14+Item.17
               +Item.20+Item.23+Item.26+Item.29 + Item.33+Item.36+Item.39
               +Item.40+Item.42+Item.45+Item.48 + Item.52+Item.55
'


tri.bif.2pl <- cfa(mod.tri.bif.2pl, data=tripm.dic, std.lv=T, 
                   ordered=colnames(tripm.dic), estimator="WLSMV", orthogonal=TRUE)

#save(tri.bif.2pl, file='tri.bif.2pl.RData')
load('tri.bif.2pl.RData') 


summary(tri.bif.2pl, fit.measure=T, standardized=F, rsquare=T)

fitMeasures(tri.bif.2pl, c("chisq.scaled", "df", "pvalue.scaled"))
fitMeasures(tri.bif.2pl, c('cfi.scaled','tli.scaled','rmsea.scaled'))
# cfi.scaled tli.scaled 
#  0.848      0.836 
# rmsea scaled: 0.038


# Model comparison three-factor model vs bifactor model
anova(tri.bif.2pl, tri.3fac.2pl) # ***

lavTestLRT(tri.bif.2pl, tri.3fac.2pl, method="satorra.bentler.2001") # ***

fitMeasures(tri.bif.2pl,"cfi.scaled") - fitMeasures(tri.3fac.2pl,"cfi.scaled") # 0.154



# Reliability estimate for bifactor model
reliability(tri.bif.2pl) 


############ Alternative factor structures #########

# 5) Alternatiave 1: correlated two factors
mod.tri.alt.2fac <- '
Boldness =~ Item.1+Item.4+Item.7 + Item.10+Item.13+Item.16+Item.19
            +Item.22+Item.25+Item.28 + Item.32+Item.35+Item.38
            +Item.41+Item.44+Item.47 + Item.50+Item.54+Item.57

Disinhi_Callous =~ Item.3+Item.5+Item.9 + Item.12+Item.15+Item.18
                  +Item.21+Item.24+Item.27 + Item.30+Item.31+Item.34+Item.37
                  +Item.43+Item.46+Item.49 + Item.51+Item.53+Item.56+Item.58
                  +Item.2+Item.6+Item.8 + Item.11+Item.14+Item.17
                  +Item.20+Item.23+Item.26+Item.29 + Item.33+Item.36+Item.39
                  +Item.40+Item.42+Item.45+Item.48 + Item.52+Item.55
'


tri.alt.2fac <- cfa(mod.tri.alt.2fac, data=tripm.dic, std.lv=T, 
                    ordered=colnames(tripm.dic), estimator="WLSMV")

#save(tri.alt.2fac, file='tri.alt.2fac.RData')
load('tri.alt.2fac.RData') 

summary(tri.alt.2fac, fit.measure=T, standardized=F, rsquare=T)


fitMeasures(tri.alt.2fac, c('cfi.scaled','tli.scaled','rmsea.scaled'))
# cfi.scaled tli.scaled 
# 0.671      0.659


# Model comparison bifactor model vs alternative two-factor model
anova(tri.bif.2pl, tri.alt.2fac)  # bifactor ***
lavTestLRT(tri.bif.2pl, tri.alt.2fac, method="satorra.bentler.2001")

fitMeasures(tri.bif.2pl,"cfi.scaled")-fitMeasures(tri.alt.2fac,"cfi.scaled") # 0.176

# Model comparison three-factor model vs alternative two-factor model
anova(tri.3fac.2pl, tri.alt.2fac) # three-factor ***
lavTestLRT(tri.3fac.2pl, tri.alt.2fac, method="satorra.bentler.2001")

fitMeasures(tri.3fac.2pl,"cfi.scaled")-fitMeasures(tri.alt.2fac,"cfi.scaled") # 0.022

# Reliability estimate for alternative two-factor model
reliability(tri.alt.2fac)


# 6) Alternatiave 2: correlated two factors with a general factor
mod.tri.alt.2fac.bif <- '
GEN =~ Item.1+Item.2+Item.3+Item.4+Item.5+Item.6+Item.7
      +Item.8+Item.9+Item.10+Item.11+Item.12+Item.13+Item.14
      +Item.15+Item.16+Item.17+Item.18+Item.19+Item.20+Item.21
      +Item.22+Item.23+Item.24+Item.25+Item.26+Item.27+Item.28
      +Item.29+Item.30+Item.31+Item.32+Item.33+Item.34+Item.35
      +Item.36+Item.37+Item.38+Item.39+Item.40+Item.41+Item.42
      +Item.43+Item.44+Item.45+Item.46+Item.47+Item.48+Item.49
      +Item.50+Item.51+Item.52+Item.53+Item.54+Item.55+Item.56+Item.57+Item.58

Boldness =~ Item.1+Item.4+Item.7 + Item.10+Item.13+Item.16+Item.19
            +Item.22+Item.25+Item.28 + Item.32+Item.35+Item.38
            +Item.41+Item.44+Item.47 + Item.50+Item.54+Item.57

Disinhi_Callous =~ Item.3+Item.5+Item.9 + Item.12+Item.15+Item.18
                 +Item.21+Item.24+Item.27 + Item.30+Item.31+Item.34+Item.37
                 +Item.43+Item.46+Item.49 + Item.51+Item.53+Item.56+Item.58
                 +Item.2+Item.6+Item.8 + Item.11+Item.14+Item.17
                 +Item.20+Item.23+Item.26+Item.29 + Item.33+Item.36+Item.39
                 +Item.40+Item.42+Item.45+Item.48 + Item.52+Item.55
'
tri.alt.2fac.bif <- cfa(mod.tri.alt.2fac.bif, data=tripm.dic, std.lv=T, orthogonal=TRUE, 
                    ordered=colnames(tripm.dic), estimator="WLSMV")

#save(tri.alt.2fac.bif, file='tri.alt.2fac.bif.RData')
load('tri.alt.2fac.bif.RData') 

summary(alt.2fac.bif, fit.measure=T, standardized=F, rsquare=T)


fitMeasures(alt.2fac.bif, c('cfi.scaled','tli.scaled'))
# cfi.scaled tli.scaled 
# 0.843      0.831 



# Model comparison bifactor model vs alternative bifactor model
#anova(tri.bif.2pl, tri.alt.2fac.bif)  # warning
#lavTestLRT(tri.bif.2pl, tri.alt.2fac.bif, method="satorra.bentler.2001") # warning

fitMeasures(tri.bif.2pl,"cfi.scaled")-fitMeasures(tri.alt.2fac.bif,"cfi.scaled") # 0.004

# Reliability estimate for alternative two-factor model
reliability(tri.alt.2fac.bif)
#reliability(tri.bif.2pl)  # for comparison

# Model comparison alternative two-factor model vs alternative bifactor model
anova(tri.alt.2fac, tri.alt.2fac.bif)   # alternative bifactor ***



############ c) Sum scores of the scale / Calculation of reliability by hand #######

# Extract the loadings of the general factor
(gen <- coef(tri.bif.2pl)[1:58])
(gen.b <- coef(tri.bif.2pl)[items.b]) 
# gen ~ boldness items
(gen.d <- coef(tri.bif.2pl)[items.d])
# gen ~ disinhibition items
(gen.c <- coef(tri.bif.2pl)[items.m])
# gen ~ meanness items

# Extract the loadings of each subscale
(tri.b <- coef(tri.bif.2pl)[59:77]) # 19
(tri.d <- coef(tri.bif.2pl)[78:97]) # 20
(tri.c <- coef(tri.bif.2pl)[98:116]) # 19
(tri.bdc <- coef(tri.bif.2pl)[59:116])


# Omega total
(w.tot<-(sum(gen)^2+sum(tri.b)^2+sum(tri.d)^2
         +sum(tri.c)^2)/(sum(gen)^2+sum(tri.b)^2+sum(tri.d)^2+sum(tri.c)^2+sum(u2)))
# 0.9583324

(w.tot.b<-(sum(gen.b)^2+sum(tri.b)^2)/(sum(gen.b)^2+sum(tri.b)^2+sum(u2[items.b])))
# 0.860488

(w.tot.d<-(sum(gen.d)^2+sum(tri.d)^2)/(sum(gen.d)^2+sum(tri.d)^2+sum(u2[items.d])))
# 0.9528182

(w.tot.c<-(sum(gen.c)^2+sum(tri.c)^2)/(sum(gen.c)^2+sum(tri.c)^2+sum(u2[items.c])))
# 0.9479739



# Omega hierarchical
(w.h <- (sum(gen)^2)/(sum(gen)^2+sum(tri.b)^2+sum(tri.d)^2+sum(tri.c)^2+sum(u2)))
# 0.7750652

(w.h.b <-(sum(tri.b)^2)/(sum(gen.b)^2+sum(tri.b)^2+sum(u2[items.b]))) # 0.8548624

(w.h.d <-(sum(tri.d)^2)/(sum(gen.d)^2+sum(tri.d)^2+sum(u2[items.d]))) # 0.04528771
(w.h.c <-(sum(tri.c)^2)/(sum(gen.c)^2+sum(tri.c)^2+sum(u2[items.c]))) # 0.2393709


compRelSEM(tri.bif.2pl, obs.var=T, ord.scale=T) 
# GEN      Boldness Disinhibition   Callousness 
#0.596         0.796         0.076         0.173 

compRelSEM(tri.bif.2pl, obs.var=F, ord.scale=T)
#GEN      Boldness Disinhibition   Callousness 
#0.582         0.774         0.077         0.176 

compRelSEM(tri.bif.2pl, obs.var=T, ord.scale=F) # Removes green and yang correction
#GEN      Boldness Disinhibition   Callousness 
#0.795         0.902         0.044         0.241 

compRelSEM(tri.bif.2pl, obs.var=F, ord.scale=F)   # Same result as omega_hierarchical above
#GEN      Boldness Disinhibition   Callousness 
#0.775         0.870         0.045         0.241 


# Communalities and uniqueness of items
(h2 <- gen^2 + tri.bdc^2)
(u2 <- 1-h2)


# Explained Common Variance of general factor
gen.sq = sum(gen^2)
oth = sum(tri.bdc^2)
(ECV = gen.sq/(gen.sq+oth))  # 0.6219525




# Sum scores of the scale and subscales
items.b <- c(1,4,7,10,13,16,19,22,25,28,32,35,38,41,44,47,50,54,57)
items.d <- c(3,5,9,12,15,18,21,24,27,30,31,34,37,43,46,49,51,53,56,58)
items.m <- c(2,6,8,11,14,17,20,23,26,29,33,36,39,40,42,45,48,52,55)

# Sum scores of main scale
sum(tripm.dic) # 23090

# Sum scores of sub scales
sum(tripm.dic[items.b])+sum(tripm.dic[items.d])+sum(tripm.dic[items.m]) # 23090

sum(tripm.dic[items.b]) # 12717
sum(tripm.dic[items.d]) # 5957
sum(tripm.dic[items.m]) # 4416



############ d) Exploratory omega ######

# Exploratory omega analyses
#omega(tripm.dic, nfactor=2)  # w_t 0.89, ECV 0.07
omega(tripm.dic)  # default(nfactor=3)
omega(tripm.dic, nfactor=4)  # w_t 0.90, w_h/asymptotic 0.65/0.72 ECV 0.45 
omega(tripm.dic, nfactor=5)  # w_t 0.91, w_h/asymptotic 0.57/0.63 ECV 0.34
omega(tripm.dic, nfactor=6)  # w_t 0.91, w_h/asymptotic 0.59/0.65 ECV 0.34
omega(tripm.dic, nfactor=7)  # w_t 0.91, w_h/asymptotic 0.59/0.65 ECV 0.33

# Exploratory omega analyses using different functions
omegaSem(tripm.dic)  # w_t 0.90 w_h/asymptotic 0.55/0.61 ECV 0.4
omegaDirect(tripm.dic)   # w_t 0.89 / w_h 0.68

# Four-factor structure
mod.tri.omg.4fac <- '
        F1 =~ Item.12+Item.18+Item.20+Item.26+Item.29+Item.34
              +Item.37+Item.40+Item.42+Item.43+Item.48+Item.49+Item.51
              +Item.55+Item.56+Item.58
        
        F2=~ Item.2+Item.11+Item.33+Item.36+Item.52   

        F3 =~ Item.13+Item.19+Item.38+Item.45+Item.57    
         
        F4 =~ Item.1+Item.7+Item.10+Item.16+Item.21+Item.22
              +Item.31+Item.44+Item.50 
              '

tri.omg.4fac <- cfa(mod.tri.omg.4fac, data=tripm.dic, std.lv=T, 
                    ordered=colnames(tripm.dic), estimator="WLSMV")

#save(tri.omg.4fac, file='tri.omg.4fac.RData')
load('tri.omg.4fac.RData') 


summary(tri.omg.4fac, fit.measure=T, standardized=F, rsquare=T)

fitMeasures(tri.omg.4fac, c('cfi.scaled','tli.scaled', 'RMSEA.scaled'))
# cfi.scaled   tli.scaled rmsea.scaled 
# 0.865        0.855        0.048 

# for comparison (bifactor model with three factors)
#fitMeasures(tri.bif.2pl, c('cfi.scaled','tli.scaled','RMSEA.scaled'))
# cfi.scaled   tli.scaled rmsea.scaled 
#      0.848        0.836        0.038 

# Reliability of four-factor model
reliability(tri.omg.4fac)

# Four-factor solution
omegaFromSem(tri.omg.4fac)



############ b1) Proportion of correct responses ##########

# Item difficulty in CTT
Ncorrect <- apply(tripm.dic, 2, sum)
cttdiff <- (Ncorrect/nrow(tripm.dic))

sort(cttdiff, decreasing=TRUE)


############ b1) Biserial correlation #############

library(psych)

# Individual sum score
(sumscores <- apply(tripm.dic, 1, sum))
tripm.dic.sum <- data.frame(tripm.dic, indv_SumScores=sumscores)

# Biserial correlation
(bis <- apply(tripm.dic.sum[,-59], 2, function(x) biserial(tripm.dic.sum$indv_SumScores, x)))
sort(bis, decreasing=F)

# Frequencies by total sum scores
table(tripm.dic.sum$indv_SumScores)


#-----------------------------------


# b2 1pl
############# b2) 1pl ltm ###########

# Load the packages
library(ltm)     
library(eRm)    
library(mirt)     
library(TAM)
library(lavaan)


# 1) ltm (MML estimation)
tripm.ltm <- ltm::rasch(tripm.dic, constraint=cbind(ncol(tripm.dic)+1, 1), IRT.param=T)

#save(tripm.ltm, file='tripm.ltm.RData')
load('tripm.ltm.RData') 

summary(tripm.ltm)
coef(tripm.ltm)


(diff.tripm.ltm <- coef(tripm.ltm)[,1]) 
(diff.tripm.ltm <- diff.tripm.ltm[order(diff.tripm.ltm)])


# ICC
plot(tripm.ltm, type="ICC", items=58,  zrange = c(-6,6)) 

# TIC
plot(tripm.ltm, type="IIC", items=0,lwd=3, zrange=c(-6,6)) 

############# b2) 1pl eRm ##########

# 2) eRm (CML estimation)
tripm.erm <- RM(tripm.dic, sum0=T)

#save(tripm.erm, file='tripm.erm.RData')
load('tripm.erm.RData')

#tripm.erm$W  #design matrix
#tripm.erm$betapar  #beta: easiness in eRm

summary(tripm.erm)


(diff.tripm.erm <- -coef(tripm.erm))
(diff.tripm.erm <- diff.tripm.erm[order(diff.tripm.erm)])

# Plot ICCs
#eRm::plotICC(tripm.erm, item.subset = 55:58)
eRm::plotICC(tripm.erm, item.subset = 58, ask = F, 
             empICC = list("raw"), empCI = list(lty = "solid"))

plotjointICC(tripm.erm)


############# b2) 1pl mIRT  ##########

# 3) mIRT (EM alghorithm or MH-RM)
(tripm.mirt <- mirt(tripm.dic, 1, itemtype = "Rasch"))

#save(tripm.mirt, file='tripm.mirt.RData')
load('tripm.mirt.RData')

#summary(tripm.mirt)   #FA results

coef(tripm.mirt, simplify=T)
coef(tripm.mirt, simplify=T, IRTpars=T) 

diff.tripm.mirt <- as.matrix(coef(tripm.mirt,simplify=T,IRTpars=T)$items) 
(diff.tripm.mirt <- diff.tripm.mirt[,2])
(diff.tripm.mirt <- diff.tripm.mirt[order(diff.tripm.mirt)])



# Test standard errors
plot(tripm.mirt, type="SE") 

# TIF
plot(tripm.mirt, type="info") 

#plot(tripm.mirt, type="infoSE") 
#plot(tripm.mirt, type="infotrace")

# Reliability function
plot(tripm.mirt, type="rxx") 

# Item probability traceline plots
#plot(tripm.mirt, type="trace")



# Eexpected total score
plot(tripm.mirt, type="score") 

# Expected item score
#plot(tripm.mirt, type="itemscore")  

dev.off()

############# b2) 1pl TAM #############
# 4) TAM
tripm.tam <- TAM::tam(tripm.dic)
summary(tripm.tam)

#save(tripm.tam, file='tripm.tam.RData')
load('tripm.tam.RData')


(diff.tripm.tam <- tripm.tam$xsi[,1]) #difficulty
(diff.tripm.tam <- diff.tripm.tam[order(diff.tripm.tam)]) 


############# b2) 1pl lavaan ###############
# 5) lavaan 

tri.1pl <- 'ksi =~ a*Item.1+a*Item.2+a*Item.3+a*Item.4+a*Item.5+a*Item.6+a*Item.7
                  +a*Item.8+a*Item.9+a*Item.10+a*Item.11+a*Item.12+a*Item.13+a*Item.14
                  +a*Item.15+a*Item.16+a*Item.17+a*Item.18+a*Item.19+a*Item.20+a*Item.21
                  +a*Item.22+a*Item.23+a*Item.24+a*Item.25+a*Item.26+a*Item.27+a*Item.28
                  +a*Item.29+a*Item.30+a*Item.31+a*Item.32+a*Item.33+a*Item.34+a*Item.35
                  +a*Item.36+a*Item.37+a*Item.38+a*Item.39+a*Item.40+a*Item.41+a*Item.42
                  +a*Item.43+a*Item.44+a*Item.45+a*Item.46+a*Item.47+a*Item.48+a*Item.49
                  +a*Item.50+a*Item.51+a*Item.52+a*Item.53+a*Item.54+a*Item.55+a*Item.56
                  +a*Item.57+a*Item.58'

tripm.lav <- cfa(tri.1pl, data=tripm.dic, std.lv=T, ordered=colnames(tripm.dic))

#save(tripm.lav, file='tripm.lav.RData')
load('tripm.lav.RData')


summary(tripm.lav, fit.measure=T, standardized=F, rsquare=T)
fitMeasures(tripm.lav, c('cfi.scaled','tli.scaled'))


standardizedSolution(tripm.lav) 
fitted(tripm.lav) 

# Convert to IRT parameters
(lambda <- tripm.lav@Model@GLIST$lambda[1,]) 
(tau <- fitted(tripm.lav)$th)
D <- 1.702    # probit to logit 

(alpha <- D*lambda/sqrt(1-lambda^2))  
(diff.tripm.unconst<-tau/lambda)   # difficulties for unconstrained Rasch

# Convert to 1pl difficulty
(diff.tripm.lav <- alpha * diff.tripm.unconst)  
(diff.tripm.lav <- diff.tripm.lav[order(diff.tripm.lav)])


############# b2) 1pl Difficulty  ############

# Compare the difficulties for 1PL
df <- as.data.frame(diff.tripm.mirt)

diffmat<-matrix(0,58,5)
colnames(diffmat)<-c("CML-eRm", "MML-ltm", "EM-mirt", "lavaan", "MML-TAM")
rownames(diffmat)<- rownames(df)


# Load the different estimates
diffmat[,1]<- diff.tripm.erm
diffmat[,2]<- diff.tripm.ltm
diffmat[,3]<- diff.tripm.mirt
diffmat[,4]<- diff.tripm.lav
diffmat[,5]<- diff.tripm.tam

diffmat

# Rescale the valeus
(diff1pl<-diffmat-matrix(rep(diffmat[30,],58), 58, byrow =T))  

# Compare IRT vs CTT difficulties
head(diff1pl)
tail(diff1pl)

# CTT
sort(cttdiff, decreasing=TRUE)

############# b2) 1pl Person parameter ###############

# 1) TAM
pers.tripm.tam <- CDM::IRT.factor.scores(tripm.tam, type="EAP")

# 2) ltm
# Person ability estimates for every individual
pers.tripm.ltm2 <- ltm::factor.scores(tripm.ltm, resp.patterns=tripm.dic)

plot(pers.tripm.ltm2) 
plot(density(pers.tripm.ltm2$score.dat$z1))


# Check linearity
r <- apply(tripm.dic,1,sum)
plot(cbind(pers.tripm.ltm2$score.dat$z1, r)) 
abline(lm(r ~ pers.tripm.ltm2$score.dat$z1))


# 3) eRm
pers.tripm.erm <- eRm::person.parameter(tripm.erm)  

summary(pers.tripm.erm) 

# Plot of person parameters by person raw scores
plot(pers.tripm.erm) 

# Person-Item map
plotPImap(tripm.erm, sorted=T)


# Check linearity
plot(cbind(unlist(pers.tripm.erm$thetapar), r)) 
abline(lm(r ~ unlist(pers.tripm.erm$thetapar)))



# 4) mirt
# Person ability estimates
pers.tripm.mirt.eap <- fscores(tripm.mirt) #default EAP
head(pers.tripm.mirt.eap)
pers.tripm.mirt.map <- fscores(tripm.mirt, method = "MAP") 
head(pers.tripm.mirt.map)
pers.tripm.mirt.ml <- fscores(tripm.mirt, method = "ML") 
head(pers.tripm.mirt.ml)

dev.off()

# Check linearity
plot(cbind(pers.tripm.mirt.eap, r))
abline(lm(r ~ pers.tripm.mirt.eap))


# 5) lavaan
pers.tripm.lav <- lavPredict(tripm.lav)


####  Comparison 1pl person parameter

# Create a dataframe containing person parameter estimates
FS <- data.frame(mirtEAP = as.vector(pers.tripm.mirt.eap), 
                 mirtMAP = as.vector(pers.tripm.mirt.map), 
                 mirtML = as.vector(pers.tripm.mirt.ml), 
                 ltm = pers.tripm.ltm2$score.dat$z1, 
                 eRm = unlist(pers.tripm.erm$thetapar),
                 lavaan = unlist(pers.tripm.lav),
                 tam =pers.tripm.tam[,1])

FS.densities <- apply(FS, 2, density)
plot(NA, xlim=range(sapply(FS.densities, "[", "x")), ylim=range(sapply(FS.densities, "[", "y")))
mapply(lines, FS.densities, col=1:length(FS.densities))
legend("topright", legend=names(FS.densities), fill=1:length(FS.densities))


############# b2) 1pl Infit / Outfit ####################

# Comparison of item fit : infit/outfit
# TAM
TAM::tam.fit(tripm.tam)

summary(TAM::tam.fit(tripm.tam))

# Plot
plot(tripm.tam, items=48)  


# eRm
pers.tripm.erm <- eRm::person.parameter(tripm.erm) 

eRm::itemfit(pers.tripm.erm) 


# Person-item map
plotPWmap(tripm.erm) # person/item parameters vs infit t-statistics (ZSTD)


plotPWmap(tripm.erm, imap = TRUE, pmap = TRUE,  # with infit t statistic
          person.subset = 1:115, 
          item.pch = 1,person.pch = 16, 
          itemCI = list()



# Calculates and plots the individual or summed item information
#plotINFO(tripm.erm) 

dev.off()


# mirt
mirt::itemfit(tripm.mirt, fit_stats='infit')

#-----------------------------------


# b3 2pl
############# b3) 2pl all packages ##############

# 1) TAM 2pl

tripm.tam.2pl <- tam.mml.2pl(tripm.dic, irtmodel='2PL')
summary(tripm.tam.2pl, IRT.param=TRUE)  

# Difficulty
tripm.tam.2pl$xsi 
(diff.tam.2pl <- tripm.tam.2pl$xsi[,1]/tripm.tam.2pl$B[59:116])

# Discrimination
(dsc.tam.2pl <- as.data.frame(tripm.tam.2pl$B)[,2]) 

# Comprison with 1pl TAM
anova(tripm.tam, tripm.tam.2pl)  # *** 





# 2) mirt 2PL 
(tripm.mirt.2pl <- mirt(tripm.dic, model=1, itemtype='2PL')) # 2PL default
#summary(tripm.mirt.2pl) 

# Difficulty
(diff.mirt.2pl_full <- as.matrix(coef(tripm.mirt.2pl, simplify=T, IRTpars=T)$items)) 
(diff.mirt.2pl <- diff.mirt.2pl_full[,2])


# Discrimination
(dsc.mirt.2pl <- diff.mirt.2pl_full[,1])

# Comprison with 1pl mirt
anova(tripm.mirt, tripm.mirt.2pl)   # *** 


############# b3) 2pl Difficulty, Discrimination ################################

# Order by item difficulty
(diff.mirt.2pl <- diff.mirt.2pl[order(diff.mirt.2pl)])
(diff.tam.2pl <- diff.tam.2pl[order(diff.tam.2pl)])


# Comparison of item difficulty
diff.2pl.mat<-matrix(0,58,2)
colnames(diff.2pl.mat)<-c("EM-mirt", "MML-TAM")

diff.order.2pl <- as.data.frame(unlist(diff.mirt.2pl))
(rownames(diff.2pl.mat)<-rownames(diff.order.2pl)) 


#load the different estimates
diff.2pl.mat[,1]<- diff.mirt.2pl
diff.2pl.mat[,2]<- diff.tam.2pl


# Rescale difficulties   
(diff.2pl <- diff.2pl.mat - matrix(rep(diff.2pl.mat[30,],58), 58, byrow =T))
#diff1pl # for comparison



# Comparison of item discrimination
(dsc.mirt.2pl <- dsc.mirt.2pl[order(dsc.mirt.2pl)])
(dsc.tam.2pl <- dsc.tam.2pl[order(dsc.tam.2pl)])

dsc.2pl.mat<-matrix(0,58,2)
colnames(dsc.2pl.mat)<-c("EM-mirt", "MML-TAM")

dsc.order.2pl <- as.data.frame(unlist(dsc.mirt.2pl))
(rownames(dsc.2pl.mat)<-rownames(dsc.order.2pl))  

dsc.2pl.mat[,1]<- dsc.mirt.2pl
dsc.2pl.mat[,2]<- dsc.tam.2pl

dsc.2pl.mat

# Plots for interesting items
#plot(tripm.tam.2pl, items=22)


############# b3) 2pl Person parameter ############

# Person parameter 2pl models

# 1) mirt
pers.2pl.mirt.eap <- fscores(tripm.mirt.2pl) #default EAP
head(pers.2pl.mirt.eap)
pers.2pl.mirt.map <- fscores(tripm.mirt.2pl, method = "MAP") 
head(pers.2pl.mirt.map)
#pers.2pl.mirt.ml <- fscores(tripm.mirt.2pl, method = "ML") 
#head(pers.2pl.mirt.ml)  # waring: failed to converge successfully

dev.off()

# Check linearity
plot(cbind(pers.2pl.mirt.eap, r))
abline(lm(r ~ pers.2pl.mirt.eap))  


# Plots
# ICCs of all 58 items
#plot(tripm.mirt.2pl, type="trace") 

# mirt : TIF and I(theata)
plot(tripm.mirt.2pl, type="infoSE") 

# 2) TAM
pers.2pl.tam.eap <- CDM::IRT.factor.scores(tripm.tam.2pl, type="EAP")
#pers.2pl.tam.wle <- CDM::IRT.factor.scores(tripm.tam.2pl, type="WLE")   #warning
#pers.2pl.tam.mle <- CDM::IRT.factor.scores(tripm.tam.2pl, type="MLE")   #warning


# Comparison of person parameters

# Plot the density of estimtes 
FS.2pl <- data.frame(mirtEAP = as.vector(pers.2pl.mirt.eap), 
                     mirtMAP = as.vector(pers.2pl.mirt.map), 
                     #mirtML = as.vector(pers.2pl.mirt.ml), 
                     #ltm = pers.2pl.ltm2$score.dat$z1, 
                     tam = pers.2pl.tam.eap[,1])

FS.2pl.densities <- apply(FS.2pl, 2, density)
plot(NA, xlim=range(sapply(FS.2pl.densities, "[", "x")), ylim=range(sapply(FS.2pl.densities, "[", "y")))
mapply(lines, FS.2pl.densities, col=1:length(FS.2pl.densities))
legend("topright", legend=names(FS.2pl.densities), fill=1:length(FS.2pl.densities))


############# b3) 2pl Infit / Outfit  ##########

# Comparison of item fit: infit/outfit
# 1) mirt
mirt::itemfit(tripm.mirt.2pl, fit_stats='infit') 

# 2) TAM
(TAM::tam.fit(tripm.tam.2pl)$itemfit)
summary(TAM::tam.fit(tripm.tam.2pl))


#-----------------------------------


# b4 DIF
############# b4) DIF: Data preparation #########

library(difR)
library(ltm) 
library(lme4)


# Data preparation for DIF analyis
tripm.dif.SEX <- data.frame(SEX=tripm$SEX, tripm.dic)
tripm.dif.PROB <- data.frame(PSYCH_PROB=tripm$PSYCH_PROB, tripm.dic)

tripm.dif.SEX$SEX <- as.numeric(tripm.dif.SEX$SEX)
tripm.dif.PROB$PSYCH_PROB <- as.numeric(tripm.dif.PROB$PSYCH_PROB)



# For multi-group DIF 
group <- rep("Female.ReqX", nrow(tripm.dic))    # female, request X as reference

group[tripm.dif.SEX$SEX==1 & tripm.dif.PROB$PSYCH_PROB==1] <- "Male.Req"
group[tripm.dif.SEX$SEX==1 & tripm.dif.PROB$PSYCH_PROB==2] <- "Male.ReqX"
group[tripm.dif.SEX$SEX==2 & tripm.dif.PROB$PSYCH_PROB==1] <- "Female.Req"
group

tripm.dif <- cbind(tripm.dic, group)
str(tripm.dif)

focal.grps <- c("Male.Req","Male.ReqX","Female.Req")


############# b4) DIF w.r.t SEX (1pl, 2pl) ########

#### 1pl

# Mantel-Haenszel statistics
(MH.SEX <-difMH(tripm.dif.SEX, group = 'SEX', focal.name = 1, purify=T)) 

#save(MH.SEX, file='DIF_SEX_1pl_MH.RData')
load('DIF_SEX_1pl_MH.RData') 

plot(MH.SEX) 


# IRT method: Lord's Qj chi squared test 
(lord.SEX.1pl <-difLord(tripm.dif.SEX, group = 'SEX', focal.name = 1, model = "1PL",purify=T)) 

#save(lord.SEX.1pl, file='DIF_SEX_1pl_lord.RData')
load('DIF_SEX_1pl_lord.RData') 

plot(lord.SEX.1pl, plot = "itemCurve", item = 11)
plot(lord.SEX.1pl, plot = "itemCurve", item = 6) 


# Breslow-day statistics
(BD.SEX <-difBD(tripm.dif.SEX, group = 'SEX', focal.name = 1, purify=T)) 

#save(BD.SEX, file='DIF_SEX_1pl_BD.RData')
load('DIF_SEX_1pl_BD.RData') 


plot(BD.SEX)  


#### 2pl
(lord.SEX.2pl <-difLord(tripm.dif.SEX, group = 'SEX', focal.name = 1, model = "2PL", purify=T))  

#save(lord.SEX.2pl, file='DIF_SEX_2pl_lord.RData')
load('DIF_SEX_2pl_lord.RData') 

plot(lord.SEX.2pl, plot = "itemCurve", item = 10)


# Comparison of the methods
(dif.SEX.1pl <- dichoDif(tripm.dif.SEX, group='SEX', focal.name=1, 
                         method=c('MH','Std','Logistic','Raju','Lord'), model='1PL'))


#save(dif.SEX.1pl, file='DIF_SEX_1pl_dichoDif.RData')
load('DIF_SEX_1pl_dichoDif.RData') 

dif.SEX.1pl


(dif.SEX.2pl <- dichoDif(tripm.dif.SEX, group='SEX', focal.name=1, 
                         method=c('MH','BD','Std','Logistic','Raju','Lord'), model='2PL',
                         save.output=T))

#save(dif.SEX.2pl, file='DIF_SEX_2pl_dichoDif.RData')
load('DIF_SEX_2pl_dichoDif.RData') 

dif.SEX.2pl

############# b4) DIF w.r.t PSYCH_PROB (1pl, 2pl) ########

### 1pl

# Mantel-Haenszel statistics
(MH.PROB <-difMH(tripm.dif.PROB, group = 'PSYCH_PROB', focal.name = 1, purify=T)) 

#save(MH.PROB, file='DIF_PROB_1pl_MH.RData')
load('DIF_PROB_1pl_MH.RData') 

MH.PROB

plot(MH.PROB) 


# Breslow-day statistics
(BD.PROB <-difBD(tripm.dif.PROB, group = 'PSYCH_PROB', focal.name = 1, purify=T)) 

#save(BD.PROB, file='DIF_PROB_1pl_BD.RData')
load('DIF_PROB_1pl_BD.RData') 

BD.PROB

plot(BD.PROB)  


# Lord's Qj chi squared test
(lord.PROB.1pl <-difLord(tripm.dif.PROB, group = 'PSYCH_PROB', focal.name = 1, model = "1PL",purify=T)) 

#save(lord.PROB.1pl, file='DIF_PROB_1pl_lord.RData')
load('DIF_PROB_1pl_lord.RData') 

lord.PROB.1pl

plot(lord.PROB.1pl) 



##### 2PL
(lord.PROB.2pl <-difLord(tripm.dif.PROB, group = 'PSYCH_PROB', focal.name = 1, model = "2PL",purify=T)) 

#save(lord.PROB.2pl, file='DIF_PROB_2pl_lord.RData')
load('DIF_PROB_2pl_lord.RData') 

lord.PROB.2pl

plot(lord.PROB.2pl, plot = "itemCurve", item = 10)


# Comparison of the methods
(dif.PROB.1pl <- dichoDif(tripm.dif.PROB, group='PSYCH_PROB', focal.name=1, purify=T,
                          method=c('MH','Std','Logistic','Raju','Lord'), model='1PL'))

#save(dif.PROB.1pl, file='DIF_PROB_1pl_dichoDif.RData')
load('DIF_PROB_1pl_dichoDif.RData')

dif.PROB.1pl  


(dif.PROB.2pl <- dichoDif(tripm.dif.PROB, group='PSYCH_PROB', focal.name=1, purify=T,
                          method=c('MH','BD','Std','Logistic','Raju','Lord'), model='2PL'))  

#save(dif.PROB.2pl, file='DIF_PROB_2pl_dichoDif.RData')
load('DIF_PROB_2pl_dichoDif.RData')

dif.PROB.2pl


############# b4) DIF multi-group comparison ########

# Uniform DIF
(mh <- difGMH(tripm.dif, group = 59, focal.names = focal.grps))

#save(mh, file='DIF_multigroup_MH.RData')
load('DIF_multigroup_MH.RData')

plot(mh)


# Lord's Qj chi squared test : 1pl
(lord1pl <- difGenLord(tripm.dif, group = 59, focal.names = focal.grps,
                       model='1PL')) 

#save(lord1pl, file='DIF_multigroup_1pl_difGenLord.RData')
load('DIF_multigroup_1pl_difGenLord.RData')

lord1pl

# Lord's Qj chi squared test : 2pl
(lord2pl <- difGenLord(tripm.dif, group = 59, focal.names = focal.grps,
                       model='2PL'))  

#save(lord2pl, file='DIF_multigroup_2pl_difGenLord.RData')
load('DIF_multigroup_2pl_difGenLord.RData')

lord2pl 



# Plots
plot(lord1pl)
plot(lord2pl)

plot(lord1pl, plot='itemCurve', item=55)
plot(lord2pl, plot='itemCurve', item=10)
plot(lord2pl, plot='itemCurve', item=55) 



# Comparision of the methods
#  1pl
(dif1pl <- genDichoDif(tripm.dif, group = 59, focal.names = focal.grps,
                       method = c("GMH","genLord","genLogistic"),
                       model='1PL'))

dif1pl

#save(dif1pl, file='DIF_multigroup_1pl_genDichoDif.RData')
load('DIF_multigroup_1pl_genDichoDif.RData')

# default: 2pl
(dif2pl <- genDichoDif(tripm.dif, group = 59, focal.names = focal.grps,
                       method = c("GMH","genLord","genLogistic"), save.output=T))


#save(dif2pl, file='DIF_multigroup_2pl_genDichoDif.RData')
load('DIF_multigroup_2pl_genDichoDif.RData')

dif2pl



#-----------------------------------



# e) measure invariance

############# e) MI of SEX #######

# Data preparation for the analysis
tripm[,1:4]  # ID, SEX, AGE, PSYCH_PROB

tripm58.mi <- cbind(tripm[,1:4], tripm58)
str(tripm58.mi)  # 58 items with 4 variables

summary(tripm$SEX)   # unequal sample sizes
# 1   2 
# 810 613 


# Fit the bifactor model to original data
mod.tri.bif.conf <- '
GEN =~ Item.1+Item.2+Item.3+Item.4+Item.5+Item.6+Item.7
      +Item.8+Item.9+Item.10+Item.11+Item.12+Item.13+Item.14
      +Item.15+Item.16+Item.17+Item.18+Item.19+Item.20+Item.21
      +Item.22+Item.23+Item.24+Item.25+Item.26+Item.27+Item.28
      +Item.29+Item.30+Item.31+Item.32+Item.33+Item.34+Item.35
      +Item.36+Item.37+Item.38+Item.39+Item.40+Item.41+Item.42
      +Item.43+Item.44+Item.45+Item.46+Item.47+Item.48+Item.49
      +Item.50+Item.51+Item.52+Item.53+Item.54+Item.55+Item.56+Item.57+Item.58

Boldness =~ Item.1+Item.4+Item.7 + Item.10+Item.13+Item.16+Item.19
            +Item.22+Item.25+Item.28 + Item.32+Item.35+Item.38
            +Item.41+Item.44+Item.47 + Item.50+Item.54+Item.57

Disinhibition =~ Item.3+Item.5+Item.9 + Item.12+Item.15+Item.18
                 +Item.21+Item.24+Item.27 + Item.30+Item.31+Item.34+Item.37
                 +Item.43+Item.46+Item.49 + Item.51+Item.53+Item.56+Item.58
Callousness =~ Item.2+Item.6+Item.8 + Item.11+Item.14+Item.17
               +Item.20+Item.23+Item.26+Item.29 + Item.33+Item.36+Item.39
               +Item.40+Item.42+Item.45+Item.48 + Item.52+Item.55
'

# Configural model
tri.bif.conf <- cfa(mod.tri.bif.conf, data=tripm58.mi, std.lv=T, 
                    ordered=colnames(tripm58.mi[,5:62]), estimator="WLSMV", orthogonal=TRUE,
                    group='SEX')

#save(tri.bif.conf, file='MI_SEX_conf.RData')
load('MI_SEX_conf.RData')



summary(tri.bif.conf, fit.measure=T, standardized=F, rsquare=T)

fitMeasures(tri.bif.conf, c('cfi.scaled','tli.scaled','rmsea.scaled'))
fitMeasures(tri.bif.conf, c("chisq.scaled", "df", "pvalue.scaled"))
# cfi.scaled   tli.scaled rmsea.scaled
# 0.853        0.841        0.051
# chisq.scaled            df pvalue.scaled
# 8727.724      3074.000         0.000


# Metric model
tri.bif.metric <- cfa(mod.tri.bif.conf, data=tripm58.mi, std.lv=T, 
                      ordered=colnames(tripm58.mi[,5:62]), estimator="WLSMV", orthogonal=TRUE,
                      group='SEX', 
                      group.equal=c('loadings'))

summary(tri.bif.metric, fit.measures=T, standardized=F)

#save(tri.bif.metric, file='MI_SEX_metric.RData')  # load the object 'tri.bif.metric'
load('MI_SEX_metric.RData')


fitMeasures(tri.bif.metric, c('cfi.scaled','tli.scaled','rmsea.scaled'))
fitMeasures(tri.bif.metric, c("chisq.scaled", "df", "pvalue.scaled"))
# cfi.scaled   tli.scaled rmsea.scaled
# 0.884        0.880        0.044
# chisq.scaled            df pvalue.scaled
# 7616.247      3186.000         0.000

fitMeasures(tri.bif.metric, 'cfi.scaled') - fitMeasures(tri.bif.conf, 'cfi.scaled') 
#       0.032 

# Comparison of metric and configiural models
lavTestLRT(tri.bif.metric, tri.bif.conf, method="satorra.bentler.2001")
# p = 0.98

#anova(tri.bif.metric, tri.bif.conf) # *** with warning



# Scalar model
tri.bif.scalar <- cfa(mod.tri.bif.conf, data=tripm58.mi, std.lv=T, 
                      ordered=colnames(tripm58.mi[,5:62]), estimator="WLSMV", orthogonal=TRUE,
                      group='SEX', 
                      group.equal=c('loadings','thresholds'))

summary(tri.bif.scalar, fit.measures=T, standardized=F)

#save(tri.bif.metric, file='MI_SEX_scalar.RData')  # load the object 'tri.bif.scalar'
load('MI_SEX_scalar.RData')


fitMeasures(tri.bif.scalar, c('cfi.scaled','tli.scaled','rmsea.scaled'))
fitMeasures(tri.bif.scalar, c("chisq.scaled", "df", "pvalue.scaled"))
# cfi.scaled   tli.scaled rmsea.scaled
# 0.865        0.864        0.047
# chisq.scaled            df pvalue.scaled
# 8491.496      3298.000         0.000


# Comparison of scalar and metric models
lavTestLRT(tri.bif.scalar, tri.bif.metric, method="satorra.bentler.2010")
# lavaan ERROR: unknown method for scaled difference test: satorrabentler2000
# lavaan WARNING: scaling factor is negative : 2001

fitMeasures(tri.bif.scalar, 'cfi.scaled') - fitMeasures(tri.bif.metric, 'cfi.scaled')
# -0.02


# Comparison of configural and scalar models
#lavTestLRT(tri.bif.scalar, tri.bif.conf, method="satorra.bentler.2001")
# n.s. p=0.10

#fitMeasures(tri.bif.scalar, 'cfi.scaled') - fitMeasures(tri.bif.conf, 'cfi.scaled')
#cfi.scaled 0.012 




# Fit indices comparison
indices <- c("chisq.scaled","df.scaled", "pvalue.scaled",
             "rmsea.scaled", "cfi.scaled", "tli.scaled")
fit.indices.SEX <- matrix(NA,3,6)
colnames(fit.indices.SEX) <- indices
rownames(fit.indices.SEX) <-c('Configural','Metric','Scalar')

fit.indices.SEX[1,]<-round(data.matrix(fitmeasures(tri.bif.conf,fit.measures = indices)), digits=3)
fit.indices.SEX[2,]<-round(data.matrix(fitmeasures(tri.bif.metric,fit.measures = indices)), digits=3)
fit.indices.SEX[3,]<-round(data.matrix(fitmeasures(tri.bif.scalar,fit.measures = indices)), digits=3)


fit.indices.SEX

############# e) MI of PSYCH_PROB ################

summary(tripm$PSYCH_PROB)   # unequal sample sizes
# 1    2 
# 400 1023 

# Configural model
tri.bif.conf.prob <- cfa(mod.tri.bif.conf, data=tripm58.mi, std.lv=T, 
                         ordered=colnames(tripm58.mi[,5:62]), estimator="WLSMV", orthogonal=TRUE,
                         group='PSYCH_PROB')

#save(tri.bif.conf.prob, file='MI_PROB_conf.RData')
load('MI_PROB_conf.RData')


summary(tri.bif.conf.prob, fit.measure=T, standardized=F, rsquare=T)

fitMeasures(tri.bif.conf.prob, c('cfi.scaled','tli.scaled','rmsea.scaled'))
fitMeasures(tri.bif.conf.prob, c("chisq.scaled", "df", "pvalue.scaled"))
# cfi.scaled   tli.scaled rmsea.scaled
# 0.867        0.857        0.052
# chisq.scaled            df pvalue.scaled
# 8884.01       3074.00          0.00



# Metric model
tri.bif.metric.prob <- cfa(mod.tri.bif.conf, data=tripm58.mi, std.lv=T, 
                           ordered=colnames(tripm58.mi[,5:62]), estimator="WLSMV", orthogonal=TRUE,
                           group='PSYCH_PROB', 
                           group.equal=c('loadings'))

#save(tri.bif.metric.prob, file='MI_PROB_metric.RData')
load('MI_PROB_metric.RData')

summary(tri.bif.metric.prob, fit.measures=T, standardized=F)

fitMeasures(tri.bif.metric.prob, c('cfi.scaled','tli.scaled','rmsea.scaled'))
fitMeasures(tri.bif.metric.prob, c("chisq.scaled", "df", "pvalue.scaled"))
# cfi.scaled   tli.scaled rmsea.scaled
# 0.903        0.899        0.043
# chisq.scaled            df pvalue.scaled
# 7416.068      3186.000         0.000



## Scalar model
tri.bif.scalar.prob <- cfa(mod.tri.bif.conf, data=tripm58.mi, std.lv=T, 
                           ordered=colnames(tripm58.mi[,5:62]), estimator="WLSMV", orthogonal=TRUE,
                           group='PSYCH_PROB', 
                           group.equal=c('loadings','thresholds'))

#save(tri.bif.scalar.prob, file='MI_PROB_scalar.RData')
load('MI_PROB_scalar.RData')

summary(tri.bif.scalar.prob, fit.measures=T, standardized=F)


fitMeasures(tri.bif.scalar.prob, c('cfi.scaled','tli.scaled','rmsea.scaled'))
fitMeasures(tri.bif.scalar.prob, c("chisq.scaled", "df", "pvalue.scaled"))
# cfi.scaled   tli.scaled rmsea.scaled
# 0.890        0.890        0.045
# chisq.scaled            df pvalue.scaled
# 8104.27       3298.00          0.00
# 


# Comparisons of metric and configural models
fitMeasures(tri.bif.metric.prob, 'cfi.scaled') - fitMeasures(tri.bif.conf.prob, 'cfi.scaled') 
# 0.036

lavTestLRT(tri.bif.metric.prob, tri.bif.conf.prob, method="satorra.bentler.2001")


# Comparisons of metric and scalar models
fitMeasures(tri.bif.scalar.prob, 'cfi.scaled') - fitMeasures(tri.bif.metric.prob, 'cfi.scaled') 
#     -0.013 

lavTestLRT(tri.bif.scalar.prob, tri.bif.metric.prob, method="satorra.bentler.2001")
anova(tri.bif.scalar.prob, tri.bif.metric.prob)   

# got warning : 2000 2001 2010

# lavaan WARNING:
#   Some restricted models fit better than less restricted models;
# either these models are not nested, or the less restricted model
# failed to reach a global optimum. Smallest difference =
#   -74.8792504964113
# 2: In lav_test_diff_SatorraBentler2001(mods[[m]], mods[[m + 1]]) :
#   lavaan WARNING: scaling factor is negative

# Threshold model
tri.bif.thres.prob <- cfa(mod.tri.bif.conf, data=tripm58.mi, std.lv=T, 
                          ordered=colnames(tripm58.mi[,5:62]), estimator="WLSMV", orthogonal=TRUE,
                          group='PSYCH_PROB', 
                          group.equal=c('thresholds'))

#save(tri.bif.thres.prob, file='MI_PROB_thres.RData')
load('MI_PROB_thres.RData')

summary(tri.bif.thres.prob, fit.measures=T, standardized=F)


fitMeasures(tri.bif.thres.prob, c('cfi.scaled','tli.scaled','rmsea.scaled'))
fitMeasures(tri.bif.thres.prob, c("chisq.scaled", "df", "pvalue.scaled"))
# cfi.scaled   tli.scaled rmsea.scaled
# 0.865        0.860        0.051
# chisq.scaled            df pvalue.scaled
# 9084.46       3186.00          0.00



# Comparison of threshold and configural models
lavTestLRT(tri.bif.thres.prob, tri.bif.conf.prob, method="satorra.bentler.2001")
## 0.02*

# Comparison of threshold and scalar models
lavTestLRT(tri.bif.scalar.prob, tri.bif.thres.prob, method="satorra.bentler.2001") # p value 1

# Fit indices comparison
indices <- c("chisq.scaled","df.scaled", "pvalue.scaled",
             "rmsea.scaled", "cfi.scaled", "tli.scaled")
fit.indices.PROB <- matrix(NA,3,6)
colnames(fit.indices.PROB) <- indices
rownames(fit.indices.PROB) <- c('Configural','Thresholds','Scalar')

fit.indices.PROB[1,]<-round(data.matrix(fitmeasures(tri.bif.conf.prob,fit.measures = indices)), digits=3)
fit.indices.PROB[2,]<-round(data.matrix(fitmeasures(tri.bif.thres.prob,fit.measures = indices)), digits=3)
fit.indices.PROB[3,]<-round(data.matrix(fitmeasures(tri.bif.scalar.prob,fit.measures = indices)), digits=3)

fit.indices.PROB


############# e) MI of AGE ############

# Median age
median(tripm$AGE) # 24

# Divide AGE into multiple groups
summary(tripm58.mi$AGE)
apply(tripm58.mi, 2, table)$AGE

age.grp <- rep("16-20",nrow(tripm58.mi))
age.grp[tripm58.mi$AGE>20 & tripm58.mi$AGE<=25] <- "21-25"
age.grp[tripm58.mi$AGE>25 & tripm58.mi$AGE<=50] <- "25-50"
age.grp[tripm58.mi$AGE>50] <- "50+"
#age.grp


tripm58.mi <- cbind(tripm58.mi, age.grp)
str(tripm58.mi)

apply(tripm58.mi["age.grp"], 2, table)
#       age.grp
# 16-20     226
# 21-25     608
# 26-50     306
# 50+       283

apply(tripm58.mi, 2, table)$age.grp
# 16-20 21-25 26-50   50+ 
#   226   608   306   283 

nrow(tripm58.mi["age.grp"])  #1423


# Configural model
tri.bif.conf.age <- cfa(mod.tri.bif.conf, data=tripm58.mi, std.lv=T, 
                        ordered=colnames(tripm58.mi[,5:62]), orthogonal=TRUE,
                        #estimator='mlr',
                        group='age.grp')

#save(tri.bif.conf.age, file='MI_AGE_conf.RData')
load('MI_AGE_conf.RData')

summary(tri.bif.conf.age, fit.measure=T, standardized=F, rsquare=T)

fitMeasures(tri.bif.conf.age, c('cfi.scaled','tli.scaled','rmsea.scaled'))
fitMeasures(tri.bif.conf.age, c("chisq.scaled", "df.scaled", "pvalue.scaled"))
# cfi   tli rmsea
# 0.726 0.705 0.064
# chisq       df   pvalue
# 15009.71  6148.00     0.00


# Metric model
tri.bif.metric.age <- cfa(mod.tri.bif.conf, data=tripm58.mi, std.lv=T, 
                          ordered=colnames(tripm58.mi[,5:62]), orthogonal=TRUE, 
                          #estimator='ml',
                          group.equal=c('loadings'),
                          group='age.grp')

#save(tri.bif.metric.age, file='MI_AGE_metric.RData')
load('MI_AGE_metric.RData')


summary(tri.bif.metric.age, fit.measure=T, standardized=F, rsquare=T)

fitMeasures(tri.bif.metric.age, c('cfi.scaled','tli.scaled','rmsea.scaled'))
fitMeasures(tri.bif.metric.age, c("chisq.scaled", "df.scaled", "pvalue.scaled"))
# cfi   tli rmsea
# 0.718 0.713 0.063
# chisq       df   pvalue
# 15588.89  6484.00     0.00


# Scalar model
tri.bif.scalar.age <- cfa(mod.tri.bif.conf, data=tripm58.mi, std.lv=T, 
                          ordered=colnames(tripm58.mi[,5:62]), orthogonal=TRUE, 
                          #estimator='ml', 
                          group.equal=c('loadings','thresholds'), group='age.grp')

#save(tri.bif.scalar.age, file='MI_AGE_scalar.RData')
load('MI_AGE_scalar.RData')

summary(tri.bif.scalar.age, fit.measure=T, standardized=F, rsquare=T)


fitMeasures(tri.bif.scalar.age, c('cfi.scaled','tli.scaled','rmsea.scaled'))
fitMeasures(tri.bif.scalar.age, c("chisq.scaled", "df.scaled", "pvalue.scaled"))

# cfi.scaled   tli.scaled rmsea.scaled
# 0.886        0.890        0.043
# chisq.scaled     df.scaled pvalue.scaled
# 11274.96       6820.00          0.00
 

# Comparison of metric and configural models
fitMeasures(tri.bif.metric.age, 'cfi.scaled') - fitMeasures(tri.bif.conf.age, 'cfi.scaled')
#  0.037

lavTestLRT(tri.bif.metric.age, tri.bif.conf.age, method="satorra.bentler.2001")  
## p value: 1 
#                     Df AIC BIC Chisq Chisq diff Df diff Pr(>Chisq)
# tri.bif.conf.age   6148         15405                              
# tri.bif.metric.age 6484         19143     130.17     336          1



# Comparison of metric and scalar models
lavTestLRT(tri.bif.scalar.age, tri.bif.metric.age, method="satorra.bentler.2001")
# 2001: error
# 2010: error. p value 1

#                      Df AIC BIC Chisq Chisq diff Df diff Pr(>Chisq)
# tri.bif.metric.age 6484         19143                              
# tri.bif.scalar.age 6820         18629     66.868     336          1

# Warning message:
# In lavTestLRT(tri.bif.scalar.age, tri.bif.metric.age, method = "satorra.bentler.2010") :
#   lavaan WARNING:
#     Some restricted models fit better than less restricted models;
#     either these models are not nested, or the less restricted model
#     failed to reach a global optimum. Smallest difference =
#     -513.445998628147


# Threshold model
tri.bif.thres.age <- cfa(mod.tri.bif.conf, data=tripm58.mi, std.lv=T, 
                         ordered=colnames(tripm58.mi[,5:62]), orthogonal=TRUE,
                         #estimator='ml', 
                         group.equal=c('thresholds'),
                         group='age.grp')


#save(tri.bif.thres.age, file='MI_AGE_thres.RData')
load('MI_AGE_thres.RData')

summary(tri.bif.thres.age, fit.measure=T, standardized=F, rsquare=T)

fitMeasures(tri.bif.thres.age, c('cfi.scaled','tli.scaled','rmsea.scaled'))
fitMeasures(tri.bif.thres.age, c("chisq.scaled", "df", "pvalue.scaled"))
# cfi.scaled   tli.scaled rmsea.scaled
# 0.860        0.857        0.049
# chisq.scaled            df pvalue.scaled
# 11965.35       6484.00          0.00



# Comparison of threshold and configural models
lavTestLRT(tri.bif.thres.age, tri.bif.conf.age, method="satorra.bentler.2001")
# p value 0.98

fitMeasures(tri.bif.thres.age, 'cfi.scaled') - fitMeasures(tri.bif.conf.age, 'cfi.scaled')
# cfi.scaled 
# -0.005 


# Comparison of threshold and scalar models
lavTestLRT(tri.bif.scalar.age, tri.bif.thres.age, method="satorra.bentler.2001")
# p value 1

fitMeasures(tri.bif.scalar.age, 'cfi.scaled') - fitMeasures(tri.bif.thres.age, 'cfi.scaled')
# cfi.scaled 
# 0.026 

# Fit indices comparison
indices <- c("chisq.scaled","df.scaled", "pvalue.scaled",
             "rmsea.scaled", "cfi.scaled", "tli.scaled")
fit.indices.AGE <- matrix(NA,3,6)
colnames(fit.indices.AGE) <- indices
rownames(fit.indices.AGE) <- c('configural','thresholds','scalar')

fit.indices.AGE[1,]<-round(data.matrix(fitmeasures(tri.bif.conf.age,fit.measures = indices)), digits=3)
fit.indices.AGE[2,]<-round(data.matrix(fitmeasures(tri.bif.thres.age,fit.measures = indices)), digits=3)
fit.indices.AGE[3,]<-round(data.matrix(fitmeasures(tri.bif.scalar.age,fit.measures = indices)), digits=3)

fit.indices.AGE

