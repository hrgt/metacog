library(lavaan)
library(foreign)
install.packages("semPlot",repos='http://cran.us.r-project.org')
library(semPlot)
install.packages("semTools",repos='http://cran.us.r-project.org')
library(semTools)
library(igraph)
library(ggplot2)

#fit model for confirmatory factor analysis
MSI.model <- ' factor1 =~ a + b + c
               factor2 =~ d + e + f
               factor3 =~ g + h + i
               factor4 =~ j + k + l '
fit <- cfa(MSI.model, data = Extracted.MC.12.7.2020)
graph_sem(model = fit)
lay = get_layout("", "", "factor1", "factor2", "factor3", "factor4", "", "",
                 "a", "b", "c", "d", "e", "f", 
                 "g", "h", "i", "j", "k", "l", rows = 2)
graph_sem(fit, layout = lay)
summary(fit)
inspect(fit,"r2")
fitmeasures(fit,c("rmsea", "cfi", "srmr", "tli" ))
fit <-sem(MSI.model, data = BITS)
summary(fit, fit.measures = TRUE, standardized=TRUE)
semPaths(fit,"std")

#Measurement invariance of tool

#configural invariance
configural <- cfa(MSI.model, data = Extracted.MC.12.7.2020, group = "Gender")
summary(configural, fit.measures=TRUE)

#metric/weak invariance
weak.invariance <- cfa(MSI.model, data = Extracted.MC.12.7.2020, group = "Gender", group.equal = "loadings")
summary(weak.invariance, fit.measures=TRUE)
anova(weak.invariance, configural)
fit.stats <-rbind(fitmeasures(configural, fit.measures = c("chisq", "df", "rmsea", "srmr", "tli", "cfi", "aic")),
                  fitmeasures(weak.invariance, fit.measures = c("chisq", "df", "rmsea", "srmr", "tli", "cfi", "aic")))
rownames(fit.stats) <- c("configural", "weak.invariance")
fit.stats

#strong invariance
strong.invariance <- cfa(MSI.model, data = Extracted.MC.12.7.2020, group = "Gender", group.equal = c( "loadings", "intercepts"))
summary(strong.invariance, fit.measures=TRUE)
anova(strong.invariance, weak.invariance)
lavTestScore(strong.invariance)
parTable(strong.invariance)

#freeing for patial
strong.invariance.x15 <- cfa(MSI.model, data = Extracted.MC.12.7.2020, group = "Gender", group.equal =c( "loadings", "intercepts"), group.partial=c("x15 ~ 1"))
lavTestScore(strong.invariance.x15)
strong.invariance.x15x11 <- cfa(MSI.model, data = Extracted.MC.12.7.2020, group = "Gender", group.equal = c("loadings", "intercepts"), group.partial = c("x15 ~ 1", "x11 ~ 1"))
lavTestScore(strong.invariance.x15x11)
strong.invariance.x15x11 <- cfa(MSI.model, data = Extracted.MC.12.7.2020, group = "Gender", group.equal = c("loadings", "intercepts"), group.partial = c("x15 ~ 1", "x11 ~ 1"))
anova(strong.invariance.x15x11, weak.invariance)
fit.stats2 <-rbind(fitmeasures(strong.invariance, fit.measures = c("chisq", "df", "rmsea", "srmr", "tli", "cfi", "aic")),
                   fitmeasures(strong.invariance.x15x11, fit.measures = c("chisq", "df", "rmsea", "srmr", "tli", "cfi", "aic")))
rownames(fit.stats2) <- c("strong", "strong with x15 x11")
fit.stats <- rbind(fit.stats, fit.stats2)
round(fit.stats, 4)

#strict invariance
strict.invariance.x15x11 <- cfa(MSI.model, data = Extracted.MC.12.7.2020, group = "Gender", group.equal = c( "loadings", "intercepts", "residuals"), group.partial = c("x15 ~ 1", "x11 ~ 1"))
anova(strong.invariance.x15x11, strict.invariance.x15x11)
fit.stats3 <-rbind(fitmeasures(strong.invariance, fit.measures = c("chisq", "df", "rmsea", "srmr", "tli", "cfi", "aic")),
                   fitmeasures(strong.invariance.x15x11, fit.measures = c("chisq", "df", "rmsea", "srmr", "tli", "cfi", "aic")))
rownames(fit.stats3) <- c("strong.invariance.x15x11", "strict invariance")
fit.stats3
