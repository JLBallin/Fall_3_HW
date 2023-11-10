insurance_t <- read.csv('~/Documents/insurance_t.csv')
insurance_v <- read.csv('~/Documents/insurance_v.csv')

#List categoricals
categorical <- c("DDA", "DIRDEP", "NSF", "SAV", "ATM", "CD", "IRA", "INV", "MM", "MMCRED",
                 "CC", "CCPURC", "SDB", "INAREA", "INS")

#missing values
colSums(is.na(insurance_t))


#Imputed flag for missings only for non categorical variables
for (i in 1:ncol(insurance_t)) {  
  if (!(colnames(insurance_t[i]) %in% categorical)){
    col_name <- names(insurance_t)[i]
    imputation_flag_col <- ifelse(is.na(insurance_t[, i]), 1, 0)
    insurance_t <- cbind(insurance_t, imputation_flag_col)
    colnames(insurance_t)[ncol(insurance_t)] <- paste0("Missing_",col_name)}
}

colnames(insurance_t)

#Impute median for continuous
for(i in names(insurance_t)){
  if (is.numeric(insurance_t[[i]])){
    insurance_t[[i]][is.na(insurance_t[[i]])] <- median(insurance_t[[i]], na.rm = TRUE)
  }
}


#Checking for number of levels over 10 to treat as
for (i in names(insurance_t)){
  if (i %in% categorical){
    print(i)
    print(nlevels(as.factor(insurance_t[[i]])))
  }
}

#Categoricals to factors 
for (i in names(insurance_t)){
  if (i %in% categorical){
    insurance_t[[i]]<- as.factor(insurance_t[[i]])
  }
}


#MARS Algorithm
library(earth)
insurance_t$INS <- as.factor(insurance_t$INS)
mars_ins <- earth(INS ~ ., data = insurance_t, glm=list(family=binomial))
summary(mars_ins)
evimp(mars_ins)



#GAM Model
set.seed(47)
gam_ins <- mgcv::gam(INS ~
                   (s(ACCTAGE) + s(DDABAL) + s(DEP) + s(DEPAMT) + s(CHECKS) + s(NSFAMT) + s(SAVBAL) +
                      s(ATMAMT) + s(POS) + s(POSAMT) + s(CDBAL) + s(IRABAL) + s(INVBAL) + s(MMBAL) +
                      s(CCBAL) + s(INCOME) + s(LORES) + s(HMVAL) + s(AGE) + s(CRSCORE) + factor(Missing_ACCTAGE) + 
                      factor(Missing_PHONE) + factor(Missing_POS) + factor(Missing_POSAMT)  + 
                      factor(Missing_INVBAL)  + factor(Missing_CCBAL)  + 
                      factor(Missing_INCOME) + factor(Missing_LORES) + factor(Missing_HMVAL) + factor(Missing_AGE) + 
                      factor(Missing_CRSCORE) + DDA + DIRDEP + NSF + PHONE + TELLER + SAV + ATM + CD + IRA + INV + 
                      MM + MMCRED + CC + CCPURC + SDB + INAREA + BRANCH),
                 method = 'REML',
                 select = TRUE,
                 data = insurance_t,
                 family = 'binomial'
)


summary(gam_ins)



#ROC Curve for MARS
library(ROCit)
insurance_t$p_hat_m <- predict(mars_ins, type = "response")
logit_roc_m <- rocit(c(insurance_t$p_hat_m), insurance_t$INS)
plot(logit_roc_m)

ciAUC(logit_roc_m, level = 0.99)

summary(logit_roc_m)




#MARS predictions
insurance_t$p_hat <- predict(mars_ins, type = "response")

#MARS Plot

font = "sans"
# aes mapping for legend and coloring
object <- list('ROC'=roc(insurance_t$INS, insurance_t$p_hat))
cols = c('ROC'='#ffb6c1', 'Chance Line'='gray')
library( pROC )
library(ggplot2)
# Create plot object and map based on color
ggroc(object, aes = c('color'), legacy.axes = TRUE, linewidth=1) +
  
  # Chance Line
  geom_abline(aes(slope= 1, intercept = 0, colour = 'Chance Line'), linetype='dashed', linewidth=1) +
  
  # Labels
  labs(x = "1 - Specificity",
       y = "Sensitivity") +
  
  # Edit Legend
  scale_colour_manual(name="Legend",values=cols) +
  
  # Theme
  theme_bw() +
  
  # Box the Legend
  theme(legend.box.background = element_rect(color="black", linewidth = 1)) +
  theme(legend.text = element_text(family = font)) +
  theme(legend.position = c(0.9, 0.2)) +
  
  # Labels Size
  theme(axis.text.x = element_text(size = 16,  
                                   family = font,
                                   angle = 45,
                                   hjust = 1,
                                   vjust = 1)) +
  
  theme(axis.text.y = element_text(size = 16,  
                                   family = font)) +
  
  theme(axis.title.x = element_text(size = 18,
                                    family = font)) +
  
  theme(axis.title.y = element_text(size = 18, 
                                    family = font,
                                    vjust = 2.25))







#GAM predictions
insurance_t$p_hat_g <- predict(gam_ins, type = "response")

#GAM Plot 

font = "sans"
# aes mapping for legend and coloring
object <- list('ROC'= roc(insurance_t$p_hat_g, insurance_t$INS))
cols = c('ROC'='#ffb6c1', 'Chance Line'='gray')
library( pROC )
library(ggplot2)
# Create plot object and map based on color
ggroc(object, aes = c('color'), legacy.axes = TRUE, linewidth=1) +
  
  # Chance Line
  geom_abline(aes(slope= 1, intercept = 0, colour = 'Chance Line'), linetype='dashed', linewidth=1) +
  
  # Labels
  labs(x = "1 - Specificity",
       y = "Sensitivity") +
  
  # Edit Legend
  scale_colour_manual(name="Legend",values=cols) +
  
  # Theme
  theme_bw() +
  
  # Box the Legend
  theme(legend.box.background = element_rect(color="black", linewidth = 1)) +
  theme(legend.text = element_text(family = font)) +
  theme(legend.position = c(0.9, 0.2)) +
  
  # Labels Size
  theme(axis.text.x = element_text(size = 16,  
                                   family = font,
                                   angle = 45,
                                   hjust = 1,
                                   vjust = 1)) +
  
  theme(axis.text.y = element_text(size = 16,  
                                   family = font)) +
  
  theme(axis.title.x = element_text(size = 18,
                                    family = font)) +
  
  theme(axis.title.y = element_text(size = 18, 
                                    family = font,
                                    vjust = 2.25))




