# HLM 실습

# Install Packages
# install.packages("pacman")
pacman::p_load(tidyverse, magrittr,
               lme4, lmerTest, bruceR,
               merTools, 
               skimr, psych, readstata13, 
               broom, broom.mixed)

# Read data
## set default Work Space 
getwd()
setwd("c:/") # 자신이 설정하고 싶은 곳으로 설정
getwd()

###########################################################################################
# 기초 R

# assign 
"A"
"B"
1
class(1)
class("A")

## variable <- value
value <- "A"
value

# factor
factor(1:5)
factor(1:5, levels = 5:1)

# vector
c(1,2,3,4,5)
vector <- c(1,2,3,4,5)
(vector <- c(1,2,3,4,5))
1:5

# data.frame
a <- data.frame(a = 1:5, b = 6:10, c=11:15)
b <- tibble(a = 1:5, b = 6:10, c=11:15)
a;b 

# data.pasta 
# install.packages("datapasta")


# read data
penguins <- read.csv("./penguins.csv")
head(penguins, 5)
tail(penguins, 5)

# pull vector 
names(penguins)
rownames(penguins)
penguins$bill_length_mm

# Correlation
cor(penguins$bill_length_mm, penguins$bill_depth_mm, use = "pairwise.complete.obs")
cor.test(penguins$bill_length_mm, penguins$ bill_depth_mm)

# linear regression
lmfit <- lm(penguins$bill_length_mm~penguins$bill_depth_mm)
summary(lmfit)
lmfit2 <- lm(penguins$bill_length_mm~penguins$bill_depth_mm+penguins$flipper_length_mm)
summary(lmfit2)
lmfit3 <- lm(bill_length_mm~bill_depth_mm+flipper_length_mm, data=penguins)
summary(lmfit3)

class(penguins$sex) 

# 범주형 변수는 factor로 전환해야 함 
penguins$sex <- factor(penguins$sex, levels= c("female","male"))
lmfit4 <- lm(bill_length_mm~bill_depth_mm+flipper_length_mm+sex, data=penguins)
summary(lmfit4)
penguins$sex <- factor(penguins$sex, levels= c("male","female"))
lmfit5 <- lm(bill_length_mm~bill_depth_mm+flipper_length_mm+sex, data=penguins)
summary(lmfit5)

# if_else
if_else(data$year>2007,1,0)

# pipe 연습
c(1,2,3,4,5)
mean(c(1,2,3,4,5))
a <- c(1,2,3,4,5)
a %>% mean
a %>% mean()

# lm with pipe
data %>% 
  lm(bill_length_mm~bill_depth_mm+flipper_length_mm+sex, data=.) %>% 
  summary()

data %>% 
  lm(bill_length_mm~bill_depth_mm+flipper_length_mm+sex, data=.) %>% 
  tidy()

###########################################################################################
# dplyr: join 함수 연습 
band_members; band_instruments; band_instruments2

full_join (x=band_members,
           y=band_instruments, 
           by = "name")
inner_join(x=band_members,
           y=band_instruments, 
           by = "name")
left_join (x=band_members,
           y=band_instruments, 
           by = "name")
right_join (x=band_members,
           y=band_instruments, 
           by = "name")

# id 명이 서로 다를떄  
left_join (x=band_members,
           y=band_instruments2, 
           by = c("name"="artist"))

# 편의상 %>% 의 사용
band_members %>% 
  left_join (y=band_instruments2, 
           by = c("name"="artist"))

###########################################################################################
# dplyr: bind 함수 연습 
one <- starwars[1:4, ]
two <- starwars[9:12, ]

## bind_rows
bind_rows(one, two)
bind_rows(list(one, two))
one %>% 
  bind_rows(two)

## with id 
bind_rows(list(a = one, b = two), .id = "id")

## bind_cols
bind_cols(one, two)
View(bind_cols(one, two))

###########################################################################################

# HLM 시작

# Read data
data_lv1 <- read.dta13("./HSB1.dta")
data_lv2 <- read.dta13("./HSB2.dta")

# Glimpse data
glimpse(data_lv1)
glimpse(data_lv2)
# Merge data by id

# Summarize
skimr::skim(data_lv1)
skimr::skim(data_lv2)

# or 
psych::describe(data_lv1)
psych::describe(data_lv2)

# data_lv1과 data_lv2의 병합
data_merged <- data_lv1 %>% 
  left_join(data_lv2, 
            by = "id") %>% 
  # make sector factor variable 
  mutate(sector = factor(if_else(sector == 1 , "Catholic", "Public"), levels = c( "Public", "Catholic")))

head(data_merged, 5)

# 평균적으로 학급당 44.91명 (sd: 11.85)
data_merged %>% 
  group_by(id) %>% 
  summarise(n=n()) %>% 
  ungroup %>% 
  pull(n) %>% 
  describe 

###########################################################################################

# Pooled OLS
model_pols <- lm(mathach ~ sector + ses, data= data_merged)
summary(model_pols)
tidy(model_pols)

# Between Model
data_btw <- data_merged %>% 
  group_by(id) %>% 
  summarise(mean_mathach = mean(mathach),
            meanses = mean(ses)) %>% 
  mutate(sector = factor(if_else(data_lv2$sector == 1 , "Catholic", "Public"), levels = c( "Public", "Catholic"))) %>%
  ungroup

model_btw <- lm(mean_mathach ~ meanses + sector, data = data_btw)
summary(model_btw)

###########################################################################################
# Centering
data_merged <- data_merged %>% 
  mutate(ses_grandmc = ses - mean(ses))%>% 
  group_by(id) %>% 
  mutate(meanses = mean(ses),
         ses_groupmc = ses - meanses) %>% 
  ungroup() 

# 각 학교별 인원, 평균 ses 확인하는 법
data_merged %>% 
  group_by(id) %>% 
  # n = 인원수, meanses = 평균 ses 점수
  summarise(n = n(),
            meanses = mean(ses)) %>% 
  ungroup()

###########################################################################################
# Five models in HLM Two-Level Analysis

# Preliminary ANOVA
model1 <-  aov(mathach~id, data=data_merged)
anova(model1)
broom::tidy(model1) %>% 
  mutate(across(where(is.numeric),~round(.,3)))


###########################################################################################

# 1. One-way ANOVA (혹은 Unconditional Model - 무조건 모형)
model1 <- lmer(mathach ~ 1 + (1 | id), data=data_merged)
summary(model1)
icc(model1)

data_merged$intercept.only.preds <- predict(model1)

# the individual random effects (level 2 residuals of the intercept, i.e. the υ0i)
ranef(model1)
ranova(model1)


## Use BruceR package
HLM_ICC_rWG(data_merged, group="id", icc.var="mathach")
HLM_summary(model1,test.rand = T, digits = 3)

###########################################################################################

# 3. Random intercept model

model3 <- lmer(mathach ~ ses_grandmc + (1 | id), data=data_merged)
icc(model3)

# the individual random effects (level 2 residuals of the intercept, i.e. the υ0i)
ranef(model3)
ranova(model3)

## Use BruceR package
HLM_summary(model3,test.rand = T, digits = 3)

# lrtest
anova(model1, model3)

###########################################################################################

# 4. Random-Coefficient Model
model4 <- lmer(mathach ~  ses_groupmc + (ses_groupmc | id), data=data_merged)
icc(model4)

# the individual random effects (level 2 residuals of the intercept, i.e. the υ0i)
ranef(model4)
ranova(model4)
bdiag(VarCorr(model4))

## Use BruceR package
HLM_summary(model4,test.rand = T, digits = 3)

# lrtest
anova(model1, model4)

###########################################################################################
#5-1 Intercepts-as-Outcomes Model
# (1) Random-effects ANCOVA with Level-2 predictor

## without random coefficient
model5 <- lmer(mathach ~  ses + sector + (1 | id), data=data_merged)
icc(model5)

# the individual random effects (level 2 residuals of the intercept, i.e. the υ0i)
ranef(model5)
ranova(model5)

## Use BruceR package
HLM_summary(model5,test.rand = T, digits = 3)

## with random coefficient
model6 <- lmer(mathach ~  ses + sector + (ses | id), data=data_merged)
icc(model6)

# the individual random effects (level 2 residuals of the intercept, i.e. the υ0i)
ranef(model6)
ranova(model6)

## Use BruceR package
HLM_summary(model6, test.rand = T, digits = 3)

anova(model5, model6)

# (2) Contextual Effect
model7 <- lmer(mathach ~ ses_groupmc + meanses + (1 | id), data = data_merged)
icc(model7)
summary(model7)

# the individual random effects (level 2 residuals of the intercept, i.e. the υ0i)
ranef(model7)
ranova(model7)

## Use BruceR package
HLM_summary(model7,test.rand = T, digits = 3)

###########################################################################################
#5-2 Slopes-as-Outcomes Model (* USE, REML = F)

## with random coefficient
model8 <- lmer(mathach ~ ses_groupmc * sector + (ses_groupmc | id), data = data_merged, REML = FALSE)

summary(model8)
icc(model8)

# the individual random effects (level 2 residuals of the intercept, i.e. the υ0i)
ranef(model8)
ranova(model8)

## Use BruceR package
HLM_summary(model8, test.rand = T, digits = 3)

## without random coefficient
model9 <- lmer(mathach ~ ses_groupmc * sector  +  (1 | id), data = data_merged, REML = FALSE)

summary(model9)
icc(model9)

# the individual random effects (level 2 residuals of the intercept, i.e. the υ0i)
ranef(model9)
ranova(model9)

## Use BruceR package
HLM_summary(model9, test.rand = T, digits = 3)

anova(model8, model9) # without randomness is better

# final 
model10 <- lmer(mathach ~ ses_groupmc * sector  + meanses + (1| id), data = data_merged, REML = FALSE)

###########################################################################################

# Marginsplot
# install.packages("sjPlot")
# install.packages("effects")
library(sjPlot)
plot_model(model8,'pred', terms = c('ses_groupmc','sector'), ci.lvl = 0.95)
plot_model(model9, type = 'pred', terms = c('ses_groupmc','sector'), ci.lvl = 0.95)

###########################################################################################

# 모형들간 비교
library(bruceR)
model_summary(model_9)
model_summary(list(model_pols,model_btw, 
                  model1, model3, model4, model5, model6, model7, model8, model9),
              std = TRUE,
              digits =3,
              file = "Models.doc")
# delete
unlink("Models.doc")

###########################################################################################

# 참고 - REML vs. MLE
## MLE (# interaction 모형에서 주로 사용해야함)
model_mle <- lmer(mathach ~ 1 + (1 | id), data=data_merged, REML = F)
HLM_summary(model_mle,test.rand = T, digits = 3)

# lrtest
model1 <- lmer(mathach ~ 1 + (1 | id), data=data_merged, REML = F)
model2 <- lmer(mathach ~ ses + (1 | id), data=data_merged, REML = F)
lrtest <- anova(model2, model1)
lrtest
tidy(lrtest)

###########################################################################################
## Robust Standard Errors
# install.packages('robustlmm')
library(robustlmm)
model_robust <- rlmer(mathach ~ 1 + (1 | id), data=data_merged, init = lmerNoFit)
summary(model_robust)

## Robust Standard Errors (2)
# install.packages('clubSandwich')
library(clubSandwich)
model11 <- lmer(mathach ~ ses_groupmc * sector + meanses + (ses_groupmc | id), data = data_merged, REML = FALSE)
coef_test(model11, 
          vcov = "CR1S", # stata default 
          cluster = data_merged$id, test = "naive-t")

