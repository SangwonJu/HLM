* HLM: STATA는 R 대비 쉬운 표현식, Robust Standard Error 가능, 신뢰도 확인이 불가능
* 표 밑부분의 카이스퀘어 테스트는 \tau항=0인지, 즉 linear model과 다른 지를 검정

* 횡단자료 실습
cd "E:\OneDrive - SNU\(B) 대학원\세미나\HLM\hlm"
dir

* level-1 자료
use HSB1.dta, clear

* level-2 자료 merge

******참고******
* 1:1 inner_join
* m:1 left_join
* 1:m right_join
* m:m full_join
***************
merge m:1 id using ".\HSB2.dta"

* 데이터 병합 확인
desc
su
codebook

* encode
encode id, gen(id1)
drop id
rename id1 id


********************************************
* Centering

* meanses
bysort id :egen meanses = mean(ses)

* Group Mean Centering
gen ses_groupmc= ses - meanses

* Grand Mean Centering
qui su ses
gen ses_grandmc = ses - r(mean)


********************************************
* Pooled OLS
reg mathach ses ib0.sector
reg mathach ses ib0.sector, beta
reg mathach ses ib0.sector, beta vce(r)

tw scatter mathach ses
tobit mathach ses ib0.sector, vce(r) ul(1.512)
********************************************
* Between OLS
snapshot erase _all
snapshot save ,label("no 1")

collapse (mean) mathach ses sector, by(id)
list in 1/10

reg mathach ses ib0.sector
reg mathach ses ib0.sector, beta vce(r)

snapshot list
snapshot restore 1
********************************************
* Model 1: One-way ANOVA
* 무조건 모형
mixed mathach || id:, reml
eststo m1

** icc 정보 (신뢰도 계수를 확인가능)
estat icc

di _b[lns1_1_1:_cons]
di _se[lns1_1_1:_cons]
display exp(_b[lns1_1_1:_cons] - 1.96*_se[lns1_1_1:_cons])^2
display exp(_b[lns1_1_1:_cons] + 1.96*_se[lns1_1_1:_cons])^2
* STATA는 CI가 항상 양수가 되도록 표준편차에 ln을 취한 후 CI를 계산하고 다시 EXP를 시켜 SD로 돌림

** fit 정보
estat ic

display "deviance=" -2*e(ll)

********************************************
* Model 3: Random Intercept
mixed mathach ses_grandmc || id:, reml
mixed mathach ses_grandmc || id:, mle vce(r)

** icc 정보 (신뢰도 계수를 확인가능)
estat icc

** fit 정보
estat ic

* lrtest
eststo m1: mixed mathach  || id:, mle
eststo m3: mixed mathach ses_grandmc || id:, mle
lrtest m1 m3, stats

********************************************
* Model 4: Random Coefficient
mixed mathach ses_groupmc || id: ses_groupmc, reml
mixed mathach ses_groupmc || id: ses_groupmc, mle vce(r)

* Covariance Structure
mixed mathach ses_groupmc || id: ses_groupmc, mle vce(r) cov(un)

** icc 정보 (신뢰도 계수를 확인가능)
estat icc

** fit 정보
estat ic

* lrtest
eststo m4: mixed mathach ses_groupmc || id: ses_groupmc, mle
lrtest m1 m4, stats
lrtest m3 m4, stats
********************************************
* Model 5-1: Intercepts-as-Outcomes Model
** (1) Random-effects ANCOVA with Level-2 predictor
** without random coefficient
mixed mathach ses ib0.sector|| id: , reml
mixed mathach ses ib0.sector|| id: , mle vce(r)

** icc 정보 (신뢰도 계수를 확인가능)
estat icc

** fit 정보
estat ic

** with random coefficient
mixed mathach ses ib0.sector|| id: ses, reml
mixed mathach ses ib0.sector|| id: ses, mle vce(r)

** icc 정보 (신뢰도 계수를 확인가능)
estat icc

** fit 정보
estat ic

* lrtest
eststo m5: mixed mathach ses_groupmc ib0.sector|| id: , mle
eststo m6: mixed mathach ses_groupmc ib0.sector|| id: ses_groupmc, mle
lrtest m4 m6
********************************************
* Model 5-1: Intercepts-as-Outcomes Model
* (2) Contextual Effect
* without random coefficient
mixed mathach ses_groupmc meanses ib0.sector|| id: , reml

** with random coefficient
mixed mathach ses_groupmc meanses ib0.sector|| id: ses_groupmc, reml
mixed mathach ses_groupmc meanses ib0.sector|| id: ses_groupmc, mle vce(r)

** icc 정보 (신뢰도 계수를 확인가능)
estat icc

** fit 정보
estat ic

* lrtest
eststo m7: mixed mathach ses_groupmc meanses ib0.sector|| id: , mle
lrtest m5 m7, stats
********************************************
* 5-2 Slopes-as-Outcomes Model (* USE, REML = F)
mixed mathach c.ses_groupmc##ib0.sector|| id: , mle
mixed mathach c.ses_groupmc##ib0.sector|| id: ses_groupmc, mle

mixed mathach c.ses_groupmc##ib0.sector meanses|| id: ses_groupmc, mle
mixed mathach c.ses_groupmc##ib0.sector meanses|| id: ses_groupmc, mle vce(r)

estat icc
estat ic

* lrtest
eststo m8: mixed mathach c.ses_groupmc##ib0.sector meanses|| id: , mle
lrtest m7 m8, stats
********************************************
* final
mixed mathach c.ses_groupmc##ib0.sector meanses|| id: ses_groupmc, mle var cov(un)
estat ic
eststo final_rand: mixed mathach c.ses_groupmc##ib0.sector meanses|| id: ses_groupmc, mle var cov(un)
eststo final_nonrand: mixed mathach c.ses_groupmc##ib0.sector meanses|| id: , mle var cov(un)
lrtest final_rand final_nonrand, stats

* marginsplot
su ses_groupmc
return list
di r(mean) - 2*r(sd),  r(mean) - r(sd),  r(mean),  r(mean) + r(sd),  r(mean) + 2*r(sd)

margins i.sector, at(ses_groupmc=(-1.3211761 -.66058805 -1.617e-10 .66058805 1.3211761))
marginsplot, xlabel( -1.3211761 "M-2SD" -.66058805 "M-SD" -1.617e-10 "M" .66058805 "M+SD" 1.3211761 "M+2SD")

********************************************
* listcoef, esttab 쓸때 유의해야 함
esttab m1 m3 m4 m5 m6 m7 m8, se transform(ln*: exp(@)^2 exp(@)^2)  aic bic
