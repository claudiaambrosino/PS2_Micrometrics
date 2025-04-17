/* Group number: 11 */
/* Group composition: Claudia Ambrosino, Flavia Grasso and Titouan Renault */

/* Gets user name */
local user = c(username)
display "`user'"

/* Stores filepath conditionally */
if ("`user'" == "erick") {
    global filepath "/home/erick/TEMP/"
}

if ("`user'" == "grasso") {
    global filepath "/Users/grasso/Documents/Bocconi ESS/2024-2025/Semester 2/20295 - Microeconometrics/Problem Set 2"
}

if ("`user'" == "user") {
    global filepath "C:/Users/user/Desktop/Microeconometrics/Problem set 2"
}

if ("`user'" == "C") {
    global filepath "/FILE/PATH/C/"
}


display "$filepath"

*Change working directory 
cd "$filepath"

global data "${filepath}/data"
display "$data"

global output "${filepath}/output"
display "$output"

******************************************************************************
******************************************************************************
                                *Exercise 1*

******************************************************************************
******************************************************************************

******************************************************************************
/* Question a. 
******************************************************************************
Because divorce rates are group averages for each state, we want to account for how many observations (individuals) were used to compute it. According to the weighting procedures summary, we should use analytic weights, weighting according to stpop.
We use analytic weights when working with group-mean observations, this ensures that results are representative and it allows to give relatively more weight to the averages computed on larger samples, which would result in more precise estimates of divorce rates.
*/

*Cluster standard errors at the state level in regressions

*****************************************************************************
/*Question b*/
*****************************************************************************

// First graph
use "$data/pset_4.dta", clear
gen reform_friedberg = (lfdivlaw >= 1968 & lfdivlaw <= 1988)

collapse (mean) div_rate [aweight=stpop], by(year reform_friedberg)

reshape wide div_rate, i(year) j(reform_friedberg)
gen diff = div_rate1 - div_rate0

twoway (line div_rate1 year,  lcolor(black) lpattern(solid)) ///
	(line div_rate0 year,  lcolor(gray) lpattern(solid)) ///
	(line diff year, lcolor(black) lpattern(dash)) ///
	, title("Divorce Rates in Reform and Control States") ///
	legend(label(1 "Reform states") label(2 "Control states (Friedberg)") label(3 "Difference in divorce rates: Reform - Control (Friedberg)") pos(1)) ///
	xtitle("Year") ///
	xlabel(1956(2)1998, angle(forty_five)) ///
	ytitle("Yearly Divorce Rates (per 1000 people)") ///
	xline(1968 1988, lcolor(gray) lpattern(dash)) ///
	xsize(12) ysize(8)

//Second graph
use "$data/pset_4.dta", clear

gen reform_early = .
replace reform_early = 1 if lfdivlaw >= 1969 & lfdivlaw <= 1973
replace reform_early = 0 if lfdivlaw == 2000
drop if missing(reform_early)

keep if year <= 1978 

collapse (mean) div_rate [aweight=stpop], by (year reform_early)

reshape wide div_rate, i(year) j(reform_early)
gen diff = div_rate1 - div_rate0

twoway (line div_rate1 year,  lcolor(black) lpattern(solid)) ///
	(line div_rate0 year,  lcolor(gray) lpattern(solid)) ///
	(line diff year, lcolor(black) lpattern(dash)) ///
	, title("Divorce Rates in Early Reform and Late Reform States") ///
	legend(label(1 "Early Reform states") label(2 "Late Reform states") label(3 "Difference in divorce rates: Early - Late") pos(1)) ///
	xtitle("Year") ///
	xlabel(1956(2)1979, angle(forty_five)) ///
	ytitle("Yearly Divorce Rates (per 1000 people)") ///
	xline(1969, lcolor(gray) lpattern(dash)) ///
	xsize(12) ysize(8)

*Add comment

/* Do your results support the assumption of parallel trends? 
seems ok - aussi dans le 2e dis que apres 1978 la diff a monte tabdis qu'avant ct assez stable
*/

*****************************************************************************
/*Question c*/
*****************************************************************************
use "$data/pset_4.dta", clear

keep if year == 1968 | year == 1978
keep if (lfdivlaw >=1968 & lfdivlaw<=1973) | (lfdivlaw==2000) //you should compare states adopting the unilateral divorce law between 1969 and 1973 to the ones that introduced it in the year 2000."

gen UNILATERAL = (lfdivlaw >= 1969 & lfdivlaw <= 1973)
gen POST = (year==1978)
gen POST_UNILATERAL = POST*UNILATERAL

/*Point i*/

*Pooled OLS 
reg div_rate POST POST_UNILATERAL [aweight = stpop], vce(robust)
*Should we save the output?

*Diff-in-diff specification
reg div_rate POST UNILATERAL POST_UNILATERAL [aweight=stpop], vce(robust)

*Based on the graphs you created in section (a), could you say something about the difference in the coefficients from regressions (i) and (ii)? What is the effect of introducing unilateral divorce laws according to this analysis?

*****************************************************************************
/*Question d*/
*****************************************************************************

// POST = 1, UNILATERAL = 1
sum div_rate if UNILATERAL==1 & POST==1 [aweight=stpop]
scalar AVG_Y_1_1 = r(mean)
// POST = 0, UNILATERAL = 1
sum div_rate if UNILATERAL==1 & POST==0 [aweight=stpop]
scalar AVG_Y_1_0 = r(mean)
// POST = 1, UNILATERAL = 0
sum div_rate if UNILATERAL==0 & POST==1 [aweight=stpop]
scalar AVG_Y_0_1 = r(mean)
// POST = 0, UNILATERAL = 0
sum div_rate if UNILATERAL==0 & POST==0 [aweight=stpop]
scalar AVG_Y_0_0 = r(mean)

matrix TABLE_1 = J(3,3,.)
matrix colnames TABLE_1 = "UNILATERAL=1" "UNILATERAL=0" "Difference 2"
matrix rownames TABLE_1 = "POST=1" "POST=0" "Difference 1"

matrix TABLE_1[1,1] = AVG_Y_1_1   // POST = 1, UNILATERAL = 1
matrix TABLE_1[1,2] = AVG_Y_0_1   // POST = 1, UNILATERAL = 0
matrix TABLE_1[1,3] = AVG_Y_1_1 - AVG_Y_0_1

matrix TABLE_1[2,1] = AVG_Y_1_0   // POST = 0, UNILATERAL = 1
matrix TABLE_1[2,2] = AVG_Y_0_0   // POST = 0, UNILATERAL = 0
matrix TABLE_1[2,3] = AVG_Y_1_0 - AVG_Y_0_0

matrix TABLE_1[3,1] = AVG_Y_1_1 - AVG_Y_1_0
matrix TABLE_1[3,2] = AVG_Y_0_1 - AVG_Y_0_0
matrix TABLE_1[3,3] = (AVG_Y_1_1 - AVG_Y_1_0) - (AVG_Y_0_1 - AVG_Y_0_0)

matrix list TABLE_1

putexcel set "$output/TABLE_1.xlsx", sheet("DiD Matrix") replace
putexcel A2=matrix(TABLE_1), names nformat(number_d2) 

*Adjust format

*****************************************************************************
/*Question e*/
*****************************************************************************

use "$data/pset_4.dta", clear

encode st, gen(st_id)
drop st
sort st_id year

xtset st_id year
keep if year >= 1956 & year <= 1988

gen IMP_UNILATERAL = (lfdivlaw <= year)

/* Regression i*/
reg div_rate IMP_UNILATERAL i.st_id i.year [aweight = stpop], vce(cluster st_id)
outreg2 using "$output/tables_e.xls", title("Regression E.i-iii") symbol() keep(IMP_UNILATERAL) excel replace

/* Regression ii */

*Get number of states
ssc install distinct
distinct st_id 

qui forval i=1/51{
	bysort st_id (year): gen tt_linear_`i'=_n if st_id==`i' 
	replace tt_linear_`i'=0 if tt_linear_`i'==.
}

reg div_rate IMP_UNILATERAL i.year i.st_id  tt_linear_* [aweight = stpop], vce(cluster st_id)
outreg2 using "$output/tables_e.xls", title("Regression E.i-iii") symbol() keep(IMP_UNILATERAL) excel append

/* Regression iii */

qui forval i = 1/51 {
    gen tt_square_`i' = tt_linear_`i'^2
}
reg div_rate IMP_UNILATERAL i.year i.st_id tt_linear_* tt_square_* [aweight = stpop], vce(cluster st_id)
outreg2 using "$output/tables_e.xls", title("Regressions E.i-iii") symbol() keep(IMP_UNILATERAL) excel append

save "$data/Epset_4.dta", replace

*Interpret the results of all 3 regressions. Can you think of a reason for the results to change across specifications? Under which assumption should these results be the same?

*****************************************************************************
/*Question f*/
*****************************************************************************

* Create simulated observations
clear
set obs 6
gen obs = _n
gen state = floor(.9 + obs/3)
bysort state: gen year = _n
gen D = state == 1 & year == 3
replace D = 1 if state == 2 & (year == 2 | year == 3)

*Creates simulated outcomes
gen Y  = 0.1 + 0.02*(year==2) + 0.05*(D==1) + runiform()/100
gen Y2 = 0.1 + 0.02*(year==2) + 0.05*(D==1) + 0.3*(state==2 & year==3) + runiform()/100
gen Y3 = 0.1 + 0.02*(year==2) + 0.05*(D==1) + 0.4*(state==2 & year==3) + runiform()/100
gen Y4 = 0.1 + 0.02*(year==2) + 0.05*(D==1) + 0.5*(state==2 & year==3) + runiform()/100

reg Y D i.state i.year 
outreg2 using "$output/tables_f.xls", title("Regressions F") symbol() excel replace
reg Y2 D i.state i.year
outreg2 using "$output/tables_f.xls", title("Regressions F") symbol() excel append
reg Y3 D i.state i.year
outreg2 using "$output/tables_f.xls", title("Regressions F") symbol() excel append
reg Y4 D i.state i.year
outreg2 using "$output/tables_f.xls", title("Regressions F") symbol() excel append

* Is it possible to estimate the treatment coefficient consistently in each of these cases?

*****************************************************************************
/*Question g*/
*****************************************************************************
*ssc install twowayfeweights, replace
*ssc install gtools, replace

twowayfeweights Y state year D, type(feTR)
twowayfeweights Y2 state year D, type(feTR)
twowayfeweights Y3 state year D, type(feTR)
twowayfeweights Y4 state year D, type(feTR)

* Can you explain why the sign of the estimated effect has changed between the regression on Y and the one on Y 4?


*****************************************************************************
/*Question h*/
*****************************************************************************
use "$data/Epset_4.dta", clear
ssc install bacondecomp

/* Point i*/
gen init_stpop = .
bysort st_id (year): replace init_stpop = stpop if _n == 1
bysort st_id: replace init_stpop = init_stpop[_n-1] if missing(init_stpop)

/* Point ii*/
reg div_rate IMP_UNILATERAL i.st_id i.year [aweight = init_stpop]
outreg2 using "$output/tables_h.xls", title("Regression H") symbol() excel replace

/* Point iii*/
bacondecomp div_rate IMP_UNILATERAL [aweight = init_stpop], stub (Bacon_)
*Modify labeling

*Briefly explain what is the analysis proposed by Goodman-Bacon (2021). Is there evidence of issues regarding negative weights?

*****************************************************************************
/*Question i*/
*****************************************************************************
gen relative_udl = year - lfdivlaw

*Create dummy for states which have adopted the unilateral divorce laws after the period of interest
gen outofperiod_udl = (lfdivlaw > 1988)
* replace relative_udl = 0 if outofperiod_udl == 1

*Creates dummies for leads and lags
forvalues k = 10(-1)2 {
gen D_`k' = relative_udl == -`k'
}
forvalues k = 0/15 {
gen D`k' = relative_udl == `k'
}

/* Point i*/
eststo reg1: reghdfe div_rate D_* D0-D15 [aweight=stpop], absorb (i.st_id i.year) vce(clusted st_id)
outreg2 using "$output/tables_i.xls", title("Regressions I.i-iii") symbol() estimates store reg1

/* Point ii*/
eststo reg2: reghdfe div_rate D_* D0-D15 tt_linear_* [aweight=stpop], absorb (i.st_id i.year) vce(clusted st_id)
estimates store reg2

/* Point iii*/
reghdfe div_rate D_* D0-D15 tt_linear_* tt_square_* [aweight=stpop], absorb (st_id year) vce(clusted st_id)
estimates store reg3

*Interpret the results of all 3 regressions. What can we see in the behaviour of divorce rates through this analysis that was not possible in the single coefficient analysis?

*****************************************************************************
/*Question j*/
*****************************************************************************
*ssc install coefplot

coefplot (reg1, label("No Trends")) (reg2, label("Linear Trends")) (reg3, label("Quadratic Trends")) , keep(D_* D*) vertical xtitle("Years relative to law introduction") ytitle("Estimated effect on divorce rate") xlabel(1 "`-10'" 2 "`-9'" 3 "`-8'" 4 "`-7'" 5 "`-6'" 6 "`-5'" ///
        7 "`-4'" 8 "`-3'" 9 "`-2'" 10 "`-1'" 11 "0" ///
        12 "1" 13 "2" 14 "3" 15 "4" 16 "5" 17 "6" ///
        18 "7" 19 "8" 20 "9" 21 "10" 22 "11" 23 "12" ///
        24 "13" 25 "14" 26 "15", angle(0)) xline(11, lpattern(dash) lcolor(gs8)) ciopts(recast(rcap))
*Save graph
		 
*****************************************************************************
/*Question k*/
*****************************************************************************
*Friedberg (1998), after controlling for state and year fixed effects as well as state-specific time trends, found that the adoption of unilateral divorce laws was associated with an increase of 0.44 divorces per 1,000 people annually. In contrast, Wolfers (2006) concludes that while unilateral divorce laws did lead to a short-run rise in divorce rates, this effect was reversed within a decade. Specifically, he finds that eight years after the implementation of the law, the estimated effect on the annual divorce rate becomes negative, but not statistically different from 0. Wolfers argues that the discrepancy between his findings and Friedberg's stems from the latter's failure to adequately separate pre-existing trends from the causal impact of the policy change. He emphasizes that divorce rates exhibit dynamic responses to legal reforms and offers four potential explanations for the observed long-run effects: a shift in the timing of divorce (i.e., earlier divorces rather than more divorces), changes in marriage rates, diffusion of new divorce norms from treated to control states, and regression to the mean (i.e., states with historically higher divorce rates were more likely to adopt reform, which may explain the convergence in divorce norms)

*****************************************************************************
/*Question l*/
*****************************************************************************
*ssc install avar
*ssc install eventstudyinteract
help eventstudyinteract

*No timetrends
eventstudyinteract div_rate D_* D0-D15 [aweight=stpop], cohort(lfdivlaw) control_cohort(outofperiod_udl) absorb(st_id year) vce(cluster st_id)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(diagonal(st_matrix("e(V_iw)"))))
matrix C = C \ A'
matrix list C
coefplot matrix(C[1]), se(C[2]) keep(D_* D*) vertical yline(0) xtitle("Years after law") ytitle("Estimated effect") ///
				title("Divorce rate") xlabel(, alternate)
*Save graph
				
*Linear time-trends
eventstudyinteract div_rate D_* D0-D15 [aweight=stpop], cohort(lfdivlaw) control_cohort(outofperiod_udl) absorb(st_id year) covariates(tt_linear_*) vce(cluster st_id)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(diagonal(st_matrix("e(V_iw)"))))
matrix C = C \ A'
matrix list C
coefplot matrix(C[1]), se(C[2]) keep(D_* D*) vertical yline(0) xtitle("Years after law") ytitle("Estimated effect") ///
				title("Divorce rate") xlabel(, alternate)
*Save graph

*Quadratic time-trends
eventstudyinteract div_rate D_* D0-D15 [aweight=stpop], cohort(lfdivlaw) control_cohort(outofperiod_udl) absorb(st_id year) covariates(tt_linear_* tt_square_*) vce(cluster st_id)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(diagonal(st_matrix("e(V_iw)"))))
matrix C = C \ A'
matrix list C
coefplot matrix(C[1]), se(C[2]) keep(D_* D*) vertical yline(0) xtitle("Years after law") ytitle("Estimated effect") ///
				title("Divorce rate") xlabel(, alternate)
*Save graph 

*Are your results consistent with the ones from the original paper? Briefly explain what kind of correction your proposed algorithm is performing.
