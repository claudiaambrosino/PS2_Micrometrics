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

if ("`user'" == "titouanrenault") {
    global filepath "/Users/titouanrenault/Desktop/Master/micrometrics/Problem set 2"
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

graph export "$output/graph_b1.png", replace


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
	
graph export "$output/graph_b2.png", replace



/* Do your results support the assumption of parallel trends? 
The first graph compares states that reformed in 1968 and 1988 and those that did not. We see that before the wave of reform, both samples seem to follow the same upward trend, supporting the parallel trends assumption. 
The second graph compares states that adopted the unilateral divorce law between 69 and 73 to the late reform states, that adopted it in 2000. Again, the trend in divorce rates seems to be the same across the control and reform states, supporting the parallel trends assumption.  
*/

*****************************************************************************
/*Question c*/
*****************************************************************************

use "$data/pset_4.dta", clear

keep if year == 1968 | year == 1978
keep if (lfdivlaw >=1968 & lfdivlaw<=1973) | (lfdivlaw==2000)
gen UNILATERAL = (lfdivlaw >= 1969 & lfdivlaw <= 1973)
gen POST = (year==1978)
gen POST_UNILATERAL = POST*UNILATERAL

* Regression i: pooled OLS
reg div_rate POST POST_UNILATERAL [aweight = stpop], vce(robust)
outreg2 using "$output/tables_c.xls", title("Regression C.i-ii") symbol() excel replace

*Regression ii: diff-in-diff specification
reg div_rate POST UNILATERAL POST_UNILATERAL [aweight=stpop], vce(robust)
outreg2 using "$output/tables_c.xls", title("Regression C.i-ii") symbol() excel append


/* The coefficients between regression i and ii have significant differences. In the pooled OLS, the estimate for post_unilateral is significantly greater than 0, suggesting that the introduction of the unilateral law has a positive effect on divorce rates. However, when we include the variable unilateral, accounting for state fixed effects, the estimate for post_unilateral reduces to -.0050148, and becomes insignificant (p-value=0.993). The difference-in-difference accounts for pre-existing differences in divorce rates between states whereas the pooled OLS estimates the difference in outcomes between the treatment and control groups in 1978 (the post year). This positive bias comes from selection of states in the treatment group, as states adopting the law generally faced higher divorce rates. */
 
 
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
matrix colnames TABLE_1 = "UNILATERAL=1 " "UNILATERAL=0 " "Difference 2"
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


*****************************************************************************
/*Question e*/
*****************************************************************************

use "$data/pset_4.dta", clear

encode st, gen(st_id)
drop st
xtset st_id year

keep if year >= 1956 & year <= 1988
gen IMP_UNILATERAL = (lfdivlaw <= year)

forval i=1/51{
	bysort st_id (year): gen t_`i'=_n if st_id==`i' 
	replace t_`i'=0 if t_`i'==.
	bysort st_id (year): gen t2_`i'=_n^2 if st_id==`i' 
	replace t2_`i'=0 if t2_`i'==.
}


/* Regression i*/
reg div_rate IMP_UNILATERAL i.st_id i.year [aweight = stpop], vce(cluster st_id)
outreg2 using "$output/tables_e.xls", title("Regression E.i-iii") symbol() keep(IMP_UNILATERAL) excel replace


/* Regression ii */
reg div_rate IMP_UNILATERAL i.st_id i.year t_* [aweight = stpop], vce(cluster st_id)
outreg2 using "$output/tables_e.xls", title("Regression E.i-iii") symbol() keep(IMP_UNILATERAL) excel append


/* Regression iii */
reg div_rate IMP_UNILATERAL i.st_id i.year t_* t2_* [aweight = stpop], vce(cluster st_id)
outreg2 using "$output/tables_e.xls", title("Regressions E.i-iii") symbol() keep(IMP_UNILATERAL) excel append

save "$data/Epset_4.dta", replace



/* The first regression controls for state and year differences yiels an estimate of -.055 suggesting that the introduction of unilateral laws decreased divorce rates by .055‰ but the result is very insignificant (p-value = .718). There is no clear evidence that unilateral divorce law had an effect on divorce rates in this regression.
The second regression adds state-specific linear time trends to regression i. The estimate for IMP_UNILATERAL goes up to 0.477, and the coefficient is statistically significant at the 5%. So once we control for linear time trends the regression finds a significant increase of 0.477 divorces per 1000 people after the unilateral divorce law was implemented. 
The third regression controls for both state-specific linear and quadratic time trends (in addition to state and year fixed effects). Here, the coefficient for unilateral is 0.33 and is also statistically significant.

Because the initial regression returns such different coefficients than the second/third regression, it points to potential omitted variable bias in the first regression. If there are unobserved state specific time trends in divorce rates that are correlated with the timing of divorce law reforms, omitting them will bias the estimated effect of unilateral divorce laws. Friedberg argues that these trends may reflect social and demographic shifts.

The estimates would be the same if the parallel trends assumption held perfectly, as the divorce time trends would be identical across reformed and control states.  
 */
 

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
* In settings with staggered treatment implementation, regressions relying on a single treatment coefficient may yield biased estimates. In our simulated dataset, we construct four potential outcomes to examine this issue. We want to identify the treatment effect by estimating the coefficient on a treatment dummy variable. The first simulated outcome, Y, simulates a situation where outcome is defined in the same way for state 1 and 2, despite the staggered treatement timing. Both states experience an outcome increase of 0.05 due to treatement. The corresponding regression correctly estimate this result, returning a coefficient of 0.0566 significant at the 10% level. In the second specification (Y2), heterogeneity is introduced: while the treatment effect remains 0.05 in State 1, State 2 experiences an additional increase of 0.3 in the outcome of interest in year 3. This extra shift may reflect either state-specific time trends or dynamic treatment effects. Similarly, the third and fourth outcomes (Y3 and Y4) introduce additional increases of 0.4 and 0.5 in State 2 relative to State 1 in year 3. This heterogenity induces a bias in the treatement coefficient estimate, which becomes negative and statistically insignifican. Moreover, the bias seems to be increasing with twith the size of the uncontrolled-for differential effect in State 2. If control and treatment groups have different underlying time trends or the effect of treatement is dynamic, and the regression doesn't model those trends, these will bias the estimate of the treatement effect. In our simulation, any additional change in outcome not due to the treatement in state 2 (which serves as the control group) is considered in the counterfactual for state 1, leading to a downward bias in the treatement estimate.

*****************************************************************************
/*Question g*/
*****************************************************************************
*ssc install twowayfeweights, replace
*ssc install gtools, replace

log using "$output/results.g", replace
twowayfeweights Y state year D, type(feTR)
twowayfeweights Y2 state year D, type(feTR)
twowayfeweights Y3 state year D, type(feTR)
twowayfeweights Y4 state year D, type(feTR)
log close

* Can you explain why the sign of the estimated effect has changed between the regression on Y and the one on Y4?

* With a TWFE regression, the estimated coefficient on the treatement dummy  is a weighted average of multiple Difference-in-Difference estimators. Each DiD compares the evolution of outcomes over time between two groups: one that changes treatement status and one that does not. However, in some cases—such as in year 3 of our simulated dataset—the comparison group is already treated when the treatment group switches. Then, the treatment effect for the earlt-treated group at the later d period gets differenced out by the DID, hence the negative weights. Negative weights represent a threat for correct treatement effect estimation in case of heterogeneous effects, because they may produce a negative coefficient estimate while all the ATEs are positive (this is what happens in regressions Y2, Y3 and Y4). In our simulated dataset, however, regression Y assumes no heterogeneity by construction. Therefore, even though negative weights exist, they do not bias the coefficient estimate in this specific case.

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
graph export "$output/graph_h.png", replace

*Briefly explain what is the analysis proposed by Goodman-Bacon (2021). Is there evidence of issues regarding negative weights?

* Goodman and Bacon (2021) analyze a DiD design where units receive treatment at different times. They show that in such settings, the two-way fixed effects (TWFE) estimator is a weighted average of all possible two-group, two-period DiD estimates in the data—including comparisons between early and later adopters, among others. The weights assigned to each 2x2 DD estimate are proportional to the size of the timing groups and to the variance of the treatment indicator in each comparison. This variance is typically highest for units treated around the middle of the panel, making these comparisons more influential in the overall estimate. When treatment effects are constant over time, all weights are positive, and the TWFE estimator provides a clean variance-weighted average of treatment effects across groups. However, if treatment effects vary over time, negative weights can emerge. 
* This happens because already-treated units sometimes serve as controls in later comparisons. The changes in their outcomes, which may be influenced by ongoing treatment, are subtracted, introducing bias when treatment effects evolve over time. While this does not violate the parallel trends assumption for untreated outcomes, it does indicate that we should be cautious when using TWFE estimators to summarize treatment effects. 
* Our plot shows how the treatment effect (y axis) varies for different weight (x axis). We also include a marker to indicate the kind of comparison computed: timing groups (circle), always treated vs timing (triangle), and never treated vs timing (x). We observe in the plot that the largest weights are for comparison between never treated and timing states, and most weights are centered around 0.  Also, when we look at the decomposition, we note that most of the weights (around 88%) are for comparison of never treated vs timing. There is also a few negatives did estimate, but their impact on the overall estimate seems relatively small due to the small weight attached to them. 
* Finally, we do not observe any clear issues related to negative weights in our case. While some negative weights are present, they are tied to DD estimates of relatively small magnitude. In contrast, the largest positive DD estimates correspond to positive weights, which reduces concerns about potential bias.

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
reghdfe div_rate D_* D0-D15 [aweight=stpop], absorb (i.st_id i.year) vce(clusted st_id) 

estimates store reg1
outreg2 using "$output/tables_i.xls", title("Regressions I.i-iii") symbol()replace

/* Point ii*/
reghdfe div_rate D_* D0-D15 tt_linear_* [aweight=stpop], absorb (i.st_id i.year) vce(clusted st_id)

estimates store reg2
outreg2 using "$output/tables_i.xls", title("Regressions I.i-iii") symbol()append

/* Point iii*/
reghdfe div_rate D_* D0-D15 tt_linear_* tt_square_* [aweight=stpop], absorb (st_id year) vce(clusted st_id)

estimates store reg3
outreg2 using "$output/tables_i.xls", title("Regressions I.i-iii") symbol()append

*Interpret the results of all 3 regressions. What can we see in the behaviour of divorce rates through this analysis that was not possible in the single coefficient analysis?

*The event study regression provides insights into the dynamic treatement effects  of the introduction of unilateral divorce laws on divorce rates.. The single coefficient analysis only allows to observe average effects over time, and it does not provide information about the underlying trends about this value. 
* In the first specification, which excludes linear and quadratic state-specific time trends, the coefficients for several treatement leads are positive and significantly different from 0. This raises concerns about the validity of the parallel trends assumption. However, once state-specific linear and quadratic time trends are included, the  pre-treatment coefficient estimates become smaller in magnitude and statistically insignificant. 
*All specification show overall significant positive effects immediately following the reform and for the years of early adoption, with declining values in the following years. The magnitude and significance of the post-treatment coefficients decline from specification (1) to (3), meaning that the first specification confounds treatement effects with state-specific time trends.
*Overall, the results of the event-study support the findings of Wolfers (2006) in highlighting the dynamic behavior of the effect of unilateral divorce laws on divorce rates. Specifically, it seems clear that the effect peaks in the 3-5 years following implementation, as a result of the pent-up demand, while it tends to diminish in the following years. 

*****************************************************************************
/*Question j*/
*****************************************************************************
*ssc install coefplot

coefplot (reg1, label("No Trends")) (reg2, label("Linear Trends")) (reg3, label("Quadratic Trends")) , keep(D_* D*) vertical xtitle("Years relative to law introduction") ytitle("Estimated effect on divorce rate") xlabel(1 "`-10'" 2 "`-9'" 3 "`-8'" 4 "`-7'" 5 "`-6'" 6 "`-5'" ///
        7 "`-4'" 8 "`-3'" 9 "`-2'" 10 "`-1'" 11 "0" ///
        12 "1" 13 "2" 14 "3" 15 "4" 16 "5" 17 "6" ///
        18 "7" 19 "8" 20 "9" 21 "10" 22 "11" 23 "12" ///
        24 "13" 25 "14" 26 "15", angle(0)) xline(11, lpattern(dash) lcolor(gs8)) ciopts(recast(rcap))
graph export "$output/graph_j.png", replace
		 
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
graph export "$output/graph_li.png", replace
				
*Linear time-trends
eventstudyinteract div_rate D_* D0-D15 [aweight=stpop], cohort(lfdivlaw) control_cohort(outofperiod_udl) absorb(st_id year) covariates(tt_linear_*) vce(cluster st_id)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(diagonal(st_matrix("e(V_iw)"))))
matrix C = C \ A'
matrix list C
coefplot matrix(C[1]), se(C[2]) keep(D_* D*) vertical yline(0) xtitle("Years after law") ytitle("Estimated effect") ///
				title("Divorce rate") xlabel(, alternate)
graph export "$output/graph_lii.png", replace

*Quadratic time-trends
eventstudyinteract div_rate D_* D0-D15 [aweight=stpop], cohort(lfdivlaw) control_cohort(outofperiod_udl) absorb(st_id year) covariates(tt_linear_* tt_square_*) vce(cluster st_id)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(diagonal(st_matrix("e(V_iw)"))))
matrix C = C \ A'
matrix list C
coefplot matrix(C[1]), se(C[2]) keep(D_* D*) vertical yline(0) xtitle("Years after law") ytitle("Estimated effect") ///
				title("Divorce rate") xlabel(, alternate)
graph export "$output/graph_liii.png", replace

*Are your results consistent with the ones from the original paper? 
*The results obtained with the implementation from the Sun and Abraham (2021) event-study estimation are overall consistent with the ones presented by Wolfers (2006). Specifically, they confirm the fact divorce law reform led to an immediate spike in the divorce rate that dissipates over time. Although, one aspect that seemed puzzling in Wolfer's analysis was the fact that the coefficients of some long-run leads were negative and statistically significant. By applying the Sun and Abraham correction, the declining trend is still present in the coefficients for the late years after implementation, but none of those is statistically different from zero.

*Briefly explain what kind of correction your proposed algorithm is performing.

* Sun and Abraham (2021) underline the fact in settings with heterogenous treatement effects and variation in treatement timing across units, using the estimates of relative periods coefficients as measures of dynamic treatement effects may lead to misleading results. Specifically, each of these coefficients can be expressed as a weighted average of cohort-specific effects from both its own relative period and other relative periods; therefore, the estimate is contaminated by effects from other periods and other cohorts. If treatement is heterogenous across cohorts in terms of effect or timing, the coefficient estimate will no longer be a reliable indicator of treatement effect at relative time l only. To correct for this, the authors propose interaction-weighted estimators (IW). IW estimators are formed by first estimating cohort average treatement effects for each cohort at the relative time l, and calculating their weighted average using as weights each cohort's share. The algorithm in eventstudyinteract automates the estimation of these weights and returns estimators that are robust to heterogenous treatement effects. 
