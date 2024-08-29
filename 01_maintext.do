* JUNE 3RD, 2024

* This script creates the main figures and tables for the paper

****************************************
********** FIGURES **********
****************************************

* Set the graph scheme
set scheme s1color

***************** JOB POSTINGS ******************

*************
* Figure 1A *
*************
* Overall trends in job postings

use "data/job_postings_analysis.dta", clear

* Adjust half_year variable for alignment
replace half_year = half_year - 1 if half_year > 0
preserve
* Collapse data to get standard deviation and count by half_year
collapse (sd) sd_diversity=diversity_flag (count) n_obs=diversity_flag, by(half_year)
* Save the collapsed dataset to a temporary file
tempfile tmp1
save `tmp1'
* Restore the original dataset
restore
* Collapse data to get mean by half_year
collapse (mean) mean_diversity=diversity_flag, by(half_year)
* Merge the mean dataset with the standard deviation and count dataset
merge 1:1 half_year using `tmp1'
* Generate standard error and confidence intervals
gen se_diversity = sd_diversity / sqrt(n_obs)
gen ci_upper = mean_diversity + 1.96 * se_diversity
gen ci_lower = mean_diversity - 1.96 * se_diversity
* Drop missing half_year values
drop if missing(half_year)
* Keep only relevant variables
keep half_year mean_diversity ci_lower ci_upper
* Create the graph
twoway (connected mean_diversity half_year, msymbol(O) mcolor(black) lcolor(black)) ///
       (rcap ci_lower ci_upper half_year, lcolor(black)) ///
       , xline(0, lpattern(dash) lcolor(red) lwidth(thick)) ///
       xtick(-4(1)6) xlabel(-4 "-2.5 Years" -3 "-2 Years" -2 "-1.5 Years" -1 "-1 Year" 0 "GF" 1 "+1 Year" 2 "+1.5 Years" 3 "+2 Years" 4 "+2.5 Years" 5 "+3 Years" 6 "+3.5 Years", angle(horizontal) labsize(*.75)) ///
       ytitle("Share of job postings focused on DEI", size(medium)) ///
       title("Figure 1A: Trends in DEI Job Postings") ///
       legend(off) ///
       name(fig1a, replace)

*************
* Figure 1B *
*************
* Job postings by organizational liberalism

use "data/job_postings_analysis.dta", clear

* Adjust half_year variable for alignment
replace half_year = half_year - 1 if half_year > 0
rename top_25th_percentile org_liberal
keep if org_liberal == 1 | bottom_25th_percentile == 1
preserve
* Collapse data to get standard deviation and count by half_year and org_liberal
collapse (sd) sd_diversity=diversity_flag (count) n_obs=diversity_flag, by(half_year org_liberal)
* Save the collapsed dataset to a temporary file
tempfile tmp2
save `tmp2'
* Restore the original dataset
restore
* Collapse data to get mean by half_year and org_liberal
collapse (mean) mean_diversity=diversity_flag, by(half_year org_liberal)
* Merge the mean dataset with the standard deviation and count dataset
merge 1:1 half_year org_liberal using `tmp2'
* Generate standard error and confidence intervals
gen se_diversity = sd_diversity / sqrt(n_obs)
gen ci_upper = mean_diversity + 1.96 * se_diversity
gen ci_lower = mean_diversity - 1.96 * se_diversity
* Drop missing half_year values
drop if missing(half_year)
* Keep only relevant variables
keep half_year org_liberal mean_diversity ci_lower ci_upper
* Create the graph
twoway (connected mean_diversity half_year if org_liberal == 1, msymbol(O) mcolor(navy) lcolor(navy)) ///
       (rcap ci_lower ci_upper half_year if org_liberal == 1, lcolor(navy)) ///
       (connected mean_diversity half_year if org_liberal == 0, msymbol(S) mcolor(crimson) lcolor(crimson)) ///
       (rcap ci_lower ci_upper half_year if org_liberal == 0, lcolor(crimson)) ///
       , xline(0, lpattern(dash) lcolor(red) lwidth(thick)) ///
       xtick(-4(1)6) xlabel(-4 "-2.5 Years" -3 "-2 Years" -2 "-1.5 Years" -1 "-1 Year" 0 "GF" 1 "+1 Year" 2 "+1.5 Years" 3 "+2 Years" 4 "+2.5 Years" 5 "+3 Years" 6 "+3.5 Years", angle(horizontal) labsize(*.75)) ///
       ytitle("Share of job postings focused on DEI", size(medium)) ///
       legend(order(1 "Top 25% Organizational Liberalism" 3 "Bottom 25% Organizational Liberalism") size(small)) ///
       title("Figure 1B: DEI Job Postings by Organizational Liberalism") ///
       name(fig1b, replace)

*************
* Figure 2A and 2B *
*************
* Event study on job postings

use "data/job_postings_analysis.dta", clear

* Generate additional time variables
gen year = year(post_date)
gen month = month(post_date)
encode state, gen(state_num)
destring rcid, replace

* Dynamic difference-in-differences
tab half_year, gen(half_year_dummy)
reghdfe diversity_flag half_year_dummy1-half_year_dummy3 o.half_year_dummy4 half_year_dummy5-half_year_dummy11 if org_liberal == 1, absorb(rcid#state_num) cluster(rcid)
est store lib
reghdfe diversity_flag half_year_dummy1-half_year_dummy3 o.half_year_dummy4 half_year_dummy5-half_year_dummy11 if bottom_25th_percentile == 1, absorb(rcid#state_num) cluster(rcid)
est store cons
coefplot (lib, mcolor(darkblue) lcolor(darkblue) msymbol(D) lpattern(solid) ciopts(recast(rcap) lcolor(darkblue))), ///
	omitted baselevels ///
    keep(half_year_dummy*) ///
    levels(95) ///
    vertical ///
    coeflabels(half_year_dummy1 = "-2.5 Years" ///
               half_year_dummy2 = "-2 Years" ///
               half_year_dummy3 = "-1.5 Years" ///
			   half_year_dummy4 = "-1 Year" ///
               half_year_dummy5= "GF" ///
               half_year_dummy6 = "+1 Year" ///
               half_year_dummy7 = "+1.5 Years" ///
               half_year_dummy8 = "+2 Years" ///
               half_year_dummy9 = "+2.5 Years" ///
			   half_year_dummy10 = "+3 Years" ///
			   half_year_dummy11 = "+3.5 Years", labsize(tiny)) ///
    msymbol(O) msize(small) mcolor(darkblue) lcolor(darkblue) /// Using grayscale for markers
    ciopts(recast(rcap) lcolor(darkblue) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(darkblue) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    xline(4, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(small)) ///
    title("Figure 2A: Change in DEI Job Postings (Top 25% Organizational Liberalism)") ///
    name(fig2a, replace)

coefplot (cons, mcolor(crimson) lcolor(crimson) msymbol(D) lpattern(solid) ciopts(recast(rcap) lcolor(crimson))), ///
	omitted baselevels ///
    keep(half_year_dummy*) ///
    levels(95) ///
    vertical ///
    coeflabels(half_year_dummy1 = "-2.5 Years" ///
               half_year_dummy2 = "-2 Years" ///
               half_year_dummy3 = "-1.5 Years" ///
			   half_year_dummy4 = "-1 Year" ///
               half_year_dummy5= "GF" ///
               half_year_dummy6 = "+1 Year" ///
               half_year_dummy7 = "+1.5 Years" ///
               half_year_dummy8 = "+2 Years" ///
               half_year_dummy9 = "+2.5 Years" ///
			   half_year_dummy10 = "+3 Years" ///
			   half_year_dummy11 = "+3.5 Years", labsize(tiny)) ///
    msymbol(O) msize(small) mcolor(crimson) lcolor(crimson) /// Using grayscale for markers
    ciopts(recast(rcap) lcolor(crimson) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(crimson) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    xline(4, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(small)) ///
    title("Figure 2B: Change in DEI Job Postings (Bottom 25% Organizational Liberalism)") ///
    name(fig2b, replace)

*************
* Figure 1C *
*************
* Overall trends in new hires

use "data/new_hires_analysis.dta", clear

replace half_year = half_year - 1 if half_year > 0
preserve
collapse (sd) sd_diversity=diversity_flag (count) n_obs=diversity_flag, by(half_year)
tempfile tmp3
save `tmp3'
restore
collapse (mean) mean_diversity=diversity_flag, by(half_year)
merge 1:1 half_year using `tmp3'
gen se_diversity = sd_diversity / sqrt(n_obs)
gen ci_upper = mean_diversity + 1.96 * se_diversity
gen ci_lower = mean_diversity - 1.96 * se_diversity
drop if missing(half_year)
keep half_year mean_diversity ci_lower ci_upper
twoway (connected mean_diversity half_year, msymbol(O) mcolor(black) lcolor(black)) ///
       (rcap ci_lower ci_upper half_year, lcolor(black)) ///
       , xline(0, lpattern(dash) lcolor(red) lwidth(thick)) ///
	xtick(-20(5)6) xlabel(-20 "-10 Years" -15 "-7.5 Years" -10 "-5 Years" -5 "-2.5 Years" 0 "GF" 5 "+2.5 Years", angle(horizontal) labsize(*.75)) ///
       ytitle("Share of all new hires focused on DEI", size(medium)) ///
       title("Figure 1C: Trends in DEI New Hires") ///
       legend(off) ///
       name(fig1c, replace)

*************
* Figure 1D *
*************
* New hires by organizational liberalism

use "data/new_hires_analysis.dta", clear

rename top_25th_percentile org_liberal
keep if org_liberal == 1 | bottom_25th_percentile == 1

replace half_year = half_year - 1 if half_year > 0
preserve
collapse (sd) sd_diversity=diversity_flag (count) n_obs=diversity_flag, by(half_year org_liberal)
tempfile tmp4
save `tmp4'
restore
collapse (mean) mean_diversity=diversity_flag, by(half_year org_liberal)
merge 1:1 half_year org_liberal using `tmp4'
gen se_diversity = sd_diversity / sqrt(n_obs)
gen ci_upper = mean_diversity + 1.96 * se_diversity
gen ci_lower = mean_diversity - 1.96 * se_diversity
drop if missing(half_year)
keep half_year org_liberal mean_diversity ci_lower ci_upper

twoway (connected mean_diversity half_year if org_liberal == 1, msymbol(O) mcolor(navy) lcolor(navy)) ///
       (rcap ci_lower ci_upper half_year if org_liberal == 1, lcolor(navy)) ///
       (connected mean_diversity half_year if org_liberal == 0, msymbol(S) mcolor(crimson) lcolor(crimson)) ///
       (rcap ci_lower ci_upper half_year if org_liberal == 0, lcolor(crimson)) ///
       , xline(0, lpattern(dash) lcolor(red) lwidth(thick)) ///
	xtick(-20(5)6) xlabel(-20 "-10 Years" -15 "-7.5 Years" -10 "-5 Years" -5 "-2.5 Years" 0 "GF" 5 "+2.5 Years", angle(horizontal) labsize(*.75)) ///
       ytitle("Share of all new hires focused on DEI", size(medium)) ///
       legend(order(1 "Top 25% Organizational Liberalism" 3 "Bottom 25% Organizational Liberalism") size(small)) ///
       title("Figure 1D: DEI New Hires by Organizational Liberalism") ///
       name(fig1d, replace)

*************
* Figure 2C and 2D *
*************
* Event study on new hires

use "data/new_hires_analysis.dta", clear

gen year = year(post_date)
gen month = month(post_date)
encode state, gen(state_num)
destring rcid, replace

* Dynamic difference-in-differences
tab half_year, gen(half_year_dummy)

reghdfe diversity_flag half_year_dummy16-half_year_dummy19 o.half_year_dummy20 half_year_dummy21-half_year_dummy26 if org_liberal == 1 & half_year > -6, absorb(rcid#state_num) cluster(rcid)
est store lib
reghdfe diversity_flag half_year_dummy16-half_year_dummy19 o.half_year_dummy20 half_year_dummy21-half_year_dummy26 if bottom_25th_percentile == 1 & half_year > -6, absorb(rcid#state_num) cluster(rcid)
est store cons

coefplot (lib, mcolor(darkblue) lcolor(darkblue) msymbol(D) lpattern(solid) ciopts(recast(rcap) lcolor(darkblue))), ///
	omitted baselevels ///
    keep(half_year_dummy*) ///
    levels(95) ///
    vertical ///
    coeflabels(half_year_dummy16 = "-10 Years" ///
               half_year_dummy17 = "-9.5 Years" ///
               half_year_dummy18 = "-9 Years" ///
			   half_year_dummy19 = "-8.5 Years" ///
               half_year_dummy20 = "GF" ///
               half_year_dummy21 = "+1 Year" ///
               half_year_dummy22 = "+1.5 Years" ///
               half_year_dummy23 = "+2 Years" ///
               half_year_dummy24 = "+2.5 Years" ///
			   half_year_dummy25 = "+3 Years", labsize(tiny)) ///
    msymbol(O) msize(small) mcolor(darkblue) lcolor(darkblue) /// 
    ciopts(recast(rcap) lcolor(darkblue) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(darkblue) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    xline(20, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(small)) ///
    title("Figure 2C: DEI New Hires (Top 25% Organizational Liberalism)") ///
    name(fig2c, replace)

coefplot (cons, mcolor(crimson) lcolor(crimson) msymbol(D) lpattern(solid) ciopts(recast(rcap) lcolor(crimson))), ///
	omitted baselevels ///
	ylabel(-0.001(0.001)0.003) ///
    keep(half_year_dummy*) ///
    levels(95) ///
    vertical ///
    coeflabels(half_year_dummy16 = "-10 Years" ///
               half_year_dummy17 = "-9.5 Years" ///
               half_year_dummy18 = "-9 Years" ///
			   half_year_dummy19 = "-8.5 Years" ///
               half_year_dummy20 = "GF" ///
               half_year_dummy21 = "+1 Year" ///
               half_year_dummy22 = "+1.5 Years" ///
               half_year_dummy23 = "+2 Years" ///
               half_year_dummy24 = "+2.5 Years" ///
			   half_year_dummy25 = "+3 Years", labsize(tiny)) ///
    msymbol(O) msize(small) mcolor(crimson) lcolor(crimson) /// 
    ciopts(recast(rcap) lcolor(crimson) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(crimson) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    xline(20, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(small)) ///
    title("Figure 2D: DEI New Hires (Bottom 25% Organizational Liberalism)") ///
    name(fig2d, replace)

graph combine fig1a fig1b fig1c fig1d, iscale(.5)
graph export "output/graphs/figure_1.png", as(png) replace

graph combine fig2a fig2b fig2c fig2d, iscale(.5)
graph export "output/graphs/figure_2.png", as(png) replace

*************
* Figure 3 *
*************

// Load dataset
use "data/race_analysis.dta", clear

*************
* Loop to Create Graphs for Subgroups *
*************

// Define the subgroups and their respective conditions
local subgroups "Black Hispanic API White Female Male"
local conditions "black==1 hispanic==1 api==1 white==1 female==1 male==1"
local titles "Black Hispanic API White Female Male"

// Loop through each subgroup
forval i = 1/6 {
    // Extract the condition for the current subgroup
    local subgroup : word `i' of `subgroups'
    local condition : word `i' of `conditions'
    local title : word `i' of `titles'
    
    // Load dataset
    use "data/race_analysis.dta", clear
    
    // Filter data for the current subgroup
    gen subgroup_flag = 0 
    replace subgroup_flag = 1 if `condition'
    
    rename subgroup_flag minority_flag
    rename top_25th_percentile org_liberal
    
    // Keep relevant observations
    keep if org_liberal == 1 | bottom_25th_percentile == 1
    
    // First stage of analysis: standard deviation and count by group
    preserve
    collapse (sd) sd_minority=minority_flag (count) n_obs=minority_flag, by(half_year org_liberal)
    tempfile tmp`i'
    save `tmp`i''
    restore
    
    // Second stage of analysis: mean by group
    collapse (mean) mean_minority=minority_flag, by(half_year org_liberal)
    
    // Merge back the first stage results
    merge 1:1 half_year org_liberal using `tmp`i''
    
    // Calculate standard error and confidence intervals
    gen se_minority = sd_minority / sqrt(n_obs)
    gen ci_upper = mean_minority + 1.96 * se_minority
    gen ci_lower = mean_minority - 1.96 * se_minority
    
    // Clean dataset for final output
    drop if missing(half_year)
    keep half_year org_liberal mean_minority ci_lower ci_upper
    
    // Set graphing scheme
    set scheme s2color
    
    // Generate graph for the current subgroup
    twoway (connected mean_minority half_year if org_liberal == 1, msymbol(O) mcolor(navy) lcolor(navy)) ///
           (rcap ci_lower ci_upper half_year if org_liberal == 1, lcolor(navy)) ///
           (connected mean_minority half_year if org_liberal == 0, msymbol(S) mcolor(crimson) lcolor(crimson)) ///
           (rcap ci_lower ci_upper half_year if org_liberal == 0, lcolor(crimson)) ///
           , xline(0, lpattern(dash) lcolor(red) lwidth(thick)) ///
           xtick(-20(5)6) xlabel(-20 "-10 Years" -15 "-7.5 Years" -10 "-5 Years" -5 "-2.5 Years" 0 "GF" 1 "+0.5 Year" 5 "+2.5 Years", angle(horizontal) labsize(*.75)) ///
           xtitle("Time Relative to Murder of George Floyd", size(medsmall)) ///
           ytitle("Share of New Hires Who Are `title'", size(medium)) ///
           legend(order(1 "Top 25% Organizational Liberalism" 3 "Bottom 25% Organizational Liberalism") size(small)) ///
           title("Figure 3: `title' New Hires by Organizational Liberalism") ///
           name(fig3_`subgroup', replace)
    
    // Export graph
    graph export "output/graphs/fig3_`subgroup'.png", as(png) replace
}


*************
* Table 1 *
*************

// Job Postings Data Summary
use "data/job_postings_analysis.dta", clear
sum diversity_flag
sum diversity_flag if pre == 1
sum diversity_flag if pre == 0

// New Hires Data Summary
use "data/new_hires_analysis.dta", clear
sum diversity_flag
sum diversity_flag if pre == 1
sum diversity_flag if pre == 0

*************
* Table 2 *
*************

// Job Postings Data Regression Analysis
use "data/job_postings_analysis.dta", clear
eststo: reghdfe diversity_flag post org_liberal post#org_liberal, absorb(year#month) cluster(rcid#state_num)
eststo: reghdfe diversity_flag post org_liberal post#org_liberal, absorb(rcid#state_num) cluster(rcid#state_num)
eststo: reghdfe diversity_flag post org_liberal post#org_liberal, absorb(year#month rcid#state_num) cluster(rcid#state_num)

// New Hires Data Regression Analysis
use "data/new_hires_analysis.dta", clear
eststo: reghdfe diversity_flag post org_liberal post#org_liberal, absorb(year#month) cluster(rcid#state_num)
eststo: reghdfe diversity_flag post org_liberal post#org_liberal, absorb(rcid#state_num) cluster(rcid#state_num)
eststo: reghdfe diversity_flag post org_liberal post#org_liberal, absorb(year#month rcid#state_num) cluster(rcid#state_num)
