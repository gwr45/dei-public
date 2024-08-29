**************************
* Figure A1 Diff in Diffs 
*******Job postings*******
**************************

*diff in diff job postings 
use "/mnt/overflow/gwr45/DEI/f500_sp500_jobpost_foranalysis.dta", clear

gen intern = 0 
replace intern = 1 if strpos(lower(jobtitle_raw), "intern") > 0
drop if intern==1
replace diversity_flag = 0 if strpos(lower(jobtitle_raw), "sdei") > 0

gen year = year(post_date_stata)
gen month = month(post_date_stata)

encode state, gen(state_num)
destring rcid, replace

*dynamic diff in diff

tab half_year, gen(half_year)

gen lib = 0
replace lib = 1 if top_25th_percentile==1

foreach y in 1 2 3 4 5 6 7 8 9 10 11{
gen lib_x_half_year`y'=lib*half_year`y'
} 

keep if top_25th_percentile ==1 | bottom_25th_percentile == 1

reghdfe diversity_flag lib_x_half_year1 lib_x_half_year2 lib_x_half_year3 o.lib_x_half_year4 lib_x_half_year5 lib_x_half_year6 lib_x_half_year7 lib_x_half_year8 lib_x_half_year9 lib_x_half_year10 lib_x_half_year11, a(year#month rcid) cluster(rcid)
est store F

coefplot F,  ///
	omitted baselevels ///
    keep(lib_x_half_year*) ///
    levels(95) ///
    vertical ///
    coeflabels(lib_x_half_year1 = "-2.5 Years" ///
               lib_x_half_year2 = "-2 Years" ///
               lib_x_half_year3 = "-1.5 Years" ///
			   lib_x_half_year4 = "-1 Year" ///
               lib_x_half_year5= "GF" ///
               lib_x_half_year6 = "+1 Year" ///
               lib_x_half_year7 = "+1.5 Years" ///
               lib_x_half_year8 = "+2 Years" ///
               lib_x_half_year9 = "+2.5 Years" ///
			   lib_x_half_year10 = "+3 Years" ///
			   lib_x_half_year11 = "+3.5 Years") ///
    msymbol(O) msize(small) mcolor(navy) lcolor(navy) /// Using grayscale for markers
    ciopts(recast(rcap) lcolor(navy) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(navy) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
        xline(4, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(medsmall)) ///
        title("A: Job Postings") ///
    name(fig_a_1, replace)
	

**************************
* Figure A1 Diff in Diffs 
*******New Hires*******
**************************

use "/mnt/overflow/gwr45/DEI/indivdual_position_sp500_f500_foranalysis.dta", replace

gen intern = 0 
replace intern = 1 if strpos(lower(jobtitle_raw), "intern") > 0
drop if intern==1
replace diversity_flag = 0 if strpos(lower(jobtitle_raw), "sdei") > 0

gen year = year(post_date_stata)
gen month = month(post_date_stata)
encode state, gen(state_num)
destring rcid, replace

*dynamic diff in diff
drop if half_year <-5
tab half_year, gen(half_year)

gen lib = 0 
replace lib = 1 if top_25th_percentile==1

keep if lib==1 | bottom_25th_percentile==1

foreach y in 1 2 3 4 5 6 7 8 9 10 11{
gen lib_x_half_year`y'=lib*half_year`y'
} 
  
reghdfe diversity_flag lib_x_half_year1 lib_x_half_year2 lib_x_half_year3 lib_x_half_year4 o.lib_x_half_year5 lib_x_half_year6 lib_x_half_year7 lib_x_half_year8 lib_x_half_year9 lib_x_half_year10 lib_x_half_year11, a(year#month rcid) cluster(rcid)
est store F
coefplot F,  ///
	omitted baselevels ///
    keep(lib_x_half_year*) ///
    levels(95) ///
    vertical ///
    coeflabels(lib_x_half_year1 = "-2.5 Years" ///
               lib_x_half_year2 = "-2 Years" ///
               lib_x_half_year3 = "-1.5 Years" ///
			   lib_x_half_year4 = "-1 Year" ///
               lib_x_half_year5= "-0.5 Year" ///
               lib_x_half_year6 = "GF" ///
               lib_x_half_year7 = "+1 year" ///
               lib_x_half_year8 = "+1.5 Years" ///
               lib_x_half_year9 = "+2 Years" ///
			   lib_x_half_year10 = "+2.5 Years" ///
			   lib_x_half_year11 = "+3 Years") ///
    msymbol(O) msize(small) mcolor(navy) /// Using grayscale for markers
    ciopts(recast(rcap) lcolor(navy) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(navy) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
        xline(5, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(medsmall)) ///
    title("B: New Hires") ///
    name(fig_a_2, replace)

	
**********************
** Figure A1 **
*Placebo event studies*
*job postings*
*************
*event study job postings 
use "/mnt/overflow/gwr45/DEI/f500_sp500_jobpost_foranalysis.dta", clear

gen year = year(post_date_stata)
gen month = month(post_date_stata)

encode state, gen(state_num)
destring rcid, replace

*PLACEBO EVENT STUDIES 

tab half_year, gen(half_year)

reghdfe diversity_flag half_year1 half_year2 o.half_year3 half_year4 if top_25th_percentile ==1 &  half_year<0, a(rcid#state_num) cluster(rcid)
est store L

reghdfe diversity_flag half_year1 half_year2 o.half_year3 half_year4  if bottom_25th_percentile ==1 &  half_year<0, a(rcid#state_num) cluster(rcid)
est store R

set scheme tab3

coefplot (L, mcolor(dknavy) lcolor(dknavy) msymbol(D) lpattern(solid) ciopts(recast(rcap) lcolor(dknavy))), ///
	omitted baselevels ///
    keep(half_year*) ///
    levels(95) ///
    	ylabel(-0.0002(0.0002)0.0008) ///
    vertical ///
    coeflabels(half_year1 = "-2.5 Years" ///
               half_year2 = "-2 Years" ///
               half_year3 = "-1.5 Years" ///
			   half_year4 = "-1 Year" ///
               half_year5= "GF" ///
               half_year6 = "+1 Year" ///
               half_year7 = "+1.5 Years" ///
               half_year8 = "+2 Years" ///
               half_year9 = "+2.5 Years" ///
			   half_year10 = "+3 Years" ///
			   half_year11 = "+3.5 Years", labsize(tiny)) ///
    msymbol(O) msize(small) mcolor(navy) lcolor(navy) /// Using grayscale for markers
    ciopts(recast(rcap) lcolor(navy) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(navy) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
        xline(3, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(small)) ///
        title("A: Change in DEI Job Postings: Top 25th Percentile Org Liberalism") ///
    name(fig_a_5a, replace)
    
    coefplot (R, mcolor(cranberry) lcolor(cranberry) msymbol(D) lpattern(solid) ciopts(recast(rcap) lcolor(cranberry))), ///
	omitted baselevels ///
	ylabel(-0.0002(0.0002)0.0008) ///
    keep(half_year*) ///
    levels(95) ///
    vertical ///
    coeflabels(half_year1 = "-2.5 Years" ///
               half_year2 = "-2 Years" ///
               half_year3 = "-1.5 Years" ///
			   half_year4 = "-1 Year" ///
               half_year5= "GF" ///
               half_year6 = "+1 Year" ///
               half_year7 = "+1.5 Years" ///
               half_year8 = "+2 Years" ///
               half_year9 = "+2.5 Years" ///
			   half_year10 = "+3 Years" ///
			   half_year11 = "+3.5 Years", labsize(tiny)) ///
    msymbol(O) msize(small) mcolor(navy) lcolor(navy) /// Using grayscale for markers
    ciopts(recast(rcap) lcolor(navy) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(navy) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
        xline(3, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(small)) ///
        title("B: Change in DEI Job Postings: Bottom 25th Percentile Org Liberalism") ///
    name(fig_a_5b, replace)
	
	reghdfe diversity_flag half_year1 o.half_year2 half_year3 half_year4 if top_25th_percentile ==1 &  half_year<0, a(rcid#state_num) cluster(rcid)
est store L

reghdfe diversity_flag half_year1 o.half_year2 half_year3 half_year4  if bottom_25th_percentile ==1 &  half_year<0, a(rcid#state_num) cluster(rcid)
est store R

set scheme tab3

coefplot (L, mcolor(dknavy) lcolor(dknavy) msymbol(D) lpattern(solid) ciopts(recast(rcap) lcolor(dknavy))), ///
	omitted baselevels ///
    keep(half_year*) ///
    levels(95) ///
    	ylabel(-0.0002(0.0002)0.0008) ///
    vertical ///
    coeflabels(half_year1 = "-2.5 Years" ///
               half_year2 = "-2 Years" ///
               half_year3 = "-1.5 Years" ///
			   half_year4 = "-1 Year" ///
               half_year5= "GF" ///
               half_year6 = "+1 Year" ///
               half_year7 = "+1.5 Years" ///
               half_year8 = "+2 Years" ///
               half_year9 = "+2.5 Years" ///
			   half_year10 = "+3 Years" ///
			   half_year11 = "+3.5 Years", labsize(tiny)) ///
    msymbol(O) msize(small) mcolor(navy) lcolor(navy) /// Using grayscale for markers
    ciopts(recast(rcap) lcolor(navy) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(navy) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
        xline(3, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(small)) ///
        title("A: Change in DEI Job Postings: Top 25th Percentile Org Liberalism") ///
    name(fig_a_5a, replace)
    
    coefplot (R, mcolor(cranberry) lcolor(cranberry) msymbol(D) lpattern(solid) ciopts(recast(rcap) lcolor(cranberry))), ///
	omitted baselevels ///
	ylabel(-0.0002(0.0002)0.0008) ///
    keep(half_year*) ///
    levels(95) ///
    vertical ///
    coeflabels(half_year1 = "-2.5 Years" ///
               half_year2 = "-2 Years" ///
               half_year3 = "-1.5 Years" ///
			   half_year4 = "-1 Year" ///
               half_year5= "GF" ///
               half_year6 = "+1 Year" ///
               half_year7 = "+1.5 Years" ///
               half_year8 = "+2 Years" ///
               half_year9 = "+2.5 Years" ///
			   half_year10 = "+3 Years" ///
			   half_year11 = "+3.5 Years", labsize(tiny)) ///
    msymbol(O) msize(small) mcolor(navy) lcolor(navy) /// Using grayscale for markers
    ciopts(recast(rcap) lcolor(navy) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(navy) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
        xline(3, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(small)) ///
        title("B: Change in DEI Job Postings: Bottom 25th Percentile Org Liberalism") ///
    name(fig_a_5b, replace)
	
*event study newhires 
use "/mnt/overflow/gwr45/DEI/indivdual_position_sp500_f500_foranalysis.dta", replace

gen year = year(post_date_stata)
gen month = month(post_date_stata)

encode state, gen(state_num)
destring rcid, replace

*dynamic diff in diff

tab half_year, gen(half_year)

reghdfe diversity_flag half_year1-half_year4 o.half_year5  half_year6-half_year11 if top_25th_percentile ==1 & half_year<-9, a(rcid#state_num) cluster(rcid)
est store L

reghdfe diversity_flag half_year1-half_year4 o.half_year5  half_year6-half_year11 if bottom_25th_percentile ==1 & half_year<-9, a(rcid#state_num) cluster(rcid)
est store R

coefplot (L, mcolor(dknavy) lcolor(dknavy) msymbol(D) lpattern(solid) ciopts(recast(rcap) lpattern(solid) lcolor(dknavy))), ///
	omitted baselevels ///
    keep(half_year*) ///
    levels(95) ///
    vertical ///
    	ylabel(-0.001(0.001)0.003) ///
        coeflabels(half_year1 = "-10 Years" ///
				half_year2 = "-9.5 Years" ///
               half_year3 = "-9 Years" ///
			   half_year4 = "-8.5 Years" ///
               half_year5= "-8 Years" ///
               half_year6 = "-7.5 Years" ///
			   half_year7 = "-7 Years" ///
               half_year8 = "-6.5 Years" ///
               half_year9 = "-6 Years" ///
			   half_year10 = "-5.5 Years" ///
               half_year11= "-5 Years" ///
               half_year12 = "-4.5 Years" ///
               half_year13 = "-4 Year" ///
               half_year14 = "-3.5 Years" ///
               half_year15 = "-3 Years" ///
			   half_year16 = "-2.5 Years" ///
			   half_year17 = "-2 Years" ///
               half_year18 = "-1.5 Years" ///
               half_year19 = "-1 Year" ///
			   half_year20 = "-.5 Year" ///
               half_year21= "GF" ///
               half_year22 = "+1 Year" ///
               half_year23 = "+1.5 Year" ///
               half_year24 = "+2 Years" ///
               half_year25 = "+2.5 Years" ///
			   half_year26 = "+3 Years", labsize(tiny)) ///
    msymbol(O) msize(small) mcolor(navy) lcolor(navy) /// 
    ciopts(recast(rcap) lcolor(navy) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(navy) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
        xline(5, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(small)) ///
        title("C: New Hires: Top 25th Percentile Org Liberalism") ///
    name(fig_a_6a, replace)
    
    coefplot (R, mcolor(cranberry) lcolor(cranberry) msymbol(D) lpattern(solid) ciopts(recast(rcap) lpattern(solid) lcolor(cranberry))), ///
	omitted baselevels ///
		ylabel(-0.001(0.001)0.003) ///
    keep(half_year*) ///
    levels(95) ///
    vertical ///
        coeflabels(half_year1 = "-10 Years" ///
				half_year2 = "-9.5 Years" ///
               half_year3 = "-9 Years" ///
			   half_year4 = "-8.5 Years" ///
               half_year5= "-8 Years" ///
               half_year6 = "-7.5 Years" ///
			   half_year7 = "-7 Years" ///
               half_year8 = "-6.5 Years" ///
               half_year9 = "-6 Years" ///
			   half_year10 = "-5.5 Years" ///
               half_year11= "-5 Years" ///
               half_year12 = "-4.5 Years" ///
               half_year13 = "-4 Year" ///
               half_year14 = "-3.5 Years" ///
               half_year15 = "-3 Years" ///
			   half_year16 = "-2.5 Years" ///
			   half_year17 = "-2 Years" ///
               half_year18 = "-1.5 Years" ///
               half_year19 = "-1 Year" ///
			   half_year20 = "-.5 Year" ///
               half_year21= "GF" ///
               half_year22 = "+1 Year" ///
               half_year23 = "+1.5 Year" ///
               half_year24 = "+2 Years" ///
               half_year25 = "+2.5 Years" ///
			   half_year26 = "+3 Years", labsize(tiny)) ///
    msymbol(O) msize(small) mcolor(navy) lcolor(navy) /// 
    ciopts(recast(rcap) lcolor(navy) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(navy) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
        xline(5, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(small)) ///
        title("D: New Hires: Bottom 25th Percentile Org Liberalism") ///
    name(fig_a_6b, replace)

reghdfe diversity_flag half_year1-half_year6 o.half_year7  half_year8-half_year13 if top_25th_percentile ==1 & half_year<-7, a(rcid#state_num) cluster(rcid)
est store L

reghdfe diversity_flag half_year1-half_year6 o.half_year7  half_year8-half_year13 if bottom_25th_percentile ==1 & half_year<-7, a(rcid#state_num) cluster(rcid)
est store R

coefplot (L, mcolor(dknavy) lcolor(dknavy) msymbol(D) lpattern(solid) ciopts(recast(rcap) lpattern(solid) lcolor(dknavy))), ///
	omitted baselevels ///
    keep(half_year*) ///
    levels(95) ///
    vertical ///
    	ylabel(-0.001(0.001)0.003) ///
        coeflabels(half_year1 = "-10 Years" ///
				half_year2 = "-9.5 Years" ///
               half_year3 = "-9 Years" ///
			   half_year4 = "-8.5 Years" ///
               half_year5= "-8 Years" ///
               half_year6 = "-7.5 Years" ///
			   half_year7 = "-7 Years" ///
               half_year8 = "-6.5 Years" ///
               half_year9 = "-6 Years" ///
			   half_year10 = "-5.5 Years" ///
               half_year11= "-5 Years" ///
               half_year12 = "-4.5 Years" ///
               half_year13 = "-4 Year" ///
               half_year14 = "-3.5 Years" ///
               half_year15 = "-3 Years" ///
			   half_year16 = "-2.5 Years" ///
			   half_year17 = "-2 Years" ///
               half_year18 = "-1.5 Years" ///
               half_year19 = "-1 Year" ///
			   half_year20 = "-.5 Year" ///
               half_year21= "GF" ///
               half_year22 = "+1 Year" ///
               half_year23 = "+1.5 Year" ///
               half_year24 = "+2 Years" ///
               half_year25 = "+2.5 Years" ///
			   half_year26 = "+3 Years", labsize(tiny)) ///
    msymbol(O) msize(small) mcolor(navy) lcolor(navy) /// 
    ciopts(recast(rcap) lcolor(navy) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(navy) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
        xline(7, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(small)) ///
        title("C: New Hires: Top 25th Percentile Org Liberalism") ///
    name(fig_a_7a, replace)
    
    coefplot (R, mcolor(cranberry) lcolor(cranberry) msymbol(D) lpattern(solid) ciopts(recast(rcap) lpattern(solid) lcolor(cranberry))), ///
	omitted baselevels ///
		ylabel(-0.001(0.001)0.003) ///
    keep(half_year*) ///
    levels(95) ///
    vertical ///
        coeflabels(half_year1 = "-10 Years" ///
				half_year2 = "-9.5 Years" ///
               half_year3 = "-9 Years" ///
			   half_year4 = "-8.5 Years" ///
               half_year5= "-8 Years" ///
               half_year6 = "-7.5 Years" ///
			   half_year7 = "-7 Years" ///
               half_year8 = "-6.5 Years" ///
               half_year9 = "-6 Years" ///
			   half_year10 = "-5.5 Years" ///
               half_year11= "-5 Years" ///
               half_year12 = "-4.5 Years" ///
               half_year13 = "-4 Year" ///
               half_year14 = "-3.5 Years" ///
               half_year15 = "-3 Years" ///
			   half_year16 = "-2.5 Years" ///
			   half_year17 = "-2 Years" ///
               half_year18 = "-1.5 Years" ///
               half_year19 = "-1 Year" ///
			   half_year20 = "-.5 Year" ///
               half_year21= "GF" ///
               half_year22 = "+1 Year" ///
               half_year23 = "+1.5 Year" ///
               half_year24 = "+2 Years" ///
               half_year25 = "+2.5 Years" ///
			   half_year26 = "+3 Years", labsize(tiny)) ///
    msymbol(O) msize(small) mcolor(navy) lcolor(navy) /// 
    ciopts(recast(rcap) lcolor(navy) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(navy) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
        xline(7, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(small)) ///
        title("D: New Hires: Bottom 25th Percentile Org Liberalism") ///
    name(fig_a_7b, replace)
    
reghdfe diversity_flag half_year1-half_year8 o.half_year9  half_year10-half_year15 if top_25th_percentile ==1 & half_year<-5, a(rcid#state_num) cluster(rcid)
est store L

reghdfe diversity_flag half_year1-half_year8 o.half_year9  half_year10-half_year15 if bottom_25th_percentile ==1 & half_year<-5, a(rcid#state_num) cluster(rcid)
est store R

coefplot (L, mcolor(dknavy) lcolor(dknavy) msymbol(D) lpattern(solid) ciopts(recast(rcap) lpattern(solid) lcolor(dknavy))), ///
	omitted baselevels ///
    keep(half_year*) ///
    levels(95) ///
    vertical ///
    	ylabel(-0.001(0.001)0.003) ///
        coeflabels(half_year1 = "-10 Years" ///
				half_year2 = "-9.5 Years" ///
               half_year3 = "-9 Years" ///
			   half_year4 = "-8.5 Years" ///
               half_year5= "-8 Years" ///
               half_year6 = "-7.5 Years" ///
			   half_year7 = "-7 Years" ///
               half_year8 = "-6.5 Years" ///
               half_year9 = "-6 Years" ///
			   half_year10 = "-5.5 Years" ///
               half_year11= "-5 Years" ///
               half_year12 = "-4.5 Years" ///
               half_year13 = "-4 Year" ///
               half_year14 = "-3.5 Years" ///
               half_year15 = "-3 Years" ///
			   half_year16 = "-2.5 Years" ///
			   half_year17 = "-2 Years" ///
               half_year18 = "-1.5 Years" ///
               half_year19 = "-1 Year" ///
			   half_year20 = "-.5 Year" ///
               half_year21= "GF" ///
               half_year22 = "+1 Year" ///
               half_year23 = "+1.5 Year" ///
               half_year24 = "+2 Years" ///
               half_year25 = "+2.5 Years" ///
			   half_year26 = "+3 Years", labsize(tiny)) ///
    msymbol(O) msize(small) mcolor(navy) lcolor(navy) /// 
    ciopts(recast(rcap) lcolor(navy) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(navy) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
        xline(9, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(small)) ///
        title("C: New Hires: Top 25th Percentile Org Liberalism") ///
    name(fig_a_8a, replace)
    
    coefplot (R, mcolor(cranberry) lcolor(cranberry) msymbol(D) lpattern(solid) ciopts(recast(rcap) lpattern(solid) lcolor(cranberry))), ///
	omitted baselevels ///
		ylabel(-0.001(0.001)0.003) ///
    keep(half_year*) ///
    levels(95) ///
    vertical ///
        coeflabels(half_year1 = "-10 Years" ///
				half_year2 = "-9.5 Years" ///
               half_year3 = "-9 Years" ///
			   half_year4 = "-8.5 Years" ///
               half_year5= "-8 Years" ///
               half_year6 = "-7.5 Years" ///
			   half_year7 = "-7 Years" ///
               half_year8 = "-6.5 Years" ///
               half_year9 = "-6 Years" ///
			   half_year10 = "-5.5 Years" ///
               half_year11= "-5 Years" ///
               half_year12 = "-4.5 Years" ///
               half_year13 = "-4 Year" ///
               half_year14 = "-3.5 Years" ///
               half_year15 = "-3 Years" ///
			   half_year16 = "-2.5 Years" ///
			   half_year17 = "-2 Years" ///
               half_year18 = "-1.5 Years" ///
               half_year19 = "-1 Year" ///
			   half_year20 = "-.5 Year" ///
               half_year21= "GF" ///
               half_year22 = "+1 Year" ///
               half_year23 = "+1.5 Year" ///
               half_year24 = "+2 Years" ///
               half_year25 = "+2.5 Years" ///
			   half_year26 = "+3 Years", labsize(tiny)) ///
    msymbol(O) msize(small) mcolor(navy) lcolor(navy) /// 
    ciopts(recast(rcap) lcolor(navy) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(navy) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
        xline(9, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(small)) ///
        title("D: New Hires: Bottom 25th Percentile Org Liberalism") ///
    name(fig_a_8b, replace)

reghdfe diversity_flag half_year1-half_year10 o.half_year11  half_year12-half_year17 if top_25th_percentile ==1 & half_year<-3, a(rcid#state_num) cluster(rcid)
est store L


reghdfe diversity_flag half_year1-half_year10 o.half_year11  half_year12-half_year17 if bottom_25th_percentile ==1 & half_year<-3, a(rcid#state_num) cluster(rcid)
est store R

set scheme tab3

coefplot (L, mcolor(dknavy) lcolor(dknavy) msymbol(D) lpattern(solid) ciopts(recast(rcap) lpattern(solid) lcolor(dknavy))), ///
	omitted baselevels ///
    keep(half_year*) ///
    levels(95) ///
    vertical ///
    	ylabel(-0.001(0.001)0.003) ///
        coeflabels(half_year1 = "-10 Years" ///
				half_year2 = "-9.5 Years" ///
               half_year3 = "-9 Years" ///
			   half_year4 = "-8.5 Years" ///
               half_year5= "-8 Years" ///
               half_year6 = "-7.5 Years" ///
			   half_year7 = "-7 Years" ///
               half_year8 = "-6.5 Years" ///
               half_year9 = "-6 Years" ///
			   half_year10 = "-5.5 Years" ///
               half_year11= "-5 Years" ///
               half_year12 = "-4.5 Years" ///
               half_year13 = "-4 Year" ///
               half_year14 = "-3.5 Years" ///
               half_year15 = "-3 Years" ///
			   half_year16 = "-2.5 Years" ///
			   half_year17 = "-2 Years" ///
               half_year18 = "-1.5 Years" ///
               half_year19 = "-1 Year" ///
			   half_year20 = "-.5 Year" ///
               half_year21= "GF" ///
               half_year22 = "+1 Year" ///
               half_year23 = "+1.5 Year" ///
               half_year24 = "+2 Years" ///
               half_year25 = "+2.5 Years" ///
			   half_year26 = "+3 Years", labsize(tiny)) ///
    msymbol(O) msize(small) mcolor(navy) lcolor(navy) /// 
    ciopts(recast(rcap) lcolor(navy) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(navy) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
        xline(11, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(small)) ///
        title("C: New Hires: Top 25th Percentile Org Liberalism") ///
    name(fig_a_9a, replace)
    
    coefplot (R, mcolor(cranberry) lcolor(cranberry) msymbol(D) lpattern(solid) ciopts(recast(rcap) lpattern(solid) lcolor(cranberry))), ///
	omitted baselevels ///
		ylabel(-0.001(0.001)0.003) ///
    keep(half_year*) ///
    levels(95) ///
    vertical ///
        coeflabels(half_year1 = "-10 Years" ///
				half_year2 = "-9.5 Years" ///
               half_year3 = "-9 Years" ///
			   half_year4 = "-8.5 Years" ///
               half_year5= "-8 Years" ///
               half_year6 = "-7.5 Years" ///
			   half_year7 = "-7 Years" ///
               half_year8 = "-6.5 Years" ///
               half_year9 = "-6 Years" ///
			   half_year10 = "-5.5 Years" ///
               half_year11= "-5 Years" ///
               half_year12 = "-4.5 Years" ///
               half_year13 = "-4 Year" ///
               half_year14 = "-3.5 Years" ///
               half_year15 = "-3 Years" ///
			   half_year16 = "-2.5 Years" ///
			   half_year17 = "-2 Years" ///
               half_year18 = "-1.5 Years" ///
               half_year19 = "-1 Year" ///
			   half_year20 = "-.5 Year" ///
               half_year21= "GF" ///
               half_year22 = "+1 Year" ///
               half_year23 = "+1.5 Year" ///
               half_year24 = "+2 Years" ///
               half_year25 = "+2.5 Years" ///
			   half_year26 = "+3 Years", labsize(tiny)) ///
    msymbol(O) msize(small) mcolor(navy) lcolor(navy) /// 
    ciopts(recast(rcap) lcolor(navy) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(navy) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
        xline(11, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(small)) ///
        title("D: New Hires: Bottom 25th Percentile Org Liberalism") ///
    name(fig_a_9b, replace)
    
reghdfe diversity_flag half_year1-half_year19 o.half_year20  half_year21-half_year26 if top_25th_percentile ==1, a(rcid#state_num) cluster(rcid)
est store L


reghdfe diversity_flag half_year1-half_year19 o.half_year20  half_year21-half_year26 if bottom_25th_percentile ==1, a(rcid#state_num) cluster(rcid)
est store R

set scheme tab3

coefplot (L, mcolor(dknavy) lcolor(dknavy) msymbol(D) lpattern(solid) ciopts(recast(rcap) lpattern(solid) lcolor(dknavy))), ///
	omitted baselevels ///
    keep(half_year*) ///
    levels(95) ///
    vertical ///
    	ylabel(-0.001(0.001)0.003) ///
        coeflabels(half_year1 = "-10 Years" ///
				half_year2 = "-9.5 Years" ///
               half_year3 = "-9 Years" ///
			   half_year4 = "-8.5 Years" ///
               half_year5= "-8 Years" ///
               half_year6 = "-7.5 Years" ///
			   half_year7 = "-7 Years" ///
               half_year8 = "-6.5 Years" ///
               half_year9 = "-6 Years" ///
			   half_year10 = "-5.5 Years" ///
               half_year11= "-5 Years" ///
               half_year12 = "-4.5 Years" ///
               half_year13 = "-4 Year" ///
               half_year14 = "-3.5 Years" ///
               half_year15 = "-3 Years" ///
			   half_year16 = "-2.5 Years" ///
			   half_year17 = "-2 Years" ///
               half_year18 = "-1.5 Years" ///
               half_year19 = "-1 Year" ///
			   half_year20 = "-.5 Year" ///
               half_year21= "GF" ///
               half_year22 = "+1 Year" ///
               half_year23 = "+1.5 Year" ///
               half_year24 = "+2 Years" ///
               half_year25 = "+2.5 Years" ///
			   half_year26 = "+3 Years", labsize(tiny)) ///
    msymbol(O) msize(small) mcolor(navy) lcolor(navy) /// 
    ciopts(recast(rcap) lcolor(navy) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(navy) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
        xline(20, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(small)) ///
        title("C: New Hires: Top 25th Percentile Org Liberalism") ///
    name(fig_a_10a, replace)
    
    coefplot (R, mcolor(cranberry) lcolor(cranberry) msymbol(D) lpattern(solid) ciopts(recast(rcap) lpattern(solid) lcolor(cranberry))), ///
	omitted baselevels ///
		ylabel(-0.001(0.001)0.003) ///
    keep(half_year*) ///
    levels(95) ///
    vertical ///
        coeflabels(half_year1 = "-10 Years" ///
				half_year2 = "-9.5 Years" ///
               half_year3 = "-9 Years" ///
			   half_year4 = "-8.5 Years" ///
               half_year5= "-8 Years" ///
               half_year6 = "-7.5 Years" ///
			   half_year7 = "-7 Years" ///
               half_year8 = "-6.5 Years" ///
               half_year9 = "-6 Years" ///
			   half_year10 = "-5.5 Years" ///
               half_year11= "-5 Years" ///
               half_year12 = "-4.5 Years" ///
               half_year13 = "-4 Year" ///
               half_year14 = "-3.5 Years" ///
               half_year15 = "-3 Years" ///
			   half_year16 = "-2.5 Years" ///
			   half_year17 = "-2 Years" ///
               half_year18 = "-1.5 Years" ///
               half_year19 = "-1 Year" ///
			   half_year20 = "-.5 Year" ///
               half_year21= "GF" ///
               half_year22 = "+1 Year" ///
               half_year23 = "+1.5 Year" ///
               half_year24 = "+2 Years" ///
               half_year25 = "+2.5 Years" ///
			   half_year26 = "+3 Years", labsize(tiny)) ///
    msymbol(O) msize(small) mcolor(navy) lcolor(navy) /// 
    ciopts(recast(rcap) lcolor(navy) lwidth(thin)) /// Darker shade for caps
    connect(l) lcolor(navy) lwidth(thin) /// <--- Connect lines option added
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
    yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
        xline(20, lpattern(dash) lcolor(red) lwidth(thick)) ///
    ytitle("Difference", margin(small)) ///
        title("D: New Hires: Bottom 25th Percentile Org Liberalism") ///
    name(fig_a_10b, replace)

*TABLE A1 THROUGH A3
// Load Job Postings Data
use "data/job_postings_analysis.dta", clear

// Define a list of organizational liberalism indicators
local org_liberal_list top_20th_percentile top_30th_percentile top_40th_percentile

// Loop over each organizational liberalism indicator
foreach org_liberal in `org_liberal_list' {

    // Create an interaction term
    gen `org_liberal'_post_interaction = post * `org_liberal'

    // Run regressions with different absorption and clustering strategies
    eststo: reghdfe diversity_flag post `org_liberal' `org_liberal'_post_interaction, absorb(year#month) cluster(rcid#state_num)
    eststo: reghdfe diversity_flag post `org_liberal' `org_liberal'_post_interaction, absorb(rcid#state_num) cluster(rcid#state_num)
    eststo: reghdfe diversity_flag post `org_liberal' `org_liberal'_post_interaction, absorb(year#month rcid#state_num) cluster(rcid#state_num)

    // Drop the interaction term to avoid confusion in the next loop iteration
    drop `org_liberal'_post_interaction
}

*TABLE A4 THROUGH A6
use "data/newhire_analysis.dta", clear

// Define a list of organizational liberalism indicators
local org_liberal_list top_20th_percentile top_30th_percentile top_40th_percentile

// Loop over each organizational liberalism indicator
foreach org_liberal in `org_liberal_list' {

    // Create an interaction term
    gen `org_liberal'_post_interaction = post * `org_liberal'

    // Run regressions with different absorption and clustering strategies
    eststo: reghdfe diversity_flag post `org_liberal' `org_liberal'_post_interaction, absorb(year#month) cluster(rcid#state_num)
    eststo: reghdfe diversity_flag post `org_liberal' `org_liberal'_post_interaction, absorb(rcid#state_num) cluster(rcid#state_num)
    eststo: reghdfe diversity_flag post `org_liberal' `org_liberal'_post_interaction, absorb(year#month rcid#state_num) cluster(rcid#state_num)

    // Drop the interaction term to avoid confusion in the next loop iteration
    drop `org_liberal'_post_interaction
}

