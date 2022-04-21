


* Make directory 
cd "/Users/josemiguelquinteroholguin/Dropbox/UChicago/Empirical Analysis III/EA3-Midterm-JMQ"
* Open data set
use "Data/forcedcoexistence_webfinal.dta", clear


* Export table 
tempname corrM
forvalues x = 0/1{
	tempname rowT
	forvalues y=0/1{
			* Capture treated
			qui sum logpcinc if (HC==`x' & FC==`y' & year==2000)
			local m1 = r(mean)
			local s1 = r(sd)
			local n1 = r(N) 
			* Merge matrix 
			mat `rowT' = (nullmat(`rowT'),(`n1', . ,`m1',`s1'))
	}
	qui sum logpcinc if (HC==`x' & year==2000)
	mat `rowT' = (nullmat(`rowT'),(r(N),.))
	mat `corrM' = (nullmat(`corrM')\(`rowT'))
}

* Export matrix as table
frmttable using "Tables/tab1_hc_fc", tex replace s(`corrM') sub(1) sd(3) ///
	ctitles("", "Forced Coexistence = 0", "", "Forced Coexistence = 1" ,"", "" \ "", "Number", "log(p.c income)", "Number", "log(p.c income)", "Total") ///
    multicol(1,2,2;1,4,2;7,2,2;7,4,2) rtitles("Historical Centralization = 0" \ "" \ "Historical Centralization = 1") ///
	hlines(101{0}1) fragment addrow("Total","69","", "113","","182")	

* Table 2: Balancing test 

* Create constant to fix two way clustering 
cap gen temp 	=1

* Create control groups
global reserveControls logpcinc_co logunempl_co logdist logruggedness logresarea_sqkm
global tribeControls HC ea_v5 ea_v30 ea_v32 ea_v66
global endResControls logpop popadultshare casino

tempname balancing

* Panel A: Reservation Characteristics
foreach x of var $reserveControls{
	tempname rowT
	* Run regression only on Forced Cohexistence
	qui reghdfe `x' FC if year==2000, absorb(temp) cluster(eaid statenumber)
	* Calculate t-stat
	local t = abs(_b[FC]/_se[FC])
	* Add to row
	mat `rowT' = (nullmat(`rowT'),(_b[FC],`t',`e(r2)'))
	* Run regression on Forced Cohexistence and Historical Concentration
	qui reghdfe `x' FC HC if year==2000, absorb(temp) cluster(eaid statenumber)
	* Calculate t-stat
	local t = abs(_b[FC]/_se[FC])
	* Add to row
	mat `rowT' = (nullmat(`rowT'),(_b[FC],`t',`e(r2)'))
	* Add to big matt 
	mat `balancing' = (nullmat(`balancing')\(`rowT'))
}


* Panel B: Tribe Characteristics
foreach x of var $tribeControls{
	tempname rowT
	* Run regression only on Forced Cohexistence
	qui reghdfe `x' FC if year==2000, absorb(temp) cluster(eaid statenumber)
	* Calculate t-stat
	local t = abs(_b[FC]/_se[FC])
	* Add to row
	mat `rowT' = (nullmat(`rowT'),(_b[FC],`t',`e(r2)'))
	* Run regression on Forced Cohexistence and Historical Concentration
	qui reghdfe `x' FC HC if year==2000, absorb(temp) cluster(eaid statenumber)
	* Get coef for HC
	local lin = _b[HC]
	if(`e(r2)'!=1){
	* Calculate t-stat
	local t = abs(_b[FC]/_se[FC])
		* Add to row
		mat `rowT' = (nullmat(`rowT'),(_b[FC],`t',`e(r2)'))
	
	}
	else{
		* Add to row
		mat `rowT' = (nullmat(`rowT'),(.,.,.))
	}
	* Add to big matt 
	mat `balancing' = (nullmat(`balancing')\(`rowT'))
}


* Panel A: Reservation Characteristics
foreach x of var $endResControls{
	tempname rowT
	* Run regression only on Forced Cohexistence
	qui reghdfe `x' FC if year==2000, absorb(temp) cluster(eaid statenumber)
	* Calculate t-stat
	local t = abs(_b[FC]/_se[FC])
	* Add to row
	mat `rowT' = (nullmat(`rowT'),(_b[FC],`t',`e(r2)'))
	* Run regression on Forced Cohexistence and Historical Concentration
	qui reghdfe `x' FC HC if year==2000, absorb(temp) cluster(eaid statenumber)
	* Calculate t-stat
	local t = abs(_b[FC]/_se[FC])
	* Add to row
	mat `rowT' = (nullmat(`rowT'),(_b[FC],`t',`e(r2)'))
	* Add to big matt 
	mat `balancing' = (nullmat(`balancing')\(`rowT'))
}


* Export matrix as table
frmttable using "Tables/tab2_balancing", tex replace s(`balancing') sd(3) ///
	ctitles("", "Regressors", "", "", "", "", "" \ "", "", "", "Forced Coexistence, Conditional", "", "", "" \ "", "Forced Coexistence Only", "", "", "on Historical Centralization", "", "" \ "Dependent (Below)", "Coeff.", "$ t$-Stat.", "$ R^2$", "Coeff.", "$ t$-Stat.", "$ R^2$") ///
    multicol(1,2,5;2,2,3;2,4,3;3,2,3;3,4,3;) hlines(101{0}1) fragment  ///
	rtitles("Surround. p.c. income" \ "Surround. p.c. unempl.-rate" \ "Distance to major city"\"log(Ruggedness Reserv )"\"log(Re-Area in sqkm)"\"Historical centralization"\"Percent calories from agriculture"\"Sedentariness"\"Complexity of local community"\"$ D$(Wealth distinctions)"\"log(Population)"\"Pop-Share Adult (0–100)"\"$ D$(Casino)") 



* Run regression OLS
tempname panelA
tempname panelB
* Run regression 
local controls
foreach x in reserveControls tribeControls endResControls extra {
	* Run regression
	qui ivreg2 logpcinc FC HC `controls' if year==2000, cluster(eaid statenumber) sm
	* Extract t and pvalue
	local fc_t = abs(_b[FC]/_se[FC])
	local fc_p = 2*ttail(e(df_r),`fc_t')
	local hc_t = abs(_b[HC]/_se[HC])
	local hc_p = 2*ttail(e(df_r),`hc_t')
	* Add to row
	mat `panelA' = (nullmat(`panelA'),(_b[FC],`fc_t' \ _b[HC],`hc_t' \ `e(r2)',.))
	* Run regression
	 qui ivreg2 logpcinc FC `controls' i.eaid if year==2000, cluster(eaid statenumber) sm
	* Extract t and pvalue
	local fc_t = abs(_b[FC]/_se[FC])
	local fc_p = 2*ttail(e(df_r),`fc_t')
	* Add to row
	mat `panelB' = (nullmat(`panelB'),(_b[FC],`fc_t' \ `e(r2)', . ))
	* Add next control 
	local controls `controls' ${`x'}

}

* Regression with state fix effects
qui ivreg2 logpcinc FC HC `controls' i.statenumber if year==2000, cluster(eaid statenumber) sm
* Extract t and pvalue
local fc_t = abs(_b[FC]/_se[FC])
local fc_p = 2*ttail(e(df_r),`fc_t')
local hc_t = abs(_b[HC]/_se[HC])
local hc_p = 2*ttail(e(df_r),`hc_t')
* Add to row
mat `panelA' = (nullmat(`panelA'),(_b[FC],`fc_t'\  _b[HC],`hc_t' \ `e(r2)',.))
qui ivreg2 logpcinc FC `controls' i.statenumber i.eaid if year==2000, cluster(eaid statenumber) sm
* Extract t and pvalue
local fc_t = abs(_b[FC]/_se[FC])
local fc_p = 2*ttail(e(df_r),`fc_t')
* Add to row
mat `panelB' = (nullmat(`panelB'),(_b[FC],`fc_t'\ `e(r2)',.))
* Create full matrix
mat tab3 = (`panelA' \ `panelB')
* Create matrix for stars
matrix annotmat = J(5,10,0)
* Loop
forvalues i = 1(2)9{
	 matrix annotmat[1,`i'] = 1
	 matrix annotmat[2,`i'] = 1
	 matrix annotmat[4,`i'] = 1
}

* Export matrix as table
frmttable using "Tables/tab3_OLS", tex replace s(tab3) sd(3) sub(1) hlines(101{0}1) ///
	ctitles("", "log(per capita income)", "", "", "", "" \ "Dependent", "(1)", "(2)", "(3)", "(4)", "(5)" \ "\textit{Panel A: OLS}", "", "", "", "", "") ///
    multicol(1,2,5;3,1,6;9,1,6)  fragment rtitles("Forced coexistence" \ "" \ "Historical centralization" \ "" \ "$ R^2$" \ "\textit{Panel B: Tribe Fixed Effects}" \ "Forced coexistence" \""\ "$ R^2$") ///
	addrow("Reservation controls","","Y", "Y","Y","Y" \ "Tribe controls","","", "Y","Y","Y" \ "Additional reservation controls","","", "","Y","Y" \ "State fixed effects","","", "","","Y") ///
	annotate(annotmat) asymbol("$ ^{***}$")
	






















