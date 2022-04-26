
clear all
set more off

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


* Create control groups
global reserveControls logpcinc_co logunempl_co logdist logruggedness logresarea_sqkm
global tribeControls  HC ea_v5 ea_v30 ea_v32 ea_v66
global endResControls logpop popadultshare casino
global stateFE _Ist*
global ivControls removal wprec_enviro homelandruggedness


tempname balancing

* Panel A: Reservation Characteristics
foreach x of var $reserveControls{
	tempname rowT
	* Run regression only on Forced Cohexistence
	qui reghdfe `x' FC if year==2000, noa cluster(eaid statenumber)
	* Calculate t-stat
	local t = abs(_b[FC]/_se[FC])
	* Add to row
	mat `rowT' = (nullmat(`rowT'),(_b[FC],`t',`e(r2)'))
	* Run regression on Forced Cohexistence and Historical Concentration
	qui reghdfe `x' FC HC if year==2000, noa cluster(eaid statenumber)
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
	qui reghdfe `x' FC if year==2000, noa cluster(eaid statenumber)
	* Calculate t-stat
	local t = abs(_b[FC]/_se[FC])
	* Add to row
	mat `rowT' = (nullmat(`rowT'),(_b[FC],`t',`e(r2)'))
	* Run regression on Forced Cohexistence and Historical Concentration
	qui reghdfe `x' FC HC if year==2000, noa cluster(eaid statenumber)
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
	qui reghdfe `x' FC if year==2000, noa cluster(eaid statenumber)
	* Calculate t-stat
	local t = abs(_b[FC]/_se[FC])
	* Add to row
	mat `rowT' = (nullmat(`rowT'),(_b[FC],`t',`e(r2)'))
	* Run regression on Forced Cohexistence and Historical Concentration
	qui reghdfe `x' FC HC if year==2000, noa cluster(eaid statenumber)
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

	* Update globals for regressions 
global tribeControls ea_v5 ea_v30 ea_v32 ea_v66
global ivControls removal wprec_enviro homelandruggedness

xi   i.statenumber	i.eaid	
* Run regression OLS
tempname panelA
tempname panelB
* Run regression 
local controls
foreach x in reserveControls tribeControls endResControls stateFE extra {
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
	 qui ivreg2 logpcinc FC `controls' _Ie* if year==2000, cluster(eaid statenumber) sm
	* Extract t and pvalue
	local fc_t = abs(_b[FC]/_se[FC])
	local fc_p = 2*ttail(e(df_r),`fc_t')
	* Add to row
	mat `panelB' = (nullmat(`panelB'),(_b[FC],`fc_t' \ `e(r2)', . ))
	* Add next control 
	local controls `controls' ${`x'}

}

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
	
cap gen wprec_enviro = wgold_enviro + wsilver_enviro

xi   i.statenumber

* Run regression FirstStage and Reduced form
tempname panelA
tempname starsA
tempname panelB
tempname starsB
* Run regression 
local controls
foreach x in reserveControls tribeControls endResControls stateFE ivControls extra {
	tempname tempStar 
	* Run regression
	qui ivreg2 FC HC instrument_gold intrument_silver `controls' if year==2000, cluster(eaid statenumber) sm
	* Extract t and pvalue
	local gold_t = _b[instrument_gold]/_se[instrument_gold]
	local gold_p = 2*ttail(e(df_r),abs(`gold_t'))
	local silver_t = _b[intrument_silver]/_se[intrument_silver]
	local silver_p = 2*ttail(e(df_r),abs(`silver_t'))
	* Add row to panel 
	mat `panelA' = (nullmat(`panelA'),(_b[instrument_gold],`gold_t' \ _b[intrument_silver],`silver_t' \ `e(r2)',.))

	* Add stars to matrix from gold instrument 
	if(`gold_p'<=0.1 & `gold_p'>0.05) mat `tempStar' = (nullmat(`tempStar'),(1,0))
	else if(`gold_p'<=0.05 & `gold_p'>0.01) mat `tempStar' = (nullmat(`tempStar'),(2,0))
	else if(`gold_p'<=0.01) mat `tempStar' = (nullmat(`tempStar'),(3,0))
	else mat `tempStar' = (nullmat(`tempStar'),(0,0))
	* Add stars to matrix from silver instrument 
	if(`silver_p'<=0.1 & `silver_p'>0.05) mat `tempStar' = (nullmat(`tempStar') \ (1,0 \ 0,0))
	else if(`silver_p'<=0.05 & `silver_p'>0.01) mat `tempStar' = (nullmat(`tempStar') \ (2,0 \ 0,0))
	else if(`silver_p'<=0.01) mat `tempStar' = (nullmat(`tempStar') \ (3,0 \ 0,0))
	else mat `silver_p' = (nullmat(`tempStar') \ (0,0 \ 0,0))
	mat `starsA' = (nullmat(`starsA'),`tempStar')
	*---------------------------*
	* Reduced form 
	*---------------------------*
	tempname tempStar2
	* Run regression
	qui ivreg2 logpcinc HC instrument_gold intrument_silver `controls' if year==2000, cluster(eaid statenumber) sm
	* Extract t and pvalue
	local gold2_t = _b[instrument_gold]/_se[instrument_gold]
	local gold2_p = 2*ttail(e(df_r),abs(`gold2_t'))
	local silver2_t = _b[intrument_silver]/_se[intrument_silver]
	local silver2_p = 2*ttail(e(df_r),abs(`silver2_t'))
	* Add row to panel 
	mat `panelB' = (nullmat(`panelB'),(_b[instrument_gold],`gold2_t' \ _b[intrument_silver],`silver2_t' \ `e(r2)', . ))
	* Add stars to matrix from gold instrument 
	if(`gold2_p'<=0.1 & `gold2_p'>0.05) mat `tempStar2' = (nullmat(`tempStar2'),(1,0))
	else if(`gold2_p'<=0.05 & `gold2_p'>0.01) mat `tempStar2' = (nullmat(`tempStar2'),(2,0))
	else if(`gold2_p'<=0.01) mat `tempStar2' = (nullmat(`tempStar2'),(3,0))
	else mat `tempStar2' = (nullmat(`tempStar2'),(0,0))
	* Add stars to matrix from silver instrument 
	if(`silver2_p'<=0.1 & `silver2_p'>0.05) mat `tempStar2' = (nullmat(`tempStar2')\(1,0 \ 0,0))
	else if(`silver2_p'<=0.05 & `silver2_p'>0.01) mat `tempStar2' = (nullmat(`tempStar2')\(2,0 \ 0,0))
	else if(`silver2_p'<=0.01) mat `tempStar2' = (nullmat(`tempStar2')\(3,0 \ 0,0))
	else mat `tempStar2' = (nullmat(`tempStar2')\(0,0 \ 0,0))
	mat `starsB' = (nullmat(`starsB'),`tempStar2')
	* Add next control 
	local controls `controls' ${`x'}

}

	
* Create full matrix
mat tab3 	= (`panelA' \ `panelB')
mat stars3	= (`starsA' \ `starsB')

* Export matrix as table
frmttable using "Tables/tab4_rf", tex replace s(tab3) sd(3) sub(1) hlines(11{0}1) ///
	ctitles("", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)" \ "\textit{Panel A: First Stage, Dependent: Forced Coexistence}", "", "", "", "", "","") ///
    multicol(2,1,7;8,1,7)  fragment rtitles("Historical gold-mining" \ "" \ "Historical silver-mining" \ "" \ "$ R^2$" \ "\textit{Panel B: Reduced Form, Dependent: log(per capita income)}" \ "Historical gold-mining" \ "" \ "Historical silver-mining" \ "" \ "$ R^2$" \ "") ///
	addrow("Historical centralization","Y","Y","Y","Y","Y","Y"\ "Reservation controls","","Y", "Y","Y","Y","Y" \ "Tribe controls","","", "Y","Y","Y","Y" \ "Additional reservation controls","","", "","Y","Y","Y" \ "State fixed effects","","", "","","Y","Y" \ "Additional IV controls", "", "", "", "","","Y") ///
	annotate(stars3) asymbol("$ ^*$", "$ ^{**}$", "$ ^{***}$")

* Regress V 

cap gen instAgg = instrument_gold + intrument_silver


* Run regression FirstStage and Reduced form
tempname panelA
tempname starsA
tempname panelB
tempname starsB
* Run regression 
local controls HC
foreach x in reserveControls tribeControls endResControls stateFE ivControls extra {
	* Run regression
	qui ivreg2 logpcinc (FC = instrument_gold  intrument_silver) `controls' if year == 2000 , cl(eaid statenumber)  sm endog(FC) partial(`controls')
	* Extract t and pvalue
	local t = _b[FC]/_se[FC]
	local p = 2*ttail(e(df_r),abs(`t'))
	* Add row to panel 
	mat `panelA' = (nullmat(`panelA'),(_b[FC],`t' \ e(widstat),. \ e(jp), . \ e(estatp), . ))
	* Add stars to matrix from gold instrument 
	if(`p'<=0.1 & `p'>0.05) mat `starsA' = (nullmat(`starsA'),(1,0\0,0\0,0\0,0))
	else if(`p'<=0.05 & `p'>0.01)  mat `starsA' = (nullmat(`starsA'),(2,0\0,0\0,0\0,0))
	else if(`p'<=0.01)  mat `starsA' = (nullmat(`starsA'),(3,0\0,0\0,0\0,0))
	else  mat `starsA' = (nullmat(`starsA'),(0,0\0,0\0,0\0,0))
	*---------------------------*
	* Reduced form 
	*---------------------------*
	* Run regression
	qui ivreg2 logpcinc (FC = instAgg) `controls' if year == 2000 , cl(eaid statenumber)  sm endog(FC) partial(`controls')
	* Extract t and pvalue
	local t = _b[FC]/_se[FC]
	local p = 2*ttail(e(df_r),abs(`t'))
	* Add row to panel 
	mat `panelB' = (nullmat(`panelB'),(_b[FC],`t' \ e(widstat),.  \ e(estatp), . ))
	* Add stars to matrix from gold instrument 
	if(`p'<=0.1 & `p'>0.05) mat `starsB' = (nullmat(`starsB'),(1,0\0,0\0,0))
	else if(`p'<=0.05 & `p'>0.01)  mat `starsB' = (nullmat(`starsB'),(2,0\0,0\0,0))
	else if(`p'<=0.01)  mat `starsB' = (nullmat(`starsB'),(3,0\0,0\0,0))
	else  mat `starsB' = (nullmat(`starsB'),(0,0\0,0\0,0))
	* Add next control 
	local controls `controls' ${`x'}
}


* Create full matrix
mat tab4 	= (`panelA' \ `panelB')
mat stars4	= (`starsA' \ `starsB')

* Export matrix as table
frmttable using "Tables/tab5_iv", tex replace s(tab4) sd(3) sub(1) hlines(101{0}1) ///
	ctitles("", "log(per capita income)", "", "", "", "", "" \"Dependent", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)" \ "\textit{Panel A: Two Instruments}", "", "", "", "", "","") ///
    multicol(1,2,6;3,1,7;11,1,7)  fragment rtitles("Forced coexistence" \ "" \ "F-statistic (instruments)" \ "" \  "$ p$-val. (over-identification test)" \ "" \ "$ p$-val. (endogeneity test)" \ "\textit{Panel B: One Instrument}" \ "Forced coexistence" \ "" \ "F-statistic (instruments)" \ "" \ "$ p$-val. (endogeneity test)" ) ///
	addrow("Historical centralization","Y","Y","Y","Y","Y","Y"\ "Res-controls","","Y", "Y","Y","Y","Y" \ "Add. tribe-controls","","", "Y","Y","Y","Y" \ "Endog. res-controls","","", "","Y","Y","Y" \ "State fixed effects","","", "","","Y","Y" \ "Add. exclusion controls", "", "", "", "","","Y") ///
	annotate(stars4) asymbol("$ ^*$", "$ ^{**}$", "$ ^{***}$")

