clear all
set more off

* Make directory 
cd "/Users/josemiguelquinteroholguin/Dropbox/UChicago/Empirical Analysis III/EA3-Midterm-JMQ"
* Open data set
use "Data/forcedcoexistence_webfinal.dta", clear


* Create control groups
global reserveControls logpcinc_co logunempl_co logdist logruggedness logresarea_sqkm
global tribeControls  HC ea_v5 ea_v30 ea_v32 ea_v66
global endResControls logpop popadultshare casino
global ivControls removal wprec_enviro homelandruggedness

* Generate aggregate control 
cap gen wprec_enviro = wgold_enviro + wsilver_enviro
cap gen aggInst = instrument_gold + intrument_silver


* run probit for estimating the propensity score 

cap drop pscore ybar pscore_lin ybar_lin
probit FC aggInst HC $reserveControls $tribeControls if year==2000
predict pscore if e(sample)
replace pscore = round(pscore,0.1) if pscore!=.
* Calculate mean Y conditional on pscore
bys pscore: egen ybar = mean(logpcinc)
* Estimate propensity score linearly
ivreg2 FC aggInst HC $reserveControls $tribeControls if year==2000, cluster(eaid statenumber) sm
predict pscore_lin if e(sample)
replace pscore_lin = min(round(pscore_lin,0.1),1) if pscore_lin!=.
* Calculate mean Y conditional on pscore
bys pscore_lin: egen ybar_lin = mean(logpcinc)
* Plot the interest variable
twoway (scatter ybar pscore, mc(cranberry%70) mlc(maroon)) (scatter ybar_lin pscore_lin, mc(ebblue%70) mlc(navy)),graphregion(fcolor(white)) ///
	   legend(order(1 "Probit" 2 "Linear Prob.") cols(1) region(lcolor(white)) ring(0) pos(4)) ylabel(,nogrid format(%3.1f)) ///
	   xti("Propensity Score") xlabel(,format(%3.1f)) yti("E[Y| p(Z,X)=p]")
	   graph export "Figures/Linear_Test.pdf", replace	   
* Common support
twoway (histogram pscore if FC==0, fcolor(cranberry%70) lcolor(maroon) width(0.1) start(0)) /// 
	   (histogram pscore if FC==1, fcolor(ebblue%70) lcolor(navy) width(0.1) start(0)),  graphregion(fcolor(white)) ///
	   legend(order(1 "Untreated" 2 "Treated") cols(1) region(lcolor(white)) ring(0) pos(11)) ylabel(,nogrid format(%3.1f)) ///
	   xti("Propensity Score") xlabel(,format(%3.1f))
	   graph export "Figures/commonSupp.pdf", replace
	   
twoway (histogram pscore_lin if FC==0, fcolor(cranberry%70) lcolor(maroon) width(0.1) start(0)) /// 
	   (histogram pscore_lin if FC==1, fcolor(ebblue%70) lcolor(navy) width(0.1) start(0)),  graphregion(fcolor(white)) ///
	   legend(order(1 "Untreated" 2 "Treated") cols(1) region(lcolor(white)) ring(0) pos(11)) ylabel(,nogrid format(%3.1f)) ///
	   xti("Propensity Score") xlabel(,format(%3.1f))
	   graph export "Figures/commonSuppLin.pdf", replace
