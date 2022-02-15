set more off
cd "~/git/migration_news/"
use "data/datasets_final/final_regression_df.dta", clear

xtset fac_pid

* base
xtreg AV_bin i.party i.political_Interest i.income_Satisfact worried_About_Econ n_Articles ///
i.pmonin, fe

* topics
xtreg AV_bin i.party i.political_Interest i.income_Satisfact worried_About_Econ n_Articles ///
i.pmonin ///
V1-V68, fe

* interaction with education 
xtreg AV_bin i.party i.political_Interest i.income_Satisfact worried_About_Econ n_Articles ///
i.pmonin ///
V1-V68 ///
c.V54#i.educ_bin c.V45#i.educ_bin c.V61#i.educ_bin c.V34#i.educ_bin c.V24#i.educ_bin /// *increasing
c.V18#i.educ_bin c.V29#i.educ_bin c.V64#i.educ_bin c.V3#i.educ_bin c.V21#i.educ_bin /// *decreasing
, fe


*** MARGINS
margins, dydx(V54 V45 V61 V34 V24 V18 V29 V64 V3 V21) at(educ_bin = (0 1)) ///
	saving(predict_educ, replace)  
marginsplot

use predict_educ, clear
saveold predict_educ_R, version(12) replace

