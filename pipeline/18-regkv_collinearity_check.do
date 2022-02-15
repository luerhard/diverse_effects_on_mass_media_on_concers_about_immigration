clear all


program define _estadd_vif, eclass
  syntax [ , prefix(name) * ]
  tempname results
  matrix `results' = e(b)
  qui vif
  local i 0
  local name "`r(name_`++i')'"
  while "`name'"!="" {
    local j = colnumb(`results',"`name'")
    if `j'<. {
      matrix `results'[1,`j'] = r(vif_`i') 
    }
    local name "`r(name_`++i')'"
  }
  ereturn matrix `prefix'vif = `results'
end

program define regkv
version 10
syntax [varlist] [if] [in] [fweight  aweight  pweight  iweight] [,cluster *]
	
* messungen, die nicht in die analyse eingehen vor erstellung der kontextvariablen ausschlie�en
qui generate usempf=1
qui foreach i in `varlist' {
replace usempf=0 if `i'==.
}

tokenize `varlist'
local first `1'
macro shift
local rest `*'
	_xt, i(`i')
local ivar "`r(ivar)'" 
qui foreach i in `rest' {
egen kv_`i' = mean (`i') if usempf==1, by (`ivar')
}
_rmcoll `rest' kv_* 
local xvars `r(varlist)'
reg `first' `xvars' `if' `in' [`weight' `exp'], `options'
estat vif
drop kv_*  usempf
estout 
end



use "/home/lukas/git/migration_news/data/datasets_final/final_regression_df.dta"

log using "/home/lukas/Desktop/regkv_multicolinearity_check.log", replace

xtset pid syear

tab party, gen(party_dummy)
drop party_dummy1

tab political_Interest, gen(pol_interest_dummy)
drop pol_interest_dummy1

tab income_Satisfact, gen(satis_dummy)
drop satis_dummy1

tab pmonin, gen(month_dummy)
drop month_dummy1

regkv AV_bin party_dummy* pol_interest_dummy* worried_About_Econ n_Articles month_dummy*  satis_dummy* V1-V68

* regkv AV_bin V1-V68

log close
