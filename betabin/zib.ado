*! version 2.1.0  04jun2017
program define zib, eclass byable(onecall) prop(ml_score irr svyb svyj svyr)
	if _by() {
		local BY `"by `_byvars'`_byrc0':"'
	}
	`BY' _vce_parserun zib, mark(INFlate OFFset CLuster) : `0'
	if "`s(exit)'" != "" {
		version 10: ereturn local cmdline `"zib `0'"'
		exit
	}

	version 6.0, missing
	if replay() {
		if "`e(cmd)'" != "zib" { error 301 }
		if _by() { error 190 }
		Display `0'
		if e(vuong)<. {
			DispV
		}
		_prefix_footnote
		exit
	}
	if _caller() >= 11 {
		local vv : di "version " string(_caller()) ":"
	}
	`vv' `BY' Estimate `0'
	version 10: ereturn local cmdline `"zib `0'"'
end

program define Estimate, eclass byable(recall)
		/* 	   
	   Model 0 is the target
	   We do not support constraints
	   
	   Regression coefficients and/or loglikelihoods from:
	   0) Zero-inf binomial model: FULL                 B0_r, B0_0  with Off0_B and G0_r, G0_0 with Off0_G
       1) Zero-inf binomial model: CONSTANT-ONLY              B1_0  with Off0_B and G1_r, G1_0 with Off0_G
	   2) Binomial model: FULL                          B2_r, B2_0  with Off0_B
	   3) Binomial model: CONSTANT-ONLY                       B3_0  with Off0_B
	   
	   If Model 0 includes a constant 
	   (-nocons- option is not specified):
	       If either one of -nolr- or -from()- is not specifed: estimate Model 1 and get LL/pLL
		   
	       If there are no given starting values
		   (-from()- option is not specified):
               Model 1 provides candidate starting values for Model 0: {0, B1_0, G1_r, G1_0}
			   
		   If the user does not specify no comparison to constant only 
		   (-nolr- option is not specified):
	           LL from Model 1  used in LR test of {B_r} in Model 0 ~ chi2
			   
	   If either one of -vuong- is specified or -from()- is not specifed: 
	   
	       if Model 0 includes a constant
		   (-nocons- is not specified):
		       estimate Model 3 and get LL/pLL 
			   
			   If there are no given starting values
		       (-from()- option is not specified):
                   Model 3 provides candidate starting values for Model 0: {0, B3_0, 0, 0}
			   
		   estimate Model 2 and get LL/pLL
	   
	       If there are no given starting values
	       (-from()- option is not specified):
	           Model 2 could provide starting values for Model 0: {B2_r, B2_0, 0, 0}
			   
	       If the user does not specify no comparison of the full zero-inflated binomial 
		   model to a binomial model without zero-inflation, and 
			   the user has not specified a form of robust variance:
		       (-vuong- is specified and the -robust-, -constraints- options are not specified):
	           Can calculate a Vuong test of {G0_r, G0_0} 
			   
		If there are no given starting values
	    (-from()- option is not specified):
		    Choose starting value as that candidate with greatest LL/pLL
	*/
	version 6.0, missing
	local cmd "`0'"
	local awopt = cond(_caller()<7,"aw","")
	#delimit ;
	syntax varlist(fv) [if] [in] [fweight `awopt' pweight iweight/] 
		, INFlate(string) [ 
			/* general options */
				noCONstant Robust CLuster(varname) 
				SCore(string) EFORM Level(cilevel) N(string)
				Link(string) ILink(string) noLR
				
		   /* optimization options */
			   MLMETHOD(string) OFFset(varname numeric) 
			   EXPosure(varname numeric) noLOg noDISPLAY
			   FROM(string) CRITTYPE(passthru)
			   
		   /* additional optimization options */
			   *
		] ;
	#delimit cr
	
	local fvops1 = "`s(fvops)'" == "true" | _caller() >= 11
	if _by() { 
		_byoptnotallowed score() `"`score'"'
	}

	GetLink "`link'" "`ilink'"
	if `r(rc)' {
		noi di as err "Illegal link specification in link(`link') and/or ilink(`ilink')"
		exit 198
	}
	local link  "`r(link)'"
	local ilink "`r(ilink)'"
	

	marksample touse
	markout `touse' `cluster', strok
	/* also see 2nd markout below for bionomial denominator */
	/* also see 3rd markout below for inflate varlist */

	tokenize `varlist'
	local dep "`1'"
	
	quietly {
		local na : word count `n'
		if `na' == 0 {
			local n "1"
		}
		tempvar nvar
		gen `nvar' = `n'
		if floor(`nvar') != `nvar' | `nvar' < `dep' | `nvar' <= 0 {
			noi di as err "N() must be an positive integer expression that is >= the outcome"
			exit 199
		}
		local narg "`n'"
	}
	markout `touse' `nvar'
	_fv_check_depvar `dep'
	mac shift
	local ind "`*'"


	if "`score'" != "" { 
		local n : word count `score'
		if `n'==1 & substr("`score'",-1,1)=="*" { 
			local score = substr("`score'",1,length("`score'")-1)
			local score `score'1 `score'2 
			local n 2
		}
		if `n' != 2 {
			di in red /*
			*/ "score():  you must specify two new variables"
			exit 198
		}
	}

	if "`weight'" != "" {
		local wtexp `"[`weight'=`exp']"'
		local wtype "`weight'"
		if ( "`wtype'" != "fweight" ) & ("`vuong'" != "") {
			di in red "vuong may not be specified with `wtype's" 
			exit 499
		}
	}
	else    local exp 1

	_get_diopts diopts options, `options'
	local diopts `diopts' level(`level') `eform'
	mlopts mlopt, `options'
	local coll `s(collinear)'
	local cns `s(constraints)'
	local mlopt `mlopt' `crittype'

	if "`cluster'" != "" {
		local clopt "cluster(`cluster')"
	}
	_vce_parse, argopt(CLuster) opt(Robust oim opg) old: ///
			`wtexp', `vce' `clopt' `robust'
	local cluster `r(cluster)'
	local robust `r(robust)'

	if "`cluster'" != "" {
		local clopt "cluster(`cluster')"
	}

	if ( "`cluster'`robust'`cns'" != "" ) & ("`vuong'" != "") {
		di in smcl as err "{p 0 6 0 `pararg'}The Vuong " /*
		*/ "statistic cannot be computed with constraints(), " /*
		*/ "vce(cluster), or vce(robust){p_end}"
		exit 499
	}

	local nc  "`constan'"

	PrseOff `inflate'
	local inflate "`s(inflate)'"
	local off2 "`s(off2)'"
	local nc2  "`s(nc2)'"
	if "`off2'" != "" {
		local off2a "offset(`off2')"
		local off2s "`off2'"
	}
	local fvops2 = "`s(fvops)'" == "true"

	if "`exposur'" != "" {
		tempvar off
		gen double `off' = ln(`exposur') if `touse'
		local offo "offset(`off')"
		local offstr "ln(`exposur')"
	}

	if "`offset'" != "" {
		if "`offstr'" != "" {
			di in red "cannot specify both exposure() and offset()"
			exit 198
		}
		tempvar off
		gen double `off' = `offset' if `touse'
		local offo "offset(`off')"
		local moff "-`off'"
		local poff "+`off'"
		local eoff "*exp(`off')"
		if "`offstr'" == "" {
			local offstr "`offset'"
		}
	}

	markout `touse' `inflate' `off2' `off' 

	if `fvops1' | `fvops2' {
		local rmcoll "version 11: _rmcoll"
		local vv : di "version " string(max(11,_caller())) ":"
		local mm    e2
		local negh  ""
		local fvexp expand
	}
	else {
		local rmcoll _rmcoll
		local vv "version 8.1:"
		local mm d2
	}

	`rmcoll' `ind' if `touse' `wtexp' , `nc' `coll' `fvexp'
	local ind "`r(varlist)'"
	`rmcoll' `inflate' if `touse' `wtexp' , `nc2' `coll' `fvexp'
	local inflate "`r(varlist)'"

	// calculate correct df
	local df 0
	foreach var of local inflate {
		_ms_parse_parts `var'
		if !`r(omit)' {
			local ++df
		}
	}

	if "`nc2'" == "" { local df = `df' + 1} 
	if "`mlmetho'" == "" { local mlmetho "`mm'" }
	if "`score'" != "" {
		local svar1 : word 1 of `score'
		local svar2 : word 2 of `score'
		confirm new variable `score'
		tempvar sc1 sc2
		local scopt "score(`sc1' `sc2')"
	}

	qui count if `dep'<0 & `touse'
	if r(N) { 
		di in red "`dep'<0 in `r(N)' obs."
		exit 459
	}

	qui { 
		if "`wtype'" != "fweight" {
			count if `touse'
			local nobs = r(N)

			count if `touse' & `dep'==0
			local nzero = r(N)
		} 
		else {
			summ `dep' `wtexp' if `touse', meanonly
			local nobs = r(N)

			summ `dep' `wtexp' if `touse' & `dep'==0, meanonly
			local nzero = r(N)
		}
	}
	

	if `nzero'==`nobs' {
		di in red "`dep' never varies; always equal to zero"
		exit 459
	}
	if `nzero'==0 {
		di in bl _n "(note: " in ye "`dep'" /*
			*/ in bl " never equal to zero; use glm instead)"
	}

	if "`log'`display'" == "" { local lll "noisily" }
	else                      { local lll "quietly" }

	if "`log'" == "" { local log "noisily" }
	else             { local log "quietly" }

	qui {
		summ `nvar' `wtexp' if `touse'
		local bind = r(sum)
		summ `dep'  `wtexp' if `touse'
		local binn = r(N)
		
		global ZIB_n   "`nvar'"  // Identify "n" binomial denominator variable for likelihood function
		local  lfprog  "zib_llf"
		
		forvalues k=1/3 { local ll`k' = . }

		if ("`vuong'" != "") | ("`from'" == "")  {
			local init2 ""
			if "`nc'" == "" {                                   
				/***********/
				/* Model 3 */
				/***********/
				`lll' di in gr _n "Fitting constant-only binomial model:"  
				`vv' `lll' glm `dep' if `touse' `wtexp', `offo' fam(bin `nvar') ///
					link(`link') nrtolerance(1e-2) tolerance(1e-1) iter(5) nodisplay
				local ll3 = e(ll)

				if "`from'" == "" {
					tempname from3 b3
					mat `b3' = get(_b)
					mat `from3' = (`b3')
				}
				local init2 "from(`b3')"
			}
		
			/***********/
			/* Model 2 */
			/***********/
			`lll' di in gr _n "Fitting full binomial model:"   
			`vv' `lll' glm `dep' `ind' if `touse' `wtexp', `offo' `nc' fam(bin `nvar') ///
					link(`link') nrtolerance(1e-2) tolerance(1e-1) iter(5) nodisplay `init2'
			local ll2 = e(ll) 

			if "`vuong'"!="" {
				global SGLM_m = 1
				tempvar xb2 mu2 
				_predict double `xb2' if `touse', xb	// BINOMIAL: xb2=X*beta 
				glim_l0$ZIB_L 1 `xb2' `mu2'				// BINOMIAL: mu2=g(X*beta)
				capture drop `xb2'
			}
		
			if "`from'" == "" { 
				tempname from2
				mat `from2' = e(b)
			} 
		}


		if "`nc'" == "" {
			if "`from3'" != "" {
				local init1ao "init(`from3')"
			}
			else { 
				tempname from1a
				qui glm `dep' if `touse' `wtexp', fam(bin `nvar') `offo' link(`link') iter(5) 
				mat `from1a' = e(b)
				local init1aopt "init(`from1a')"
			}

			/***********/
			/* Model 1 */
			/***********/
			global SGLM_m = 1        // We handle binomial size directly
			`lll' di in gr _n "Fitting constant-only zero-inflated binomial model:"
			#delimit ;
			`vv'
			`lll' ml model `mlmetho' `lfprog'
				(`dep': `dep' = , `offo')
				(inflate: `inflate', `nc2' `off2a')
				if `touse' `wtexp', wald(0) 
				collinear missing max nooutput nopreserve 
				`mlopt' search(off) `init1ao' 
				title("Constant-only model:")
				`robust' nocnsnotes `negh' ;
			#delimit cr
			local ll1 = e(ll)
			local llarg "lf0(1 `e(ll)')"
			if "`from'"=="" {
				tempname from1
				mat `from1' = get(_b)  // Keep for starting values in full model
			}
		}
	}

	if "`from'" == "" & "`search'" != "on" {	// If no specified starting values 
		local srcharg "on"						// and search is not on, choose starting
		local maxk = 0							// values from comparison models
		local ll0  = .
		forvalues k=1/3 {
			if `ll`k'' != . {
				if `maxk'==0 | `ll`k'' > `ll`maxk''  {
					local maxk = `k'
				}
			}
		}
		if `ll1' != . { local maxk = 1 }
		if `maxk' != 0 { 
			local from "`from`maxk''" 
			local srcharg "off"
		}
		if "`search'" == "" { local search "`srcharg'" }
	}
	
	// Specify -init()- and -search()- depending on results and user specifications
	if "`from'" == "" { 
		local initopt ""
		if "`search'" != "off" { local srchopt "search(on)" }
	}
	else { 
		local initopt "init(`from', skip)" 
		if "`search'" != "on"  { local srchopt "search(off)" }
	}
	
	/***********/
	/* Model 0 */
	/***********/
	global SGLM_m = 1        // We handle binomial size directly
	`lll' di in gr _n "Fitting full zero-inflated binomial model:"
	
	#delimit ;
	`vv'
	`log' ml model `mlmetho' `lfprog'
		(`dep': `dep' = `ind', `nc' `offo')
		(inflate: `inflate', `nc2' `off2a')
		if `touse' `wtexp',
		collinear missing max nooutput nopreserve `mlopt'
		title(Zero-inflated binomial regression) 
		`scopt' `robust' `clopt' 
		`initopt'`srchopt' `llarg' `negh' ;
	#delimit cr

	if "`score'" != "" { 
		rename `sc1' `svar1' 
		rename `sc2' `svar2' 
		est local scorevars `svar1' `svar2'
	}
	
	if "`vuong'" != "" {
		quietly {
			tempvar xb1 mu1 zg1 gamma1 vv
			_predict double `xb1', eq(#1) xb  // ZERO-INFLATED BINOMIAL: xb1=X*beta1 
			global SGLM_m = 1
			glim_l0$ZIB_L 1 `xb1' `mu1'       // ZERO-INFLATED BINOMIAL: mu1=g(X*beta1)
			capture drop `xb1'
			_predict double `zg1', eq(#2) xb  // ZERO-INFLATED BINOMIAL: zg1=Z*gamma1
			global SGLM_m = 1
			glim_l0$ZIB_I 1 `zg1' `gamma1'    // ZERO-INFLATED BINOMIAL: gamma1=h(Z*gamma1)
			capture drop `zg1'
			
			// RECALL that we earlier calculated for BINOMIAL: xb2=X*beta2 and mu2=g(X*beta2)
			
			// Vuong statistic is based on the vector (over observations) of: loglik(ZIB) - loglik(BIN)
			
			// For y=0
			gen double `vv' = log( (`gamma1'+(1-`gamma1')*(1-`mu1')^`nvar') / ((1-`mu2')^`nvar')) if `touse' 
			
			// For y>0
			replace `vv' = log(1-`gamma1') + (`nvar'-`dep')*log((1-`mu1')/(1-`mu2')) + `dep'*log(`mu1'/`mu2') if `touse' & `dep'>0
			
			summ `vv' `wtexp' if `touse'
			est scalar vuong = sqrt(`r(N)'/`r(Var)')*`r(mean)'
                        est scalar vuongaic = sqrt(`r(N)'/`r(Var)')*(`r(mean)' - (`df')/`r(N)')
                        est scalar vuongbic = sqrt(`r(N)'/`r(Var)')*(`r(mean)' - (`df'*ln(`r(N)')/(2*`r(N)')))
		}
	}
	
	// calculate correct df_m
	local coln : colfullnames e(b)
	local dfm 0
	foreach name of local coln {
		tokenize `name', parse(":")
		if "`1'" == "`dep'" {
			if "`3'" != "_cons" {
				_ms_parse_parts `3'
				// constraints are not marked as omitted
				if !r(omit) {
					local dfm = `dfm' + ///
					cond(_b[`3']==0 & _se[`3']==0,0,1)
				}
			}
		}
	}
	est local cmd
	est scalar N_zero = `nzero'
	est scalar df_m = `dfm'
	est scalar df_c = `df'
	est scalar llb  = `ll2'
	est scalar bin_d = `bind'
	est scalar bin_n = `binn'
	
	if "`lr'" != "" { local ll1 = . }
	
	if `ll1' == . { est local chi2type "Wald" }
	else          { 
		est local chi2type "LR"  
		est scalar chi2 = -2*(`ll1'-e(ll))
	}

	est local narg    "`narg'"
	est local inflate "`ilink'"
	est local offset1 "`offstr'"
	est local offset2 "`off2s'"
	est local predict "zib_p"
	est local link    "`link'"
	est local ilink   "`ilink'"
	est local cmd     "zib"
	if "`display'" == "" {
		Display, `diopts'
		DispV
		_prefix_footnote
	}
end


program define Display
	if "`e(prefix)'" == "svy" {
		_prefix_display `0'
		exit
	}
	syntax [, Level(cilevel) EFORM *]

	_get_diopts diopts, `options'
	DispHd
	di
	version 9: ml di, level(`level') `eform' nohead nofootnote `diopts'
end

program define DispV
	if !missing(e(vuong)) {
		noi di as txt "Vuong test of zib vs. standard binomial:" ///
			_col(51) "z = "  as res %8.2f `e(vuong)'			 ///
			_col(66) as txt "Pr>z = " as res %6.4f normprob(-`e(vuong)')
                noi di as txt "    Bias-corrected (AIC) Vuong test:" ///
                        _col(51) "z = "  as res %8.2f `e(vuongaic)'                      ///
                        _col(66) as txt "Pr>z = " as res %6.4f normprob(-`e(vuongaic)')
                noi di as txt "    Bias-corrected (BIC) Vuong test:" ///
                        _col(51) "z = "  as res %8.2f `e(vuongbic)'                      ///
                        _col(66) as txt "Pr>z = " as res %6.4f normprob(-`e(vuongbic)')
	}
end

program define DispHd
	if !missing(e(df_r)) {
		local model _col(51) as txt "F(" ///
			as res %3.0f e(df_m) as txt "," ///
			as res %6.0f e(df_r) as txt ")" _col(67) ///
			"=" _col(70) as res %9.2f e(F)
		local pvalue _col(51) as txt "Prob > F" _col(67) ///
			"=" _col(70) as res %9.4f Ftail(e(df_m),e(df_r),e(F))
	}
	else {
		if "`e(chi2type)'" == "" {
			local chitype Wald
		}
		else	local chitype `e(chi2type)'
		local model _col(51) as txt `"`chitype' chi2("' ///
			as res e(df_m) as txt ")" _col(67) ///
			"=" _col(70) as res %9.2f e(chi2)
		local pvalue _col(51) as txt "Prob > chi2" _col(67) ///
			"=" _col(70) as res %9.4f chiprob(e(df_m),e(chi2))
	}

	local crtype = upper(substr(`"`e(crittype)'"',1,1)) + /*
		*/ substr(`"`e(crittype)'"',2,.)

	local infl "" 
	local crlen = max(length("`infl'"),length(`"`crtype'"')) 
	di in gr _n "`e(title)'" _col(51) "Number of obs   =" ///
		in ye _col(70) %9.0g e(N) 
	di in gr "Regression link: " as res "`e(link)'" as txt _col(51) "Nonzero obs" _col(67) "=" ///
		in ye _col(70) %9.0g e(N) - e(N_zero) 
	di in gr "Inflation link : " as res "`e(ilink)'" as txt _col(51) "Zero obs" _col(67) "=" ///
		in ye _col(70) %9.0g e(N_zero) 
		
	di in gr  `model' 
	di in gr %-`crlen's "`crtype'" " = " in ye %9.0g e(ll) `pvalue' 
end


program define PrseOff, sclass

	local 0 : subinstr local 0 "_cons" "", word
	local 0 : subinstr local 0 "_cons," ",", word
	syntax [varlist(default=none fv)] [, OFFset(varname numeric) noCONstant]
	local fvops = "`s(fvops)'" == "true"
	sret clear
	sret local inflate "`varlist'"
	sret local off2 "`offset'"
	sret local nc2  "`constan'"
	sret local fvops = cond(`fvops', "true", "")
end

program define GetLink, rclass
	args link ilink
	foreach mac in link ilink {
		if "``mac''" == "" {
			local `mac' "logit"
		}
	}

	local linkrc  = 1
	local ilinkrc = 1
	foreach mac in link ilink {
		foreach lnk in logit probit loglog cloglog {
			if "``mac''" == substr("`lnk'", 1, max(3, length("``mac''"))) {
				local ``mac'' = "`lnk'"
				local `mac'rc = 0
			}
		}
	}
	if "`link'"  == "logit"   { global ZIB_L = 2 }
	if "`link'"  == "probit"  { global ZIB_L = 8 }
	if "`link'"  == "loglog"  { global ZIB_L = 6 }
	if "`link'"  == "cloglog" { global ZIB_L = 7 }
	if "`ilink'" == "logit"   { global ZIB_I = 2 }
	if "`ilink'" == "probit"  { global ZIB_I = 8 }
	if "`ilink'" == "loglog"  { global ZIB_I = 6 }
	if "`ilink'" == "cloglog" { global ZIB_I = 7 }
	
	ret scalar rc = `linkrc'+`ilinkrc'
	ret local link "`link'"
	ret local ilink "`ilink'"
end

exit


