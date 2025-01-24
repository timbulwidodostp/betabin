*! version 1.3.0  02apr2017
program define betabin, eclass byable(onecall) prop(ml_score or svyb svyj svyr)
	if _by() {                                                  /* Standard opening for estimation commands */
		local BY `"by `_byvars'`_byrc0':"'
	}                   
	`BY' _vce_parserun betabin, mark(INFlate OFFset CLuster) : `0'
	if "`s(exit)'" != "" {
		version 10: ereturn local cmdline `"betabin `0'"'
		exit
	}

	version 6.0, missing
	if replay() {
		if "`e(cmd)'" != "betabin" { error 301 }
		if _by() { error 190 }
		Display `0'
		_prefix_footnote
		exit
	}
	if _caller() >= 11 { local vv : di "version " string(_caller()) ":"	}
	`vv' `BY' Estimate `0'
	version 10: ereturn local cmdline `"betabin `0'"'
end

program define Estimate, eclass byable(recall)
	/* 	   
	   Model 0 is the target
	   We do not support constraints
	   
	   Regression coefficients and/or loglikelihoods from:
	   0) Betabinomial model: FULL             B0_r, B0_0, S0  with offset/exposure Off
	   1) Betabinomial model: CONSTANT-ONLY          B1_0, S1  with offset/exposure Off
	   2) Binomial model: FULL                 B2_r, B2_0      with offset/exposure Off
           3) Binomial model: CONSTANT-ONLY              B3_0      with offset/exposure Off
	   
	   Note that the betabinomial variance (S0) is in terms of log.  So, references to zero 
	   are introduced as log(variance) in the code as -7.
	   
	   If Model 0 includes a constant 
	   (-nocons- option is not specified):
	       If -from()- is not specifed: estimate Model 1 and get LL/pLL
		   
	       If there are no given starting values
		   (-from()- option is not specified):
               Model 1 provides candidate starting values for Model 0: {0, B1_0, S1}
	           LL from Model 1  used in LR test of {B_r} in Model 0 ~ chi2
			   
	   If -from()- is not specifed: 
	   
	       if Model 0 includes a constant
		   (-nocons- is not specified):
		       estimate Model 3 and get LL/pLL 
			   
			   If there are no given starting values
	           (-from()- option is not specified):
	               Model 3 could provide starting values for Model 0: {0, B3_0, 0}
			   
			   If the user does not specify no comparison of the full beta binomial 
		       model to a binomial model without beta overdispersion, and 
			   the user has not specified a form of robust variance:
		       (-robust-, -constraints- options are not specified):
			       LL from Model 3 could be used in LR test of {B_r, S} in Model 0 ~ chibar/Vuong
		   
		   estimate Model 2 and get LL/pLL
	   
	       If there are no given starting values
	       (-from()- option is not specified):
	           Model 2 could provide starting values for Model 0: {B2_r, B2_0, 0}
			   
	       If the user does not specify no comparison of the full beta binomial 
		   model to a binomial model without beta overdispersion, and 
			   the user has not specified a form of robust variance:
		       (-robust-, -constraints- options are not specified):
	           LL from Model 2 could be used in LR test of {S} in Model 0 ~ chibar/Vuong
			   
		If there are no given starting values
	    (-from()- option is not specified):
		    Choose starting value as that candidate with greatest LL/pLL
	*/
	version 6.0, missing
	local cmd "`0'"
	local awopt = cond(_caller()<7,"aw","")
	#delimit ;
	syntax varlist(fv) [if] [in] [fweight `awopt' pweight iweight/] 
		, N(string) 
		[ 
			/* general options */
				noCONstant Level(cilevel) Link(string)
				
			/* optimization options */ 
				Robust CLuster(varname) SCore(string) EFORM 
				MLMETHOD(string) OFFset(varname numeric) noLRBB noLR
				EXPosure(varname numeric) noLOg noDISPLAY
				FROM(string) CRITTYPE(passthru)
				
			/* additional optimization options */
				* 
		] ;
	#delimit cr

	local fvops1 = "`s(fvops)'" == "true" | _caller() >= 11
	if _by() { _byoptnotallowed score() `"`score'"' }
	
	GetLink "`link'"
	if `r(rc)' {
		noi di as err "Unsupported specification in link(`link')"
		exit 198
	}
	local link  "`r(link)'"

	marksample touse
	markout `touse' `cluster', strok	/* also see 2nd markout below for bionomial denominator */

	tokenize `varlist'
	local dep "`1'"
	
	quietly {
		tempvar nvar
		gen `nvar' = `n'
		if floor(`nvar') != `nvar' | `nvar' < `dep' | `nvar' <= 0 {
			noi di as err "N() must be a positive integer expression that is >= the outcome"
			exit 199
		}
		summ `nvar'
		if `r(max)' == 1 {
			noi di as err "N() always equal to 1; use binreg or glm instead"
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
			di in red "score():  you must specify two new variables"
			exit 198
		}
	}

	if "`weight'" != "" {
		local wtexp `"[`weight'=`exp']"'
		local wtype "`weight'"
	}
	else    local exp 1

	_get_diopts diopts options, `options'
	local diopts `diopts' level(`level') `eform'
	mlopts mlopt, `options'
	local coll  `s(collinear)'
	local cns   `s(constraints)'
	local mlopt `mlopt' `crittype'

	if "`cluster'" != "" { local clopt "cluster(`cluster')"	}
	_vce_parse, argopt(CLuster) opt(Robust oim opg) old: ///
			`wtexp', `vce' `clopt' `robust'
	local cluster `r(cluster)'
	local robust `r(robust)'

	if "`cluster'" != "" { local clopt "cluster(`cluster')"	}
	local nc  "`constan'"

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
		if "`offstr'" == "" { local offstr "`offset'" }
	}

	markout `touse' `off2' `off' 

	if `fvops1' {
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

	// calculate correct df
	local df 0
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

	if "`log'`display'" == "" { local lll "noisily" }   // Comparison model display iterator
	else                      { local lll "quietly" }
	if "`log'" == ""          { local log "noisily" }   // Main estimation display iterator
	else                      { local log "quietly" }
	

	qui {
		summ `nvar' `wtexp' if `touse'
		local bind = r(sum)
		summ `dep'  `wtexp' if `touse'
		local binn = r(N)
		
		global BBIN_n  "`nvar'"  // Identify "n" denominator variable for betabinomial likelihood function
		local  lfprog  "betabin_llf"
		
		forvalues k=1/3 { local ll`k' = . }
		
		if ("`lr'" == "") | ("`from'" == "")  {
			local init2 ""
			if "`nc'" == "" {                                   
				/***********/
				/* Model 3 */
				/***********/
				`lll' di in gr _n "Fitting constant-only binomial model:"      
				`vv' `lll' glm `dep' `wtexp' if `touse', `offo' `nc' fam(bin `nvar') link(`link') nodisplay
				local ll3 = e(ll)

				if "`from'" == "" {
					tempname from3 b3
					mat `b3' = get(_b)
					mat `from3' = (`b3', -7)
					mat colnames `from3' = `dep':_cons lnsigma:_cons
				}
				local init2 "from(`b3')"
			}
		
			/***********/
			/* Model 2 */
			/***********/
			`lll' di in gr _n "Fitting full binomial model:"      
			`vv' `lll' glm `dep' `ind' `wtexp' if `touse', ///
				`offo' `nc' fam(bin `nvar') link(`link') nodisplay `init2'
			local ll2 = e(ll) 
		
			if "`from'" == "" { 
				tempname from2 s
				mat `from2' = e(b)
				mat `s' = (-7)
				mat colnames `s' = lnsigma:_cons
				mat `from2' = (`from2', `s')
			} 
		}

		
		if "`nc'" == "" { 
			if "`from2'" != "" {
				local init1aopt "from(`from2')"
			}
			else { 
				tempname from1a b1a
				glm `dep' `wtexp' if `touse', `offo' `nc' fam(bin `nvar') link(`link') iter(5)
				mat `b1a' = get(_b)
				mat `from1a' = (`b1a', -7)
				mat colnames `from1a' = `dep':_cons lnsigma:_cons
				local init1aopt "from(`from1a')"
			}
			
			`lll' di in gr _n "Fitting constant-only model:" 
			global SGLM_m = 1        // We handle binomial size directly (force denom=1 for -glm- link functions)
			/***********/
			/* Model 1 */
			/***********/
			#delimit ;
			`vv'
			`lll' ml model `mlmetho' `lfprog'
				(`dep': `dep' = , `offo')
				/lnsigma
				if `touse' `wtexp', wald(0) 
				collinear missing max nooutput nopreserve 
				`mlopt' search(off) `init1aopt' 
				title("Constant-only model:")
				`robust' nocnsnotes `negh'  
				diparm(lnsigma, exp label("sigma")) 
				; 
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

	`lll' di in gr _n "Fitting full model:"
	global SGLM_m = 1        // We handle binomial size directly
	/***********/
	/* Model 0 */
	/***********/
	#delimit ;
	`vv'
	`log' ml model `mlmetho' `lfprog'
		(`dep': `dep' = `ind', `nc' `offo')
		/lnsigma
		if `touse' `wtexp',
		collinear missing max nooutput nopreserve `mlopt'
		title(Beta-binomial regression) 
		`scopt' `robust' `clopt' 
		`initopt' `srchopt' `llarg' `negh' 
		diparm(lnsigma, exp label("sigma")) 
		;
	#delimit cr
	
	if "`score'" != "" { 
		rename `sc1' `svar1' 
		rename `sc2' `svar2' 
		est local scorevars `svar1' `svar2'
	}
	
	if "`ll2'"!="" {
		est local chi2_ct "LR"
		est scalar ll_c = `ll2'
		if (e(ll) < e(ll_c)) | (_b[/lnsigma] < -15) {
			est scalar chi2_c = 0
				/* otherwise, let it be negative when
				   it does not converge	*/
		}
		else	est scalar chi2_c = 2*(e(ll)-e(ll_c))
	}
	
	if "`cluster'"=="" & "`weight'"!="pweight" {
		est scalar r2_p = 1 - e(ll)/e(ll_0)
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

	est scalar df_m  = `dfm'
	est scalar df_c  = `df'
	est scalar sigma = exp(_b[/lnsigma])
	est scalar bin_d = `bind'
	est scalar bin_n = `binn'
	
	est scalar k_aux = 1                             /* Need this to properly display beta coefficient */
	*est hidden local diparm_opt2 noprob  /* Need this to properly display beta coefficient */
	
	est local narg    "`narg'"
	est local link    "`link'"
	est local offset1 "`offstr'"
	est local predict "betabin_p"
	est local cmd     "betabin"
	if "`display'" == "" {
		Display, `diopts'
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
	version 9: ml di, level(`level') `eform' nohead nofootnote `diopts'
	
	if "`e(chi2_ct)'"!="LR" { exit }

	local fmt "%8.2e"
	if ((e(chi2_c) > 0.005) & (e(chi2_c)<1e4)) | (ln(e(sigma)) < -20) { 
		local fmt "%8.2f" 
	}

	tempname pval
	scalar `pval' =  chiprob(1, e(chi2_c))*0.5
	if ln(e(sigma)) < -20 { scalar `pval'= 1 }
	di in smcl as txt "Likelihood-ratio test of sigma=0:  " /*
        */ as txt "{help j_chibar##|_new:chibar2(01) =}" as res `fmt' /*
        */ e(chi2_c) as txt " Prob>=chibar2 = " as res %5.3f /*
        */ `pval'
end


program define DispHd
	di in gr _n "`e(title)'" _col(51) "Number of obs   =" ///
		in ye _col(70) %9.0g e(N) 
		
	if ("`e(chi2type)'"== "Wald") {
		di in gr "Link           = " as res "`e(link)'"
		di in gr "Dispersion     = " /*
			*/ as res "beta-binomial" _col(51)/*
			*/ as txt "`e(chi2type)' chi2(" as res e(df_m) in gr ")" /*
			*/ _col(67) "=" in ye _col(70) %9.2f e(chi2)
		di in gr     "Log likelihood = " as res e(ll) _col(51) /*
			*/ as txt "Prob > chi2"  _col(67) "=" /*
			*/ in ye _col(70) %9.4f e(p) _n

	}
	else if ("`e(chi2type)'"== "LR") {
		di in gr "Link           = " as res "`e(link)'" in gr _col(51) /*
			*/"`e(chi2type)' chi2(" as res e(df_m) in gr ")" _col(67) "="/*
			*/ in ye _col(70) %9.2f e(chi2)
		di in gr "Dispersion     = " as res "beta-binomial" /*
			*/ _col(51) in gr "Prob > chi2" _col(67) "=" /*
			*/ in ye _col(70) %9.4f e(p)
		di in gr "Log likelihood = " as res e(ll) _col(51) /*
			*/ in gr "Pseudo R2"   _col(67) "=" /*
			*/ in ye _col(70) %9.4f e(r2_p) _n
	}
end


program define GetLink, rclass
	args link
	if "`link'" == "" {	local link "logit" }
	local linkrc  = 1
	foreach lnk in logit probit loglog cloglog {
		if "`link'" == substr("`lnk'", 1, max(3, length("`link'"))) {
			local `link' = "`lnk'"
			local linkrc = 0
		}
	}
	if "`link'" == "logit"   { global BBIN_L = 2 }
	if "`link'" == "probit"  { global BBIN_L = 8 }
	if "`link'" == "loglog"  { global BBIN_L = 6 }
	if "`link'" == "cloglog" { global BBIN_L = 7 }
	ret scalar rc = `linkrc'
	ret local link "`link'"
end

exit

