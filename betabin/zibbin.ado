*! version 2.1.0  04jun2017
program define zibbin, eclass byable(onecall) prop(ml_score irr svyb svyj svyr)
	if _by() {
		local BY `"by `_byvars'`_byrc0':"'
	}
	`BY' _vce_parserun zibbin, mark(INFlate OFFset CLuster) : `0'
	if "`s(exit)'" != "" {
		version 8: ereturn local cmdline `"zib `0'"'
		exit
	}

	version 6.0, missing
	if replay() {
		if "`e(cmd)'" != "zibbin" { error 301 }
		if _by() { 
			error 190 
		}
		Display `0'
		if e(vuong) < . {
			DispV
		}
		_prefix_footnote
		exit
	}
	if _caller() >= 11 {
		local vv : di "version " string(_caller()) ":"
	}
	`vv' `BY' Estimate `0'
	version 10: ereturn local cmdline `"zibbin `0'"'
end

program define Estimate, eclass byable(recall)
	/* 	   
	   Model 0 is the target
	   We do not support constraints
	   
	   Regression coefficients and/or loglikelihoods from:
	   0) Zero-inf betabinomial model: FULL             B0_r, B0_0, S0  with Off0_B and G0_r, G0_0 with Off0_G
	   1) Zero-inf betabinomial model: CONSTANT-ONLY          B1_0, S1  with Off0_B and G1_r, G1_0 with Off0_G 
	   2) Zero-inf binomial model: FULL                 B2_r, B2_0      with Off0_B and G2_r, G2_0 with Off0_G
       3) Zero-inf binomial model: CONSTANT-ONLY              B3_0      with Off0_B and G3_r, G3_0 with Off0_G
	   4) Betabinomial model: FULL                      B4_r, B4_0, S4  with Off0_B
	   5) Betabinomial model: CONSTANT-ONLY                   B5_0, S5  with Off0_B
	   
	   Note that the betabinomial variance (S0) is in terms of log.  So, references to zero 
	   are introduced as log(variance) in the code as -7.
	   
	   If Model 0 includes a constant 
	   (-nocons- option is not specified):
	       If either one of -nolr- or -from()- is not specifed: estimate Model 1 and get LL/pLL
		   
	       If there are no given starting values
		   (-from()- option is not specified):
               Model 1 provides candidate starting values for Model 0: {0, B1_0, S1, G1_r, G1_0}
			   
		   If the user does not specify no comparison to constant only 
		   (-nolr- option is not specified):
	           LL from Model 1  used in LR test of {B_r} in Model 0 ~ chi2
			   
	   If -zib- is specifed: 
	   
	       if Model 0 includes a constant
		   (-nocons- is not specified):
		       estimate Model 3 and get LL/pLL 
			   
			   If there are no given starting values
		       (-from()- option is not specified):
                   Model 3 provides candidate starting values for Model 0: {0, B3_0, 0, G3_r, G3_0}
			   
			   If the user does not specify no comparison of the full zero-inflated beta binomial 
		       model to a zero-inflated binomial model without beta overdispersion, and 
			   the user has not specified a form of robust variance:
		       (-robust-, -constraints- options are not specified):
			       LL from Model 3 could be used in LR test of {B_r, S} in Model 0 ~ chibar/Vuong
		   
		   estimate Model 2 and get LL/pLL
	   
	       If there are no given starting values
	       (-from()- option is not specified):
	           Model 2 could provide starting values for Model 0: {B2_r, B2_0, 0, G2_r, G2_0}
			   
	       If the user does not specify no comparison of the full beta binomial 
		   model to a binomial model without beta overdispersion, and 
			   the user has not specified a form of robust variance:
		       (-robust-, -constraints- options are not specified):
	           LL from Model 2 could be used in LR test of {S} in Model 0 ~ chibar/Vuong
			   
		
		If -vuong- is specifed: 
	   
	       if Model 0 includes a constant
		   (-nocons- is not specified):
		       estimate Model 5 and get LL/pLL 
			   
			   If there are no given starting values
	           (-from()- option is not specified):
	               Model 5 could provide starting values for Model 0: {0, B5_0, S5, 0, 0}
			   
			   If the user does not specify no comparison of the full zero-inflated beta binomial 
		       model to a beta binomial model without zero-inflation, and 
			   the user has not specified a form of robust variance:
		       (-robust-, -constraints- options are not specified):
			       LL from Model 5 could be used in LR test of {B_r, G_r, G_0} in Model 0 ~ chibar/Vuong
		   
		   estimate Model 4 and get LL/pLL
	   
	       If there are no given starting values
	       (-from()- option is not specified):
	           Model 4 could provide starting values for Model 0: {B4_r, B4_0, S4, 0, 0}
			   
	       If the user does not specify no comparison of the full beta binomial 
		   model to a binomial model without beta overdispersion, and 
			   the user has not specified a form of robust variance:
		       (-robust-, -constraints- options are not specified):
	           LL from Model 4 could be used in LR test of {G_r, G_0} in Model 0 ~ chibar/Vuong   
			   
		If there are no given starting values
	    (-from()- option is not specified):
		    Choose starting value as that candidate with greatest LL/pLL
	*/
	version 6.0, missing
	local cmd "`0'"
	local awopt = cond(_caller()<7,"aw","")
	#delimit ;
	syntax varlist(fv) [if] [in] [fweight `awopt' pweight iweight/] 
		, INFlate(string) N(string) [ 
				/* general options */
				noCONstant Level(cilevel) Link(string)
				
				/* optimization options */
				Robust CLuster(varname) SCore(string) EFORM 
				MLMETHOD(string) OFFset(varname numeric) ZIB noLRtest
				EXPosure(varname numeric) noLOg noDISPLAY
				FROM(string) CRITTYPE(passthru)
				Link(string) ILink(string)
				
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
		tempvar nvar
		gen `nvar' = `n'
		if floor(`nvar') != `nvar' | `nvar' <= `dep' | `nvar' <= 0 {
			noi di as err "N() must be an positive integer expression that is >= the outcome"
			exit 199
		}
		summ `nvar'
		if `r(max)'==1 {
			noi di as err "N() always equal to 1; use zib instead"
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
			local score `score'1 `score'2 `score'3 
			local n 3
		}
		if `n' != 3 {
			di in red "score():  you must specify three new variables"
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
	
	if "`cluster'" != "" { local clopt "cluster(`cluster')" }
	_vce_parse, argopt(CLuster) opt(Robust oim opg) old: `wtexp', `vce' `clopt' `robust'
	local cluster `r(cluster)'
	local robust `r(robust)'

	if "`cluster'" != "" { local clopt "cluster(`cluster')" }
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
		if "`offstr'" == "" { local offstr "`offset'" }
	}

	markout `touse' `inflate' `off2' `off' 
	
	if (`fvops1' | `fvops2') {
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

	if "`nc2'" == ""     { local df = `df' + 1} 
	if "`mlmetho'" == "" { local mlmetho "`mm'" }
	if "`score'" != "" {
		local svar1 : word 1 of `score'
		local svar2 : word 2 of `score'
		local svar3 : word 3 of `score'
		confirm new variable `score'
		tempvar sc1 sc2 sc3
		local scopt "score(`sc1' `sc2' `sc3')"
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
		di in bl _n "(note: " in ye "`dep'" in bl " never equal to zero; use glm instead)"
	}

	if "`log'`display'" == "" { local lll "noisily" }
	else                      { local lll "quietly" }
	if "`log'" == ""          { local log "noisily" }
	else                      { local log "quietly" }

	
	qui {
		summ `nvar' `wtexp' if `touse'
		local bind = r(sum)
		summ `dep'  `wtexp' if `touse'
		local binn = r(N)
		
		global ZIBB_n   "`nvar'"  // Identify "n" binomial denominator variable for likelihood function
		local  lfprog  "zibbin_llf"
		
		forvalues k=1/7 { local ll`k' = . }
			
		if ("`vuong'" != "") { // | ("`from'" == "")  {
			local init4 ""
			if "`nc'" == "" {                                   
				/***********/
				/* Model 5 */
				/***********/
				`lll' di in gr _n "Fitting constant-only beta binomial model:"  
				`vv' `lll' betabin `dep' if `touse' `wtexp', `offo' n(`nvar') ///
					link(`link') nrtolerance(1e-2) tolerance(1e-1) iter(5) nodisplay
				local ll5 = e(ll)

				if "`from'" == "" {
					tempname from5 b5
					mat `b5' = get(_b)
					mat `from5' = (`b5')
				}
				local init4 "from(`b5')"
			}
		
			/***********/
			/* Model 4 */
			/***********/
			`lll' di in gr _n "Fitting full beta binomial model:"      
			`vv' `lll' betabin `dep' `ind' if `touse' `wtexp', `offo' `nc' n(`nvar') ///
					link(`link') nrtolerance(1e-2) tolerance(1e-1) iter(5) nodisplay `init4'
			local ll4 = e(ll) 
			
			if "`vuong'"!="" {
				global SGLM_m = 1
				tempvar xb2 mu2 sigma2
				_predict double `xb2' if `touse', xb eq(#1)		// BETABINOMIAL: xb2=X*beta 
				glim_l0$ZIBB_L 1 `xb2' `mu2'					// BETABINOMIAL: mu2=g(X*beta)
				_predict double `sigma2' if `touse', xb eq(#2)	// BETABINOMIAL: sigma2=ln(sigma)
				replace `sigma2'=exp(`sigma2')					// BETABINOMIAL: sigma2=sigma
				capture drop `xb2'
			}
		
			if "`from'" == "" { 
				tempname from4
				mat `from4' = e(b)
			} 
		}
		
		
		if ("`zib'" != "") { // | ("`from'" == "")  {
			local init2 ""
			if "`nc'" == "" {                                   
				/***********/
				/* Model 3 */
				/***********/
				`lll' di in gr _n "Fitting constant-only zero-inflated binomial model:"  
				`vv' `lll' zib `dep' if `touse' `wtexp', `offo' `nc' inflate(`inflate', `nc2' `off2a') n(`nvar') ///
					link(`link') ilink(`ilink') nrtolerance(1e-2) tolerance(1e-1) iter(5) nodisplay
				local ll3 = e(ll)

				if "`from'" == "" {
					tempname from3 b3 s
					mat `b3' = e(b)
					mat `s' = (-7)
					mat colnames `s' = lnsigma:_cons
					mat `from3' = (`b3', `s')
					local init2 "from(`b3')"
				}
			}
		
			/***********/
			/* Model 2 */
			/***********/
			`lll' di in gr _n "Fitting full zero-inflated binomial model:"
			`vv' `lll' zib `dep' `ind' if `touse' `wtexp', `offo' `nc' inflate(`inflate', `nc2' `off2a') n(`nvar') ///
					link(`link') ilink(`ilink') nrtolerance(1e-2) tolerance(1e-1) iter(5) nodisplay `init2'	
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
				zib `dep', inflate(`inflate', `nc2' `off2a') n(`nvar') ///
					link(`link') ilink(`ilink') nrtolerance(1e-2) tolerance(1e-1) iter(5) nodisplay
				mat `b1a' = get(_b)
				mat `from1a' = (`b1a', -7)
				mat colnames `from1a' = `dep':_cons lnsigma:_cons
				local init1aopt "from(`from1a')"
			}

			/***********/
			/* Model 1 */
			/***********/
			global SGLM_m = 1        // We handle binomial size directly
			`lll' di in gr _n "Fitting constant-only model:"
			
			#delimit ;
			`vv'
			`lll' ml model `mlmetho' `lfprog'
				(`dep': `dep' = , `offo')
				(inflate: `inflate', `nc2' `off2a')
				(lnsigma:)
				if `touse' `wtexp', wald(0) 
				collinear missing max nooutput nopreserve 
				`mlopt' search(off) `init1aopt' 
				title("Constant-only model:")
				diparm(lnsigma, exp label("sigma"))
				nocnsnotes `negh' iter(8) nrtolerance(1e-2) ltolerance(1e-1) ;
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
		forvalues k=1/7 {
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
	`lll' di in gr _n "Fitting full model:"
	#delimit ;
	`vv'
	`log' ml model `mlmetho' `lfprog'
		(`dep': `dep' = `ind', `nc' `offo')
		(inflate: `inflate', `nc2' `off2a')
		(lnsigma:)
		if `touse' `wtexp',
		collinear missing max nooutput nopreserve `mlopt'
		title(Zero-inflated beta-binomial regression) 
		`scopt' `robust' `clopt' 
		diparm(lnsigma, exp label("sigma"))
		`initopt' `srchopt' `llarg' 
		`negh' ;
	#delimit cr

	if "`score'" != "" { 
		rename `sc1' `svar1' 
		rename `sc2' `svar2' 
		rename `sc3' `svar3'
		est local scorevars `svar1' `svar2' `svar3'
	}
	
	if "`vuong'" != "" {
		quietly {
			tempvar xb1 mu1 zg1 gamma1 sigma1 vv
			global SGLM_m = 1
			_predict double `xb1', eq(#1) xb  				// ZERO-INFLATED BETABINOMIAL: xb1=X*beta1 
			glim_l0$ZIBB_L 1 `xb1' `mu1'       				// ZERO-INFLATED BETABINOMIAL: mu1=g(X*beta1)
			capture drop `xb1'
			global SGLM_m = 1
			_predict double `zg1', eq(#2) xb  				// ZERO-INFLATED BETABINOMIAL: zg1=Z*gamma1
			glim_l0$ZIBB_I 1 `zg1' `gamma1'    				// ZERO-INFLATED BETABINOMIAL: gamma1=h(Z*gamma1)
			capture drop `zg1'
			_predict double `sigma1' if `touse', eq(#3) xb 	// ZERO-INFLATED BETABINOMIAL: sigma1=ln(sigma) 
			replace `sigma1'=exp(`sigma1')					// ZERO-INFLATED BETABINOMIAL: sigma1=sigma
			
			// RECALL that we earlier calculated for BETABINOMIAL: xb2=X*beta2, mu2=g(X*beta2), and sigma2
			
			// Vuong statistic is based on the vector (over observations) of: loglik(ZIBB) - loglik(BBIN)
			
			// For y=0
			#delimit ;
			gen double `vv' = log( `gamma1' + (1-`gamma1') * 
									exp(  lngamma(1/`sigma1')                - lngamma(`nvar'+1/`sigma1') 
										- lngamma(`nvar'+(1-`mu1')/`sigma1') + lngamma((1-`mu1')/`sigma1') 
										)
								) 
								        - lngamma(1/`sigma2')                + lngamma(`nvar'+1/`sigma2')  
								        + lngamma(`nvar'+(1-`mu2')/`sigma2') - lngamma((1-`mu2')/`sigma2') 
								    
				if `touse' ;
			#delimit cr
			
			// For y>0
			#delimit ;
			replace `vv' = log(1-`gamma1') - lngamma(`nvar'+1/`sigma1') + lngamma(`dep' + `mu1'/`sigma1') + lngamma(1/`sigma1') 
							- lngamma((1-`mu1')/`sigma1') - lngamma(`mu1'/`sigma1')	+ lngamma(`nvar'-`dep'+(1-`mu1')/`sigma1') 
										   + lngamma(`nvar'+1/`sigma2') - lngamma(`dep' + `mu2'/`sigma2') - lngamma(1/`sigma2') 
							+ lngamma((1-`mu2')/`sigma2') + lngamma(`mu2'/`sigma2')	- lngamma(`nvar'-`dep'+(1-`mu2')/`sigma2') 
					if `touse' & `dep'>0 ;
			#delimit cr
			
			summ `vv' `wtexp' if `touse'
			est scalar vuong = sqrt(`r(N)'/`r(Var)')*`r(mean)'
                        est scalar vuongaic = sqrt(`r(N)'/`r(Var)')*(`r(mean)' - (`df')/`r(N)')
                        est scalar vuongbic = sqrt(`r(N)'/`r(Var)')*(`r(mean)' - (`df'*ln(`r(N)')/(2*`r(N)')))
		}
	}
	
	if "`ll2'"!="" & "`zib'" != "" {
		est local chi2_ct "LR"
		est scalar ll_c = `ll2'
		if (e(ll) < e(ll_c)) | (_b[/lnsigma] < -15) {
			est scalar chi2_c = 0
				/* otherwise, let it be negative when
				   it does not converge	*/
		}
		else	est scalar chi2_c = 2*(e(ll)-e(ll_c))
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
	est scalar df_m   = `dfm'
	est scalar df_c   = `df'
	if "`llb'" != "" { est scalar llb    = `llb' }
	est scalar k_aux = 1                             /* Need this to properly display beta coefficient */
	est hidden local diparm_opt3 noprob  /* Need this to properly display beta coefficient */
	est scalar bin_d = `bind'
	est scalar bin_n = `binn'

	est local narg    "`narg'"
	est local inflate "`ilink'"
	est local offset1 "`offstr'"
	est local offset2 "`off2s'"
	est local predict "zibbin_p"
	est local link    "`link'"
	est local ilink   "`ilink'"
	est local cmd     "zibbin"
	
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

	local fmt "%8.2e"
	if ((e(chi2_c) > 0.005) & (e(chi2_c)<1e4)) | (ln(e(sigma)) < -20) { 
		local fmt "%8.2f" 
	}
	
	if !missing(e(chi2_c)) {
		tempname pval
		scalar `pval' =  chiprob(1, e(chi2_c))*0.5
		if ln(e(sigma)) < -20 { scalar `pval'= 1 }
		di in smcl as txt "Likelihood-ratio test of sigma=0:  " /*
			*/ as txt "{help j_chibar##|_new:chibar2(01) =}" as res `fmt' /*
			*/ e(chi2_c) as txt " Prob>=chibar2 = " as res %5.3f /*
			*/ `pval'
	}
end

program define DispV
	if !missing(e(vuong)) {
		noi di as txt "Vuong test of zibb vs. standard beta binomial:" ///
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
		
	if !missing(e(llb)) {
		noi di as txt "Test of no zero inflation" _col(51) "LR chi2(" ///
			as res `e(df_c)' as text ")" _col(67) "=" _col(70) as res %9.2f (2*(e(ll)-e(llb))) 
		noi di _col(51) as txt "Prob > chi2" _col(67) ///
			"=" _col(70) as res %9.4f chiprob(e(df_c),(2*(e(ll)-e(llb))))
	}			
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
	sret local off2    "`offset'"
	sret local nc2     "`constan'"
	sret local fvops = cond(`fvops', "true", "")
end

program define GetLink, rclass
	args link ilink
	foreach mac in link ilink {
		if "``mac''" == "" { local `mac' "logit" }
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
	if "`link'"  == "logit"    { global ZIBB_L = 2 }
	if "`link'"  == "probit"   { global ZIBB_L = 8 }
	if "`link'"  == "loglog"   { global ZIBB_L = 6 }
	if "`link'"  == "cloglog"  { global ZIBB_L = 7 }
	
	if "`ilink'" == "logit"    { global ZIBB_I = 2 }
	if "`ilink'" == "probit"   { global ZIBB_I = 8 }
	if "`ilink'" == "loglog"   { global ZIBB_I = 6 }
	if "`ilink'" == "cloglog"  { global ZIBB_I = 7 }

	
	ret scalar rc = `linkrc'+`ilinkrc'
	ret local link  "`link'"
	ret local ilink "`ilink'"
end

exit



