*! version 1.0.0  20dec2012
program define zibbin_p
	version 6, missing

	syntax [anything] [if] [in] [, SCores * ]
	if `"`scores'"' != "" {
		ml score `0'
		exit
	}

		/* Step 1:
			place command-unique options in local myopts
			Note that standard options are
			LR:
				Index XB Cooksd Hat
				REsiduals RSTAndard RSTUdent
				STDF STDP STDR noOFFset
			SE:
				Index XB STDP noOFFset
		*/
	local myopts "N OR Pr"


		/* Step 2:
			call _propts, exit if done,
			else collect what was returned.
		*/
	_pred_se "`myopts'" `0'
	if `s(done)' {
		exit
	}
	local vtyp  `s(typ)'
	local varn `s(varn)'
	local 0 `"`s(rest)'"'

		/* Step 3:
			Parse your syntax.
		*/
	syntax [if] [in] [, `myopts' noOFFset]


		/* Step 4:
			Concatenate switch options together
		*/
	local type "`n'`or'`pr'"
	local args `"`pr1'"'
	if "`pr1'" != "" {
		local propt pr(`pr1')
	}

		/* Step 5:
			quickly process default case if you can
			Do not forget -nooffset- option.
		*/

		/* Step 6:
			mark sample (this is not e(sample)).
		*/
	marksample touse

		/* Step 7:
			handle options that take argument one at a time.
			Comment if restricted to e(sample).
			Be careful in coding that number of missing values
			created is shown.
			Do all intermediate calculations in double.
		*/

		/* Step 8:
			handle switch options that can be used in-sample or
			out-of-sample one at a time.
			Be careful in coding that number of missing values
			created is shown.
			Do all intermediate calculations in double.
		*/
	tempvar xb pr
	qui _predict double `xb' if `touse', xb `offset' eq(#1)
	qui _predict double `pr' if `touse', xb eq(#2)

        if "`e(link)'" == "logit" {
                qui replace `xb' = exp(`xb')/(1+exp(`xb'))
        }
        else if "`e(link)'" == "probit" {
                qui replace `xb' = normprob(`xb')
        }
        else if "`e(link)'" == "loglog" {
                qui replace `xb' = exp(-exp(-`xb'))
        }
        else if "`e(link)'" == "cloglog" {
                qui replace `xb' = 1-exp(-exp(`xb'))
        }
        if "`e(inflate)'" == "logit" {
                qui replace `pr' = exp(`pr')/(1+exp(`pr'))
        }
        else if "`e(inflate)'" == "probit" {
                qui replace `pr' = normprob(`pr')
        }
        else if "`e(inflate)'" == "loglog" {
                qui replace `pr' = exp(-exp(-`pr'))
        }
        else if "`e(inflate)'" == "cloglog" {
                qui replace `pr' = 1-exp(-exp(`pr'))
        }

	if ("`type'"=="" & `"`args'"'=="") | "`type'"=="n" {
		if "`type'"=="" {
			di in gr /*
			*/ "(option n assumed; predicted number of successes)"
		}
		gen `vtyp' `varn' = e(narg)*`xb'*(1-`pr') if `touse'
		label var `varn' "Predicted number of successes"
		exit
	}

	if "`type'"=="ir" {
		tempvar xb_noof
		qui _predict double `xb_noof' if `touse', xb nooffset eq(#1)
		qui replace `xb_noof' = exp(`xb_noof')/(1+exp(`xb_noof'))
		gen `vtyp' `varn' = e(narg)*`xb_noof'*(1-`pr') if `touse'
		label var `varn' "Predicted number of succeses"
		exit
	}
	if "`type'"=="pr" {
		gen `vtyp' `varn' = `pr' if `touse'
		label var `varn' "Pr(`e(depvar)'=0)"
		exit
	}
	error 198
end
