*! version 1.0.0  20dec2012
program define betabin_p
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
	local myopts "N Pr Sigma LNSigma"

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
	local nw : word count `n' `pr' `sigma' `lnsigma'
	if `nw' > 1 {
		noi di as err "Only one of {n, pr, sigma, lnsigma} allowed"
		exit 199
	}
	local type "`n'`pr'`sigma'`lnsigma'"
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

	if ("`type'"=="" | "`type'"=="pr" | "`type'"=="n") {
		if "`type'"=="" {
			di in gr /*
			*/ "(option pr assumed; predicted probability of success)"
		}
		tempvar xb
		qui _predict double `xb' if `touse', xb `offset'
		qui glim_l0$BBIN_L 1 `xb' `varn'
		label var `varn' "Predicted probability"
		qui replace `varn' = . if `touse'==0
		if "`type'"=="n" & "`e(narg)'" != "" {
			qui replace `varn' = `e(narg)'*`varn' if `touse'
			label var `varn' "Predicted number of successes"
		}
		exit
	}

	quietly {
		local s 1
		local lab1 "sigma"
		local lab2 "ln(sigma)"
		_predict `varn' if `touse', eq(#2)
		if "`type'" == "sigma" {
			replace `varn' = exp(`varn')
		}
		else {
			local s 2
		}
		label var `varn' "`lab`s''"
		exit
	}
	error 198
end

