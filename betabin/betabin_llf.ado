program define betabin_llf
	args todo b lnf g H sc1 sc2 
	quietly {
		tempvar eta1 eta2
		mleval `eta1' = `b', eq(1)
		mleval `eta2' = `b', eq(2) scalar
		
		// Need to evaluate zero-inflated binomial when the eta2 is too (negatively) large to avoid computational problems.
		
		tempvar  mu 
		tempname sigma
		glim_l0$BBIN_L 1 `eta1' `mu'
		scalar `sigma' = exp(`eta2')
		
		local n "$BBIN_n"
		if "`n'" == "" {
			local n = 1
		}		
		local y "$ML_y1"
	
		local limit = -20
		
		if `eta2' < `limit' {
			scalar `sigma' = 0
			mlsum `lnf' =  lngamma(1+`n') - lngamma(1+`n'-`y') - lngamma(1+`y') + `y'*log(`mu') + (`n'-`y')*log(1-`mu') 
		}
		else {
			#delimit ;
			mlsum `lnf' =  lngamma(1+`n') - lngamma(1+`n'-`y') - lngamma(1+`y') - lngamma(`n'+1/`sigma') 
								+ lngamma(`y'+`mu'/`sigma') + lngamma(1/`sigma') - lngamma((1-`mu')/`sigma') 
								- lngamma(`mu'/`sigma') + lngamma(`n'-`y'+(1-`mu')/`sigma') ;
			#delimit cr		
		}
		
		if (`todo'==0 | `lnf'>=.) exit
		
		tempvar dmu 
		glim_l0$BBIN_L 2 `eta1' `mu' `dmu'
	
		if `eta2' < `limit' {
			replace `sc1' = (`y'/`mu' - (`n'-`y')/(1-`mu'))*`dmu'
			replace `sc2' = 0 
		}
		else {
			#delimit ;
			replace `sc1' = (			
							digamma(`y'+`mu'/`sigma') + digamma((1-`mu')/`sigma')
							- digamma(`mu'/`sigma')   - digamma(`n'-`y'+(1-`mu')/`sigma') 
						) * `dmu' / `sigma' ;
			
			replace `sc2' = (																	
							digamma(`n'+1/`sigma') - digamma(1/`sigma') + digamma((1-`mu')/`sigma') - digamma(`n'-`y'+(1-`mu')/`sigma') + 
							`mu'*(
								- digamma(`y'+`mu'/`sigma') - digamma((1-`mu')/`sigma')
								+ digamma(`mu'/`sigma')     + digamma(`n'-`y'+(1-`mu')/`sigma')
							)
						) / `sigma' ;
			#delimit cr
		}
		tempname d1 d2
		mlvecsum `lnf' `d1' = `sc1', eq(1)
		mlvecsum `lnf' `d2' = `sc2', eq(2)
		matrix `g' = (`d1', `d2')
		
		if (`todo'==1 | `lnf'>=.) exit
		
		
		
		tempvar sc12 t1 t2 dmu2
		glim_l0$BBIN_L 3 `eta1' `mu' `dmu2'
		
		if `eta2' < `limit' {
			gen double `t1'   = (`y'/`mu' - (`n'-`y')/(1-`mu'))*`dmu2'-(`y'/`mu'^2+(`n'-`y')/(1-`mu')^2)*`dmu'^2
			gen double `sc12' = 1
			gen double `t2'   = 1 
		}
		else {
			#delimit ;
			gen double `t1' =															
				(		
					(											
						trigamma(`y'+`mu'/`sigma') - trigamma((1-`mu')/`sigma')  		
						-trigamma(`mu'/`sigma')    + trigamma(`n'-`y'+(1-`mu')/`sigma') 
					)*`dmu'^2 
					+`sigma'*(												
						digamma(`y'+`mu'/`sigma') + digamma((1-`mu')/`sigma')  		
						-digamma(`mu'/`sigma')    - digamma(`n'-`y'+(1-`mu')/`sigma')	
					)*`dmu2' 																	
				) / `sigma'^2 ;
				
			gen double `sc12' =															
				(																		
					`sigma'*(															
						-digamma(`y'+`mu'/`sigma')  - digamma((1-`mu')/`sigma')  		
						+digamma(`mu'/`sigma')      + digamma(`n'-`y'+(1-`mu')/`sigma')	
					) 
					- trigamma((1-`mu')/`sigma') 
					+ `mu'*(
						-trigamma(`y'+`mu'/`sigma') + trigamma((1-`mu')/`sigma') 
						+trigamma(`mu'/`sigma')     - trigamma(`n'-`y'+(1-`mu')/`sigma')	
					) 	
					+ trigamma(`n'-`y'+(1-`mu')/`sigma')
				) * `dmu' / `sigma'^2 ;
				
			gen double `t2' =																					
				(																								
						-`sigma'*digamma(`n'+1/`sigma')  +      `mu'*`sigma'*digamma(`y'+`mu'/`sigma')         
						+`sigma'*digamma(1/`sigma')      + (-1+`mu')*`sigma'*digamma((1-`mu')/`sigma')         
				   -`mu'*`sigma'*digamma(`mu'/`sigma')   - (-1+`mu')*`sigma'*digamma(`n'-`y'+(1-`mu')/`sigma')  
								-trigamma(`n'+1/`sigma') +            `mu'^2*trigamma(`y'+`mu'/`sigma')         
								+trigamma(1/`sigma')     -       (-1+`mu')^2*trigamma((1-`mu')/`sigma')         
						 -`mu'^2*trigamma(`mu'/`sigma')  +       (-1+`mu')^2*trigamma(`n'-`y'+(1-`mu')/`sigma')  
				) / `sigma'^2 ;
			#delimit cr
		}
		tempname d11 d12 d22
		mlmatsum `lnf' `d11' = `t1'  , eq(1)
		mlmatsum `lnf' `d12' = `sc12', eq(1,2)
		mlmatsum `lnf' `d22' = `t2'  , eq(2)
		
		matrix `H' = (`d11', `d12' \ `d12'', `d22')
	}
end
