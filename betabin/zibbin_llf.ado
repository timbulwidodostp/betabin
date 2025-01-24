program define zibbin_llf
	args todo b lnf g H sc1 sc2 sc3
	quietly {
		tempvar eta1 eta2 eta3
		mleval `eta1' = `b', eq(1)
		mleval `eta2' = `b', eq(2)
		mleval `eta3' = `b', eq(3) scalar

		local n "$ZIBB_n"
		if "`n'" == "" {
			local n = 1
		}
		
		// We are using the GLM programs to handle the links, inverse links, and their derivatives. 
		// LOGIT :  glim_l02
		// PROBIT:  glim_l08
		// LOGLOG:  glim_l06
		// CLOGLOG: glim_l07
		
		// Need to evaluate zero-inflated binomial when the eta3 is too (negatively) large to avoid computational problems.
		
		tempvar  mu gamma 
		tempname s
		glim_l0$ZIBB_L 1 `eta1' `mu'
		glim_l0$ZIBB_I 1 `eta2' `gamma' 
		scalar `s' = exp(`eta3')
		
		local limit = -20
		
		local y "$ML_y1"
		
		tempvar a1 a2 a3 a4 G
		quietly {
			gen double `a1' = 1/`s'
			gen double `a2' = `n'+1/`s'
			gen double `a3' = `n'+(1-`mu')/`s'
			gen double `a4' = (1-`mu')/`s'
			gen double `G'  = (lngamma(`a1') - lngamma(`a2')) + (lngamma(`a3') - lngamma(`a4'))
		}
		
		if `eta3' < `limit' {
			#delimit ;
			mlsum `lnf' = cond($ML_y1==0,													
				/* y=0  */	ln(`gamma' + (1-`gamma')*(1-`mu')^`n'), 						
				/* y!=0 */	ln(1-`gamma') + (`n'-`y')*ln(1-`mu') + `y'*ln(`mu') +		
								lngamma(1+`n') - lngamma(1+`n'-`y') - lngamma(1+`y')	
			) ;
			#delimit cr
		}
		else {
			#delimit ;
			mlsum `lnf' = cond($ML_y1==0, 
				ln(`gamma' + (1-`gamma')* exp(`G')), 
				ln(1-`gamma') + lngamma(`n'+1) - lngamma(`y'+1) - lngamma(`n'-`y'+1) + lngamma(1/`s') 
					+ lngamma(`y'+`mu'/`s') + lngamma(`n'-`y'+(1-`mu')/`s') - lngamma(`n'+1/`s') 
					- lngamma(`mu'/`s') - lngamma((1-`mu')/`s')) ;
			#delimit cr
		}
		
		
		if (`todo'==0 | `lnf'>=.) exit
	
		tempvar dmudeta1 dmudeta2 
		glim_l0$ZIBB_L 2 `eta1' `mu'    `dmudeta1'
		glim_l0$ZIBB_I 2 `eta2' `gamma' `dmudeta2'
		
		if `eta3' < `limit' {
			local dL0deta1 "-`n'*(1-`gamma')*(1-`mu')^(-1+`n')*`dmudeta1' / (`gamma'-(-1+`gamma')*(1-`mu')^`n')"
			local dL1deta1 "(-`y'+`n'*`mu')*`dmudeta1' / (`mu'*(`mu'-1))"
			local dL0deta2 "-(-1+(1-`mu')^`n')*`dmudeta2' / (`gamma'-(-1+`gamma')*(1-`mu')^`n')"
			local dL1deta2 "`dmudeta2'/(-1+`gamma')"
			local dL0deta3 "0"
			local dL1deta3 "0"
		}
		else {
			local dL0deta1 "-((-1+`gamma')*(digamma(`a3')-digamma(`a4'))*exp(`G')*`dmudeta1')"
			local dL0deta1 "`dL0deta1'/(`s'*(-exp(`G')+`gamma'*(exp(`G')-1)))"
			local dL1deta1 "(digamma(`y'+`mu'/`s') - digamma(`n'-`y'+(1-`mu')/`s') + digamma((1-`mu')/`s') - digamma(`mu'/`s'))*(`dmudeta1'/`s')"
			
			local dL0deta2 " (1-exp(`G'))*`dmudeta2'/(`gamma'+(1-`gamma')*exp(`G'))"
			local dL1deta2 "-`dmudeta2'/(1-`gamma')"
			
			local dL0deta3 "-((-1+`gamma')*(-digamma(`a2')+digamma(`a3')+digamma(`a1')-digamma(`a4')-`mu'*digamma(`a3')+`mu'*digamma(`a4'))*exp(`G'))"
			local dL0deta3 "`dL0deta3'/(`s'*(-exp(`G')+`gamma'*(exp(`G')-1)))"
			local dL1deta3 "(digamma(`n'+1/`s')-digamma(`n'-`y'+(1-`mu')/`s')-digamma(1/`s')+digamma((1-`mu')/`s')+"
			local dL1deta3 "`dL1deta3'`mu'*(digamma(`n'-`y'+(1-`mu')/`s')-digamma(`y'+`mu'/`s')-digamma((1-`mu')/`s')+digamma(`mu'/`s')))/`s'"
		}
		
		forvalues k=1/3 {
			replace `sc`k'' = cond($ML_y1==0, `dL0deta`k'', `dL1deta`k'')
			tempname d`k'
			mlvecsum `lnf' `d`k'' = `sc`k'', eq(`k')
		}
		
		matrix `g' = (`d1', `d2', `d3')
		
		if (`todo'==1 | `lnf'>=.) exit
		
		tempvar sc11 sc12 sc13 sc22 sc23 sc33 dmu2deta1 dmu2deta2
		glim_l0$ZIBB_L 3 `eta1' `mu'    `dmu2deta1'
		glim_l0$ZIBB_I 3 `eta2' `gamma' `dmu2deta2'
		
		if `eta3' < `limit' {
			#delimit ;
			gen double `sc11'   = cond($ML_y1==0, 
					/* y=0  */	(`n'*(-1+`gamma')*(1-`mu')^(-2+`n')* 
									(
										(
											-`gamma'*(-1+`n'+(1-`mu')^`n')+(1-`mu')^`n'
										)*`dmudeta1'^2 + 
										(
											`gamma'*(-1+(1-`mu')^`n') - (1-`mu')^`n'
										)*(-1+`mu')*`dmu2deta1'
									)
								) / (`gamma'+(1-`mu')^`n'-`gamma'*(1-`mu')^`n')^2,
					/* y!=0 */ ((-`y'+2*`y'*`mu'-`n'*`mu'^2)*`dmudeta1'^2+(-1+`mu')*`mu'*(-y+`n'*`mu')*`dmu2deta1') / 
									((-1+`mu')*`mu')^2 
				) ;
			gen double `sc12' = cond($ML_y1==0, 
					/* y=0  */	(`n'*(1-`mu')^(-1+`n')*`dmudeta2'*`dmudeta1') / (`gamma'+(1-`mu')^`n'-`gamma'*(1-`mu')^`n')^2, 
					/* y!=0 */	0
				) ;
			gen double `sc13' = 0 ;
			gen double `sc22'   = cond($ML_y1==0, 
					/* y=0  */	(-1+(1-`mu')^`n')*(-(-1+(1-`mu')^`n')*`dmudeta2'^2-(`gamma'-(-1+`gamma')*(1-`mu')^`n')*`dmu2deta2') / 
									(`gamma'-(-1+`gamma')*(1-`mu')^`n')^2, 
					/* y!=0 */	(-`dmudeta2'^2+(-1+`gamma')*`dmu2deta2')/(-1+`gamma')^2
				) ;
			gen double `sc23' = 0 ;
			gen double `sc33' = 0 ;
			#delimit cr
		}
		else {
			#delimit ;
			gen double `sc11' = cond($ML_y1==0, 
				-(
					(`gamma'-1)*exp(`G')*(
						exp(`G')*((trigamma(`a3')-trigamma(`a4'))*`dmudeta1'^2 - `s'*(digamma(`a3')-digamma(`a4'))*`dmu2deta1')
						+
						`gamma'*(
							(
								1*(
									digamma(`a3')^2-2*digamma(`a3')*digamma(`a4')+digamma(`a4')^2+
									trigamma(`a3')-trigamma(`a4')
								) + exp(`G')*(-trigamma(`a3')+trigamma(`a4'))
							)*`dmudeta1'^2 + `s'*(exp(`G')-1)*(digamma(`a3')-digamma(`a4'))*`dmu2deta1'
						) 
					)
				) / ( `s'^2*(	exp(`G') + `gamma'*(1-exp(`G'))	)^2	),
				(`a1'^2)*(
					(
						trigamma(`n'-`y'+`a4')-trigamma(`a4')-trigamma(`mu'/`s')+trigamma(`y'+`mu'/`s')
					)*`dmudeta1'^2 -
					`s'*(
						digamma(`n'-`y'+`a4')-digamma(`a4')+digamma(`mu'/`s')-digamma(`y'+`mu'/`s')
					)*`dmu2deta1' 
				)) ;
				
			gen double `sc12' = cond($ML_y1==0, 
				(
					exp(`G')*(digamma(`a3')-digamma(`a4'))*`dmudeta1'*`dmudeta2'
				) /
				(
					`s'*(exp(`G')+`gamma'*(1-exp(`G')))^2
				), 
				0) ;
				
			gen double `sc13' = cond($ML_y1==0, 
				(
					(`gamma'-1)*exp(`G')*				
					(-exp(`G')*(`s'*digamma(`a3')-`s'*digamma(`a4')+trigamma(`a3')-trigamma(`a4')+`mu'*(-trigamma(`a3')+trigamma(`a4')))
					+ 
					`gamma'*(
							exp(`G')*(`s'*digamma(`a3')-`s'*digamma(`a4')+trigamma(`a3')-trigamma(`a4'))
							- 1*(
								digamma(`a3')^2+digamma(`a3')*(`s'-digamma(`a2')+digamma(`a1')-2*digamma(`a4'))
								-(`s'-digamma(`a2')+digamma(`a1'))*digamma(`a4') + digamma(`a4')^2 + trigamma(`a3') - trigamma(`a4')
							) 
							+ `mu' * (
								1*(
									digamma(`a3')^2-2*digamma(`a3')*digamma(`a4')+digamma(`a4')^2+trigamma(`a3')-trigamma(`a4')
								) 
								+ exp(`G')*(-trigamma(`a3')+trigamma(`a4'))
							)
						)
					)*`dmudeta1'
				) / ( `s'^2*( exp(`G')+`gamma'*(1-exp(`G')))^2 ), 
				(
					`s'*digamma(`n'-`y'+(1-`mu')/`s')-`s'*digamma(`y'+`mu'/`s')-`s'*digamma((1-`mu')/`s')+
					`s'*digamma(`mu'/`s')+trigamma(`n'-`y'+(1-`mu')/`s')-trigamma((1-`mu')/`s')+
						`mu'*(-trigamma(`n'-`y'+(1-`mu')/`s')-trigamma(`y'+`mu'/`s')+trigamma((1-`mu')/`s')+trigamma(`mu'/`s'))
				)*`dmudeta1'/(`s'^2)) ;


			gen double `sc22'   = cond($ML_y1==0, 
				(
					-(
						-1 + exp(`G')
					)^2*`dmudeta2'^2 + 
					(
						1-exp(`G')
					) * (
						`gamma'-(`gamma'-1)*exp(`G')
					) * `dmu2deta2'
				) / (
					`gamma' - (`gamma'-1)*exp(`G')
				)^2,
				(-`dmudeta2'^2+(`gamma'-1)*`dmu2deta2')/(`gamma'-1)^2) ;

			
			gen double `sc23' = cond($ML_y1==0,
				(
					exp(`G')*(
						-digamma(`a2')+digamma(`a3')+digamma(`a1')-digamma(`a4')+`mu'*(
							-digamma(`a3')+digamma(`a4')
						)
					)*`dmudeta2'
				) / (
					`s'*(
						exp(`G')+`gamma'*(1-exp(`G'))
					)^2
				), 0) ;
				
			gen double `sc33' = cond($ML_y1==0, 
				(
					exp(`G')*(
						-(-1+`gamma')^2*(
							-digamma(`a2')+digamma(`a3')+digamma(`a1')-digamma(`a4')+`mu'*(-digamma(`a3')+digamma(`a4'))
						)^2*exp(`G') 
						+ 
						(1-`gamma')*(`gamma'-(-1+`gamma')*exp(`G'))*(
							- `s'*digamma(`a2') + digamma(`a2')^2 - (-1+`mu')*`s'*digamma(`a3')
							+ 2*(-1+`mu')*digamma(`a2')*digamma(`a3')
							+ (-1+`mu')^2*digamma(`a3')^2 + `s'*digamma(`a1') - 2*digamma(`a2')*digamma(`a1')
							- 2*(-1+`mu')*digamma(`a3')*digamma(`a1')+digamma(`a1')^2
							+ (-1+`mu')*`s'*digamma(`a4') - 2*(-1+`mu')*digamma(`a2')*digamma(`a4')
							- 2*(-1+`mu')^2*digamma(`a3')*digamma(`a4') + 2*(-1+`mu')*digamma(`a1')*digamma(`a4')
							+ (-1+`mu')^2*digamma(`a4')^2-trigamma(`a2')
							+ (-1+`mu')^2*trigamma(`a3')+trigamma(`a1')-(-1+`mu')^2*trigamma(`a4')
						)
					)
				) / (`s'^2*(exp(`G')+`gamma'*(1-exp(`G')))^2), 
				(
					-`s'*digamma(`a2')-(-1+`mu')*`s'*digamma(`n'-`y'+(1-`mu')/`s')
					+`mu'*`s'*digamma(`y'+`mu'/`s')+`s'*digamma(`a1')+(-1+`mu')*`s'*digamma(`a4')
					-`mu'*`s'*digamma(`mu'/`s')-trigamma(`a2')+(-1+`mu')^2*trigamma(`n'-`y'+(1-`mu')/`s')
					+`mu'^2*trigamma(`y'+`mu'/`s')+trigamma(`a1')-(-1+`mu')^2*trigamma(`a4')-`mu'^2*trigamma(`mu'/`s')
				)/(`s'^2)) ;
			#delimit cr
		}
			
		tempname d11 d12 d13 d22 d23 d33
		mlmatsum `lnf' `d11' = `sc11', eq(1)
		mlmatsum `lnf' `d12' = `sc12', eq(1,2)
		mlmatsum `lnf' `d13' = `sc13', eq(1,3)
		mlmatsum `lnf' `d22' = `sc22', eq(2)
		mlmatsum `lnf' `d23' = `sc23', eq(2,3)
		mlmatsum `lnf' `d33' = `sc33', eq(3)
		
		matrix `H' = (`d11', `d12', `d13' \ `d12'', `d22', `d23' \ `d13'', `d23'', `d33')
	}
end
