program define zib_llf
	args todo b lnf g H sc1 sc2
	quietly {
		tempvar eta1 eta2
		mleval `eta1' = `b', eq(1)
		mleval `eta2' = `b', eq(2)

		local n "$ZIB_n"
		if "`n'" == "" {
			local n = 1
		}
		
		// We are using the GLM programs to handle the links, inverse links, and their derivatives. 
		// LOGIT :  glim_l02
		// PROBIT:  glim_l08
		// LOGLOG:  glim_l06
		// CLOGLOG: glim_l07

		
		tempvar mu gamma
		glim_l0$ZIB_L 1 `eta1' `mu'
		glim_l0$ZIB_I 1 `eta2' `gamma' 
		
		local y "$ML_y1"
		
		#delimit ;
		mlsum `lnf' = cond($ML_y1==0,													
				/* y=0  */	ln(`gamma' + (1-`gamma')*(1-`mu')^`n'), 						
				/* y!=0 */	ln(1-`gamma') + (`n'-`y')*ln(1-`mu') + `y'*ln(`mu') +		
								lngamma(1+`n') - lngamma(1+`n'-`y') - lngamma(1+`y')	
			) ;
		#delimit cr
		
		if (`todo'==0 | `lnf'>=.) exit
	
		tempvar dmu dgamma
		glim_l0$ZIB_L 2 `eta1' `mu'    `dmu'
		glim_l0$ZIB_I 2 `eta2' `gamma' `dgamma'
	

		#delimit ;
		replace `sc1' = cond($ML_y1==0, 
				/* y=0  */	-`n'*(1-`gamma')*(1-`mu')^(-1+`n')*`dmu' / (`gamma'-(-1+`gamma')*(1-`mu')^`n'), 
				/* y!=0 */	(-`y'+`n'*`mu')*`dmu' / (`mu'*(`mu'-1))
			) ;
			
		replace `sc2' = cond($ML_y1==0, 
				/* y=0  */	-(-1+(1-`mu')^`n')*`dgamma' / (`gamma'-(-1+`gamma')*(1-`mu')^`n'),
				/* y!=0 */	`dgamma'/(-1+`gamma')
			) ;
		#delimit cr
		
		tempname d1 d2
		mlvecsum `lnf' `d1' = `sc1', eq(1)
		mlvecsum `lnf' `d2' = `sc2', eq(2)
		
		matrix `g' = (`d1', `d2')
		
		if (`todo'==1 | `lnf'>=.) exit
		
		tempvar sc12 t1 t2 dmu2 dgamma2
		glim_l0$ZIB_L 3 `eta1' `mu'    `dmu2'
		glim_l0$ZIB_I 3 `eta2' `gamma' `dgamma2'
		
		#delimit ;
		gen double `t1'   = cond($ML_y1==0, 
				/* y=0  */	(`n'*(-1+`gamma')*(1-`mu')^(-2+`n')* 
								(
									(
										-`gamma'*(-1+`n'+(1-`mu')^`n')+(1-`mu')^`n'
									)*`dmu'^2 + 
									(
										`gamma'*(-1+(1-`mu')^`n') - (1-`mu')^`n'
									)*(-1+`mu')*`dmu2'
								)
							) / (`gamma'+(1-`mu')^`n'-`gamma'*(1-`mu')^`n')^2,
				/* y!=0 */ ((-`y'+2*`y'*`mu'-`n'*`mu'^2)*`dmu'^2+(-1+`mu')*`mu'*(-`y'+`n'*`mu')*`dmu2') / 
								((-1+`mu')*`mu')^2 
			) ;
		gen double `sc12' = cond($ML_y1==0, 
				/* y=0  */	(`n'*(1-`mu')^(-1+`n')*`dgamma'*`dmu') / (`gamma'+(1-`mu')^`n'-`gamma'*(1-`mu')^`n')^2, 
				/* y!=0 */	0
			) ;
		gen double `t2'   = cond($ML_y1==0, 
				/* y=0  */	(-1+(1-`mu')^`n')*(-(-1+(1-`mu')^`n')*`dgamma'^2-(`gamma'-(-1+`gamma')*(1-`mu')^`n')*`dgamma2') / 
								(`gamma'-(-1+`gamma')*(1-`mu')^`n')^2, 
				/* y!=0 */	(-`dgamma'^2+(-1+`gamma')*`dgamma2')/(-1+`gamma')^2
			) ;
		#delimit cr
		
		tempname d11 d12 d22
		mlmatsum `lnf' `d11' = `t1'  , eq(1)
		mlmatsum `lnf' `d12' = `sc12', eq(1,2)
		mlmatsum `lnf' `d22' = `t2'  , eq(2)
		
		matrix `H' = (`d11', `d12' \ `d12'', `d22')
	}
end
