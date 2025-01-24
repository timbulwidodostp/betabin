{smcl}
{* *! version 1.0.0  20may2012}{...}
{hi:help zibbin}{right: ({browse "http://www.stata-journal.com/article.html?article=up0058":SJ18-1: st0337_1})}
{hline}

{title:Title}

{p2colset 5 15 17 2}{...}
{p2col :{hi:zibbin} {hline 2}}Zero-inflated beta-binomial regression{p_end}
{p2colreset}{...}


{marker syntax}{...}
{title:Syntax}

{p 8 15 2}
{cmd:zibbin} {depvar} [{indepvars}] {ifin} {weight}{cmd:,}{break}
   {opt inf:late}{cmd:(}{varlist}[{cmd:,} {opth off:set(varname)}]|{cmd:_cons)}
   {cmd:n(}{it:varname_n}{cmd:)}
   [{it:options}]

{synoptset 28 tabbed}{...}
{synopthdr:options}
{synoptline}
{p2coldent :* {opt inf:late()}}independent variables that determine whether the number of successes is
zero{p_end}
{p2coldent :* {opt n(varname_n)}}binomial denominator; you may also specify a scalar as the argument{p_end}
{synopt :{opt nocon:stant}}suppress constant term{p_end}
{synopt :{opt exp:osure(varname_e)}}include {opt ln(varname_e)} in model with
coefficient constrained to 1{p_end}
{synopt :{opt off:set(varname_o)}}include {it:varname_o} in model with
coefficient constrained to 1{p_end}
{synopt :{cmdab:const:raints(}{it:{help estimation options##constraints():constraints}}{cmd:)}}apply specified linear constraints{p_end}
{synopt :{opt col:linear}}keep collinear variables{p_end}
{synopt :{opt zib}}include likelihood-ratio test comparing zero-inflated beta-binomial to zero-inflated binomial (tests sigma=0){p_end}
{synopt :{opt l:ink(linkname)}}specify the link function for the binomial model ({cmd:logit}, {cmd:probit}, {cmd:loglog}, {cmd:cloglog}){p_end}
{synopt :{opt il:ink(linkname)}}specify the link function for the inflation model ({cmd:logit}, {cmd:probit}, {cmd:loglog}, {cmd:cloglog}){p_end}
{synopt :{opth vce(vcetype)}}{it:vcetype} may be {opt oim},
  {opt r:obust}, {opt cl:uster} {it:clustvar}, {opt opg}, {opt boot:strap},
  or {opt jack:knife}{p_end}
{synopt :{opt l:evel(#)}}set confidence level; default is {cmd:level(95)}{p_end}
{synopt :{opt eform}}report exponentiated coefficients in the binomial model{p_end}
{synopt :{opt nocnsr:eport}}do not display constraints{p_end}
{synopt :{it:{help zib##display_options:display_options}}}control
INCLUDE help shortdes-displayoptall
{synopt :{it:{help zibbin##maximize_options:maximize_options}}}control the maximization process; seldom used{p_end}

INCLUDE help shortdes-coeflegend
{synoptline}
{p2colreset}{...}
{p 4 6 2}
* {opt inf:late}{cmd:(}{it:varlist}[{cmd:,} {opt off:set(varname)}]|{cmd:_cons)} and {opt n:}{cmd:(}{it:varname_n}{cmd:)} are required.{p_end}
INCLUDE help fvvarlist2
{p 4 6 2}{cmd:bootstrap}, {cmd:by}, {cmd:jackknife}, {cmd:rolling},
{cmd:statsby}, and {cmd:svy} are allowed; see {help prefix}.{p_end}
{p 4 6 2}Weights are not allowed with the {helpb bootstrap} prefix.{p_end}
{p 4 6 2}
{opt vce()} and weights are not allowed with the {helpb svy}
prefix.{p_end}
{p 4 6 2}{cmd:fweight}s, {cmd:iweight}s, and {cmd:pweight}s are allowed; see
{help weight}.{p_end}
{p 4 6 2}See {help zibbin postestimation} for features available after
estimation.{p_end}


{marker description}{...}
{title:Description}

{pstd}
{cmd:zibbin} estimates a zero-inflated beta-binomial regression of
{it:depvar} on {it:indepvars}, where {it:depvar} is a nonnegative count
variable of the number of successes.


{marker options}{...}
{title:Options}

{phang}
{cmd:inflate(}{varlist}[{cmd:,} {cmd:offset(}{varname}{cmd:)}]|{cmd:_cons)}
specifies the equation that determines whether the observed outcome is zero.
Conceptually, omitting {opt inflate()} would be equivalent to fitting the
model with {helpb glm} with the logit link and the same denominator.
{cmd:inflate()} is required.

{pmore}
{cmd:inflate(}{it:varlist}[{cmd:, offset(}{it:varname}{cmd:)}]{cmd:)}
specifies the variables in the equation.  You may optionally include an
offset for this {it:varlist}.

{pmore}
{cmd:inflate(_cons)} specifies that the equation determining whether the count
is zero contains only an intercept.  To run a zero-inflated model of {depvar}
with only an intercept in both equations, type 
{bind:{cmd:zib} {it:depvar}{cmd:,} {cmd:inflate(_cons)}}.

{phang}
{opt n(varname_n)} specifies the binomial denominator.  You may also specify a
scalar as the argument.  {cmd:n()} is required.

{phang}
{opt noconstant}, {opt exposure(varname_e)}, {opt offset(varname_o)}, 
{opt constraints(constraints)}, {cmd:collinear}; see 
{helpb estimation options:[R] estimation options}.

{phang}
{cmd:zib} includes likelihood-ratio test comparing zero-inflated beta-binomial
to zero-inflated binomial (tests sigma=0).

{phang}
{opt link(linkname)} specifies the link function for the binomial model
({cmd:logit}, {cmd:probit}, {cmd:loglog}, {cmd:cloglog}).

{phang}
{opt ilink(linkname)} specifies the link function for the inflation model
({cmd:logit}, {cmd:probit}, {cmd:loglog}, {cmd:cloglog}).

INCLUDE help vce_asymptall

{phang}
{opt level(#)}; see
{helpb estimation options##level():[R] estimation options}.

{phang}
{opt eform} reports estimated coefficients transformed to exponentiated
coefficients.  Standard errors and confidence intervals are similarly
transformed.  This option affects how results are displayed, not how they are
estimated or stored.  {opt eform} may be specified at estimation or when
replaying previously estimated results.

{phang}
{opt nocnsreport}; see
{helpb estimation options##nocnsreport:[R] estimation options}.

{marker display_options}{...}
{phang}
{it:display_options}:
{opt noomit:ted},
{opt vsquish},
{opt noempty:cells},
{opt base:levels},
{opt allbase:levels},
{opth cformat(%fmt)},
{opt pformat(%fmt)},
{opt sformat(%fmt)}, and
{opt nolstretch};
see {helpb estimation options##display_options:[R] estimation options}.

{phang}
{it:maximize_options}: {opt dif:ficult},
{opth tech:nique(maximize##algorithm_spec:algorithm_spec)}, 
{opt iter:ate(#)}, [{cmd:{ul:no}}]{cmd:{ul:lo}}{cmd:g}, {opt tr:ace}, 
{opt grad:ient}, {opt showstep}, {opt hess:ian}, {opt showtol:erance},
{opt tol:erance(#)}, {opt ltol:erance(#)},
{opt nrtol:erance(#)}, {opt nonrtol:erance}, and {opt from(init_specs)}; see
{manhelp maximize R}.  These options are seldom used.

{pmore}
Setting the optimization type to {cmd:technique(bhhh)} resets the default
{it:vcetype} to {cmd:vce(opg)}.

{phang}
{opt coeflegend}; see
{helpb estimation options##coeflegend:[R] estimation options}.


{marker examples}{...}
{title:Examples}

{pstd}Setup{p_end}
{phang2}{cmd:. set seed 12345}{p_end}
{phang2}{cmd:. set obs 1000}{p_end}
{phang2}{cmd:. generate z1 = uniform() < 0.5}{p_end}
{phang2}{cmd:. generate z2 = uniform() < 0.5}{p_end}
{phang2}{cmd:. generate zg = -1+z1+z2}{p_end}
{phang2}{cmd:. generate z  = rbinomial(1, 1-normprob(zg))}{p_end}
{phang2}{cmd:. generate x1 = uniform() < 0.5}{p_end}
{phang2}{cmd:. generate xb = -1+x1}{p_end}
{phang2}{cmd:. generate n  = floor(8*uniform()) + 1}{p_end}
{phang2}{cmd:. local sigma = 0.5}{p_end}
{phang2}{cmd:. generate p  = exp(xb)/(1+exp(xb))}{p_end}
{phang2}{cmd:. generate y  = rbinomial(n, rbeta((1/`sigma')*p,(1/`sigma')*(1-p)))}{p_end}
{phang2}{cmd:. generate yo = y*z}{p_end}

{pstd}
Fit zero-inflated beta binomial model{p_end}
{phang2}{cmd:. zibbin yo x1, inflate(z1 z2) n(n) link(logit) ilink(probit)}{p_end}

{pstd}
Replay results, displaying odds ratios{p_end}
{phang2}{cmd:. zibbin, eform}{p_end}


{marker saved_results}{...}
{title:Stored results}

{pstd}
{cmd:zibbin} stores the following in {cmd:e()}:

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Scalars}{p_end}
{synopt:{cmd:e(N)}}number of observations{p_end}
{synopt:{cmd:e(N_zero)}}number of zero observations{p_end}
{synopt:{cmd:e(k)}}number of parameters{p_end}
{synopt:{cmd:e(k_eq)}}number of equations in {cmd:e(b)}{p_end}
{synopt:{cmd:e(k_eq_model)}}number of equations in overall model test{p_end}
{synopt:{cmd:e(k_dv)}}number of dependent variables{p_end}
{synopt:{cmd:e(df_m)}}model degrees of freedom{p_end}
{synopt:{cmd:e(ll)}}log likelihood{p_end}
{synopt:{cmd:e(ll_0)}}log likelihood, constant-only model{p_end}
{synopt:{cmd:e(ll_c)}}log likelihood, comparison model (constant only in the binomial part of the model){p_end}
{synopt:{cmd:e(llb)}}log likelihood, comparison model (full binomial without zero-inflation){p_end}
{synopt:{cmd:e(df_c)}}degrees of freedom for comparison test{p_end}
{synopt:{cmd:e(N_clust)}}number of clusters{p_end}
{synopt:{cmd:e(chi2)}}chi-squared{p_end}
{synopt:{cmd:e(p)}}significance of model test{p_end}
{synopt:{cmd:e(rank)}}rank of {cmd:e(V)}{p_end}
{synopt:{cmd:e(ic)}}number of iterations{p_end}
{synopt:{cmd:e(bin_n)}}sum of the numerators of the outcomes{p_end}
{synopt:{cmd:e(bin_d)}}sum of the denominators of the outcomes{p_end}
{synopt:{cmd:e(rc)}}return code{p_end}
{synopt:{cmd:e(converged)}}{cmd:1} if converged, {cmd:0} otherwise{p_end}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Macros}{p_end}
{synopt:{cmd:e(cmd)}}{cmd:zibbin}{p_end}
{synopt:{cmd:e(cmdline)}}command as typed{p_end}
{synopt:{cmd:e(depvar)}}name of dependent variable{p_end}
{synopt:{cmd:e(link)}}name of the link function for the binomial component{p_end}
{synopt:{cmd:e(ilink)}}name of the link function for the inflation component{p_end}
{synopt:{cmd:e(inflate)}}name of the link function for the inflation component{p_end}
{synopt:{cmd:e(wtype)}}weight type{p_end}
{synopt:{cmd:e(wexp)}}weight expression{p_end}
{synopt:{cmd:e(title)}}title in estimation output{p_end}
{synopt:{cmd:e(clustvar)}}name of cluster variable{p_end}
{synopt:{cmd:e(offset1)}}offset{p_end}
{synopt:{cmd:e(offset2)}}offset for {cmd:inflate()}{p_end}
{synopt:{cmd:e(chi2type)}}{cmd:Wald} or {cmd:LR}; type of model chi-squared
	test{p_end}
{synopt:{cmd:e(vce)}}{it:vcetype} specified in {cmd:vce()}{p_end}
{synopt:{cmd:e(vcetype)}}title used to label Std. Err.{p_end}
{synopt:{cmd:e(opt)}}type of optimization{p_end}
{synopt:{cmd:e(which)}}{cmd:max} or {cmd:min}; whether optimizer is to perform
                         maximization or minimization{p_end}
{synopt:{cmd:e(ml_method)}}type of {cmd:ml} method{p_end}
{synopt:{cmd:e(user)}}name of likelihood-evaluator program{p_end}
{synopt:{cmd:e(technique)}}maximization technique{p_end}
{synopt:{cmd:e(properties)}}{cmd:b V}{p_end}
{synopt:{cmd:e(predict)}}program used to implement {cmd:predict}{p_end}
{synopt:{cmd:e(asbalanced)}}factor variables {cmd:fvset} as {cmd:asbalanced}{p_end}
{synopt:{cmd:e(asobserved)}}factor variables {cmd:fvset} as {cmd:asobserved}{p_end}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Matrices}{p_end}
{synopt:{cmd:e(b)}}coefficient vector{p_end}
{synopt:{cmd:e(Cns)}}constraints matrix{p_end}
{synopt:{cmd:e(ilog)}}iteration log (up to 20 iterations){p_end}
{synopt:{cmd:e(gradient)}}gradient vector{p_end}
{synopt:{cmd:e(V)}}variance-covariance matrix of the estimators{p_end}
{synopt:{cmd:e(V_modelbased)}}model-based variance{p_end}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Functions}{p_end}
{synopt:{cmd:e(sample)}}marks estimation sample{p_end}
{p2colreset}{...}


{title:Author}

{pstd}James W. Hardin{p_end}
{pstd}Institute for Families in Society{p_end}
{pstd}Department of Epidemiology and Biostatistics{p_end}
{pstd}University of South Carolina{p_end}
{pstd}Columbia, SC{p_end}
{pstd}jhardin@sc.edu{p_end}


{title:Also see}

{p 4 14 2}Article:  {it:Stata Journal}, volume 18, number 1: {browse "http://www.stata-journal.com/article.html?article=up0058":st0337_1},{break}
                    {it:Stata Journal}, volume 14, number 2: {browse "http://www.stata-journal.com/article.html?article=st0337":st0337}

{p 7 14 2}Help:  {help zibbin postestimation} (if installed), 
{manhelp nbreg R}, {manhelp poisson R}, 
{manhelp svy_estimation SVY:svy estimation},
{manhelp tnbreg R}, {manhelp tpoisson R},
{manhelp xtpoisson XT}, {manhelp zinb R}{p_end}
