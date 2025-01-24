{smcl}
{* *! version 1.0.0  20dec2012}{...}
{cmd:help betabin postestimation}{right: ({browse "http://www.stata-journal.com/article.html?article=up0058":SJ18-1: st0337_1})}
{hline}

{title:Title}

{p2colset 5 31 37 2}{...}
{p2col :{cmd:betabin postestimation} {hline 2}}Postestimation tools for beta-binomial 
regression models{p_end}
{p2colreset}{...}


{marker description}{...}
{title:Description}

{pstd}
The following postestimation commands are available after {cmd:betabin}:

{synoptset 17 notes}{...}
{p2coldent :Command}Description{p_end}
{synoptline}
INCLUDE help post_contrast
INCLUDE help post_estatic
INCLUDE help post_estatsum
INCLUDE help post_estatvce
INCLUDE help post_svy_estat
INCLUDE help post_estimates
INCLUDE help post_lincom
INCLUDE help post_linktest
INCLUDE help post_lrtest_star
INCLUDE help post_margins
INCLUDE help post_marginsplot
INCLUDE help post_nlcom
INCLUDE help post_predictnl
INCLUDE help post_pwcompare
INCLUDE help post_suest
INCLUDE help post_test
INCLUDE help post_testnl
{synoptline}
{p2colreset}{...}
INCLUDE help post_lrtest_star_msg


{marker syntax_predict}{...}
{marker predict}{...}
{title:Syntax for predict}

{p 8 16 2}
{cmd:predict} {dtype} {newvar} {ifin} [{cmd:,} {it:statistic}
{opt nooff:set}]

{p 8 16 2}
{cmd:predict} {dtype} {c -(}{it:stub}{cmd:*}{c |}{it:{help newvar:newvar_reg}}
 {it:{help newvar:newvar_disp}}{c )-}
 {ifin}{cmd:,} {opt sc:ores}

{synoptset 11}{...}
{synopthdr :statistic}
{synoptline}
{synopt :{cmd:pr}}predicted probability of success; the default{p_end}
{synopt :{cmd:n}}predicted number of successes{p_end}
{synopt :{cmd:xb}}linear prediction{p_end}
{synopt :{cmd:stdp}}standard error of the linear prediction{p_end}
{synoptline}
{p2colreset}{...}
INCLUDE help esample


{marker options_predict}{...}
{title:Options for predict}

{phang}
{opt pr} calculates the predicted probability of success, which is g(xb) if
neither {opt offset(varname_o)} nor {opt exposure(varname_e)} was
specified when the model was fit; g(xb + offset) if {opt offset()} was
specified.  Here g() is the link function specified.

{phang}
{opt n} calculates the predicted number of successes, which is N*g(xb) if
neither {opt offset(varname_o)} nor {opt exposure(varname_e)} was
specified when the model was fit; N*g(xb + offset) if {opt offset()} was
specified.  Here g() is the link function specified and N is the binomial
denominator.

{phang}
{opt xb} calculates the linear prediction.

{phang}
{opt stdp} calculates the standard error of the linear prediction.

{phang}
{opt nooffset} is relevant only if you specified {opt offset()} or
{cmd:exposure()} when you fit the model.  It modifies the calculations made by
{cmd:predict} so that they ignore the offset or exposure variable; the linear
prediction is treated as xb rather than as xb + offset or xb + ln(exposure_j).

{phang}
{opt scores} calculates equation-level score variables.

{pmore}The first new variable will contain the derivative of the log
likelihood with respect to the regression equation.

{pmore}The second new variable will contain the derivative of the log
likelihood with respect to the dispersion equation.


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

{p 7 14 2}Help:  {helpb betabin} (if installed){p_end}
