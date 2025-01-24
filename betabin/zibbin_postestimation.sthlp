{smcl}
{* *! version 1.0.0  20nov2012}{...}
{hi:help zibbin postestimation}{right: ({browse "http://www.stata-journal.com/article.html?article=up0058":SJ18-1: st0337_1})}
{hline}

{title:Title}

{p2colset 5 30 32 2}{...}
{p2col :{hi:zibbin postestimation} {hline 2}}Postestimation tools for zibbin
{p_end}
{p2colreset}{...}


{marker description}{...}
{title:Description}

{pstd}
The following postestimation commands are available after
{cmd:zibbin}:

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
INCLUDE help post_lrtest_star
INCLUDE help post_margins
INCLUDE help post_marginsplot
INCLUDE help post_nlcom
{synopt :{helpb zibbin postestimation##predict:predict}}predictions, residuals,
influence statistics, and other diagnostic measures{p_end}
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

{p 8 18 2}
{cmd:predict} {dtype} {newvar} {ifin}
[{cmd:,} {it:statistic} {opt nooff:set}]

{p 8 18 2}
{cmd:predict} {dtype} {{it:stub}{cmd:*}|{it:{help newvar:newvar_reg}}
   {it:{help newvar:newvar_inflate}}}
   {ifin}{cmd:,} {opt sc:ores}

{marker statistic}{...}
{synoptset 11}{...}
{synopthdr:statistic}
{synoptline}
{synopt :{opt n}}number of successes; the default{p_end}
{synopt :{opt ir}}incidence rate{p_end}
{synopt :{opt p:r}}probability of a degenerate zero{p_end}
{synopt :{opt xb}}linear prediction{p_end}
{synopt :{opt stdp}}standard error of the linear prediction{p_end}
{synoptline}
{p2colreset}{...}
INCLUDE help esample


{marker options_predict}{...}
{title:Options for predict}

{phang}
{opt n}, the default, calculates the predicted number of successes, which is
(1-p_j)*N*exp(x_j b)/{1+exp(x_j b)} if neither {cmd:offset()} nor
{cmd:exposure()} was specified when the model was fit, where p_j is the
predicted probability of a zero outcome; (1-p_j)*N*exp{(x_j b +
offset_j)/(1+exp(x_j b + offset_j)) + offset_j} if {opt offset()} was
specified; or {bind:(1-p_j)*N*{exp(x_j b) * exposure_j}}/{1+exp(x_j
b)*exposure_j} if {opt exposure()} was specified.

{phang}
{cmd:ir} calculates the incidence rate exp(xb), which is the predicted number
of events when exposure is 1.  This is equivalent to specifying both the
{cmd:n} and the {cmd:nooffset} options.

{phang}
{opt pr} calculates the probability Pr(y = 0), where this 0 was obtained from
the degenerate distribution F(zg).  If {opt offset()} was specified within the
{opt inflate()} option, then F(zg + offset^g) is calculated.

{phang}
{opt xb} calculates the linear prediction, which is xb if neither
{cmd:offset()} nor {opt exposure()} was specified; {bind:xb + offset} if
{cmd:offset()} was specified; or {bind:xb + ln(exposure)} if {cmd:exposure()}
was specified; see {opt nooffset} below.

{phang}
{opt stdp} calculates the standard error of the linear prediction.

{phang}
{opt nooffset} is relevant only if you specified {opt offset()} or
{cmd:exposure()} when you fit the model.  It modifies the calculations made by
{cmd:predict} so that they ignore the offset or exposure variable; the linear
prediction is treated as xb rather than as {bind:xb + offset} or 
{bind:xb + ln(exposure)}.  Specifying {bind:{cmd:predict} ...{cmd:, nooffset}}
is equivalent to specifying {bind:{cmd:predict} ...{cmd:, ir}}.

{phang}
{opt scores} calculates equation-level score variables.

{pmore}
The first new variable will contain the derivative of the log likelihood with
respect to the regression equation.

{pmore}
The second new variable will contain the derivative of the log likelihood with
respect to the inflation equation.


{marker examples}{...}
{title:Examples}

{pstd}Setup{p_end}
{phang2}{cmd:. set seed 12345}{p_end}
{phang2}{cmd:. set obs 1000}{p_end}
{phang2}{cmd:. generate z1 = uniform() < 0.5}{p_end}
{phang2}{cmd:. generate z2 = uniform() < 0.5}{p_end}
{phang2}{cmd:. generate zg = -1+z1+z2}{p_end}
{phang2}{cmd:. generate z  = rbinomial(1, 1/(1+exp(zg)))}{p_end}
{phang2}{cmd:. generate x1 = uniform() < 0.5}{p_end}
{phang2}{cmd:. generate xb = -1+x1}{p_end}
{phang2}{cmd:. generate n  = floor(8*uniform()) + 1}{p_end}
{phang2}{cmd:. generate p  = exp(xb)/(1+exp(xb))}{p_end}
{phang2}{cmd:. local sigma=0.5}{p_end}
{phang2}{cmd:. generate y  = rbinomial(n, rbeta((1/`sigma')*p,(1/`sigma')*(1-p)))}{p_end}
{phang2}{cmd:. generate yo = y*z}{p_end}

{pstd}Fit zero-inflated binomial model{p_end}
{phang2}{cmd:. zibbin yo x1, inflate(z1 z2) n(n)}{p_end}

{pstd}Get predicted values{p_end}
{phang2}{cmd:. predict yo_hat, pr}{p_end}


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

{p 7 14 2}Help:  {helpb zibbin} (if installed){p_end}
