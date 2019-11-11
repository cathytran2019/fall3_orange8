***************************************************
*
*	Program Name: hw1.sas
*	Program Purpose: Homework 1
*
*	Created By: Lance Hudson
*	Creation Date: 01NOV2019
*
*	Notes:
*
***************************************************
*
*	Modified By:
*	Modification Date:
*
*	Changes:
*
***************************************************;

/*	Clear Work Directory and Setup Libraries  */
proc datasets lib=work nolist kill;
run;
quit;

libname lib "C:\Users\Admin\Documents\AA502\Survival Analysis\Homework\HW1";

/**************************************************/
/*	Import Data
/**************************************************/

data hurricane;
	set lib.hurricane;

	if survive = 1 & reason = 0 then reason = .;

run;

/**************************************************/
/*	Data Exploration
/**************************************************/

/*	Percentage of Pumps that Survive */
proc freq data=hurricane;
	tables survive / nocum norow nocol;

run;

/*	Percentage of Pumps that Survive and Failures within Their Reasons	*/
proc freq data=hurricane;
	tables reason / nocum norow nocol;

run;

/*	Comparison of Means	*/
ods select OverallANOVA means;
proc glm data=hurricane;
	class reason;
	model hour=reason;
	lsmeans reason / diff adjust=tukey; 
	means reason / hovtest=levene;

run;
quit;

/*	Comparison of Ranks and Medians	*/
proc npar1way data=hurricane wilcoxon median;
	class reason;
	var hour;

run;
quit;

/**************************************************/
/*	Graphs
/**************************************************/

/*	Survival probability across time 
	for all pumps together – not broken down by failure type. */
proc lifetest data=hurricane plots=s;
	time hour * survive(1);
	ods output ProductLimitEstimates=est survivalplot=plot;

run;

/*	Survival probability across time for pumps 
	broken down by failure type overlaid into one graph. */
proc lifetest data=hurricane plots=s;
	time hour * survive(1);
	strata reason;
	ods output ProductLimitEstimates=est_strata survivalplot=plot_strata;

run;

/*	Conditional failure probabilities across time for all 
	pumps together – not broken down by failure type. */
proc lifetest data=hurricane method=life width=1 plots=(hazard);
	time hour * survive(1);
	ods output LifetableEstimates=condprob;

run;

data condprob;
	set condprob;
	keep lowertime uppertime stratum cum_sum condprobfail;

	cum_sum + condprobfail;

	if missing(uppertime) then delete;

run;

/*	Conditional failure probabilities across time for pumps 
	broken down by failure type overlaid into one graph. */
proc lifetest data=hurricane method=life width=1 plots=(hazard);
	time hour * survive(1);
	strata reason;
	ods output LifetableEstimates=condprob_strata;

run;

data condprob_strata;
	set condprob_strata;
	keep lowertime uppertime stratum cum_sum condprobfail;

	cum_sum + condprobfail;

	if missing(uppertime) then delete;

run;

/*	Provide a statistical test if the major types of failure 
	have similar survival probabilities across time. */
ods select HomTests;
proc lifetest data=hurricane;
	strata reason;
	time hour * survive(1);

run;

/*	Currently, the Committee groups failures into two 
	groups – water-based (flood/surge) and mechanical (motor/jammed). 
	Do you agree with this grouping? 
	If not, please provide your own groupings (if any).*/
ods select SurvivalPlot;
proc lifetest data=hurricane;
	time hour * survive(1);
	strata reason;

run;

/*	Export datasets for graphing  */
proc datasets nolist;
	copy in=work out=lib;
	select condprob condprob_strata plot plot_strata;

run;
quit;


