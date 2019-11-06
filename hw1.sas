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

filename report "Report.pdf";
ods pdf file=report;

/**************************************************/
/*	Data Exploration
/**************************************************/

/*	Percentage of Pumps that Survive and Failures within Their Reasons	*/
proc freq data=hurricane;
	tables reason*survive;

run;

/*	Average Failure Time for Each Type of Failure	*/
ods select moments;
proc univariate data=hurricane;
	class reason;
	var hour;

run;

/*	Comparison of Means	*/
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
proc lifetest data=hurricane;
	time hour * survive(1);
	ods output ProductLimitEstimates=est;

run;

proc sort data=est;
	by stratum hour descending left;

run;

data lib.est (rename=(surv=survival));
	retain surv;
	set est;
	by stratum hour descending left;
	drop survival;

	if not missing(survival) then surv = survival;
	else surv = surv;

run;

/*	Survival probability across time for pumps 
	broken down by failure type overlaid into one graph. */
proc lifetest data=hurricane;
	time hour * survive(1);
	strata reason;
	ods output ProductLimitEstimates=est_strata;

run;

proc sort data=est_strata;
	by stratum hour descending left;

run;

data lib.est_strata (rename=(surv=survival));
	retain surv;
	set est_strata;
	by stratum hour descending left;
	drop survival;

	if not missing(survival) then surv = survival;
	else surv = surv;

run;

/*	Conditional failure probabilities across time for all 
	pumps together – not broken down by failure type. */
proc lifetest data=hurricane method=life width=1 plots=(hazard);
	time hour * survive(1);
	ods output LifetableEstimates=condprob;

run;

/*	Conditional failure probabilities across time for pumps 
	broken down by failure type overlaid into one graph. */
proc lifetest data=hurricane method=life width=1 plots=(hazard);
	time hour * survive(1);
	strata reason;
	ods output LifetableEstimates=condprob_strata;

run;

data lib.condprob;
	set condprob;

	cum_sum + condprobfail;

run;

proc lifetest data=hurricane method=life width=1 plots=(hazard);
	strata reason;
	time hour * survive(1);
	ods output LifetableEstimates=condprob_strata;

run;

data lib.condprob_strata;
	set condprob_strata;

	cum_sum + condprobfail;

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

ods pdf close;
