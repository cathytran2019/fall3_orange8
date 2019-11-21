/*Cathy Tran*/
/*Survival Analysis HW3*/
/*Nov. 19, 2019*/

/*Create libname*/
libname sa '\\vmware-host\Shared Folders\Documents\Fall III\Survival Analysis\Homework1_SA\';

/*Import dataset*/
PROC IMPORT DATAFILE= "\\vmware-host\Shared Folders\Documents\Fall III\Survival Analysis\Homework1_SA\new_df.csv"
            DBMS=csv
			OUT= hurricane REPLACE;
     		GETNAMES=YES;
RUN;

/*NOTE: 173 missing obs*/

/*Find out if motor failure is more likely if the motor is constantly running */
/*for 12 hours before the motor has failed */

/* Create a new variable that indicates whether the target failed due to flood or not*/
data hurricane; 
	set hurricane;
	if reason = 2 then motor_survive = 0;
	else motor_survive = 1;
	sq_slope = slope**2;
	cub_slope = slope**3;
	sq_age = age**2;
	cub_age = age**3;
run;


/*-----------------------------*/
/* Check assumptions */
/*-----------------------------*/

/* Linearity */
/* only include main effects*/
/* assess var= statement - include only cont. vars*/

/* H0: predictors are linearly correlated with target*/
/* age p-value's fluctuate pending on seed (0.0260 & 0.0340). Airing on caution - age violated linearity assumption
/* slope violated linearity assumption*/
proc phreg data = hurricane;
	model hour*motor_survive(1) = age backup bridgecrane elevation gear servo slope trashrack / ties=efron;
	assess var=(age slope) / resample;
run;

/* Fix linearity issues*/
/* Look at the solid line from the above phreg for which transformation to use.*/
/* Went with cub_slope and cub_age*/
proc phreg data = hurricane;
	model hour*motor_survive(1) = age sq_age cub_age backup bridgecrane elevation gear servo slope sq_slope cub_slope trashrack / ties=efron;
	assess var=(age sq_age cub_age slope sq_slope cub_slope) / resample;
run;

/* Proportional Hazard Test - Martingale Residuals */
/* H0: meets PH assumption*/
/* wants high p-value*/
/* trashrack failed PH assumption - needs transformation*/
proc phreg data=hurricane;
	model hour*motor_survive(1) = backup bridgecrane elevation gear servo trashrack / ties=efron;
		assess ph / resample;
run;

/* Proportional Hazard Test - Schoenfeld Residuals */
/* needs to transform trashrack*/
proc phreg data = hurricane zph(global fit=loess);
	model hour*motor_survive(1) = backup bridgecrane elevation gear servo trashrack / ties = efron;
run;

/* NOTE: 
/* Transformation - pick the highest correlation and lowest p-value*/
/* since we don't know how to do KM yet, picked IDENTITY to transform trash rack*/
proc phreg data=hurricane;
	model hour*motor_survive(1) = backup bridgecrane elevation gear servo trashrack trashrackhr/ ties=efron;
	trashrackhr = trashrack*hour;
run;

/* After transformation, all p-values are insignificant. Meet PH assumption*/
proc phreg data = hurricane zph(global transform=km fit=loess);
	model hour*motor_survive(1) = age backup bridgecrane elevation gear servo slope trashrack / ties = efron;
	ods output zphTest = PH_km;
run;

proc phreg data = hurricane zph(global transform=identity fit=loess);
	model hour*motor_survive(1) = age backup bridgecrane elevation gear servo slope trashrack / ties = efron;
	ods output zphTest = PH_t;
run;

proc phreg data = hurricane zph(global transform=log fit=loess);
	model hour*motor_survive(1) = age backup bridgecrane elevation gear servo slope trashrack / ties = efron;
	ods output zphTest = PH_log;
run;

data PH_comb;
	set PH_km PH_t PH_log;
run;

proc print data=PH_comb;
run;

/* Time Varying Variables */
/* running is not significant*/
proc phreg data=hurricane;
	model hour*motor_survive(1) = age sq_age cub_age backup bridgecrane elevation gear servo slope sq_slope cub_slope trashrack running;
	array h(*) h1-h48;
	if h[hour-1] + h[hour-2]+ h[hour-3] + h[hour-4] + h[hour-5] + h[hour-6]+ h[hour-7] + h[hour-8]+ h[hour-9]
		+ h[hour-10] + h[hour-11] + h[hour-12]= 12 then running =1; else running = 0;
run;


/*Factors drive differences in failure time between groups*/
/*Backward automatic selection technique*/

/* significant: age sq_age cub_age, servo, cub_slope*/
/* bc of hierarchy, also need to include slope, sq_slope*/
proc phreg data=hurricane;
	model hour*motor_survive(1) = age sq_age cub_age backup bridgecrane elevation gear servo slope sq_slope cub_slope trashrack trashrackhr/ ties=efron selection=backward;
	trashrackhr = trashrack*hour;
run;

/* most significant var: servo bc there's no interpretability with age and slope since we transformed them*/
