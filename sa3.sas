/*Cathy Tran*/
/*Survival Analysis HW3*/
/*Nov. 19, 2019*/

/*Create libname*/
libname sa '\\vmware-host\Shared Folders\Documents\Fall III\Survival Analysis\Homework1_SA\';

/*Import dataset*/
PROC IMPORT DATAFILE= "\\vmware-host\Shared Folders\Documents\Fall III\Survival Analysis\Homework1_SA\hurricane.xlsx"
            DBMS=xlsx
			OUT= hurricane REPLACE;
     		GETNAMES=YES;
RUN;

/*NOTE: 173 missing obs*/

/*Find out if motor failure is more likely if the motor is constantly running */
/*for 12 hours before the motor has failed */

/*not sure which test I should*/

/* Time Varying Variables */
/* significant: age, slope, running*/
/*ERROR: An ARRAY index is out of bounds in statement number 1 at line 325 column 5*/
proc phreg data=hurricane;
	model hour*new_survive(1) = backup age bridgecrane servo gear trashrack slope
								elevation running;
	array h(*) h1-h48;
	running = h[hour]+ h[hour-11];
run;


/*Factors drive differences in failure time between groups*/
/*Backward automatic selection technique*/
/*Significant variables: age, slope, h19, h33*/

proc phreg data=hurricane;
	model hour*new_survive(1) = backup age bridgecrane servo gear trashrack slope
							elevation h1-h48/ ties=efron selection=backward;
run;

/* Linearity */
/*Should var running be included in the linearity check? seems to be yes based on the hint*/
/*Should all vars in the model statement be in the assess var= statement?*/


/*trashrack, elevation failed linearity assumption*/
proc phreg data = hurricane;
	model hour*new_survive(1) = age servo trashrack slope elevation running / ties=efron;
	array h(*) h1-h48;
	running = h[hour]+ h[hour-11];
	assess var=(age servo trashrack slope elevation running) / resample;
run;

/*w/o running*/
/*trashrack, elevation failed linearity assumption*/
proc phreg data = hurricane;
	model hour*new_survive(1) = age servo trashrack slope elevation / ties=efron;
	assess var=(age servo trashrack slope elevation) / resample;
run;


/* Proportional Hazard Test - Martingale Residuals */
/*trashrack, elevation failed Martingale Residuals assumption*/
proc phreg data=hurricane;
	model hour*new_survive(1) = age servo trashrack slope elevation running / ties=efron;
	array h(*) h1-h48;
	running = h[hour]+ h[hour-11];
	assess ph / resample;
run;

/*w/o running*/
/*trashrack, elevation failed Martingale assumption*/
proc phreg data=hurricane;
	model hour*new_survive(1) = age servo trashrack slope elevation / ties=efron;
	assess ph / resample;
run;

/* Proportional Hazard Test - Schoenfeld Residuals */
/*not seeing a table w/ correlation*/
ods graphics on;
proc phreg data = hurricane zph(global transform=km fit=loess);
	model hour*new_survive(1) = age servo trashrack slope elevation / ties = efron;
	ods output zphTest = PH_km;
run;

ods output zphTest = PH_km;
proc phreg data = hurricane zph(global transform=identity fit=loess);
	model hour*new_survive(1) = age servo trashrack slope elevation / ties = efron;
	ods output zphTest = PH_t;
run;

proc phreg data = hurricane zph(global transform=log fit=loess);
	model hour*new_survive(1) = age servo trashrack slope elevation / ties = efron;
	ods output zphTest = PH_log;
run;

data PH_comb;
	set PH_km PH_t PH_log;
run;

proc print data=PH_comb;
run;

/*Extraneous code*/
/* Proportional Hazards Model */
/*Significant variables (alpha: 0.03)- age, slope, elevation, h6, h9, h19*/

proc phreg data=hurricane;
	model hour*new_survive(1) = backup age bridgecrane servo gear trashrack slope
								elevation h1-h48/ ties=efron risklimits=pl;
run;

/*Significant variables (alpha: 0.03)- age, servo, slope*/
proc phreg data=hurricane;
	model hour*new_survive(1) = backup age bridgecrane servo gear trashrack slope
								elevation / ties=efron risklimits=pl;
run;


proc phreg data=hurricane;
    model hour*Motor_Failure(0) =age servo slope failed;
    array H(*) H1-H48;    
failed=H[hour] + (H[hour]-1) ;
run;
