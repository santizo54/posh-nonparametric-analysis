/* LOESS regression with degree 1 */

proc loess data=rdata;
	model m_net_earnings=month/clm alpha=0.05;
		ods output OutputStatistics=results
run;

symbol1 color=black value=dot;
symbol2 color=black value=none interpol=join line=1;
symbol3 color=black value=none interpol=join line=2;
symbol4 color=black value=none interpol=join line=2;

proc gplot data=results;
	by SmoothingParameter;
		plot (DepVar Pred LowerCL UpperCL)*month/ overlay;
run;

/* LOESS regression with degree 2 */

proc loess data=rdata;
	model m_net_earnings=month/degree=2 clm alpha=0.05;
		ods output OutputStatistics=results;
run;

proc gplot data=results;
	by SmoothingParameter;
		plot (DepVar Pred LowerCL UpperCL)*month/ overlay;
run;

/* LOESS regressions (degree 2) with different smoothing parameters */

proc loess data=rdata;
	model m_net_earnings=month/degree=2 clm smooth =0.2 0.3 0.4 0.5;
		ods output OutputStatistics=results;
run;

proc gplot data=results;
	by SmoothingParameter;
		plot (DepVar Pred LowerCL UpperCL)*month/ overlay name='graph';
run;

goption display;
proc greplay nofs tc=sashelp.templt template=l2r2;
	igout gseg;
		treplay 1:graph 2:graph2 3:graph1 4:graph3;
run;
quit;

/*SPLINE ESTIMATION PROCEDURE , M=2*/

proc tpspline data=rdata;
	model m_net_earnings=(month)/m=2;
		output out=result pred lclm uclm;
run;

title 'm=2';
symbol1 color=black value=dot;
symbol2 color=black value=none interpol=join line=1;
symbol3 color=black value=none interpol=join line=2;
symbol4 color=black value=none interpol=join line=2;

proc gplot data=result;
	plot (m_net_earnings p_m_net_earnings lclm_m_net_earnings uclm_m_net_earnings)*month/
		overlay name='graph2';
run;

/*SPLINE ESTIMATION PROCEDURE , M=3*/

proc tpspline data=rdata;
	model m_net_earnings=(month)/m=3;
		output out=result pred lclm uclm;
run;

title 'm=3';
proc gplot data=result;
	plot (m_net_earnings p_m_net_earnings lclm_m_net_earnings uclm_m_net_earnings)*month/
		overlay name='graph3';
run;

/*SPLINE ESTIMATION PROCEDURE , M=4*/

proc tpspline data=rdata;
	model m_net_earnings=(month)/m=4;
		output out=result pred lclm uclm;
run;

title 'm=4';
proc gplot data=result;
	plot (m_net_earnings p_m_net_earnings lclm_m_net_earnings uclm_m_net_earnings)*month/
		overlay name='graph4';
run;

/*SPLINE ESTIMATION PROCEDURE , M=5*/

proc tpspline data=rdata;
	model m_net_earnings=(month)/m=5;
		output out=result pred lclm uclm;
run;

title 'm=5';
proc gplot data=result;
	plot (m_net_earnings p_m_net_earnings lclm_m_net_earnings uclm_m_net_earnings)*month/
		overlay name='graph5';
run;

/* Display the four TPS curves */

goption display;
proc greplay nofs tc=sashelp.templt template=l2r2;
	igout gseg;
		treplay 1:graph2 2:graph4 3:graph3 4:graph5;
run;



