*=================================================================================================================================================;
*                                                              Kevin Amrelle;
*                                                                8006669815;
*                                                                ECON 6218;
*                                       S&P500 with Vector Autoregression & Conditional Forecasting;
*=================================================================================================================================================;






%let data_dir=C:\Users\kamrelle\Desktop\S&P500 Capstone;

proc import datafile="&data_dir\data.xlsx"
			out     =data replace;
            sheet   =Sheet1;
            range   ="A1:E127";
			getnames=yes;

run;

proc sort data=data; by date;run;

*=======================================================================================;
* The DF-GLS Test;


Proc Autoreg Data=data;

Model SP500 = / STATIONARITY=(ERS) ;


Run;
quit;


Proc Autoreg Data=data;

Model Real_GDP = / STATIONARITY=(ERS) ;


Run;
quit;


Proc Autoreg Data=data;

Model Nonfarm = / STATIONARITY=(ERS) ;


Run;
quit;


Proc Autoreg Data=data;

Model PCE = / STATIONARITY=(ERS) ;


Run;
quit;

*=======================================================================================;
*Chow Test; 
*Known Break Date;
*============================;

*Jul 1990 as Break Date-- start for the 1990-91 recession;

Proc Autoreg Data=data;

Model  SP500 = Nonfarm  / chow=(22) ;


Run;
quit;

*Jan 2008 as Break Date-- start for the 2008-2009 recession;

Proc Autoreg Data=data;

Model  SP500 = Nonfarm  / chow=(92) ;


Run;
quit;

*=======================================================================================;
*The Breusch-Godfrey (BG) Test;


proc autoreg data= data;

model SP500   =   Real_GDP Nonfarm PCE   / Godfrey=4 ;


run;
quit;


*Granger-Causality Test at Lag 1;*SBC=1520.277;
proc varmax data=data;
model SP500 PCE Nonfarm Real_GDP  / p=1;
	causal group1=(SP500) group2=(PCE Nonfarm Real_GDP);
	causal group1=(SP500) group2=(PCE Nonfarm);
	causal group1=(SP500) group2=(PCE);
	causal group1=(SP500) group2=(Nonfarm);
	causal group1=(SP500) group2=(Real_GDP);
	causal group1=(PCE) group2=(SP500 Nonfarm Real_GDP);
	causal group1=(Nonfarm) group2=(SP500 PCE Real_GDP);
	causal group1=(Real_GDP) group2=(SP500 PCE Nonfarm);
run;


*Granger-Causality Test at Lag 2;);*SBC=1574.378;

proc varmax data=data;
model SP500 PCE Nonfarm Real_GDP  / p=2;
	causal group1=(SP500) group2=(PCE Nonfarm Real_GDP);
	causal group1=(SP500) group2=(PCE Nonfarm);
	causal group1=(SP500) group2=(PCE);
	causal group1=(SP500) group2=(Nonfarm);
	causal group1=(SP500) group2=(Real_GDP);
	causal group1=(PCE) group2=(SP500 Nonfarm Real_GDP);
	causal group1=(Nonfarm) group2=(SP500 PCE Real_GDP);
	causal group1=(Real_GDP) group2=(SP500 PCE Nonfarm);
run;


*Granger-Causality Test at Lag 3;*SBC=1627.683;


proc varmax data=data;
model SP500 PCE Nonfarm Real_GDP  / p=3;
	causal group1=(SP500) group2=(PCE Nonfarm Real_GDP);
	causal group1=(SP500) group2=(PCE Nonfarm);
	causal group1=(SP500) group2=(PCE);
	causal group1=(SP500) group2=(Nonfarm);
	causal group1=(SP500) group2=(Real_GDP);
	causal group1=(PCE) group2=(SP500 Nonfarm Real_GDP);
	causal group1=(Nonfarm) group2=(SP500 PCE Real_GDP);
	causal group1=(Real_GDP) group2=(SP500 PCE Nonfarm);
run;

*Granger-Causality Test at Lag 4;*1632.704;
proc varmax data=data;
model SP500 PCE Nonfarm Real_GDP  / p=4;
	causal group1=(SP500) group2=(PCE Nonfarm Real_GDP);
	causal group1=(SP500) group2=(PCE Nonfarm);
	causal group1=(SP500) group2=(PCE);
	causal group1=(SP500) group2=(Nonfarm);
	causal group1=(SP500) group2=(Real_GDP);
	causal group1=(PCE) group2=(SP500 Nonfarm Real_GDP);
	causal group1=(Nonfarm) group2=(SP500 PCE Real_GDP);
	causal group1=(Real_GDP) group2=(SP500 PCE Nonfarm);
run;







*=======================================================================================;


*AR(1)*;*SBC=-322.773;
proc arima data= data;

identify var= SP500 noprint;

estimate p=1 method=cls;

forecast lead=8 interval=QTR id=date out=AR1;

run;
quit;

*AR(2)*;*SBC=-318.096;
proc arima data= data;

identify var= SP500 noprint;

estimate p=2 method=cls;

forecast lead=8 interval=QTR id=date out=AR2;

run;
quit;
*AR(3)*;*SBC=-313.605;
proc arima data= data;

identify var= SP500 noprint;

estimate p=3 method=cls;

forecast lead=8 interval=QTR id=date out=AR3;

run;
quit;
*AR(4)*;*SBC=-308.901;
proc arima data= data;

identify var= SP500 noprint;

estimate p=4 method=cls;

forecast lead=8 interval=QTR id=date out=AR4;

run;
quit;



*=======================================================================================;

*ARIMA Model;

*Idennifying ARIMA (p,d,q) for SP500; 
proc arima data=data;
identify var = SP500;
run;
quit;

*Idennifying ARIMA (p,d,q) for SP500 SCAN; 
proc arima data=data;
identify var = SP500 SCAN;
run;
quit;						

*Forecasting ARIMA;*SBC=-322.773;
proc arima data=data;
identify var = SP500 noprint;
estimate p=1 q=0;
forecast lead=8 interval=qtr id=Date out=Forecast_ARIMA;
run;
quit;


*Forecasting ARIMA;*SBC=-323.018;
proc arima data=data;
identify var = SP500 noprint;
estimate p=0 q=1;
forecast lead=8 interval=qtr id=Date out=Forecast_ARIMA;
run;
quit;





*=======================================================================================;

*Estimating Correlation Coeff;
proc corr data=data;
var SP500 Real_GDP Nonfarm PCE;
run;
quit;












* Step 1;
*=======================================================================================;
*The VAR Model;
*==============================;

* How Many Variables ? ;

* SP500 Real_GDP Nonfarm ;

proc varmax data=data;

model SP500 Real_GDP/ p=1 ;*  SBC=1131.852; 
	

run;
quit;


* SP500 Real_GDP Nonfarm;


proc varmax data=data;

model SP500 Real_GDP Nonfarm/ p=1 ;*  SBC=867.0413;
	

run;
quit;

* SP500 Nonfarm;


proc varmax data=data;

model SP500 Nonfarm/ p=1 ;*  SBC=-820.349;*Preferred model;

	
run;
quit;


* SP500 Real_GDP Nonfarm;


proc varmax data=data;

model SP500 Real_GDP Nonfarm PCE/ p=1 ;*  SBC=1520.277;

	
run;
quit;

						

*Step 2;


*=========================;
* How many lags? ;
*=========================;


* Lag-Order Selection ;

* Lag 1 ;

proc varmax data=data;

model SP500 Nonfarm/ p=1 ;*  SBC=-820.349;*Preferred model;
	

run;
quit;

*Lag 2;

proc varmax data=data;

model SP500 Nonfarm/ p=2 ;*  SBC=-798.418;  
	

run;
quit;

*Lag 3;


proc varmax data=data;

model SP500 Nonfarm/ p=3 ;*  SBC=-778.187;
	

run;
quit;

*Lag 4;

proc varmax data=data;

model SP500 Nonfarm/ p=4 ;*  SBC=-755.555;
	

run;
quit;


*=======================================================================================;
*Forecasting with VAR modeling        ;

*=====================================;

*forecasted values for PCE are saved in the datafile "forecast"; 

proc varmax data=data;

model SP500 Nonfarm / p=2;

id date interval=qtr;
     output lead=8 out=forecast;	

run;
quit;



