



%let data_dir=C:\Users\kamrelle\Desktop\CapSAS;



proc import datafile="&data_dir\capstonepc.xlsx"			
			out     =data replace;
            sheet   =Sheet1;
            range   ="A1:E131";
			

run;

*=======================================================================================;
*Real GDP;


proc model data=data;
SP500      = ao + a1*Real_GDP  ;
fit   SP500;
solve SP500/  out=cond_fore forecast;
outvars date;
run;
quit;


proc sort data=cond_fore; by date ; run;


data cond_fore2;
set cond_fore;

drop _type_ _mode_ _errors_ ;

run;

*=======================================================================================;
*NonFarm Payroll;

proc model data=data;
SP500      = ao + a1*Nonfarm  ;
fit   SP500;
solve SP500/  out=cond_fore forecast;
outvars date;
run;
quit;


proc sort data=cond_fore; by date ; run;


data cond_fore2;
set cond_fore;

drop _type_ _mode_ _errors_ ;

run;








*=======================================================================================;
*PCE;

proc model data=data;
SP500      = ao + a1*PCE  ;
fit   SP500;
solve SP500/  out=cond_fore forecast;
outvars date;
run;
quit;


proc sort data=cond_fore; by date ; run;


data cond_fore2;
set cond_fore;

drop _type_ _mode_ _errors_ ;

run;
