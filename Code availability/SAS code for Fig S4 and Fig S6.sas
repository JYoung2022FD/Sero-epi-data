
*================================Fig 2A seronegative data-male================================;
proc import datafile="D:\1-9 cohort\2. 1-9\1-9data\dat_all_m.xlsx" 
out=d replace;
sheet="Sheet 1";
run;
data d;
set d;
p16=(logtiter<4);
run;
PROC glimmix data=d;* laplace for AIC;
class kid;
effect spl=spline(agemonth/BASIS=bspline KNOTMETHOD=percentiles(3) degree=3 );*lowest -2logL;
model p16(event='1')=spl/dist=bin link=logit ;
random int spl/subject=kid;
NLOPTIONS TECH = NRRIDG ;
store out=m33;
ods output fitstatistics=c33;
run;
*creat new dataset;
proc means data=d max;
var agemonth;
run;
data newtime;
	do agemonth = 0 to 155 by 0.01;
	output;
    end;
run;

proc plm source=m33; 
score data=newtime out=predm predicted=p stderr=se;  
run; 

data predm ;
set predm ;
p2=exp(p)/(1+exp(p));
uclp=exp(p+1.96*se)/(1+exp(p+1.96*se));
lclp=exp(p-1.96*se)/(1+exp(p-1.96*se));
run;

ods graphics /  reset width=600px HEIGHT=400px border=off ANTIALIASMAX=15600;
*plot*;
proc sgplot data= predm;
series y=p2 x=agemonth/lineattrs=(pattern=solid  THICKNESS=2 );
band x=agemonth upper=uclp lower=lclp / transparency=0.3;
xaxis  label='Age (year)' values=(0 to 155 by 1);
yaxis label='Seroprevalence' LABELATTRS=(Size=15);
run;
*export to R;
proc export data=predm
OUTFILE='D:\1-9 cohort\2. 1-9\1-9data\dat_all_seronegative_fit_line_male.xlsx'
DBMS=excel replace;
run;

*================================Fig 2A seronegative data-female================================;
proc import datafile="D:\1-9 cohort\2. 1-9\1-9data\dat_all_f.xlsx" 
out=d replace;
sheet="Sheet 1";
run;
data d;
set d;
p16=(logtiter<4);
run;
PROC glimmix data=d;
class kid;
effect spl=spline(agemonth/BASIS=bspline KNOTMETHOD=percentiles(4) degree=3 );*lowest -2logL;
model p16(event='1')=spl/dist=bin link=logit ;
random int spl/subject=kid;
NLOPTIONS TECH = NRRIDG;
store out=m43;
ods output fitstatistics=c43;
run;
*creat new dataset;
proc means data=d max;
var agemonth;
run;
data newtime;
	do agemonth = 0 to 155 by 0.01;
	output;
    end;
run;

proc plm source=m43;*lowest -2logL; 
score data=newtime out=predm predicted=p stderr=se;  
run; 

data predm ;
set predm ;
p2=exp(p)/(1+exp(p));
uclp=exp(p+1.96*se)/(1+exp(p+1.96*se));
lclp=exp(p-1.96*se)/(1+exp(p-1.96*se));
run;

ods graphics /  reset width=600px HEIGHT=400px border=off ANTIALIASMAX=15600;
*plot*;
proc sgplot data= predm;
series y=p2 x=agemonth/lineattrs=(pattern=solid  THICKNESS=2 );
band x=agemonth upper=uclp lower=lclp / transparency=0.3;
xaxis  label='Age (year)' values=(0 to 155 by 1);
yaxis label='Seroprevalence' LABELATTRS=(Size=15);
run;
*export to R;
proc export data=predm
OUTFILE='D:\1-9 cohort\2. 1-9\1-9data\dat_all_seronegative_fit_line_female.xlsx'
DBMS=excel replace;
run;

*================================Fig 3A GMT dynamic-male================================;
proc import datafile="D:\1-9 cohort\2. 1-9\1-9data\dat_all_m.xlsx" 
out=d replace;
sheet="Sheet 1";
run;
PROC glimmix data=d;
class kid;
effect spl=spline(agemonth/BASIS=bspline KNOTMETHOD=percentiles(3) degree=2 );*lowest AIC;
model logtiter=spl;
random int spl/subject=kid;
store out=m32;
ods output fitstatistics=c32;
run;
proc means data=d max;
var agemonth;
run;
data newtime;
do agemonth = 0 to 155 by 0.01;
	output;
	end;
run;
proc plm source=m32; 
score data=newtime out=predm predicted=p stderr=se;  
run; 

data predm ;
set predm;
p3=2**p;
uclp=2**(p+1.96*se);
lclp=2**(p-1.96*se);
run;
*plot*;
proc sgplot data=predm;
series y=p3 x=agemonth/lineattrs=(pattern=solid  THICKNESS=2 );
band x=agemonth upper=uclp lower=lclp / transparency=0.3;
xaxis  label='Age (mo)' values=(0 to 155 by 1);
yaxis type=log logstyle=logexpand logbase=2 label='Neutraliztion antibody' LABELATTRS=(Size=15) values=(4 8 16 32 64 128 256);
refline 16/ axis=y lineattrs=(pattern=dash color=grey) ;
run;
*export to R;
proc export data=predm
OUTFILE='D:\1-9 cohort\2. 1-9\1-9data\dat_all_fit_line_m.xlsx'
DBMS=excel replace;
run;

*================================Fig 3A GMT dynamic-female================================;
proc import datafile="D:\1-9 cohort\2. 1-9\1-9data\dat_all_f.xlsx" 
out=d replace;
sheet="Sheet 1";
run;
PROC glimmix data=d;
class kid;
effect spl=spline(agemonth/BASIS=bspline KNOTMETHOD=percentiles(3) degree=2 );*lowest AIC;
model logtiter=spl;
random int spl/subject=kid;
store out=m32;
ods output fitstatistics=c32;
run;
proc means data=d max;
var agemonth;
run;
data newtime;
do agemonth = 0 to 155 by 0.01;
	output;
	end;
run;
proc plm source=m32; 
score data=newtime out=predm predicted=p stderr=se;  
run; 

data predm;
set predm;
p3=2**p;
uclp=2**(p+1.96*se);
lclp=2**(p-1.96*se);
run;
*plot*;
proc sgplot data=predm;
series y=p3 x=agemonth/lineattrs=(pattern=solid  THICKNESS=2 );
band x=agemonth upper=uclp lower=lclp / transparency=0.3;
xaxis  label='Age (mo)' values=(0 to 155 by 1);
yaxis type=log logstyle=logexpand logbase=2 label='Neutraliztion antibody' LABELATTRS=(Size=15) values=(4 8 16 32 64 128 256);
refline 16/ axis=y lineattrs=(pattern=dash color=grey) ;
run;
*export to R;
proc export data=predm
OUTFILE='D:\1-9 cohort\2. 1-9\1-9data\dat_all_fit_line_f.xlsx'
DBMS=excel replace;
run;


*================================Fig 3B data after infection-male================================;
proc import datafile="D:\1-9 cohort\2. 1-9\1-9data\data_aftinf_p_male.xlsx" 
out=dp replace;
sheet="Sheet 1";
run;

PROC glimmix data=dp ;
class kid;
effect spl=spline(agemonth/BASIS=bspline KNOTMETHOD=percentiles(5) degree=3 );*lowest AIC;
model logtiter=spl;
random int spl/subject=kid;
store out=m53;
ods output fitstatistics=c53;
run;
proc means data=dp min max;
var agemonth;
run;
data newtime;
	do agemonth = 2 to 155 by 0.01;
	output;
    end;
run;
proc plm source=m53;
score data=newtime out=predm predicted=p stderr=se;  
run; 
data predm ;
set predm ;
uclp=p+1.96*se;
lclp=p-1.96*se;
run;
*plot*;
proc sgplot data=predm;
series y=p x=agemonth/lineattrs=(pattern=solid  THICKNESS=2 );
band x=agemonth upper=uclp lower=lclp / transparency=0.3;
xaxis  label='Age (mo)' values=(0 to 155 by 1);
yaxis label='Neutraliztion antibody' LABELATTRS=(Size=15) ;
refline 16/ axis=y lineattrs=(pattern=dash color=grey) ;
run;
*export to R;
proc export data=predm
OUTFILE='D:\1-9 cohort\2. 1-9\1-9data\dat_aftinfec_fit_male.xlsx'
DBMS=excel replace;
run;

*================================Fig 3B data after infection-female================================;
proc import datafile="D:\1-9 cohort\2. 1-9\1-9data\data_aftinf_p_female.xlsx" 
out=dp replace;
sheet="Sheet 1";
run;
PROC glimmix data=dp ;
class kid;
effect spl=spline(agemonth/BASIS=bspline KNOTMETHOD=percentiles(3) degree=3 );*lowest AIC;
model logtiter=spl;
random int spl/subject=kid;
store out=m33;
ods output fitstatistics=c33;
run;
proc means data=dp min max;
var agemonth;
run;
data newtime;
	do agemonth = 2 to 155 by 0.01;
	output;
    end;
run;
proc plm source=m33;
score data=newtime out=predm predicted=p stderr=se;  
run; 
data predm ;
set predm ;
uclp=p+1.96*se;
lclp=p-1.96*se;
run;
*plot*;
proc sgplot data=predm;
series y=p x=agemonth/lineattrs=(pattern=solid  THICKNESS=2 );
band x=agemonth upper=uclp lower=lclp / transparency=0.3;
xaxis  label='Age (mo)' values=(0 to 155 by 1);
yaxis label='Neutraliztion antibody' LABELATTRS=(Size=15) ;
refline 16/ axis=y lineattrs=(pattern=dash color=grey) ;
run;
*export to R;
proc export data=predm
OUTFILE='D:\1-9 cohort\2. 1-9\1-9data\dat_aftinfec_fit_female.xlsx'
DBMS=excel replace;
run;
