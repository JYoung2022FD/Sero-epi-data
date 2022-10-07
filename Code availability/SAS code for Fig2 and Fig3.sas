
*================================Fig 2A seronegative data================================;
proc import datafile="D:\1-9 cohort\2. 1-9\1-9data\dat_all_0727.xlsx" 
out=d replace;
sheet="Sheet 1";
run;
data d;
set d;
p16=(logtiter<4);
run;
PROC glimmix data=d;
class kid;
effect spl=spline(agemonth/BASIS=bspline KNOTMETHOD=percentiles(4) degree=3 );
model p16(event='1')=spl/dist=bin link=logit ;
random int spl/subject=kid;
NLOPTIONS TECH = NRRIDG;
store out=m43;
ods output fitstatistics=c43;
run;
proc means data=d max;
var agemonth;
run;
data newtime;
	do agemonth = 0 to 155 by 0.01;
	output;
    end;
run;

proc plm source=m43; 
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
OUTFILE='D:\1-9 cohort\2. 1-9\1-9data\dat_all_seronegative_fit_line0802-43.xlsx'
DBMS=excel replace;
run;



*================================Fig 3A GMT dynamic of all data================================;
proc import datafile="D:\1-9 cohort\2. 1-9\1-9data\dat_all_0727.xlsx" 
out=d replace;
sheet="Sheet 1";
run;
data d;
set d;
p16=(logtiter>=4);
run;

PROC glimmix data=d;
class kid;
effect spl=spline(agemonth/BASIS=bspline KNOTMETHOD=percentiles(3) degree=2 );
model logtiter=spl;
random int spl/subject=kid;
store out=m32;
ods output fitstatistics=c32;
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
OUTFILE='D:\1-9 cohort\2. 1-9\1-9data\dat_all_fit_line0727.xlsx'
DBMS=excel replace;
run;

*================================Fig 3B data after infection================================;
proc import datafile="D:\1-9 cohort\2. 1-9\1-9data\dat_aftinfec_0726.xlsx" 
out=d replace;
sheet="Sheet 1";
run;
data dp;
set d;
if logtiter<4 then delete;
run;
PROC glimmix data=dp ;
class kid;
effect spl=spline(agemonth/BASIS=bspline KNOTMETHOD=percentiles(3) degree=3 );
model logtiter=spl;
random int spl/subject=kid;
store out=m33;
ods output fitstatistics=c33;
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
*export to R;
proc export data=predm
OUTFILE='D:\1-9 cohort\2. 1-9\1-9data\dat_aftinfec_0726_fit line.xlsx'
DBMS=excel replace;
run;
