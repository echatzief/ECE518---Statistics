/* LAB 4A:   */

*The Data;

filename amesh url 'http://www.openintro.org/stat/data/ames_sas.csv';

proc import datafile=amesh out=work.ames dbms=csv replace;
            getnames=yes;
            guessingrows=max;
run;

proc univariate data=work.ames;
     var Gr_Liv_Area;
     histogram Gr_Liv_Area;
     inset mean median std min max / pos=ne;
run;

*The Unknown Sampling Distribution;

proc surveyselect data=work.ames out=work.amessample sampsize=50
     method=srs ranuni;
run;

proc print data=amessample;
run;

proc means data=work.amessample mean;
     var Gr_Liv_Area;
run;

proc surveyselect data=work.ames out=work.amessampler sampsize=50
     method=srs reps=5000 ranuni;
run;

proc means data=work.amessampler mean noprint;
     by replicate;
     var Gr_Liv_Area;
     output out=work.reprun mean=sampmean;
run;

proc univariate data=work.reprun;
     var sampmean;
     histogram sampmean;
run;

*Sample Size and the Sampling Distribution;
proc univariate data=work.reprun;
     var sampmean;
     histogram sampmean;
run;

proc surveyselect data=work.ames out=work.amessampler10 sampsize=10
     method=srs reps=5000 ranuni;
run;

proc means data=work.amessampler10 mean noprint;
     by replicate;
     var Gr_Liv_Area;
     output out=work.reprun10 mean=sampmean;
run;

proc surveyselect data=work.ames out=work.amessampler100 sampsize=100
     method=srs reps=5000 ranuni;
run;

proc means data=work.amessampler100 mean noprint;
     by replicate;
     var Gr_Liv_Area;
     output out=work.reprun100 mean=sampmean;
run;

proc univariate data=work.reprun;
     title 'Sample Size = 50';
     var sampmean;
     histogram sampmean;
run;

proc univariate data=work.reprun10;
     title 'Sample Size = 10';
     var sampmean;
     histogram sampmean;
run;

proc univariate data=work.reprun100;
     title 'Sample Size = 100';
     var sampmean;
     histogram sampmean;
run;

title;

data work.allsamples;
     set work.reprun (IN=Sample1)
	     work.reprun10 (IN=Sample2)
		 work.reprun100 (IN=Sample3);

     group = 1*(Sample1) + 2*(Sample2) + 3*(Sample3);
run;

proc sgpanel data=work.allsamples;
     panelby group;
	 histogram sampmean;
run;
