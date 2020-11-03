
/* Include the csv */
filename cdc url 'http://www.openintro.org/stat/data/cdc.csv'; 
proc import datafile=cdc out=work.cdc dbms=csv replace; getnames=yes; 
run;

/* See the names of variables */
proc contents data=work.cdc short; 
run;

/* Exercice 1 */
data length_dataset;
	Len=$(work.cdc)
run;
proc print data=length_dataset;
run;