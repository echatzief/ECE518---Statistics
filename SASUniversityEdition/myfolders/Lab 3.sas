/* Lab 3: Introduction to Data */

/* Include the csv */
filename cdc url 'http://www.openintro.org/stat/data/cdc.csv'; 
proc import datafile=cdc out=work.cdc dbms=csv replace; getnames=yes; 
run;

/* See the names of variables */
proc contents data=work.cdc short; 
run;

/* Exercise 1 */
title "Exercise 1";
proc contents data=work.cdc; 
run;
/* End of Exercise 1 */

proc print data=work.cdc (obs=10);
run;

/* Summaries and Tables */
title;
proc univariate data=work.cdc;
 var weight;
run;

proc freq data=work.cdc;
 tables weight;
run;

proc freq data=work.cdc;
 tables smoke100 / plots=freqplot(scale=percent);
run;


/* Exercise 2 */
title "Exercise 2";
proc univariate data=work.cdc;
 var height;
run;
proc univariate data=work.cdc;
 var age;
run;

proc freq data=work.cdc;
 tables gender;
run;
title;

proc freq data=work.cdc;
 tables exerany;
run;
/* End of Exercise 2 */

proc freq data=work.cdc;
 tables gender*smoke100;
run;

proc freq data=work.cdc;
 tables gender*smoke100 / plots=mosaicplot;
run;

/* Exercise 3 */
title "Exercise 3";
data exercise_3_answer;
Answer = 'The chunk of the females that smoke less than 100 cigarettes is larger than the piece of males. Also, the chunk of males that smoke more than 100 cigarettes is larger than the piece of females.';
run;
proc print data = exercise_3_answer noobs;
run;
title;
/* End of Exercise 3 */


/* Interlude: How SAS Processes Data */
proc print data=work.cdc (obs=10);
run;

proc contents data=work.cdc;
run;

data work.newcdc;
 set work.cdc;
 if gender="m" and age>30;
run;
proc print data=work.newcdc (obs=10);
run;

data work.newcdc;
 set work.cdc;
 wtkilos=weight*0.453592;
 wtdiff=sum(weight,-wtdesire);
 genhlth=propcase(genhlth);
 now=today();
run;
proc print data=work.newcdc (obs=10);
run;

/* A Little More about Subsetting */ 
data work.newcdc;
 set work.cdc;
 where gender="m";
run;
proc print data=work.newcdc (obs=10);
run;

data work.newcdc;
 set work.cdc;
 where gender="M" and age>30;
run;
proc print data=work.newcdc (obs=10);
run;

data work.newcdc;
 set work.cdc;
 if gender="M" or age>30;
run;
proc print data=work.newcdc (obs=10);
run;

data work.newcdc;
 set work.cdc;
 if gender="m" or (gender="f" and age>30);
run;
proc print data=work.newcdc (obs=10);
run;

/* Exercise 4 */
title "Exercise 4";
data under23smoke;
 set work.cdc;
 if age<23 and smoke100=1;
proc print data=under23smoke (obs=10);
run;
title;
/* End of Exercise 4 */

/* Quantitative Data */
ods graphics;
proc univariate data=work.cdc plots;
 var weight;
run;

ods graphics;
proc univariate data=work.cdc plots;
 class gender;
 var Weight;
run;

proc sort data=work.cdc;
 by gender;
run;

ods graphics;
proc univariate data=work.cdc plots;
 by gender;
 var Weight;
run;

data work.cdcbmi;
 set work.cdc;
 bmi=(weight/height**2) * 703;
run;

proc sgplot data=work.cdcbmi;
 vbox bmi / group=genhlth;
run;

/* Exercise 5 */
title "Exercise 5";
data exercise_5_answer;
Answer = 'The box plot shows how the bmi varies at every value of the genhlth variable.';
run;
proc print data = exercise_5_answer noobs;

proc sgplot data=work.cdcbmi;
 vbox bmi / group=gender;
run;

data description_5;
Answer = 'Gender is a reliable choice for BMI. By definition, the BMI is related to weight and height, values that are influenced by the fact that a person is a man or a woman.';
run;
proc print data = description_5 noobs;
/* End of Exercise 5 */

proc univariate data=work.cdcbmi;
 var bmi;
 histogram bmi;
run;

proc univariate data=work.cdcbmi;
 var bmi;
 histogram bmi / nmidpoints=50;
run;

ods graphics;
proc univariate data=work.cdc;
 var age;
 histogram age / nmidpoints=50;
 inset mean std median / position=NE;
run;

/* On Your Own */ 

title "1. Make a scatter plot of weight versus desired weight.";
proc sgscatter data= work.cdc;
 plot weight*wtdesire;
run;
data exercise_1;
Description = 'The two variables increase the same way.';
run;
proc print data = exercise_1 noobs;

title "2. Create this new variable by subtracting the two columns in the new data set and
assigning them to a new object named wdiff.";

data work.newcdc;
 set work.cdc;
 wdiff = wtdesire - weight;
run;

proc print data=work.newcdc (obs=10);
run;

title "3. What type of variable is wdiff? If an observation for wdiff is 0, what does this mean about the
person’s weight and desired weight. What if wdiff is positive or negative?";

data exercise_3;
	Type = "Both wtdesire and weight are numerical. Thus, the difference of the two variables is also numerical.";
	wdiff_equals_0 ='If wdiff is 0, then the person has the weight equal to the desired.';
	wdiff_positive_negative = 'If wdiff is positive then the weight is smaller than the desired.Also if wdiff is negative then the weight is larger than the desired.';
run;
proc print data = exercise_3 noobs;

title "4. Describe the distribution of wdiff in terms of its center, shape, and spread, including any plots you 
use. What does this tell us about how people feel about their current weight?";

proc univariate data=work.newcdc;
 var wdiff;
 histogram wdiff / nmidpoints=50;
 inset mean std median / position=NE;
run;

data exercise_4;
 Answer = "The distribution of wdiff tells us that the people have weight larger than the desired because the wdiff is negative.";
run;
proc print data = exercise_4 noobs;

title "5. Using numerical summaries and a side-by-side box plot, determine whether men tend to view their
weight differently than women.";

proc univariate data=work.newcdc plots;
 by gender;
 var wdiff;
run;

title "6. Now it’s time to get creative. Find the mean and standard deviation of weight and determine what
proportion of the weights is within one standard deviation of the mean.";

proc univariate data=work.cdc;
 var weight;
run;
