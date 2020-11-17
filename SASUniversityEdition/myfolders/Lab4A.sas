/* LAB 4A:   */

*Importing the Data;


filename amesh url 'http://www.openintro.org/stat/data/ames_sas.csv';

proc import datafile=amesh out=work.ames dbms=csv replace;
   getnames=yes;
   guessingrows=max;
run;


proc univariate data=work.ames;
   var Gr_Liv_Area;
   histogram Gr_Liv_Area;
   inset mean median std min max / pos=ne;
   histogram Gr_Liv_Area / nmidpoints=5;
   inset mean median std min max / pos=ne;
run;

/* Exercise 1 */
title "Exercise 1";
data exercise_1_answer;
Answer = 'This is a Right-Skewed distribution and unimodal as we can see from the histogram.';
run;
proc print data = exercise_1_answer noobs;
run;
title;
/* End of Exercise 1 */

*Taking a sample of 50 to simplify the work;
proc surveyselect data=work.ames out=work.amessample sampsize=50
                  method=srs ranuni;
run;

proc print data=work.amessample;
	var Gr_Liv_Area;
	var SalePrice;
run;

/* Exercise 2 */
title "Exercise 2";
data exercise_2_answer;
Answer = 'Since the samples vary each time we cannot say for sure, but in this case it is a Right-Skewed distribution and follows the population distribution,also having a mean close to the population distribution.The only differences is that the range is not as wide and the outliers are not as extreme.';
run;
proc print data = exercise_2_answer noobs;
run;

proc univariate data=work.amessample;
   var Gr_Liv_Area;
   histogram Gr_Liv_Area;
   inset mean median std min max / pos=ne;
run;

/* End of Exercise 2 */

*Calculating the  average living area in our sample using the sample mean.;
proc means data=work.amessample mean;
   var Gr_Liv_Area;
run;
title;

/* Exercise 3 */
title "Exercise 3";
*Taking a second sample of 50;
proc surveyselect data=work.ames out=work.amessample2 sampsize=50
                  method=srs ranuni;
run;

proc univariate data=work.amessample2;
   var Gr_Liv_Area;
   histogram Gr_Liv_Area;
   inset mean median std min max / pos=ne;
run;

*Computing the mean of the second sample.;
proc means data=work.amessample2 mean;
   var Gr_Liv_Area;
run;


data exercise_3_answer;
Answer = 'The means of the two samples differ with the second one having a mean close to the population mean, while the first sample has a mean way higher of the population.';
run;
proc print data = exercise_3_answer noobs;
run;

*Taking a sample of 100;
proc surveyselect data=work.ames out=work.amessample3 sampsize=100
                  method=srs ranuni;
run;

*Computing the mean of the third sample.;
proc means data=work.amessample3 mean;
   var Gr_Liv_Area;
run;

*Taking a sample of 1000;
proc surveyselect data=work.ames out=work.amessample4 sampsize=1000
                  method=srs ranuni;
run;

*Computing the mean of the fourth sample.;
proc means data=work.amessample4 mean;
   var Gr_Liv_Area;
run;

data description_3;
Answer = ' After observing the mean of every sample we conclude that the higher the sample size is, the closer it gets to the population mean. So the sample of size 1000 provides a more accurate estimation.But, since all the samples are random each time, it is not always guaranteed that the bigger sample size will be more accurate.';
run;
proc print data = description_3 noobs;
title;
/* End of Exercise 3 */

*Generating a data set with 5000 samples mean.;
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

/* Exercise 4 */
title "Exercise 4";
data exercise_4_answer;
Answer = 'The work.reprum contains 5000 means with each mean originating from 50 samples, so the observations are 5000. We can observe from the histogram that it is almost symmetrical, so it is close to a normal distribution with the center being at 1500. The distribution would not really change at the 50.000 sample means, it will only get closer and closer to a normal distribution.';
run;
proc print data = exercise_4_answer noobs;
run;
title;
/* End of Exercise 4 */

/* Exercise 5 */
title "Exercise 5";

*Generating a data set with 100 samples mean.;
proc surveyselect data=work.ames out=work.amessampler2 sampsize=50
                  method=srs reps=100 ranuni;
run;

proc means data=work.amessampler2 mean noprint;
   by replicate;
   var Gr_Liv_Area;
   output out=work.reprunsmall mean=sampmean;
run;

proc print data=work.reprunsmall;
run;

data exercise_5_answer;
Answer = ' The work.reprunsmall contains 100 observations. Each observation representing a sample mean from a sample of size 50.';
run;
proc print data = exercise_5_answer noobs;
run;
title;
/* End of Exercise 5 */

proc univariate data=work.reprun;
   var sampmean;
   histogram sampmean;
run;

*Sampling distribution with a sample size of 10;
proc surveyselect data=work.ames out=work.amessampler10 sampsize=10
                  method=srs reps=5000 ranuni;
run;

proc means data=work.amessampler10 mean noprint;
   by replicate;
   var Gr_Liv_Area;
   output out=work.reprun10 mean=sampmean;
run;

*Sampling distribution with a sample size of 100;
proc surveyselect data=work.ames out=work.amessampler100 sampsize=100
                  method=srs reps=5000 ranuni;
run;

proc means data=work.amessampler100 mean noprint;
   by replicate;
   var Gr_Liv_Area;
   output out=work.reprun100 mean=sampmean;
run;

*Plot the 3 different sample distributions of sample sizes 50, 10 and 100 respectively.;
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

*Combining the 3 different histograms in a single one.;
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


/* Exercise 6 */
title "Exercise 6";
data exercise_6_answer;
Answer = 'When the sample size is larger, the center still stays relatively the same in comparison to smaller sample sizes, but the center of the sampling disbribution would get closer and closer to the center of the population. However, the spread gets smaller as there is less variability.';
run;
proc print data = exercise_6_answer noobs;
run;

title;
/* End of Exercise 6 */

/* On Your Own */ 

title "1. Take a random sample of size 50 for SalePrice. Using this sample, what is your best point estimate of the population mean?.";
proc surveyselect data=work.ames out=work.amessamplePrice sampsize=50
                  method=srs ranuni;
run;
title;

proc means data=work.amessamplePrice mean;
   var SalePrice;
run;

title "2. Because you have access to the population, simulate the sampling distribution for x¯ price by taking 5000 samples from the population of size 50 and computing 5000 sample means. Update the code to account for changing the variable in question. Name the storage data set work.repprice. Plot the data and then describe the shape of this sampling distribution. Based on this sampling distribution, what would you guess the mean home price of this population to be? Finally, calculate and report the population mean.";
proc surveyselect data=work.ames out=work.amessamplePrice sampsize=50
                  method=srs reps=5000 ranuni;
run;
title;

proc means data=work.amessamplePrice mean noprint;
   by replicate;
   var SalePrice;
   output out=work.repprice mean=sampmean;
run;

proc univariate data=work.repprice;
   var sampmean;
   histogram sampmean;
   inset mean / pos=ne;
run;

*Computing the mean of the population.;
proc means data=work.ames mean;
   var SalePrice;
run;

data exercise_2;
Description = 'The shape of the sampling mean distribution resembles a normal distrubtion since we have a high sample size, the sample mean home price is at 180.700 which can differ but with minimal deviation since it originates from 5000 random samples of size 50. The spread is small. Before calculating the population mean home price I am estiamting it to be around 180.000, which is very close to the sampling mean.';
run;
proc print data = exercise_2 noobs;
run;

title "3. Change your sample size from 50 to 150, and then compute the sampling distribution using the same method as above. Store these means in a new data set named work.repprice150. Describe the shape of this sampling distribution, and compare it to the sampling distribution for a sample size of 50. Based on this sampling distribution, what would you guess to be the mean sale price of homes in Ames?";
proc surveyselect data=work.ames out=work.amessamplePrice2 sampsize=150
                  method=srs reps=5000 ranuni;
run;
title;

proc means data=work.amessamplePrice2 mean noprint;
   by replicate;
   var SalePrice;
   output out=work.repprice150 mean=sampmean;
run;

proc univariate data=work.repprice150;
   var sampmean;
   histogram sampmean;
   inset mean / pos=ne;
run;

data exercise_3;
Description = 'The shape of the 150 sampling mean distribution still resembles a normal distrubtion and it is closer to that distribution compared to the sample distribution of 50 samples. Also, the sample mean home price is closer to the population mean which also can differ but with a std deviation lower than the sample distribution of 50 we have higher accuracy. The spread is smaller. My guess would still be around 180.000.';
run;
proc print data = exercise_3 noobs;
run;

title "4. Of the sampling distributions from 2 and 3, which has a smaller spread? If we’re concerned with making estimates that are more often close to the true value, would we prefer a distribution with a large or small spread?.";
data exercise_4;
Description = 'After observing the two sampling distributions, the distribution from 3 has smaller spread. Το estimate closer to the true value we want a distribution with a large spread.';
run;
proc print data = exercise_4 noobs;
run;




