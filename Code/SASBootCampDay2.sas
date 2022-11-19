*************   P   R   O   G   R   A   M       H   E   A   D   E   R   *****************
*****************************************************************************************
*                                                                                       *
*   PROGRAM:    SAS Bootcamp Day 2.sas                                                  *
*   PURPOSE:    Introduce SAS Programming		                                        *
*   AUTHOR:     Laura Grau	                                                            *
*   CREATED:    2022	                                                                *
*   NOTES:		Portions of this program were adapted from Jud Blatchford's lectures    *                                                                                    *
****************************************************************************************
***********************************************************************************; RUN;

%let Root = C:\Users\graul\OneDrive - The University of Colorado Denver\Teaching\BIOS SAS Bootcamp\SAS Boot Camp Course Materials;

libname prc "&root/DataProcessed";

****************************************************************************************
PART SIX: SORTING DATASETS
****************************************************************************************;

*Part 6.1: Introduction to sorting data;

*	By default, SAS over-writes the data set that is sorted
	Therefore, create a copy of a data set to work with when 'experimenting'
		with the data so you don't over-write source data!

	Creating a temporary data set 'Labs' to use for illustrations
	Note that the data is sorted by the variable 'Lab_Date'   ;
	DATA	WORK.Labs;
		SET	prc.Laboratory_2015;
		RUN;

	PROC PRINT DATA = WORK.Labs;
		RUN;

*Part 6.2: Basic Sorts;
*	PROC SORT is used to sort a SAS data set
	The sort order is specified in the BY statement
		The BY statement is required only for the SORT procedure
		(it is an optional statement on most other procedures)
	The default order is ascending order for the BY variable(s)   ;
	PROC SORT DATA = WORK.Labs;
		BY	Subject_ID;
		RUN;

	PROC PRINT DATA = WORK.Labs;
		RUN;

*	Note:  Missing values are ordered before non-missing values (when using ascending order)   *;
	PROC SORT DATA = WORK.Labs;
		BY	Lab_Value;
		RUN;

	PROC PRINT DATA = WORK.Labs;
		RUN;


*Part 6.3: Basic Sorts: Sorting by Multiple Variables;

*	Variables in the BY statement are referred to as "BY variables"

	The 1st variable in the BY statement is the first priority in the sort order
		The 2nd variable ONLY affects observations which have identical
			values of the 1st variable!
	So typically if multiple variables are specified then the first tend to be
		classification or "grouping" variables   ;
	PROC SORT DATA = WORK.Labs;
		BY	Lab_Test Subject_ID;
		RUN;

	PROC PRINT DATA = WORK.Labs;
		RUN;

	*	Example using 3 BY-variables   ;
	PROC SORT DATA = WORK.Labs;
		BY	Lab_Test Subject_ID Lab_Date;
		RUN;

	PROC PRINT DATA = WORK.Labs;
		RUN;


*Part 6.4: Basic Sorts: Sorting in descending order;

*	The BY statement option "DESCENDING" must PRECEDE the desired sort variable   ;
	PROC SORT DATA	= WORK.Labs;
		BY	DESCENDING Lab_Value;
		RUN;

	PROC PRINT DATA = WORK.Labs;
		RUN;


	*	DESCENDING Only Applies to the 1 Next Following Variable;

		PROC SORT DATA	= WORK.Labs;
			BY	DESCENDING Lab_Test Lab_Value; * Note:  Lab_Values are in ascending order ;
			RUN;

		PROC PRINT DATA = WORK.Labs;
			RUN;



*Part 6.5: Basic Sorts: Sorting CHARACTER data;


*	Creating data to use for the following illustrations
	Note:  I've made the name "Eamon" lower-case for illustration purposes only!   *;
DATA	WORK.Olympics;
	INFILE	DATALINES;
	INPUT	Event		$	1-20
			ORHolder	$	21-40
			Country		$	41-53;
* Ruler  1    1    2    2    3    3    4    4    5    5
1---5----0----5----0----5----0----5----0----5----0----5;
	DATALINES;
50 m Freestyle      Cesar Cielo         Brazil
100 m Freestyle     eamon Sullivan      Australia
200 m Freestyle     Michael Phelps      United States
400 m Freestyle     Sun Yang            China
1500 m Freestyle    Sun Yang            China
;


*	Using the SORTSEQ Option   *; 

	*	By default, upper-case letters sort before lower-case letters
		Note:  Sorting by strict character values is not always helpful   *;
	PROC SORT
			DATA	= WORK.Olympics
			OUT		= WORK.BySwimmer;
		BY	ORHolder;
		RUN;

	PROC PRINT DATA = WORK.BySwimmer;
		RUN;

	*	The SORTSEQ option can be useful to sort character values ignoring case
		The SORTSEQ= option has 4 possibilities,
			1 is LINGUISTIC which specifies linguistic collation which sorts characters in
				a culturally-sensitive manner according to rules that are associated with
				a language and locale.   *;
	PROC SORT
			DATA	= WORK.Olympics
			OUT		= WORK.BySwimmer
			SORTSEQ	= LINGUISTIC;
		BY	ORHolder;
		RUN;

	PROC PRINT DATA = WORK.BySwimmer;
		RUN;

*	Using the SORTSEQ Option with NUMERIC_COLLATION Suboption   *; 

	*	Note:  Sorting by strict character values is not always helpful   *;
	PROC SORT
			DATA	= WORK.Olympics
			OUT		= WORK.ByEvent;
		BY	Event;
		RUN;

	PROC PRINT DATA = WORK.ByEvent;
		RUN;

*	The SORTSEQ option can be useful when sorting character values starting with integers
	The LINGUISTIC option has several suboptions which modify the linguistic collating sequence
		NUMERIC_COLLATION=ON orders numbers by the numeric value (default is OFF)   *;
	PROC SORT
			DATA	= WORK.Olympics
			OUT		= WORK.ByEvent
			SORTSEQ	= LINGUISTIC (NUMERIC_COLLATION = ON);
		BY	Event;
		RUN;

	PROC PRINT DATA = WORK.ByEvent;
		RUN;
*	Note:  SAS will only look for numeric digits
	(e.g. if 1500 had been 1,500 it would've considered it as a 1)   *;



****************************************************************************************
PART SEVEN: REMOVING DUPLICATES
****************************************************************************************;

*Part 7.1:  Using the NODUPKEY Option   *; 

*	The NODUPKEY option will remove any duplicate observations where
		duplication is based on the variables in the BY statement

	Goal 1:  Create a list of unique subjects who have experienced an Adverse Event (AE)   *;
	TITLE1 "All Adverse Events";
	PROC PRINT DATA = prc.AELog;
		RUN;

	PROC SORT
			DATA	= prc.AELog
			NODUPKEY
			OUT		= WORK.SubjsWithAEs;
		BY	SubjID;
		RUN;

	TITLE1 "Subjects with Adverse Events";
	PROC PRINT DATA = WORK.SubjsWithAEs;
		VAR SubjID;
		RUN;
	TITLE;


*	Goal 2:  Create a list of unique AEDescr/Grade combinations, using
	Grades:	1 = Mild
			2 = Moderate
			3 = Severe
			4 = Life-Threatening
			5 = Death			*;
	PROC SORT
			DATA	= prc.AELog
			NODUPKEY
			OUT		= WORK.AEDescrGradeCombos;
		BY	AEDescr Grade;
		RUN;

	TITLE1 "All Combinations of Adverse Event by Grade";
	PROC PRINT DATA = WORK.AEDescrGradeCombos NOOBS;
		BY	AEDescr;
		ID	AEDescr;
		VAR	Grade;
		RUN;
	TITLE;

	PROC PRINT DATA = WORK.AEDescrGradeCombos;
	RUN;

*	Goal 3: Create a data set without any duplicate observations

	Duplicate observations contain identical values for all variables
	To detect this, all variables must be in the BY statement
	Use the keyword _ALL_ to accomplish this!   *;
	PROC SORT
			DATA	= prc.AELog
			NODUPKEY
			OUT		= WORK.NoDuplicObs;
		BY	_ALL_;
		RUN;

	TITLE1 "All Unique Adverse Events";
	PROC PRINT DATA = WORK.NoDuplicObs;
		RUN;
	TITLE1;
*	Often an observation which is a complete duplicate is an error!   *;



****************************************************************************************
PART EIGHT: SUBSETTING DATASETS
****************************************************************************************;

*Part 8.1:  RESTRICTING OBSERVATIONS READ INTO A DATA SET   *; 
*	The SASHelp.Cars data set will be used to illustrate some concepts
	The data set has 428 observations
	The data set is sorted by Make (Acura -> Volvo), then Type   *;

	PROC PRINT DATA = SASHelp.Cars;
		RUN;


	*	Using the OBS= Data Set Option   *;

	*	The OBS= data set option specifies the last observation to process
			(not necessarily the number being processed)
		This option is used with the data set that is being read (not the data set being created)   *;
	DATA	WORK.Illus;
		SET	SASHelp.Cars	(OBS = 10);
		RUN;

	PROC PRINT DATA = WORK.Illus;
		RUN;


	*	Using the FIRSTOBS= Data Set Option   *; 

	*	The FIRSTOBS= data set option specifies the first observation to process   *;
	DATA	WORK.Illus;
		SET	SASHelp.Cars	(FIRSTOBS = 400);
		RUN;

	PROC PRINT DATA = WORK.Illus;
		RUN;


	*	Using Both the FIRSTOBS= and OBS= Data Set Options   *;

	DATA	WORK.Illus;
		SET	SASHelp.Cars	(FIRSTOBS = 20	OBS = 30);
		RUN;

	PROC PRINT DATA = WORK.Illus;
		RUN;


*Part 8.2:  CONDITIONALLY RESTRICTING DATA SET OBSERVATIONS   *; 

*	Using the Subsetting IF Statement   *; 

*	The Subsetting IF statement continues processing only observations for which
		the condition specified in the expression following 'IF' is true

	The expression after the 'IF' keyword is evaluated.  When the expression is:
		TRUE:  SAS continues processing the observation
		FALSE:  SAS 1) Stops processing the observation
					2) Returns to the start of the DATA step
					3) Loads the next observation into the PDV   *;

	DATA	WORK.Illus;
		SET	SASHelp.Cars;
		IF	Origin = 'Asia';
		RUN;

	PROC PRINT DATA = WORK.Illus;
		RUN;


*	Multiple Conditions May be Specified   *; 

*	Use a compound expression to specify the multiple conditions
	The expressions may be joined using the logical operators AND or OR
		AND:  Evaluated as true if both conditions are true
		OR:   Evaluated as true if either condition is true *;
	DATA	WORK.Illus;
		SET	SASHelp.Cars;
		IF	Origin 		= 'Asia'	AND
			DriveTrain	= 'Rear'	;
		RUN;

	PROC PRINT DATA = WORK.Illus;
		RUN;
*	Although multiple IF statement could produce the same results, the code and processing
		are less efficient because SAS must process multiple statements

	Example using the OR logical operator   *;
	DATA	WORK.Illus;
		SET	SASHelp.Cars;
		IF	Origin 		= 'Asia'	OR
			DriveTrain	= 'Rear'	;
		RUN;

	PROC PRINT DATA = WORK.Illus;
		RUN;



*	Using DELETE Conditionally   *;

*	Use the DELETE statement conditionally to not include observations that don't
		meet the specified condition(s)   *;
	DATA	WORK.Illus;
		SET	SASHelp.Cars;
		IF	Origin 		= 'Asia'	OR
			DriveTrain	= 'Rear'	THEN DELETE;
		RUN;

	PROC PRINT DATA = WORK.Illus;
		RUN;


*	Using a WHERE Statement   *;

	*	The WHERE statement selects observations which meet a particular condition   *;
	DATA	WORK.ThirdBasemen;
		SET	SASHelp.Baseball;
		WHERE	FINDC(Position, '3') > 0;
		RUN;

	PROC PRINT DATA = WORK.ThirdBasemen;
		VAR	Name Team Position;
		RUN;


*	Using the WHERE= Data Set Option   *; 

*	The WHERE= data set option specifies the condition(s) which must be met to select
		observations from a data set   *;
	DATA	WORK.ThirdBasemen;
		SET	SASHelp.Baseball	(	WHERE = (	FINDC(Position, '3') > 0	)
								);
		RUN;

	PROC PRINT DATA = WORK.ThirdBasemen;
		VAR	Name Team Position;
		RUN;

*	Comment:  The WHERE statement and WHERE= data set option perform equally well.
	When we work with multiple data sets, it will be useful to know:
		1) If you want the same condition to apply to all data sets, it is easier to code
				a single WHERE statement
		2) If different conditions should be applied to different data sets, use
				WHERE= data set options for each data set	*;


*   Specifying Multiple Conditions   *; 

*	Method 1:  Using a compound WHERE statement   *;
	DATA	WORK.Illus;
		SET	SASHelp.Cars;
		WHERE	Origin		= 'Asia'	AND
				DriveTrain	= 'Rear';
		RUN;

	PROC PRINT DATA = WORK.Illus;
		RUN;

*	Method 2:  Using WHERE SAME AND
	Same comment as above   *;
DATA	WORK.Illus;
	SET	SASHelp.Cars;
	WHERE			Origin		= 'Asia';
	WHERE SAME AND	DriveTrain	= 'Rear';
	RUN;

PROC PRINT DATA = WORK.Illus;
	RUN;

*	NB:  Don't use 2 WHERE statements as shown below!
	The first WHERE statement will be replaced by the 2nd WHERE statement   *;
DATA	WORK.Illus;
	SET	SASHelp.Cars;
	WHERE	Origin		= 'Asia';
	WHERE 	DriveTrain	= 'Rear';
	RUN;

PROC PRINT DATA = WORK.Illus;
	RUN;


*Part 8.3:  Comparing IF vs WHERE:  How SAS Processes the PDV   *; 

*	The IF statement tests the expression and only outputs observations to
		the output data set if the condition is true.
		However, every observation is loaded into the PDV

	The WHERE statement tests the expression when the observation is
		in memory (in the input page buffer)
		Only observations for which the expression is true are loaded into the PDV

	Conclusion:  The output data set is created more efficiently using the WHERE statement

	However, the variables tested must exist in the input data set
		If they don't, then the IF statement must be used   *;

	*	Comparing IF vs WHERE:  When the expression uses a new variable   *; 

	*	Goal:  Keep the subset of observations containing students who are underweight (BMI < 16)   *;
	DATA	WORK.Underweight;
		SET	SASHelp.Class;

		BMI		= (Weight * 0.45) / (Height * 0.025)**2;
		FORMAT	BMI	4.1;

		IF	BMI < 16;

		RUN;

	PROC PRINT DATA = WORK.Underweight;
		RUN;

	*	Be aware of the following common programming errors!
		Wrong Approach 1:  Using a WHERE statement   *;
	DATA	WORK.Underweight;
		SET	SASHelp.Class;

		BMI		= (Weight * 0.45) / (Height * 0.025)**2;
		FORMAT	BMI	4.1;

		WHERE	BMI < 16;

		RUN;

	*	Wrong Approach 2:  Using an IF statement
		Can you determine why the data isn't filtered correctly?	*;
	DATA	WORK.Underweight;
		SET	SASHelp.Class;

		IF	BMI < 16;

		BMI		= (Weight * 0.45) / (Height * 0.025)**2;
		FORMAT	BMI	4.1;

		RUN;

	PROC PRINT DATA = WORK.Underweight;
		RUN;


	*	Subsetting When Using a Procedure   *;

	*	The IF statement is only a valid statement in the DATA step
		So procedures can only process subsets when using a WHERE statement (or data set option)
		Goal:  Print the data for the male students

		Correct Approach:  Use the WHERE statement   *;
	PROC PRINT	DATA = SASHelp.Class;
		WHERE	Sex = 'M';
		RUN;

	*	Incorrect Approach:  Using the IF statement   *;
	PROC PRINT	DATA = SASHelp.Class;
		IF	Sex = 'M';
		RUN;


****************************************************************************************
QUICK EXERCISE: 
	Task:  Print the observations from the 'prc.Laboratory_2015' data set
			which meet the following 2 conditions
			1)	Observations containing hematocrit values
			2)	In the date range from 9/16/2015 through 11/15/2015   *;

****************************************************************************************;

	PROC PRINT DATA = prc.Laboratory_2015;
		[add statement to filter the observations];
		RUN;


*	Additional Material: Special WHERE Operators   *; 

*	Special WHERE operators are not available for use with the IF statement

		/*The CONTAINS operator specifies a character string to search for*/
		/*Goal:  Create a subset of all 3rd basemen   *;*/
		/*	PROC PRINT DATA = SASHelp.Baseball;*/
		/*	WHERE	Position CONTAINS '3';*/
		/*	VAR	Name Team Position;*/
		/*	RUN;*/
		/**/
		/**	The BETWEEN AND operator specifies an inclusive range*/
		/*Using a compound WHERE statement   *;*/
		/*	PROC PRINT DATA = CanImpt.Laboratory_2015;*/
		/*	WHERE	Lab_Test = 'WHITE BLOOD CELL COUNT'	AND*/
		/*	Lab_Value	BETWEEN 8 AND 10	;*/
		/*	RUN;*/
		/**/
		/**/
		/**	The IS MISSING and IS NOT MISSING operators*/
		/*Note:  The alias is "IS NULL"   *;*/
		/*	PROC PRINT DATA = CanImpt.Laboratory_2015;*/
		/*	WHERE	Lab_Value IS MISSING;*/
		/*	RUN;*/
		/**/
		/**	The LIKE operator is used to match patterns*/
		/*The _ symbol represents a single space*/
		/*The % symbol represents any number of spaces*/
		/*Goal:  Print all subjects whose first name:*/
		/*1)	Begins with 'J', and*/
		/*2)	The 3rd letter is 'M'   *;*/
		/*	PROC PRINT DATA = CanImpt.Address;*/
		/*	WHERE	First LIKE 'J_M%';*/
		/*	RUN;*/
		/**/
		/**	Goal:  Print all subjects whose last name ends with 'N'   *;*/
		/*	PROC PRINT DATA = CanImpt.Address;*/
		/*	WHERE	Last LIKE '%N';*/
		/*	RUN;*/




*Part 8.4: SUBSETTING VARIABLES IN A DATA SET   *; 


*	Restricting the variables kept using DROP and KEEP statements   *;

*	Goal:  Keep all of the 'Career' baseball statistics

		Method 1:  KEEP the variables desired   *;
	DATA	WORK.Career;
		SET	SASHelp.Baseball;
		KEEP	Name Cr:;
		RUN;

	PROC PRINT DATA = WORK.Career (OBS = 5);
		RUN;

	*	Method 2:  DROP the undesired variables   *;
	DATA	WORK.Career;
		SET	SASHelp.Baseball;
		DROP	Team -- YrMajor League -- logSalary;
		RUN;

	PROC PRINT DATA = WORK.Career (OBS = 5);
		RUN;


*	Multiple DROP or KEEP Statements are Permitted   *; 

	DATA	WORK.Career;
		SET	SASHelp.Baseball;
		DROP	Team -- YrMajor;
		DROP	League -- logSalary;
		RUN;

	PROC PRINT DATA = WORK.Career (OBS = 5);
		RUN;


*	DROP and KEEP Statements may Both be Used   *;

*	Goal:  Keep all career statistics except walks   *;
	DATA	WORK.Career;
		SET	SASHelp.Baseball;
		KEEP	Name Cr:;
		DROP	CrBB;
		RUN;

	PROC PRINT DATA = WORK.Career (OBS = 5);
		RUN;


*	Using the DROP= and KEEP= Data Set Options   *;

*	The DROP= and KEEP= data set options limit the variables read into the PDV   *;
	DATA	WORK.Career;
		SET	SASHelp.Baseball	(	KEEP	= Name Cr:	);
		RUN;

	PROC PRINT DATA = WORK.Career (OBS = 5);
		RUN;


*	An Efficient Approach   *;

*	Goal:  Keep only the student's name and BMI   *;
	DATA	WORK.BMI	(	KEEP	= Name BMI	); * Keeping only the desired variables *;

		SET	SASHelp.Class	(	KEEP	= Name Height Weight	); * Reading only needed variables into the PDV *;

		BMI		= (Weight * 0.45) / (Height * 0.025)**2;
		FORMAT	BMI	4.1;
		RUN;

	PROC PRINT DATA = WORK.BMI;
		RUN;




****************************************************************************************
PART NINE: STACKING DATASETS
****************************************************************************************;
*Part 9.1: CONCATENATING DATA SETS   *; 

*	Concatenating "stacks" the data sets vertically on top of each other
	Concatenating data is most commonly performed when the data sets contain the same variables

	In the output data set, the final number of:
		1)	Observations is the sum of the observations in each component data set
		2)	Variables is the number of unique variables across all component data sets   *;


*	Using the SET Statement to Concatenate Data Sets   *; 

	DATA	WORK.Illus;
		SET	prc.Laboratory_2015
			prc.Laboratory_2016;
		RUN;

	PROC PRINT DATA = WORK.Illus;
		RUN;

*	Note:  The order of observations in the output data set is determined by the order in
			the SET statement
			So the 2015 data is before the 2016 data in the output data set
	In WORK.Illus, the number of:
		1)	Observations is 81 + 45 = 126
		2)	Variables is 5 (the same 5 variables are in both component data sets)   *;
*	Interleaving the Observations   *; 

*	Interleaving is more efficient than concatenating and then re-sorting
	The observations within each source data set must be ordered by the BY variable(s)

	Goal:  Concatenate the 'Lab_data_ptN' patient lab data into a data set that's ordered by 'LabDate'
		Notice that in each component data set the observations are in order by date   *;
	DATA	WORK.Illus;
		SET	prc.Lab_:;
		BY	LabDate;
		RUN;

	PROC PRINT DATA = WORK.Illus;
		RUN;

*	In WORK.Illus, the number of:
		1)	Observations is 4 + 3 + 4 + 4 + 4 = 19
		2)	Variables is 6 (the same 6 variables are in each component data set)   *;

****************************************************************************************
PART TEN: MERGING DATASETS (ie Combining data horizontally)
****************************************************************************************;

*	Combining data horizontally is most commonly referred to as "merging" data
	Merging data is most often performed when the data sets contain different variables

	For a merge, in the output data set, the final number of:
		1)	Observations is the maximum of the number of observations from the component data sets
		2)	Variables is sum of the uniquely-named variables across all component data sets

	For a match-merge, in the output data set, the final number of:
		1)	Observations is the number of unique observations which are in each of the component data sets
				(where uniqueness is determined by the BY-variables)
		2)	Variables is sum of the uniquely-named variables across all component data sets   *;




*Part 10.1:  ONE-TO-ONE MERGES   *;


*	Performing a Merge   *; 

*	A merge simply joins together observations by row
		e.g. The first rows from each data set are combined
			 The 2nd rows from each data set are combined, etc.

	Creating data for the illustration   *;
DATA	WORK.A;
	INFILE	DATALINES;
	INPUT	Name $ SexCd $;
	DATALINES;
Aika F
Brandon M
Carol F
;

PROC PRINT DATA = WORK.A;
	RUN;

DATA	WORK.B;
	INFILE	DATALINES;
	INPUT	First $ Race $9.;
	DATALINES;
Carol Caucasian
Aika Asian
Brandon Black
;

PROC PRINT DATA = WORK.B;
	RUN;

*	This is a merge   *;
	DATA	WORK.Illus;
		MERGE	WORK.A
				WORK.B;
		RUN;

	PROC PRINT DATA = WORK.Illus;
		RUN;

*	NB:  This is most often NOT what is desired

	Nevertheless, in WORK.Illus, the number of:
		1)	Observations is 3 (there are 3 in both component data sets)
		2)	Variables is 2 + 2 = 4   *;

*Part 10.2:  Performing a Match-Merge   *;

*	Most commonly the observations being merged are related to each other

	We therefore want to join the observations according to a variable that identifies
		each observation (multiple variables may be needed to uniquely identify each obs)
	The variable(s) that uniquely identify an observation are "key" variable(s)
	NB:  The "key" variable must have the same type and name to merge the data sets

	A one-to-one match-merge is appropriate when a single observation in one data
		set is related to one and only one observation in another data set

	The MERGE statement is used to combine data sets horizontally
	The BY statement is required for a match-merge
	Recall:  When using a BY statement, the data sets must be ordered by the BY variable(s)

	Goal:  Combine the contact and demographic data for each subject   *;
	DATA	WORK.Illus;
		MERGE	prc.Contact
				prc.Demog;
		BY	SubjID;
		RUN;

	PROC PRINT DATA = WORK.Illus;
		RUN;

*	The order of variables in the output data set is determined by the order in
		the MERGE statement
		So the variables from 'Contact' are before the variables from 'Demog' data set
	In WORK.Illus, the number of:
		1)	Observations is 16 (the same 16 are in both component data sets)
		2)	Variables is 10 + 7 - 1 = 16
				'SubjID' is in both data sets (thus the -1),
				so there are 16 uniquely-named variables across the component data sets   *;

*	Merging a Summary Statistic Into a Data Set   *; 

*	Creating data for the illustration   *;
DATA	WORK.Basketball;
	INFILE	DATALINES;
	INPUT	Team	$
			Player	$
			Points	;
	DATALINES;
Cobras  Chloe   16
Cobras  Claire  4
Cobras  Callie  14
Cobras  Caden   12
Cobras  Caleb   3
Lynx    Lucas   2
Lynx    Layla   5
Lynx    Lily    4
Lynx    Lucy    9
Lynx    Logan   16
;

	PROC PRINT DATA = WORK.Basketball;
		RUN;

*	Task:  Calculate the percent of the team's points each player scored

	Creating summary statistic (total points) for each team
	This could also be accomplished with PROC MEANS   *;
	DATA	WORK.TeamTotals	(	KEEP	=	Team TotalPoints	);
		SET	WORK.Basketball;
		BY	Team;

		IF	FIRST.Team	= 1	THEN	TotalPoints = .;

		TotalPoints + Points;

		IF	LAST.Team	= 1;

		RUN;

	PROC PRINT DATA = WORK.TeamTotals;
		RUN;

*	Having a common variable ('Team') allows us to perform a
		one-to-many match-merge   *;
	DATA	WORK.Statistics;
		MERGE	WORK.Basketball
				WORK.TeamTotals;
		BY	Team;

		Percent	= Points / TotalPoints;
		FORMAT	Percent	PERCENT5.;

		RUN;

	PROC PRINT DATA = WORK.Statistics;
		RUN;


****************************************************************************************
PART ELEVEN: RESTRUCTURING DATASETS
****************************************************************************************;
*Part 11.1: CREATING MULTIPLE DATA SETS USING OUTPUT STATEMENTS   *; 


*	 Multiple Data Sets In the DATA Statement   *; 

*	Listing multiple data sets in the DATA statement creates multiple data sets
	Note:  When no OUTPUT statements are used, each observation goes to all data sets listed   *;
	DATA	WORK.Illus
			WORK.Illus2;
		SET	prc.Scans;
		RUN;

	PROC PRINT DATA = WORK.Illus;
		RUN;

	PROC PRINT DATA = WORK.Illus2;
		RUN;


*Part 11.2: Conditionally Outputting Observations   *; 

*	Observations may be output to data sets conditionally based on evaluating 1 or more expressions   *;
	DATA	WORK.Scans_Site1
			WORK.Scans_Site2;
		SET	prc.Scans;

		IF			SUBSTR(SubjID, 1, 1) = '1'	THEN	OUTPUT WORK.Scans_Site1;
			ELSE IF SUBSTR(SubjID, 1, 1) = '2'	THEN	OUTPUT WORK.Scans_Site2;

		RUN;

	TITLE1 "Site 1 Scans";
	PROC PRINT DATA = WORK.Scans_Site1;
		RUN;

	TITLE1 "Site 2 Scans";
	PROC PRINT DATA = WORK.Scans_Site2;
		RUN;


*Part 11.3: CREATING "LONG" DATA FROM "WIDE" DATA   *; 

*	Re-structuring a data set is sometimes referred to as "rotating" a data set

	The observations and variables create a "rows" by "columns" matrix
	A transpose simply converts rows to column and vice versa

	The 'CanImpt.Lab_Data_Pt1' data set is in a 'Wide' format, i.e.

	Pt	LabDate		WBC		RBC		HGB		HCT
	--	---------	----	----	----	----
	1	28JUL2015	10.8	 4.1	12.1	49.3
	1	18AUG2015	 9.4	 3.8	11.4	38.5
	...	...			...		...		...		...

	Goal:  Re-structure the data set into a 'Long' format, i.e.
	Pt	LabDate		LabTest	LabValue
	--	---------	-------	--------
	1	28JUL2015	WBC		10.8
	1	28JUL2015	RBC		 4.1
	1	28JUL2015	HGB		12.1
	1	28JUL2015	HCT		49.3
	2	18AUG2015	WBC		 9.4
	2	18AUG2015	RBC		 4.1
	2	18AUG2015	HGB		11.4
	2	18AUG2015	HCT		38.5
	...	...			...		...	

	Data may be re-structured using either PROC TRANSPOSE or the DATA step
	PROC TRANSPOSE often requires less code
		(variable names will just need to be specified in a few statements)
	The DATA step is preferred if you need to do other data manipulation
		(e.g. creating new variables, conditional processing, etc.)
	The DATA step may be easier to manage if you understand how data is
		processed during execution through the PDV   *;


*	A Basic Transposition of Data   *; 

*	PROC TRANSPOSE can be used to re-structure a data set
	By default, PROC TRANSPOSE will transpose each numeric variable into a row and store the
		transposed data set into a temporary data set named 'DataN' (N = sequential numbers)


	When PROC TRANSPOSE transposes the rows and columns, the new columns are assigned
		generic names by default:
		_NAME_, which contains the list of variables transposed, and
		COL1 -- COLn, which used to be the rows   *;
	PROC PRINT DATA = SASHelp.Class;
		RUN;

	PROC TRANSPOSE	DATA = SASHelp.Class;
		RUN;

	PROC PRINT DATA = Data1;
		RUN;


*	Using the OUT= Option   *;

*	The OUT= option names a data set to contain the transposed data   *;
	PROC TRANSPOSE
			DATA	= SASHelp.Class
			OUT		= WORK.Class_Tran;
		RUN;

	PROC PRINT DATA = WORK.Class_Tran;
		RUN;


*	Transposing a Single Variable Using PROC TRANSPOSE   *;

*	First, let's look at the 'WBC' variable in the 'CanImpt.Lab_Data_Pt1' data set   *;
	PROC PRINT DATA = prc.Lab_Data_Pt1;
		VAR WBC;
		RUN;

*	The 'WBC' values are currently in a column (i.e. a 4x1 matrix)
	The VAR statement lists the variables to be transposed (i.e. turned into rows)   *;
	PROC TRANSPOSE
			DATA	= prc.Lab_Data_Pt1
			OUT		= WORK.Lab_Data_Pt1T;
		VAR	WBC;
		RUN;

	PROC PRINT DATA = WORK.Lab_Data_Pt1T;
		RUN;
*	Now the 'WBC' values are in a row (i.e. a 1x4 matrix)   *;


*	Transposing Multiple Variables Using PROC TRANSPOSE   *; 

*	This transposes a 4x4 matrix into a 4x4 matrix   *;
	PROC PRINT DATA = prc.Lab_Data_Pt1;
		RUN;

	PROC TRANSPOSE
			DATA	= prc.Lab_Data_Pt1
			OUT		= WORK.Lab_Data_Pt1T;
		VAR	WBC -- HCT;
		RUN;

	PROC PRINT DATA = WORK.Lab_Data_Pt1T;
		RUN;


*	Using the BY Statement to Specify 1 x p Matrices   *; 

*	The BY statement transposes data within groups

	Using BY-group processing will transpose each BY-group individually
	In this case, each BY-group is a single row
		(because our data has 1 row per unique LabDate value)

	Recall:  To use BY-group processing, data must be ordered by the variables in the BY statement

	This shows the data before the transposition   *;
	PROC PRINT DATA = prc.Lab_Data_Pt1;
		RUN;

	PROC TRANSPOSE
			DATA	= prc.Lab_Data_Pt1
			OUT		= WORK.LabsPt1_Long;
		VAR	WBC -- HCT;
		BY	LabDate;
		RUN;

	PROC PRINT DATA = WORK.LabsPt1_Long;
		RUN;


*	Including Other Variables in the Output   *; 

*	Other variables will be included in the output data set when named in the BY statement
	In this illustration, I am adding the 'Pt' variable to the BY statement
	Note:  This approach is typically fine in a "wide" to "long" conversion
			because when the data is "wide" the other variables can't vary
			within each level of the BY variable   *;
	PROC TRANSPOSE
			DATA	= prc.Lab_Data_Pt1
			OUT		= WORK.LabsPt1_Long(rename=(col1=LabValue _name_=LabTest));
		VAR	WBC -- HCT;
		BY	Pt LabDate;
		RUN;

	PROC PRINT DATA = WORK.LabsPt1_Long;
		RUN;


*Part 11.4: CREATING "WIDE" DATA FROM "LONG" DATA   *; 

*	Now I will reverse the process to turn the 'WORK.LabsPt1_Long' data set into
		a "wide" data set named 'WORK.LabsPt1_Wide' structured like the original data set   *;


*	Using PROC TRANSPOSE   *; 

*	Let's look at the long data   *;
	PROC PRINT DATA = WORK.LabsPt1_Long;
		RUN;

*	Realize that we want to make each date's 4x1 column into a 1x4 row
		(i.e. we don't want to end up with a single 1x16 row!)
	To accomplish this we will put 'LabDate' in the BY statement   *;
	PROC TRANSPOSE
			DATA	= WORK.LabsPt1_Long
			OUT		= WORK.LabsPt1_Wide	(	DROP	= _NAME_	);
		VAR	LabValue; * This is a 4x1 matrix within each value of 'LabDate' *;
		BY	Pt LabDate;
		RUN;

	PROC PRINT DATA = WORK.LabsPt1_Wide;
		RUN;


*	Naming Transposed Variables Using the PREFIX= Option   *;

*	The PREFIX= option allow the specification of a prefix for the transposed variables
		(note that the default prefix is 'COL')   *;
	PROC TRANSPOSE
			DATA	= WORK.LabsPt1_Long
			OUT		= WORK.LabsPt1_Wide	(	DROP	= _NAME_	)
			PREFIX	= LabVal;
		VAR	LabValue;
		BY	Pt LabDate;
		RUN;
*	Note:  In Wide-to-Long transformations I rarely use the PREFIX= option because typically
			a single column is being created
			(so I name that column using the RENAME= data set option)   *;

	PROC PRINT DATA = WORK.LabsPt1_Wide;
		RUN;


*	Naming Transposed Variables Using the RENAME= Data Set Option   *; 

*	We could also change the variable names using the RENAME= data set option   *;
	PROC TRANSPOSE
			DATA	= WORK.LabsPt1_Long
			OUT		= WORK.LabsPt1_Wide	(	DROP	= _NAME_
											RENAME	= (	COL1	= WBC
														COL2	= RBC
														COL3	= HGB
														COL4	= HCT	)
										);
		VAR	LabValue;
		BY	Pt LabDate;
		RUN;

	PROC PRINT DATA = WORK.LabsPt1_Wide;
		RUN;


*	Naming Transposed Variables Using the ID Statement   *; 

*	The ID statement specifies which variable's values should be used as
		the names of the transposed variables.
	When performing a Long-to-Wide transformation, there SHOULD be a variable whose
		values identify each value (such as our 'LabTest' variable)!
		Using such a variable in the ID statement is very useful
			(i.e. it obviates the need for renaming several variables!)
	When performing a Wide-to-Long transformation, there is often not a variable which
		would be useful in an ID statement (so I typically don't use the ID statement then)
	If the ID variable is numeric, underscores will precede the value in the variable name

	Note:  Values of the variable(s) in the ID statement must be unique within BY groups
			(because you can't have multiple variables with the same name in a data set,
				which is what would be attempted in the transposed data set!)   *;
	PROC TRANSPOSE
			DATA	= WORK.LabsPt1_Long
			OUT		= WORK.LabsPt1_Wide	(	DROP	= _NAME_	);
		VAR	LabValue;
		BY	Pt LabDate;
		ID	LabTest;
		RUN;

	PROC PRINT DATA = WORK.LabsPt1_Wide;
		RUN;


*	Using the PREFIX= Option with the ID Statement   *; 

*	This shows another option for naming the transposed variables
		(creating variable names using the combination of a prefix and variable value) *;
	PROC TRANSPOSE
			DATA	= WORK.LabsPt1_Long
			OUT		= WORK.LabsPt1_Wide	(	DROP	= _NAME_	)
			PREFIX	= LabVal_;
		VAR	LabValue;
		BY	Pt LabDate;
		ID	LabTest;
		RUN;

	PROC PRINT DATA = WORK.LabsPt1_Wide;
		RUN;


*	Using the ID Statement with Multiple Variables   *; 

*	Suppose within each subject we'd like all the values to be in a single observation
	Using the statement 'ID LabTest' won't work because the values of LabTest aren't
		unique within each subject (i.e. every subject has several values of 'WBC' etc.

	Multiple variables (e.g. LabTest and LabDate) can be specified to create unique
		combinations --> unique variable names
	The concatenated values will become the variable names   *;
	PROC TRANSPOSE
			DATA	= WORK.LabsPt1_Long
			OUT		= WORK.LabsPt1_SuperWide	(	DROP	= _NAME_	);
		VAR	LabValue;
		BY	Pt;
		ID	LabTest LabDate;
		FORMAT	LabDate YYMMDDD10.;
		RUN;

	PROC PRINT DATA = WORK.LabsPt1_SuperWide;
		RUN;


*NOTE: There are ways to restructure a dataset using the DATA Step!;
		*If you want to learn this, come talk to me;

****************************************************************************************
QUICK EXERCISE: 
	Below is a data set of years different teams have won the Super Bowl
		(in "wide" format);

*	Task:  Re-structure the data set to be in "long" format, which will look like:
	Team		Year
	--------	----
	Cowboys		1972
	Cowboys		1978
	...			...
	Steelers	2009   *;

****************************************************************************************;

DATA WORK.SuperBowlWins;
	INFILE DATALINES DELIMITER=',' MISSOVER;
	INPUT	Team $ Year1 Year2 Year3 Year4 Year5 Year6;
	DATALINES;
Cowboys,1972,1978,1993,1994,1996
Patriots,2002,2004,2005,2015,2017,2019
Steelers,1975,1976,1979,1980,2006,2009
;

PROC PRINT DATA = WORK.SuperBowlWins;
	RUN;


PROC TRANSPOSE
		DATA = WORK.SuperBowlWins
		OUT	 = WORK.SBWinsLong	(	DROP	= _NAME_
									RENAME	= (	[finish option]	)
								);
	BY	[finish statement];
	VAR	[finish statement];
	RUN;



****************************************************************************************
PART TWELVE: STATISTICS PROCs and the OUTPUT DELIVERY SYSTEM!
****************************************************************************************;

*Part 12.1: CREATING OUTPUT DATA SETS CONTAINING ANALYSIS RESULTS   *; 

*	The Output Delivery System (ODS) may be used to create output data sets
		which contain the results from any statistical procedure

	Statistical procedures often produce several tables of output results
	Each table has a unique identifying table name which may be used to save the results
	To use ODS to save the results:

		Step 1:  Figure out the table name of the desired output using either
					A)	The SAS documentation, or
					B)	ODS TRACE
						1) Place the statement ODS TRACE ON before the procedure
						2) Place the statement ODS TRACE OFF after the procedure
						3) Look in the log for the name of each table produced

		Step 2:  Use an ODS statement to send the table's contents to a data set
					Use the syntax:
					ODS OUTPUT desired-table-name = destination-data-set;


*	DESCRIPTIVE STATISTICS FOR CHARACTER VARIABLES   *; 

*	PROC FREQ is the procedure designed to summarize and analyze distinct levels
		of variables
	PROC FREQ is used predominantly with classification variables (which are typically
		character), but may also be used with numeric variables

	The TABLES statement specifies the variable(s) to be summarized
	Although it is an optional statement, when it isn't used frequencies for ALL variables
		are displayed (often not desired and can produce voluminous output)
	By default, values are displayed in order of the unformatted values

	PROC FREQ produces 2 types of output data sets which may be requested as follows:
	1)	Specify the OUT= option in a TABLES statement
			This creates an output data set that contains frequency counts and percentages
	2)	Specify an OUTPUT statement
			This creates an output data set that contains statistics   *;


*	Generating a Frequency Table of the Levels for a Variable   *;

*	Variable	Type		Classification	Why Use in PROC FREQ
	--------	----		--------------	--------------------
	TxNm		Character	Yes				Summarize
	EthRaceCd	Numeric		Yes				Summarize
	SubjID		Character	No				Data cleaning - check for unique values
	AgeAtEnroll	Numeric		No				Data cleaning - check ranges (PROC MEANS better)   *;
	
	PROC FREQ	DATA = prc.CanAnalysis1;
		TABLES	TxNm EthRaceCd SubjID AgeAtEnroll;
		RUN;

	*	Creating a Data Set with PROC FREQ Results   *; 

	*	Use ODS   *;

	ODS TRACE ON;
	PROC FREQ	DATA = prc.CanAnalysis1;
		TABLES	EthRaceCd;
		RUN;
	ODS TRACE OFF;



*	In the log window we can see that the table name is 'OneWayFreqs'   *;

	ODS OUTPUT OneWayFreqs = WORK.FREQResults1;
	PROC FREQ	DATA = prc.CanAnalysis1;
		TABLES	EthRaceCd;
		RUN;

	PROC PRINT DATA = WORK.FREQResults1;
		RUN;

*Part 12.2: DESCRIPTIVE STATISTICS FOR NUMERIC VARIABLES   *; 

*	PROC MEANS is a procedure designed to summarize numeric variables
	PROC MEANS may only be used on numeric variables
	PROC SUMMARY uses the same exact syntax as PROC MEANS
	PROC UNIVARIATE also provides extensive summaries of numeric variables
		(PROC UNIVARIATE is discussed in SECTION 5.1.6 - Additional Material) 

	The VAR statement specifies the variable(s) to be summarized
	Although it is an optional statement, when it's not used summaries for ALL numeric
		variables are displayed (often not desired)
		Although the output is concise, it can be non-sensical on some variables (e.g. BirthDt)

	PROC MEANS produces output data sets which may be requested as follows:
	1)	Specify an OUTPUT statement
			This creates an output data set that contains statistics   *;


*	Using PROC MEANS   *; 

*	Note that summarizing 'AgeAtEnroll' is better using PROC MEANS than PROC FREQ   *;
	PROC MEANS	DATA = prc.CanAnalysis1;
		VAR	AgeAtEnroll LesionRatio;
		RUN;

*	PROC MEANS will work on any numeric variable (such as EthRaceCd)
	However, this illustration produces non-sensical output   *;
	PROC MEANS	DATA = prc.CanAnalysis1;
		VAR	EthRaceCd;
		RUN;


*	Creating a Data Set with PROC MEANS Results   *; 

*	Use ODS   *;
	ODS TRACE ON;
	PROC MEANS	DATA = prc.CanAnalysis1;
		VAR	AgeAtEnroll LesionRatio;
		RUN;
	ODS TRACE OFF;
*	In the log we can see that the table name is 'Summary'   *;

	ODS OUTPUT Summary = WORK.MEANSResults1;
	PROC MEANS	DATA = prc.CanAnalysis1;
		VAR	AgeAtEnroll LesionRatio;
		RUN;
	*	This can produce v-e-r-y wide output   *;

	PROC PRINT DATA = WORK.MEANSResults1;
		RUN;

*	 Displaying Results by Levels of a Classification Variable   *; 

*	Use the CLASS statement to specify 1 or more classification variables
	A row of summary statistics is displayed for every level of the classification variable
	When the CLASS statement is used, NObs is displayed by default *;
	PROC MEANS
			DATA	= prc.CanAnalysis1
			MAXDEC	= 1
			MEAN MEDIAN;
		CLASS	HadAEInd;
		VAR	AgeAtEnroll;
		RUN;

*Part 12.3: ASSOCIATION BETWEEN 2 CHARACTER VARIABLES   *; 

*	PROC FREQ can assess associations between categorical variables
	Use an asterisk (*) to "cross" variables in the TABLES statement
	The last variable listed is always the Column variable
	The 2nd-to-last variable listed is always the Row variable

	The following all have the same meaning:
		Cross-Tabulation
		2-Way Table (also 3-Way, ... , N-Way)
		Contingency Table   *;


*	Creating a Cross-Tabulation   *; 

*	Goal:  Find the percentage of subjects who had an AE in each treatment group   *;
	PROC FREQ DATA = prc.CanAnalysis1;
		TABLES	HadAEInd TxNm;
		TABLES	HadAEInd * TxNm; * This produces a cross-tabulation *;
		RUN;


*	Customizing the Output with TABLES Options   *; 

*	NOFREQ		Removes the display of the cell frequencies
	NOPERCENT	Removes the display of the cell, marginal, and overall percentages
	NOROW		Removes the display of the row percentages
	NOCOL		Removes the display of the column percentages   *;
	PROC FREQ DATA = prc.CanAnalysis1;
		TABLES	HadAEInd * TxNm	/	NOPERCENT NOROW NOCOL;
		RUN;


*	Testing the Association Between 2 Classification Variables   *; 

*	The CHISQ option will perform a Chi-Square test of association between
		the crossed variables
	Due to the small Expected cell counts, Fisher's Exact Test would be a more
		appropriate statistical test than a Chi-Square test (in this illustration)
	CHISQ will produce Fisher's Exact Test results for 2 x 2 tables
	FISHER will produce Fisher's Exact Test results for r x c tables*;
	PROC FREQ DATA = prc.CanAnalysis1;
		TABLES	 TxNm *HadAEInd	/	NOPERCENT NOROW CHISQ;
		RUN;


*	Creating a Data Set with PROC FREQ Results   *;

*	Use ODS   *;
	ODS TRACE ON;
	PROC FREQ DATA = prc.CanAnalysis1;
		TABLES	HadAEInd * TxNm	/	NOPERCENT NOROW CHISQ;
		RUN;
	ODS TRACE OFF;
	*	In the log we can see that tables of interest include 'CrossTabFreqs' and 'ChiSq'   *;

	ODS OUTPUT	CrossTabFreqs = WORK.CrossTabResults1 
				ChiSq = WORK.ChiSqResults1;
	PROC FREQ DATA = prc.CanAnalysis1;
		TABLES	HadAEInd * TxNm	/	NOPERCENT NOROW CHISQ;
		RUN;

	PROC PRINT DATA = WORK.CrossTabResults1;
		RUN;

	PROC PRINT DATA = WORK.ChiSqResults1;
		RUN;



*Part 12.4: ASSOCIATION BETWEEN 2 NUMERIC VARIABLES   *; 

*	Producing Correlation Statistics   *; 

*	PROC CORR will perform a correlation analysis
	Correlations will be produced between all variables listed in the
		VAR statement   *;
	PROC CORR DATA = prc.CanAnalysis1 PLOTS=MATRIX;
		VAR	LesionSumCmMon0 LesionSumCmMon6 LesionRatio;
		RUN;


*	Creating a Data Set with PROC CORR Results   *; 

*	Use ODS   *;
	ODS TRACE ON;
	PROC CORR
			DATA	= prc.CanAnalysis1
			PLOTS	= MATRIX;
		VAR	LesionSumCmMon0 LesionSumCmMon6 LesionRatio;
		RUN;
	ODS TRACE OFF;
	*	In the log we can see that the table name is 'PearsonCorr'   *;

	ODS OUTPUT	PearsonCorr = WORK.CorrResults1;
	PROC CORR
			DATA	= prc.CanAnalysis1
			PLOTS	= MATRIX;
		VAR	LesionSumCmMon0 LesionSumCmMon6 LesionRatio;
		RUN;

	PROC PRINT DATA = WORK.CorrResults1;
		RUN;



*	Performing a Simple Linear Regression (PROC REG and PROC GLM)  *;

*	PROC REG is one of a few interactive procedures
	These PROCs need a QUIT statement to end their invocation   *;
	PROC REG DATA = prc.CanAnalysis1	PLOTS = NONE;
		MODEL	LesionSumCmMon6 = LesionSumCmMon0;
		RUN;
		QUIT;


	PROC GLM DATA = prc.CanAnalysis1;
		MODEL	LesionSumCmMon6 = LesionSumCmMon0/solution;
		RUN;
		QUIT;

*	Creating a Data Set with PROC REG Results   *; 

*	 Use ODS   *;
	ODS TRACE ON;
	PROC REG DATA = prc.CanAnalysis1	PLOTS = NONE;
		MODEL	LesionSumCmMon6 = LesionSumCmMon0;
		RUN;
		QUIT;
	ODS TRACE OFF;
	*	In the log we can see that the table name is 'ParameterEstimates'   *;

	ODS OUTPUT	ParameterEstimates = WORK.BetasResults1;
	PROC REG DATA = prc.CanAnalysis1	PLOTS = NONE;
		MODEL	LesionSumCmMon6 = LesionSumCmMon0;
		RUN;
		QUIT;

	PROC PRINT DATA = WORK.BetasResults1;
		RUN;



*Part 12.5: ASSOCIATION BETWEEN 1 CHARACTER AND 1 NUMERIC VARIABLE   *; 


*	Performing a t-Test   *; 

*	PROC TTEST will perform a t-test between groups defined by the values of the
		variable listed in the CLASS statement (this must have only 2 levels)
	The dependent variable is listed in the VAR statement

	Goal:  Perform a t-test on the 'LesionRatio' variable between the 2
			treatment groups (i.e. by the 'TxNm' variable)   *;
	PROC TTEST DATA = prc.CanAnalysis1;
		CLASS	TxNm;
		VAR	LesionRatio;
		RUN;


*	Creating a Data Set with PROC TTEST Results   *; 

*	 Use ODS   *;
	ODS TRACE ON;
	PROC TTEST DATA = prc.CanAnalysis1;
		CLASS	TxNm;
		VAR	LesionRatio;
		RUN;
	ODS TRACE OFF;
*	In the log we can see that desired tables include 'Statistics' and 'TTests'   *;

	ODS OUTPUT	Statistics	= WORK.StatsResults1
				TTests		= WORK.TStatsResults1;
	PROC TTEST DATA = prc.CanAnalysis1;
		CLASS	TxNm;
		VAR	LesionRatio;
		RUN;

	PROC PRINT DATA = WORK.StatsResults1;
		RUN;

	PROC PRINT DATA = WORK.TStatsResults1;
		RUN;


*	Performing an Analysis of Variance (ANOVA)   *; 

*	PROC ANOVA will perform an Analysis of Variance (ANOVA) which is a test
		of the null hypothesis that the mean of a specified continuous variable
		is equal across all levels of a classification variable

	By default, the levels of the CLASS variable are the formatted values
	ORDER = INTERNAL orders the levels by unformatted values

	Goal:  Test whether the mean of LesionRatio is the same across all levels
			of adverse event grade (i.e. of 'MaxGrade')   *;
	PROC ANOVA
			DATA	= prc.CanAnalysis1
			ORDER	= INTERNAL;
		CLASS	MaxGrade;
		MODEL	LesionRatio = MaxGrade;
		RUN;
		QUIT;


*	Creating a Data Set with PROC ANOVA Results   *; 

	*	Use ODS   *;
	ODS TRACE ON;
	PROC ANOVA
			DATA	= prc.CanAnalysis1
			ORDER	= INTERNAL;
		CLASS	MaxGrade;
		MODEL	LesionRatio = MaxGrade;
		RUN;
		QUIT;
	ODS TRACE OFF;
	*	In the log we can see that desired tables include 'OverallANOVA' and 'ModelANOVA'   *;

	ODS OUTPUT	OverallANOVA	= WORK.ANOVAResults1
				ModelANOVA		= WORK.ModelResults1;
	PROC ANOVA
			DATA	= prc.CanAnalysis1
			ORDER	= INTERNAL;
		CLASS	MaxGrade;
		MODEL	LesionRatio = MaxGrade;
		RUN;
		QUIT;

	PROC PRINT DATA = WORK.ANOVAResults1;
		RUN;

	PROC PRINT DATA = WORK.ModelResults1;
		RUN;



*Part 12.6: MIXED MODELS USING SAS;

* 	There are a lot of options for PROC MIXED.... so check the documentation:
		https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.3/statug/statug_mixed_syntax01.htm;

*	PROC MIXED DATA= <dataname> 	<METHOD=REML/ML/.../> ;
*		CLASS <categorical variables>;
*		MODEL <outcome> = <covariates> / SOLUTION;
*		RANDOM <random effects>/SUB=<subject> TYPE=<covariance structure>;
* 		...		;
*		RUN;


*In order to teach this section, I reran some of the models you learned in BIOS 6612;

*Random intercept;
	
	*This is the model in R;
	/*pigs_randint = lmer(weight ~ (1 | id.num) + num.weeks, data = pig.weights)*/
	/*summary(pigs_randint)*/
	/*summary(pigs_randint)$coefficients*/

	PROC MIXED DATA=prc.pigweights;
		CLASS id_num;
		MODEL weight=num_weeks/solution;
		RANDOM intercept/SUB=id_num TYPE=un;
		run;



*Random intercept and slope;

	*This is the model in R;
	/*pigs_randslope = lmer(weight ~ (1 + num.weeks | id.num) + num.weeks, data = pig.weights)*/
	/*summary(pigs_randslope)*/
	/*summary(pigs_randslope)$coefficients*/

	PROC MIXED DATA=prc.pigweights;
		CLASS id_num;
		MODEL weight=num_weeks/solution;
		RANDOM intercept num_weeks/SUB=id_num TYPE=un;
		run;


	*Other SAS PROCs that can fit mixed models
	-	GLM: includes a RANDOM statement, in general should not use for mixed models
	-	GLIMMIX: has similar statements to PROC MIXED but can be applied to non-normal outcomes
	-	NLMIXED: very different set of statements but an extremely useful PROC, has functions included but can also program your own likelihood
	-	GENMOD: similar statement structure to proc mixed, fits GEE models, can be used for normal and non-normal outcomes. Note: default cov structure is independent cov effects;



****************************************************************************************
PART THIRTEEN: SUPER QUICK INTRO TO SAS FIGURES
****************************************************************************************;

* Types of graphs you can make;
*HBAR 		-> Horizontal bar graph
*VBAR 		-> Vertical bar graph
*SCATTER 	-> Scatterplot
*REG		-> Linear regression
*HBOX 		-> Horizontal boxplot
*VBOX		-> Vertical boxplot
*SERIES 	-> Spaghetti plot
*There are many more!: https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/grstatproc/p1t32i8511t1gfn17sw07yxtazad.htm;


*Vertical boxplot of Lesion Ratio by Sex;
	PROC SGPLOT DATA = prc.CanAnalysis1;
		VBOX	LesionRatio	/	CATEGORY	= SexCd;
		FORMAT sexcd _all_; *This is just to make this dataset work--> It's from my class;
		RUN;

	PROC SGPLOT DATA = prc.CanAnalysis1;
		VBOX	LesionRatio	/	GROUP	= SexCd;
		FORMAT sexcd _all_; *This is just to make this dataset work--> It's from my class;
		RUN;

*Vertical boxplot of Lesion Ratio by Sex;
DATA	WORK.TumorVol;
	INFILE	DATALINES;
	INPUT	Day	Volume;
	DATALINES;
0  63
4  73
7  88
11 104
14 120
18 143
21 161
25 209
28 247
;

*	Goal:  Plot the tumor volume by time (Day)   *;
	PROC SGPLOT DATA = WORK.TumorVol;
		SERIES	X	= Day
				Y	= Volume
			/	MARKERS;
		RUN;

*Yes, R figures are prettier-- Here are some tips to make SAS figures pretty;

*	XAXIS, YAXIS, and REFLINE Statements and Options   *; 

*	Multiple REFLINE statements may be specified, and multiple values may
		be specified for each statement   *;
	PROC SGPLOT DATA = WORK.TumorVol;

		* This places the reference line BEHIND the time series plot *;
		REFLINE	126
			/	AXIS		= Y
				LABEL		= 'Tumor Doubling Threshold'
				LABELLOC	= INSIDE
				LABELPOS	= MIN;

		SERIES	X	= Day
				Y	= Volume
			/	MARKERS;

		XAXIS	VALUES		= (	0 4 7 11 14 18 21 25 28	)
				OFFSETMIN	= 0 /* Specifies the proportion of the entire line to appear left of the minimum value */
				LABEL		= 'Day of Tumor Measurement';

		YAXIS	VALUES		= (	0 TO 300 BY 50	)
				OFFSETMIN	= 0
				LABEL		= 'Tumor Volume (mm^3)'
				GRID;

		RUN;


*	Line Attributes   *; 

*	Goal:  Customize the series plot and reference lines   *;
	PROC SGPLOT DATA = WORK.TumorVol;

		REFLINE	126
			/	AXIS		= Y
				LINEATTRS	= (	COLOR		= RED
								PATTERN		= SHORTDASH	)
				TRANSPARENCY= 0.5
				LABEL		= 'Tumor Doubling Threshold'
				LABELLOC	= INSIDE;

		SERIES	X	= Day
				Y	= Volume
			/	MARKERS
				LINEATTRS	= (	COLOR		= PURPLE
								THICKNESS	= 2		)	;

		RUN;


*	Marker Attributes   *; 

*	Goal:  Customize the series plot markers   *;
	PROC SGPLOT DATA = WORK.TumorVol;

		SERIES	X	= Day
				Y	= Volume
			/	MARKERS
				MARKERATTRS	= (	SYMBOL	= CIRCLEFILLED
								SIZE	= 10 PX
								COLOR	= PURPLE)
				LINEATTRS	= (	COLOR		= PURPLE
								THICKNESS	= 2		)	;

		RUN;



*  An example of a beautiful figure;

PROC FORMAT;
	VALUE	$GrpNm
				'Ctl'	= 'Control'
				'Tx'	= 'Treatment';
	RUN;

DATA	WORK.GrpTumorVol;
	INFILE	DATALINES;
	INPUT	Grp	$	Day	Volume;
	FORMAT	Grp $GrpNm.;
	DATALINES;
Ctl 0  63
Ctl 4  73
Ctl 7  88
Ctl 11 104
Ctl 14 120
Ctl 18 143
Ctl 21 161
Ctl 25 209
Ctl 28 247
Tx  0  66
Tx  4  81
Tx  7  87
Tx  11 86
Tx  14 81
Tx  18 80
Tx  21 83
Tx  25 95
Tx  28 116
;


TITLE1	"Growth Curves of Patient-Derived Tumor Xenograft Models";
PROC SGPLOT
		DATA	= WORK.GrpTumorVol;

	REFLINE	126
		/	AXIS		= Y
			LINEATTRS	= (	COLOR		= RED
							PATTERN		= SHORTDASH	)
			TRANSPARENCY= 0.5
			LABEL		= 'Tumor Doubling Threshold'
			LABELLOC	= INSIDE
			LABELPOS	= MAX;

	STYLEATTRS	DATACONTRASTCOLORS	= (	PURPLE MEDIUMBLUE	);

	SERIES	X	= Day
			Y	= Volume
		/	GROUP		= Grp
			MARKERS
			MARKERATTRS	= (	SYMBOL	= CIRCLEFILLED
							SIZE	= 10 PX);

	XAXIS	VALUES		= (	0 4 7 11 14 18 21 25 28	)
			OFFSETMIN	= 0
			LABEL		= 'Day of Tumor Measurement'
			LABELATTRS	= (	WEIGHT	= BOLD	);

	YAXIS	VALUES		= (	0 TO 300 BY 50	)
			OFFSETMIN	= 0
			LABEL		= 'Tumor Volume'
			GRID
			LABELATTRS	= (	WEIGHT	= BOLD	);

	KEYLEGEND	/	NOBORDER;

	LABEL	Grp	= 'Trial Group:';

	INSET	'CRC 006   CRC 061'
			'CRC 020   CRC 077'
			'CRC 027   CRC 098'
			'CRC 048   CRC 120'
		/	POSITION	= TOPLEFT
			BORDER
			TITLE = 'CRC Models Used:'
			TITLEATTRS	= (	WEIGHT	= BOLD	);

	RUN;
TITLE;

****************************************************************************************
PART FOURTEEN: REPRODUCIBLE REPORTS
****************************************************************************************;
*	USING THE OUTPUT DELIVERY SYSTEM (ODS) TO CREATE A REPORT   *; 

*	When output is generated, reports are created by default
	The report format depends on the SAS interface
	1)	Windowing environment
			Default:  HTML
			Available:  HTML and LISTING (text)
			To change:  To Tools -> Options -> Preferences -> Results
			
	2)	SAS Studio
			Default:  HTML, PDF, and RTF
			Available:  HTML, PDF, and RTF
			To change:  More application options -> Preferences -> Results

	3)	Enterprise Guide
			Default:  SASREPORT
			Available:  SASREPORT, HTML, PDF, RTF, LISTING, POWERPOINT, EXCEL
			To change:  Tools -> Options -> Results General

	The above are interactive ways to control the report formats

	ODS statements may be used to create a physical file that can be viewed
		outside of SAS

	The ODS statement will programmatically control:
	1)	The format of the report created, including:

			FORMAT		EXTENSION	FILE TYPE
			---------	---------	--------------------------
			HTML		.html		Hypertext Markup Language
			PDF			.pdf		Portable Document Format
			RTF			.rtf		Rich Text Format
			LISTING		.txt		Text
			SASREPORT	.xml		Extensible Markup Language
			POWERPOINT	.ppt		Microsoft PowerPoint
			EXCEL		.xlsx		Microsoft Excel

	2)	The location of where the report is created
	3)	The file-name of the report created

	Use the ODS statement with appropriate options to specify the
		1) format, 2) destination-path, and 3) name of the desired report   *;


*	Controlling Reports Programmatically   *;

*	Goal:  Create a
			1)	PDF report
			2)	Sent to the 'Reports' folder for the Cancer RCT study
			3)	Named 'Cancer RCT Demographics'   *;
		ODS PDF FILE = "&Root/Reports/Cancer RCT Demographics.pdf";

		TITLE1	"Cancer Randomized Controlled Trial";
		TITLE2	"Subject Demographics";
		PROC PRINT DATA = prc.CanAnalysis1 LABEL;
			VAR	SubjID SexCd RaceCd AgeAtEnroll;
			RUN;

		ODS PDF CLOSE;
