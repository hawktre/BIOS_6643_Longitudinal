*************   P   R   O   G   R   A   M       H   E   A   D   E   R   *****************
*****************************************************************************************
*                                                                                       *
*   PROGRAM:    SAS Bootcamp.sas                                                  		*
*   PURPOSE:    Introduce SAS Programming		                                        *
*   AUTHOR:     Laura Grau	                                                            *
*   CREATED:    2022	                                                                *
*   NOTES:		Portions of this program were adapted from Jud Blatchford's lectures    *                                                                                    *
****************************************************************************************
***********************************************************************************; RUN;

%let CourseRoot = C:\Users\graul\OneDrive - The University of Colorado Denver\Teaching\BIOS SAS Bootcamp\SAS Boot Camp Course Materials;

****************************************************************************************
PART ONE: INTRODUCTION TO SAS PROGRAMS
****************************************************************************************;

*Part 1.1:	A SAS program is a sequence of DATA and/or PROC steps.;

	*	DATA steps and PROC steps begin with either a DATA statement or a PROC statement,
	and they end with either a RUN, QUIT, or the start of another step.;

	*	Example 1 - A brief SAS program with two steps (a DATA step, and a PROC step);

	DATA example1;
	SET sashelp.bweight;
	if Weight<1500 then low_birth_weight=1; else low_birth_weight=0;
	RUN;

	PROC FREQ data=example1;
	TABLES low_birth_weight;
	RUN;

*Part 1.2:	Submitting a SAS program;

	*	Demonstration Running Man and F3;

*Part 1.3:	SAS Statements;

	*	Statements may be:
		1)	Valid only within a DATA step
			(e.g. The SET statement)
		2)	Valid only within a PROC step
			Further, this depends on the specific PROC used
			(e.g. PROC SORT does not have a MODEL statement, but PROC REG does)
		3)	Valid in either a DATA step or a PROC step
			(e.g. The FORMAT statement)
		4)  GLOBAL statements outside of DATA or PROC steps 
			GLOBAL statements execute as soon as they're submitted and 
			stay in effect until a) changed or b) SAS session ends
			Examples: 
			OPTIONS
			LIBNAME
			TITLE
			FOOTNOTE
		5)   Comments 
			*Comment statements;
			/*Block comments*/

	* Demonstration support.sas.com ;

*Part 1.4:	SAS Syntax;

	*	SAS code is free format with very few rules

	In short, the basic rules are:
		1)	Every statement ends with a semicolon
		2)	Keywords must be spelled correctly
		3)	Quoted strings must have matching quotation marks

	It will be very beneficial to learn the statements within the DATA step and
		the options associated with each statement;

*   Free Format Can Lead to Difficult Code to Read   *; 

*	The syntax is correct and therefore the code below will execute correctly   *;
		data work.classbmi; set sashelp.class; bmi=(00.45*weight)/(0.025*height)**2;
		format bmi 4.1; run; proc sort data=work.classbmi; by sex descending bmi;
		run; proc print data=work.classbmi; by sex; id name; var age bmi; run;


****************************************************************************************
PART TWO: INTRODUCTION TO SAS DATASETS & VARIABLES
****************************************************************************************;

*Part 2.1:	SAS datasets contain variables (columns) and observations (rows);

	PROC PRINT	data = SASHelp.Cars;
	RUN;

*Part 2.2:	There are two types of SAS variables: character and numeric;

	*	Character variables can contain any character, letter, or number;
	*	The default length is 8 bytes (or 8 characters);

	*	Numeric variables may only contain the following characters:
	-	Numeric digits
	-	A leading + or - sign
	-	A decimal
	-	The letter 'E' (for scientific notation)   *;


******************************************************
Special tip for R users:

If you bring data in from R, missings are typically
coded as NA--which is not a valid value for numeric 
variables--so SAS will read it in as a character variable.

Missingness in SAS:
If your variable is character, missing=''
If your variable is numeric, missing=.

When this happens, you can change the NA to missing, then
you can convert the variable from character to numeric.

*if variable="NA" then variable="";
*num_var=input(variable, informat.);


*Quick Example - Create a dataset;
DATA example_for_r;
INFILE datalines;
INPUT var $4.;
DATALINES;
8.29
NA
9.33
4.33
NA
;

	*Print the new dataset;
	PROC PRINT DATA=example_for_r;
	RUN;

	*Look at the types of variables in the data;
	PROC CONTENTS DATA=example_for_r;
	RUN;

	*Convert character variable to numeric;
	DATA example_for_r2;
	SET example_for_r;
	IF var="NA" THEN var="";
	num_var=input(var,8.2);
	RUN;

******************************************************;


*Part 2.3:	SAS Dates- Dates are stored as the number of days from January 1, 1960;

	*	-	Dates after January 1, 1960 are positive integers.  For example,
			January 2, 1960 is stored as 1
			January 10, 1960 is stored as 9
			January 1, 1961 is stored as 366
			etc
		-	Dates before January 1, 1960 are negative integers.  For example,
			December 31, 1959 is stored as -1
			December 29, 1959 is stored as -3
			January 1, 1959 is stored as -365
			etc   *;

	*To display SAS dates as dates, use a FORMAT statement;
	*FORMAT	VariableName WORDDATE.;
	*FORMAT	VariableName MMDDYY10.;

	*	Example 2 - To enter a date into SAS, use a date literal;
	DATA example2;
	date="07AUG2022"d;
	RUN;

	PROC PRINT data=example2;
	RUN;

	PROC PRINT data=example2;
	FORMAT date MMDDYY10.;
	RUN;

*Part 2.3:	SAS Times - Times are stored as number of seconds from midnight;

	*	-	Positive integers are the number of seconds after midnight
		-	Negative integers are the number of seconds before midnight;

	*To display SAS times as times, use a FORMAT statement;
	*FORMAT	VariableName TIME9.;

	*	Example 3 - To enter a time into SAS, use a time literal;
	DATA example3;
	time="12:01:00"t;
	RUN;

	PROC PRINT data=example3;
	RUN;

	PROC PRINT data=example3;
	FORMAT time time9.;
	RUN;

	* Demonstration: Please google "SAS FORMATS" ;

****************************************************************************************
PART THREE: SAS LIBRARIES - Where your data lives!
****************************************************************************************;

*Part 3.1:	Existing SAS Libraries;
	*WORK:  	This is the only temporary library! It gets deleted at the end of each SAS session
	*SASHELP:  	This library contains example data sets (e.g. Class, Cars, etc)
	*SASUSER:  	This library is provided by SAS for users to have a permanent location to store files;

*Part 3.2:	User-defined SAS Libraries;

	*LIBNAME libref "Pathway";

	*Demonstration of library creation;

	*In SAS OnDemand;
	*In Base SAS;

*Part 3.3:	Recommendation: Keep your files organized!;

	*Demonstration;

	*Create a folder for a project;
	%let Root = ;



	*Set SAS options;
	OPTIONS DLCREATEDIR;

	*Create folders;
	LIBNAME create "&root/DataRaw";
	LIBNAME create "&root/DataProcessed";


	*Reset options;
	LIBNAME Create CLEAR;

	OPTIONS NODLCREATEDIR;


****************************************************************************************
PART FOUR: READING YOUR DATA INTO SAS

Two notes:
*	If you remember ONE thing from today: Don't trust PROC IMPORT for CSV files. ALWAYS check;
*	We will only cover reading in SAS data, CSV files, and XLSX files, as these are commonly
used to share data. If you have other types of data, consult the SAS documentation

****************************************************************************************;



*Part 4.1: SAS Datasets;

	*SAS datasets have the .sas7bdat file extension;

	*Demonstration;

	*To refer to SAS datasets, you need to know two things:
		1) What library is this dataset in? -> LIBREF
		2) What is this dataset called? 	-> NAME; 

	* You refer to the dataset as LIBREF.NAME;

	*If a dataset is in the WORK library, you do not need to write WORK.NAME.
	You can simply refer to the dataset as NAME.;

	*	Example 4 - Creating a dataset from SASHELP into the WORK library;
	DATA birthweight; /* <- This is the new dataset I am creating*/
	SET sashelp.bweight; /* <- This is the existing dataset*/
	RUN;

****************************************************************************************
QUICK EXERCISE: 
You have the scans.sas7bdat dataset in your DataRaw folder. Create a copy of it into the
DataProcessed folder.

HINT: You need 2 LIBNAME statements and 1 DATA step
****************************************************************************************;


*Part 4.2: XLSX files;

	*Please note: How you can read data into SAS depends on what computer, the SAS version,
	the SAS bitness, the Excel bitness, and the Excel File extension;


	*Option 1: EXCEL/XLSX engine;

	LIBNAME Labs XLSX "&CourseRoot/DataRaw/Laboratory.xlsx" ;
	PROC PRINT
	DATA = Labs.NJCC___2015
	LABEL; * Note:  MS Excel column headings become variable labels *;
	RUN;
	LIBNAME Labs CLEAR;

	*Option 2: PROC IMPORT;

	/*
	PROC IMPORT
	DATAFILE=	"Pathwayyyy"
	OUT=		NameYourData
	DBMS=		TypeOfData
	REPLACE		 *;	
	*/

	*There are a TON more options for PROC IMPORT--check the documentation!!!;

	PROC IMPORT
	DATAFILE="&CourseRoot/DataRaw/Laboratory.xlsx"
	OUT=new_labs
	DBMS=XLSX
	REPLACE;
	RUN;


****************************************************************************************
QUICK EXERCISE: 
You have the Utah Vitals.xlsx dataset in your DataRaw folder. Read it into SAS, and save
a permanent SAS dataset in the DataProcessed folder.

What are the attributes of the variables in this dataset?
****************************************************************************************;




*Part 4.3: CSV files- This could be an entire 2 hour lecture!;


	*Part 4.3.1: Introduction to reading in CSV files;

	*CSV files should be read in using a DATA step;
	*If you read them in using PROC IMPORT, make sure they read in correctly;

	/*DATA nameyourdata;*/
	/*INFILE *Give SAS instructions on where to find the data and how to read it;*/
	/*INPUT  *Give SAS instructions on what variables to read in and how;*/
	/*RUN;*/

	*For every variable, we need to tell SAS three things:
		1) Name, 2) Type, and 3) Location;

	*In the INPUT statement, for every variable to be imported:
		1)	Specify the variable name(s) of your choice (in the order they appear in the raw data)
		2)	Designate a character variable by following the name with a $ sign (and give it a length, if needed)

	*Notes: the default delimiter is a SPACE!;


	*	Example 5 - Demonstration of reading in delimited data;

DATA	example5;
INFILE	DATALINES;
INPUT	ID		
Middle	$
GENDER	$; 
DATALINES;
001 WILLIAM MALE
002 RANDALL MALE
003 JO FEMALE
;

	PROC PRINT DATA=example5 NOOBS;
	RUN;


DATA	example5_v2;
INFILE	DATALINES /*delimiter=","*/;
INPUT	ID		
Middle	$
GENDER	$; 
DATALINES;
001,WILLIAM,MALE
002,RANDALL,MALE
003,JO,FEMALE
;

	PROC PRINT DATA=example5_v2 NOOBS;
	RUN;

	DATA	example5_v3;
	INFILE	"&CourseRoot/DataRaw/Address.csv"	DELIMITER = ','; * Alias is 'DLM' *;
	INPUT	ID
	First		$
	Middle		$
	Last		$
	Street_Num	;
	RUN;

	PROC PRINT DATA = WORK.Illus;
	RUN;


*Part 4.3.2: Special Considerations -- Missing data;

	* You should know (if possible) how many observations are in the raw data;

	*	Example 6 - Demonstration of reading in delimited data with missing variables;

	*	MISSOVER and TRUNCOVER instruct SAS to not load a new record if variable(s) remain un-filled   *;
	DATA	example6;
	INFILE	"&CourseRoot/DataRaw/Address.csv"	DELIMITER = ','	MISSOVER;
	INPUT	ID
	First		$
	Middle		$
	Last		$
	Street_Num
	Street_Name	$
	Zip
	HOME		$
	CELL		$	;
	RUN;
	*	Notice that now there are 11 observations because SAS did not load the following record when it
	encountered a missing value for an expected variable   *;

	PROC PRINT DATA = example6;
	RUN;


*Part 4.3.2: Special Considerations -- Missing data (continued);

	* SAS By Default Treats Consecutive Delimiters As a Single Delimiter;
	*	Most often, the convention used with delimited data is that consecutive delimiters designate missing value(s)
	DSD stands for "Delimiter-Sensitive Data"
	The DSD option performs 3 functions:
		1)	Creates a missing value when consecutive delimiters are encountered
		2)	Makes the comma the default delimiter (so DLM=',' does not need to be specified)
		3)	It allows the delimiter to be part of a value if it's contained within quotes (see Illustration C.3)   *;

	*	Example 7 - Demonstration of reading in delimited data with missing variables;

	DATA	example7;
	INFILE	"&CourseRoot/DataRaw/Address.csv"	DELIMITER = ','	MISSOVER	DSD;
	INPUT	ID
	First		$
	Middle		$
	Last		$
	Street_Num
	Street_Name	$
	Zip
	HOME		$
	CELL		$	;
	RUN;

	PROC PRINT DATA = example7;
	RUN;

	*	Example 8 - Read the same data in using PROC IMPORT;
	PROC IMPORT 
	DATAFILE="&CourseRoot/DataRaw/Address.csv"
	DBMS=CSV
	OUT=example8
	REPLACE;
	RUN;


****************************************************************************************
PART FIVE: CREATING NEW VARIABLES
****************************************************************************************;

*Part 5.1: Renaming variables;
	*What variables do we have?;
	PROC CONTENTS DATA=sashelp.bweight VARNUM;
	RUN;

	*Option a. Rename weight to infant_weight;
	DATA part5_1a;
	SET sashelp.bweight(RENAME=(Weight=infant_weight));
	*If there is subsequent code using this variable, use NEW name;
	infant_weight_kg=infant_weight/1000;
	RUN;

	*Option b. Rename weight to infant_weight;
	DATA part5_1b;
	SET sashelp.bweight;
	RENAME weight=infant_weight;
	*If there is subsequent code to manipulate this variable, use OLD name;
	infant_weight_kg=weight/1000;

	*Demo: This does not work;
	/*infant_weight_kg=infant_weight/1000;*/
	RUN;

****************************************************************************************
QUICK EXERCISE: 
Rename SubjID to ID in the DataPrc.Scans dataset
****************************************************************************************;


*Part 5.2: Converting numeric to character OR character to numeric;
	DATA part5_2a;
	SET sashelp.bweight;
	*Convert weight to character- PUT() function;
	weight_char=put(weight,8.);
	RUN;

	PROC PRINT DATA=part5_2a (OBS=10);
	VAR weight weight_char;
	RUN;

	*Conversely.....;

	DATA part5_2b;
	SET part5_2a;
	*Convert weight_char to numeric - INPUT() function;
	weight_num=input(weight_char,8.);
	RUN;

	PROC PRINT DATA=part5_2b (OBS=10);
	VAR weight weight_char weight_num;
	RUN;

	*Specify the correct number of decimals-- if the numbers were like 10.42, my input would be input(weight_char, 8.2);

****************************************************************************************
QUICK EXERCISE: 
1) Convert LesionSumCmMon0 from Character to Numeric, retaining the same name in the DataPrc.Scans dataset.
2) Calculate the difference between Month 6 and Baseline.
****************************************************************************************;

	
*Part 5.3: IF/THEN Statements;
	PROC PRINT DATA=sashelp.class;
	RUN;

	*Example with character variable;
	DATA part5_3a;
	SET sashelp.birthgt;
	IF married="Yes" THEN marital_status="Married";
	ELSE marital_status="Not married";
	RUN; 

	*Example with numeric variable;
	DATA part5_3b;
	LENGTH weight_char $20;
	SET sashelp.bweight;
	IF weight=. THEN weight_char="";
	ELSE IF weight < 1500 THEN weight_char="Less than 1500g";
	ELSE IF weight < 2500 THEN weight_char="1500-2499 g";
	ELSE IF weight < 4000 THEN weight_char="2500 - 3999 g";
	ELSE weight_char=">=4000 g";
	RUN;

	*Checking I did it correctly;
	PROC MEANS DATA=part5_3b MIN MAX;
	CLASS weight_char;
	VAR weight;
	RUN;

	*You can also do this with multiple variables: IF X=1 AND Y=0 THEN ... IF X=1 OR Y=0 THEN... IF X=1 OR X=2 THEN...;


*Part 5.4: Creating indicator variables;
	PROC FREQ DATA=sashelp.birthwgt;
	TABLES race;
	RUN;

	*Option a: IF/THEN statements;
	DATA part5_4a;
	SET sashelp.birthwgt;
	IF race="Asian" THEN DO;
		Asian=1;
		Black=0;
		Hispanic=0;
		White=0;
		Native=0;
	END;
	ELSE IF race="Black" THEN DO;
		Asian=0;
		Black=1;
		Hispanic=0;
		White=0;
		Native=0;
	END;
	ELSE IF race="Hispanic" THEN DO;
		Asian=0;
		Black=0;
		Hispanic=1;
		White=0;
		Native=0;
	END;
	ELSE IF race="Native" THEN DO;
		Asian=0;
		Black=0;
		Hispanic=0;
		White=0;
		Native=1;
	END;
	ELSE IF race="White" THEN DO;
		Asian=0;
		Black=0;
		Hispanic=0;
		White=1;
		Native=0;
	END;
	RUN;


	*Option b: Less code is always better;
	DATA part5_4b;
	SET sashelp.birthwgt;
	Asian=(race="Asian");
	Black=(race="Black");
	Hispanic=(race="Hispanic");
	Native=(race="Native");
	White=(race="White");
	RUN;

	PROC FREQ DATA=part5_4b;
	TABLES race*(Asian Black Hispanic Native White);
	RUN;


*Part 5.5: Essential functions and tips for longitudinal data;

	*FIRST and LAST -- Automatic SAS variables in BY processing;
DATA	longdata;
INFILE	DATALINES MISSOVER;
INPUT	ID	
Visit 
Name	$
Gender	$
Weight
; 
DATALINES;
001 1 WILLIAM MALE 77
001 2 WILLIAM MALE 89
001 3 WILLIAM MALE 90
001 4 WILLIAM MALE 89
001 5 WILLIAM MALE 91
002 1 RANDALL MALE 65
002 2 RANDALL MALE 66
002 3 RANDALL MALE 64
003 1 JO FEMALE 55
003 2 JO FEMALE .
003 3 JO FEMALE 55
;

	PROC SORT DATA=longdata OUT=longdata_sorted;
	BY Name Visit;
	RUN;

	DATA longdata_sorted;
	SET longdata_sorted;
	BY Name;
	IF FIRST.Name Then FIRST=1;
	IF LAST.Name Then LAST=1;
	RUN;

	PROC PRINT DATA=longdata_sorted;
	RUN;


	*LAG() -- grab the previous observation's value of the variable;

	DATA longdata_lagex;
	SET longdata_sorted;
	lag_weight=lag(weight);
	RUN;

	PROC PRINT DATA=longdata_lagex;
	RUN;
	*Note: BE CAREFUL! You might grab data from the wrong person!!;

	*Here is a better way to LAG;
	DATA longdata_lagex2;
	SET longdata_sorted;
	BY Name;

	lag_weight=lag(weight);

	IF FIRST.Name then lag_weight=.;
	RUN;

	PROC PRINT DATA=longdata_lagex2;
	RUN;


*Part 5.6: Essential functions for NUMERIC DATA;

	* Creating data for the illustration;
DATA	WORK.Scores;
	INFILE	DATALINES;
	INPUT	Name	$
			Quiz1 - Quiz3;
	DATALINES;
Amy 4 . 5
Bob 5 5 4
;

	*	Numeric functions typically ignore missing values;
	DATA	WORK.Illus;
		SET	WORK.Scores;

		QuizTotal1	= Quiz1 + Quiz2 + Quiz3;
		QuizTotal2	= SUM(OF Quiz1 - Quiz3);

		* NB:  QuizTotal3 creates incorrect result (no "OF" keyword), but no ERROR message *;
		QuizTotal3	= SUM(Quiz1 - Quiz3);

		RUN;

	PROC PRINT DATA = WORK.Illus;
		RUN;

	
	* Useful Mathematical Functions;
	DATA WORK.Illus;
		
		Ln			= LOG(2.72);
		Log10		= LOG10(100);
		Expon		= EXP(2);
		Minimum		= MIN(2, 4, 8);
		Maximum		= MAX(2, 4, 8);
		Mean		= MEAN(2, 4, 8);
		InputCnt	= N(2, 4, 8);
		InputCnt2	= N(2, 4, 8, .); * Note: N returns the number of non-missing arguments *;
		MissCnt		= NMISS(70, ., 90, 80, ., 100); * Intended for only numeric arguments *;
		AnyMiss		= CMISS(70, ., 90, 80, ., 100, 'Amy', ' ', 'Chris'); * For numeric or character arguments *;

		RUN;

	PROC PRINT DATA = WORK.Illus;
		RUN;

	*Others include: INT(), FLOOR(), CEIL(), INTCK() etc;
	*There are some specific to dates: MONTH(), DAY(), YEAR();
			*To create a date: MDY();
	*There are some specific to times: HOUR(), MINUTE(), SECOND();
			*To create a time: HMS();


*Part 5.7: Essential functions for CHARACTER DATA;

	*Case;
	DATA	WORK.Illus;
		
		Name = "SARAH o'malley";

		UpperCase		= UPCASE(Name);
		LowerCase		= LOWCASE(Name);
		ProperCase1		= PROPCASE(Name);
		ProperCase2		= PROPCASE(Name, " '");

		RUN;

	PROC PRINT DATA = WORK.Illus;
		RUN;


	*Additional;
	DATA	WORK.Illus;

		City = ' Phoenix,  AZ:     85034 ';

		* 13 characters *;
		LeftAligned		= LEFT(City); * Shifts first non-blank character to first position *;
		RightAligned	= RIGHT(City);
		Trimmed			= TRIM(City); * Removes trailing blanks *;
		Stripped		= STRIP(City); * Removes leading and trailing blanks *;
		CompressBlanks	= COMPBL(City); * Replaces multiple blanks with a single blank *;
		RemoveBlanks	= COMPRESS(City); * Default character to remove is a blank *;

		*	Remove other characters   *;
		*	Note:  If anything is specified, blanks must be specified to also remove them   *;
		RemovePunct		= COMPRESS(City, ' ,:'); * Removing blanks, commas, and colons *;
		Nice			= STRIP(COMPBL(COMPRESS(City, ',:')));

		RUN;

		PROC PRINT DATA = WORK.Illus;
			VAR City LeftAligned RightAligned Trimmed Stripped;
			FORMAT	LeftAligned RightAligned Trimmed Stripped $QUOTE26.;
			RUN;

		PROC PRINT DATA = WORK.Illus;
			VAR City CompressBlanks RemoveBlanks RemovePunct Nice;
			FORMAT	CompressBlanks RemoveBlanks	RemovePunct Nice	$QUOTE26.;
			RUN;

	*Others to look into:
			SCAN()
			TRANWRD();

*END OF DAY 1;
