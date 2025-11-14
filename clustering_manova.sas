/* Generated Code (IMPORT) */
/* Source File: dat-3.csv */
/* Source Path: /home/u64365443/sasuser.v94 */
/* Code generated on: 11/14/25, 1:43 PM */



/*****************************************
                  K = 3
******************************************/

%web_drop_table(WORK.PCA);


FILENAME REFFILE '/home/u64365443/sasuser.v94/dat-3.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.PCA;
	GETNAMES=YES;
RUN;

%web_open_table(WORK.PCA);

ods select CharStruct MultStat;

proc glm data = PCA;
    class label;                /* Your k-means grouping variable */
    model PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 PC9 PC10 PC11 PC12 PC13 =
          label;

    /* Request Wilks', Pillai's, Hotelling-Lawley, Roy’s tests */
    manova h = label / printe printh;
run;
quit;





/*****************************************
                  K = 4
******************************************/

%web_drop_table(WORK.PCA);


FILENAME REFFILE '/home/u64365443/sasuser.v94/dat-4.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.PCA;
	GETNAMES=YES;
RUN;

%web_open_table(WORK.PCA);

ods select CharStruct MultStat;
proc glm data = PCA;
    class label;                /* Your k-means grouping variable */
    model PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 PC9 PC10 PC11 PC12 PC13 =
          label;

    /* Request Wilks', Pillai's, Hotelling-Lawley, Roy’s tests */
    manova h = label / printe printh;
run;
quit;




/*****************************************
                  K = 5
******************************************/

%web_drop_table(WORK.PCA);


FILENAME REFFILE '/home/u64365443/sasuser.v94/dat-5.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.PCA;
	GETNAMES=YES;
RUN;

%web_open_table(WORK.PCA);

ods select CharStruct MultStat;
proc glm data = PCA;
    class label;                /* Your k-means grouping variable */
    model PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 PC9 PC10 PC11 PC12 PC13 =
          label;

    /* Request Wilks', Pillai's, Hotelling-Lawley, Roy’s tests */
    manova h = label / printe printh;
run;
quit;




/*****************************************
                  K = 6
******************************************/

%web_drop_table(WORK.PCA);


FILENAME REFFILE '/home/u64365443/sasuser.v94/dat-6.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.PCA;
	GETNAMES=YES;
RUN;

%web_open_table(WORK.PCA);

ods select CharStruct MultStat;
proc glm data = PCA;
    class label;                /* Your k-means grouping variable */
    model PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 PC9 PC10 PC11 PC12 PC13 =
          label;

    /* Request Wilks', Pillai's, Hotelling-Lawley, Roy’s tests */
    manova h = label / printe printh;
run;
quit;





/*****************************************
                  K = 7
******************************************/

%web_drop_table(WORK.PCA);


FILENAME REFFILE '/home/u64365443/sasuser.v94/dat-7.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.PCA;
	GETNAMES=YES;
RUN;

%web_open_table(WORK.PCA);

ods select CharStruct MultStat;
proc glm data = PCA;
    class label;                /* Your k-means grouping variable */
    model PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 PC9 PC10 PC11 PC12 PC13 =
          label;

    /* Request Wilks', Pillai's, Hotelling-Lawley, Roy’s tests */
    manova h = label / printe printh;
run;
quit;