/*** Elif Keleş - 161044033 ***/
/*** HW1 Part1 ***/

/* To finish REPL press enter */
/* KEYWORDS are only recognizable in lower case */

/*** Definition Section ***/

%{
	/* to track opening and closing quotes */
	int quotenumber = 0; 
	/* file pointers */
    FILE * outFile;
    FILE * infile;
%} 
  
/*** Rule Section ***/

UNWANTED [0-9][a-zA-Z0-9_]*

COMMENT ;;.*

%% 

^\n 	{return 0;} /*ending for program*/

and 	{fprintf (outFile, "KW_AND\n");}
or 		{fprintf (outFile, "KW_OR\n");}
not 	{fprintf (outFile, "KW_NOT\n");}
equal  	{fprintf (outFile, "KW_EQUAL\n");}
less 	{fprintf (outFile, "KW_LESS\n");}
nil 	{fprintf (outFile, "KW_NIL\n");}
list 	{fprintf (outFile, "KW_LIST\n");}
append 	{fprintf (outFile, "KW_APPEND\n");}
concat 	{fprintf (outFile, "KW_CONCAT\n");}
set 	{fprintf (outFile, "KW_SET\n");}
deffun 	{fprintf (outFile, "KW_DEFFUN\n");}
for 	{fprintf (outFile, "KW_FOR\n");}
if 		{fprintf (outFile, "KW_IF\n");}
exit 	{fprintf (outFile, "KW_EXIT\n");}
load 	{fprintf (outFile, "KW_LOAD\n");}
disp  	{fprintf (outFile, "KW_DISP\n");}
true 	{fprintf (outFile, "KW_TRUE\n");}
false 	{fprintf (outFile, "KW_FALSE\n");}

[a-zA-Z][a-zA-Z0-9]* 	{fprintf (outFile, "IDENTIFIER\n");}

"-"		{fprintf (outFile, "OP_MINUS\n");}
"+" 	{fprintf (outFile, "OP_PLUS\n");}
"/" 	{fprintf (outFile, "OP_DIV\n");}
"*" 	{fprintf (outFile, "OP_MULT\n");}
"(" 	{fprintf (outFile, "OP_OP\n");}
")" 	{fprintf (outFile, "OP_CP\n");}
"**" 	{fprintf (outFile, "OP_DBLMULT\n");}
"\"" 	{quotenumber++; 
		if(quotenumber%2==1) 
			fprintf (outFile, "OP_OC\n"); 
		else fprintf (outFile, "OP_CC\n");}
"," 	{fprintf (outFile, "OP_COMMA\n");}

{COMMENT} 	{fprintf (outFile, "COMMENT\n");}

0 		{fprintf (outFile, "VALUE\n");}
[1-9][0-9]*	{fprintf (outFile, "VALUE\n");}

{UNWANTED}	{fprintf(outFile, "ERROR FOR: %s\n", yytext  );}

[ \t]+	 /*for errors */{}
.			{fprintf( outFile, "ERROR FOR: %s\n", yytext  );}
%% 

/*** Code Section ***/

int yywrap(){} 

int main(int argc, char **argv)
{
/* Explanation: 
	yywrap - wraps the above rule section 
	yyin - takes the file pointer which contains the input
	yylex - this is the main flex function which runs the Rule Section	
	yytext - is the text in the buffer
*/

	char fileName[25];  

	//open the output file 
    outFile = fopen ("parsed_cpp.txt","w");


    //if any input file is given
    if(argc > 1)
    {
    	//open the input file
    	infile = fopen(argv[1], "r");
    	if(infile==NULL)
    	{
    		fclose(infile);
    		return -1;
    	}

    	yyin= infile;
    	yylex();
    	fclose(infile);	
    }

    else
    {
    	yylex();	
    }

    //close the output file
	fclose(outFile);  
	return 0; 
} 

