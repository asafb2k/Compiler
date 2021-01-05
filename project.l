%{
#include <string.h>
%}
%%
"/%"([^%]|%+[^%/])*%+"/" ;
Main    {return MAIN;}
bool	{return BOOL;}
char	{return CHAR;}
int     {return INT;}
real	{return REAL;}
string	{return STRING;}
if	{return IF;}
else	{return ELSE;}
while	{return WHILE;}
var	{return VAR;}
func	{return FUNC;}
proc	{return PROC;}
return	{return RETURN;}
null	{return NULL_VAL;}
char"*"	{return CHAR_ARR;}
int"*"	{return INT_ARR;}
real"*"	{return REAL_ARR;}
true 	{yylval.node = mknode(yytext,NULL,NULL);return TRUE;}
false 	{yylval.node = mknode(yytext,NULL,NULL);return FALSE;}



[0-9]+                          {yylval.node = mknode(strdup(yytext),NULL,NULL);return INT_VAL;}
[0-9]+"."[0-9]+                 {yylval.node = mknode(strdup(yytext),NULL,NULL);return REAL_VAL;}	
[a-zA-Z][a-zA-Z0-9_]*          	{yylval.string = strdup(yytext); return ID;}
"^"[a-zA-Z][a-zA-Z0-9_]*        {yylval.string = strdup(yytext); return POINTER_VAL;}
"&"[a-zA-Z][a-zA-Z0-9_]*        {yylval.string = strdup(yytext); return POINTER_ADD;}
"!"[a-zA-Z][a-zA-Z0-9_]*	{yylval.string = strdup(yytext); return NOT_BOOL_VAL;}
\".*\"				{yylval.string = strdup(yytext); return STRING_VAL;}
\'.\'				{yylval.string = strdup(yytext); return CHAR_VAL;}


\/	return OP_DIV;
\*	return OP_MUL;
\+	return OP_PLUS;
\-	return OP_MINUS;
\=	{yylval.node = mknode(yytext,NULL,NULL); return OP_ASS;}
\>	{yylval.node = mknode(yytext,NULL,NULL);return OP_LEFT_BIGGER;}
\<	{yylval.node = mknode(yytext,NULL,NULL);return OP_RIGHT_BIGGER;}
\&	return OP_BY_REF;
\^	return OP_POWER;


"&&"	{yylval.node = mknode(yytext,NULL,NULL);return OP_AND;}
"=="	{yylval.node = mknode(yytext,NULL,NULL);return OP_EQ;}
">="	{yylval.node = mknode(yytext,NULL,NULL);return OP_EQ_LIB;}
"<="	{yylval.node = mknode(yytext,NULL,NULL);return OP_EQ_RIB;}
"!="	{yylval.node = mknode(yytext,NULL,NULL);return OP_NOTEQ;}
"||"	{yylval.node = mknode(yytext,NULL,NULL);return OP_OR;}

" "     ;
[\t]    ;
[\n]    {yylineno++;}
.	return yytext[0];
%%
