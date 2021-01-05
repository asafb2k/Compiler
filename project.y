%union{
	char* string;
	struct node* node;
}

%{
#include<stdio.h>
#include<string.h>
#include<stdlib.h>
typedef enum {false,true} bool;
int i=0;
typedef struct node
{
 char *token;
 struct node *left;
 struct node *right;
} node;


typedef struct Function 
{
	char* name;
	struct Variable* args;
        char* returnType; 
	int argNum;
	bool findreturn;
}Function;


typedef struct Variable
{	
	char* name;
	char* value;
	char* type;
}Variable;


typedef struct Scope
{	
	char* name;
	Variable* var;
	int VarCount;
	int Fcount;
	Function** func;
	struct Scope* nextScope;
	struct Scope* preScope;
}Scope;


Scope* make_scope(char* name);
node *mknode(char *token, node *left, node *right);
void printtree(node *tree);
void Analyser(node *tree, Scope *currentScope);
void addFunc_toScope(char* name,Scope* current_scope);
void addVar_toScope(node* tree,Scope* current_scope);
void addVariabledstoscope(node* tree, Scope* currentScope);
void var_finder(char* var , Scope* s);
void func_checker(char* function, Scope* s, node* tree);
void func_checker_helper(int numofvars, Variable* FC, Variable* FD);
void func_return_checker(char* type, node* tree);
Variable* func_call_var_arr_creator(node* tree, Variable* temp);
char* expression_type_valuator(node* tree);
void func_call_assaignment(char* id_type, char* func_name, Scope* currentScope);
void type_id_check(char* id_type,Scope* s, char* type);
void expression_checker_for_condition(node* exp1, node* exp2);


void var_arr_printer(Variable* vars, int numofvars){int i = 0; for(i = 0; i<numofvars; i++) printf("var: %s\nvar type: %s\n", vars[i].name, vars[i].type);}
void func_variables_printer(Function* f){int i = 0; printf("function name: %s \nnumofargs: %d \n", f->name, f->argNum); for(i = 0; i < f->argNum; i++) {printf("var: %s\nvar type: %s\n", f->args[i].name, f->args[i].type);}}
void scope_func_printer(Scope* s){int i = 0; printf("scopes name: %s \nnumofuncs: %d \n",s->name, s->Fcount); for(i = 0; i < s->Fcount; i++) {printf("%s\n", s->func[i]->name);}}
void scope_var_printer(Scope* s){int i = 0;printf("scopes name: %s \nnumovars: %d \n",s->name, s->VarCount); for(i = 0; i < s->VarCount; i++) {printf("var: %s\nvar type: %s\n", s->var[i].name, s->var[i].type);}}


char* var_type_extractor(node* tree);
Variable var_extractor(char* varname, Scope* s);

void scope_initiator();

Scope* globalScope = NULL;
Scope* currentScope;
int func_call_var_counter = 0;


%}


%token MAIN,BOOL,CHAR,INT,REAL,STRING,INT_ARR,CHAR_ARR,REAL_ARR,IF,ELSE,WHILE,VAR,FUNC,
PROC,RETURN,NULL_VAL,OP_DIV,OP_MUL,OP_PLUS,OP_MINUS,OP_BY_REF,OP_POWER
%left OP_DIV,OP_MUL,OP_MINUS,OP_PLUS

%token<node> TRUE, FALSE, REAL_VAL, INT_VAL, OP_ASS,OP_LEFT_BIGGER,OP_RIGHT_BIGGER, OP_AND,OP_EQ,OP_EQ_LIB,OP_EQ_RIB,OP_NOTEQ,OP_OR
%type<node> CODE, FUNC_DECLARATION, PROC_DECLARATION, MAIN_, SECOND, MAIN_DEC, MAIN_DEC, STATEMENT, IF_DEC, IF_COND, IF_ELSE_DEC, ELSE_HELPER
%type<node> WHILE_LOOP,CONDITION, BOOL_CONDITION, BOOL_OP, COND_OP,FUNC_CALL, FUNC_PROC_HELPER, BLOCK, BLOCK_DEC, FUNC_VARIABLES_DEF
%type<node> FUNC_VARIABLES, FUNC_VARIABLES_CALL, TEMP1, VAR_TYPE,VAR_DECLARATION, ARRAY_DEC, ASSAIGNMENT,EXPR,POINET_EXPR, WHILE
%type<node> EXPR, FUNC_VARIABLES_ARRAY, CHAR_VALUE, CONDITION_EXPR, BOOL_CONDITION_EXPR, TRUE_OR_FALSE, COND_OP_NUMBERS, COND_OP_NOT_NUMBERS
%token<string> ID,POINTER_VAL,POINTER_ADD,CHAR_VAL,STRING_VAL, NOT_BOOL_VAL
%%


FIRST: CODE ;

CODE: FUNC_DECLARATION CODE   {$$ = mknode("CODE",$1,$2);}
 |  PROC_DECLARATION CODE       {$$ = mknode("CODE",$1,$2);}
 |  MAIN_                    {$$ = mknode("",$1,NULL);}  ;

MAIN_: MAIN_DEC SECOND     {$$= mknode("MAIN",$1,$2);};

SECOND: FUNC_DECLARATION SECOND   {$$= mknode("",$1,$2);}
 |  PROC_DECLARATION SECOND       {$$= mknode("",$1,$2);}
 |;

MAIN_DEC: PROC MAIN {scope_creator("MAIN"); addFunc_toScope("MAIN", currentScope->preScope);} '('')' '{' BLOCK '}' {$$= mknode("",$7,NULL); scope_deleter();} ;

STATEMENT: ASSAIGNMENT ';'    {$$=$1;}
 | FUNC_CALL ';'       	      {$$=$1;}
 | VAR_DECLARATION ';'        {$$=mknode("VAR",$1,NULL);}
 | FUNC_DECLARATION           {$$=mknode("FUNC",$1,NULL);}
 | PROC_DECLARATION           {$$=mknode("PROC",$1,NULL);}
 | IF_ELSE_DEC                {$$=mknode("",$1,NULL);}
 | IF_DEC                     {$$=mknode("",$1,NULL);}      
 | WHILE_LOOP                 {$$=$1;}
 | ARRAY_DEC ';'              {$$=mknode("ARRAY",NULL,NULL);}
 | '{'{scope_creator("BLOCK_WITHOUT_FATHER");}BLOCK'}'	  	      {$$=$3; scope_deleter();};

IF_DEC: IF_COND '{' BLOCK '}' {$$=mknode("",$1,$3); scope_deleter();};

IF_COND: IF '(' CONDITION ')' {$$=mknode("IF",$3,NULL); scope_creator("IF");};

IF_ELSE_DEC: IF_COND '{' BLOCK '}' {scope_deleter(); scope_creator("ELSE");} ELSE_HELPER {$$=mknode("",mknode("",$1, $3),$6);};

ELSE_HELPER: ELSE '{' BLOCK '}' {$$=mknode("ELSE",$3,NULL); scope_deleter();};

WHILE_LOOP: WHILE '(' CONDITION ')' '{' BLOCK '}' {$$=mknode("",mknode("",$1, $3),$6);};

CONDITION: '('CONDITION')' BOOL_CONDITION			{$$=$2;}
 | '!' CONDITION 						{$$ = mknode("!",$2, NULL);}
 | EXPR COND_OP_NUMBERS EXPR BOOL_CONDITION 			{$$=mknode("",$2,mknode("",$1, mknode("",$3, $4)));expression_checker_for_condition($1, $3);}
 | EXPR COND_OP_NOT_NUMBERS EXPR BOOL_CONDITION 		{$$=mknode("",$2,mknode("",$1, mknode("",$3, $4)));}
 | EXPR COND_OP_NOT_NUMBERS TRUE_OR_FALSE BOOL_CONDITION	{$$=mknode("",$2,mknode("",$1, mknode("",$3, $4)));}
 | TRUE_OR_FALSE COND_OP_NOT_NUMBERS EXPR BOOL_CONDITION	{$$=mknode("",$2,mknode("",$1, mknode("",$3, $4)));}
 | TRUE_OR_FALSE BOOL_CONDITION					{$$=mknode("",$1,$2);}
 | ID BOOL_CONDITION                        			{$$=mknode("",mknode($1,NULL,NULL), $2); type_id_check($1,currentScope, "BOOL");};

BOOL_CONDITION:  BOOL_OP CONDITION {$$=mknode("",$1,$2);} 
 | ;

BOOL_OP: OP_AND {$$=$1;}
 | OP_OR	{$$=$1;};

COND_OP: COND_OP_NUMBERS	{$$=$1;}
 | COND_OP_NOT_NUMBERS 			{$$=$1;};

COND_OP_NOT_NUMBERS: OP_EQ {$$=$1;} | OP_NOTEQ {$$=$1;};
COND_OP_NUMBERS: OP_LEFT_BIGGER {$$=$1;} | OP_RIGHT_BIGGER {$$=$1;} | OP_EQ_LIB {$$=$1;} | OP_EQ_RIB {$$=$1;};

TRUE_OR_FALSE: TRUE {$$ = $1;} || FALSE {$$ = $1;};

FUNC_CALL: ID '('  FUNC_VARIABLES_CALL ')' {$$=mknode("",mknode($1,NULL,NULL),$3);func_checker($1, currentScope, $3);} ;

FUNC_DECLARATION: FUNC_PROC_HELPER RETURN VAR_TYPE '{' BLOCK RETURN EXPR ';' '}' {$$=mknode("FUNC",$1,mknode("",$3,mknode("",$5,$7))); currentScope->preScope->func[currentScope->preScope->Fcount - 1]->returnType = $3->token; func_return_checker($3->token, $7); scope_deleter();};

PROC_DECLARATION: PROC ID '(' FUNC_VARIABLES_DEF {scope_creator($2); addVariabledstoscope($4, currentScope); addFunc_toScope($2, currentScope->preScope);} ')' '{' BLOCK '}'{$$=mknode("PROC",mknode($2,NULL,NULL),mknode("",$4,$8)); scope_deleter();};

FUNC_PROC_HELPER: FUNC ID '('  FUNC_VARIABLES_DEF ')' {$$=mknode("",mknode($2,NULL,NULL),$4);scope_creator($2); addVariabledstoscope($4, currentScope);addFunc_toScope($2, currentScope->preScope);};

BLOCK: STATEMENT BLOCK_DEC  {$$=mknode("BLOCK",$1,$2);}|;

BLOCK_DEC: STATEMENT BLOCK_DEC {$$=mknode("",$1,$2);}
 | ;

FUNC_VARIABLES_DEF: FUNC_VARIABLES ';' FUNC_VARIABLES_DEF {$$=mknode("VAR",$1,$3);} 
 | FUNC_VARIABLES  {$$=mknode("VAR",$1,NULL);}
 ;

FUNC_VARIABLES: ID ',' FUNC_VARIABLES {$$=mknode("",mknode($1,NULL,NULL),$3);}
 | ID ':' VAR_TYPE {$$=mknode("",mknode($1,NULL,NULL),$3);}| ;

FUNC_VARIABLES_CALL: TEMP1  {$$=$1;} |  {$$=NULL;};

TEMP1: EXPR ',' TEMP1  		{$$=mknode("",$1,$3);func_call_var_counter++;}
// | CHAR_VAL ',' TEMP1  	{$$=mknode("",mknode($1,NULL,NULL),$3);func_call_var_counter++;}
// | STRING_VAL ',' TEMP1  	{$$=mknode("",mknode($1,NULL,NULL),$3);func_call_var_counter++;}
// | INT_VAL ',' TEMP1     	{$$=mknode("",$1,$3);func_call_var_counter++;}
// | REAL_VAL ',' TEMP1    	{$$=mknode("",$1,$3);func_call_var_counter++;}
// | INT_VAL			{$$=$1;func_call_var_counter++;}
// | REAL_VAL			{$$=$1;func_call_var_counter++;}
 | EXPR                		{$$=$1;func_call_var_counter++;};
// | CHAR_VAL          		{$$=mknode($1,NULL,NULL);func_call_var_counter++;}
// | STRING_VAL        		{$$=mknode($1,NULL,NULL);func_call_var_counter++;};

VAR_TYPE: CHAR {$$=mknode("CHAR",NULL,NULL);}
 | INT {$$=mknode("INT",NULL,NULL);}
 | BOOL {$$=mknode("BOOL",NULL,NULL);}
 | REAL {$$=mknode("REAL",NULL,NULL);}
 | STRING {$$=mknode("STRING",NULL,NULL);}
 | CHAR_ARR {$$=mknode("CHAR_ARR",NULL,NULL);}
 | INT_ARR {$$=mknode("INT_ARR",NULL,NULL);} 
 | REAL_ARR {$$=mknode("REAL_ARR",NULL,NULL);};

VAR_DECLARATION: VAR FUNC_VARIABLES {$$=mknode("",$2,NULL); addVar_toScope($2, currentScope);};

ARRAY_DEC: VAR FUNC_VARIABLES_ARRAY '[' EXPR ']' {$$=mknode("",$2,$4); addVar_toScope($2, currentScope); if(strcmp(expression_type_valuator($4), "INT") != 0){yyerror("THE EXPRESSION WITHIN [] IS NOT INT!");} };

FUNC_VARIABLES_ARRAY: ID ',' FUNC_VARIABLES_ARRAY {$$=mknode("",mknode($1,NULL,NULL),$3);}
 | ID ':' STRING {$$=mknode("",mknode($1,NULL,NULL),mknode("STRING",NULL,NULL));}| ;

ASSAIGNMENT: ID OP_ASS '!' EXPR  { $$ = mknode("=",mknode($1,NULL,NULL),$4); var_finder($1, currentScope); if(strcmp(var_extractor($1,currentScope).type, "BOOL") != 0 || strcmp(expression_type_valuator($4), "BOOL") != 0){yyerror("ONE OF THE ARGUMENTS IN THE ASSAIGNMENT IS NOT BOOL!");} }
 | ID OP_ASS EXPR  {$$ = mknode("=",mknode($1,NULL,NULL),$3); var_finder($1, currentScope); type_id_check($1,currentScope, expression_type_valuator($3)); }
 | ID OP_ASS FUNC_CALL { $$ = mknode("=",mknode($1,NULL,NULL),$3);var_finder($1, currentScope);func_call_assaignment($1, $3->left->token,currentScope);}
 | ID OP_ASS CONDITION_EXPR { $$ = mknode("=",mknode($1,NULL,NULL),$3); var_finder($1, currentScope); type_id_check($1,currentScope, "BOOL");}
 | ID OP_ASS '('CONDITION_EXPR')' { $$ = mknode("=",mknode($1,NULL,NULL),$4); var_finder($1, currentScope); type_id_check($1,currentScope, "BOOL");}
 | ID '[' EXPR ']' OP_ASS CHAR_VALUE {$$ = mknode("=",mknode($1,NULL,NULL),$3); var_finder($1, currentScope); if(strcmp(expression_type_valuator($3), "INT") != 0){yyerror("THE EXPRESSION WITHIN [] IS NOT INT!");}}
 | POINTER_VAL OP_ASS EXPR {$$ = mknode("=",mknode($1,NULL,NULL),$3); var_finder($1, currentScope);type_id_check($1,currentScope, expression_type_valuator($3));}; 

CONDITION_EXPR: '!' CONDITION_EXPR			{$$=mknode("!",$2,NULL);}
 | EXPR COND_OP EXPR BOOL_CONDITION 			{$$=mknode("",$2,mknode("",$1, mknode("",$3, $4)));  }
 | TRUE_OR_FALSE BOOL_CONDITION				{$$=mknode("",$1,$2);}
 | EXPR BOOL_CONDITION_EXPR			 	{$$=mknode("",$1, $2); if(strcmp(expression_type_valuator($1), "BOOL") != 0){yyerror("NOT BOOL TYPE IN CONDITION!");}};

BOOL_CONDITION_EXPR: BOOL_OP CONDITION {$$=mknode("",$1,$2);};

CHAR_VALUE: CHAR_VAL {$$ = mknode($1,NULL,NULL);}
 | ID {$$ = mknode($1,NULL,NULL); if(strcmp(var_extractor($1, currentScope).type, "CHAR") != 0){yyerror("TRYING TO ASSAIGN WRONG TYPE TO STRING!");}}
 | ID '[' EXPR ']' {$$ = mknode($1,NULL,NULL); var_finder($1, currentScope); if(strcmp(expression_type_valuator($3), "INT") != 0){yyerror("THE EXPRESSION WITHIN [] IS NOT INT!");}} ;


POINET_EXPR: POINTER_ADD {$$ = mknode($1,NULL,NULL); var_finder($1, currentScope);}
 | NULL_VAL      	 {$$ = mknode("NULL",NULL,NULL);}
 | POINTER_VAL           {$$ = mknode($1,NULL,NULL); var_finder($1, currentScope);};

EXPR: '('EXPR')' 	{$$ = $2;}
 | EXPR OP_PLUS EXPR { $$ = mknode("+",$1,$3); }
 | EXPR OP_MINUS EXPR { $$ = mknode("-",$1,$3); }
 | EXPR OP_MUL EXPR { $$ = mknode("*",$1,$3); }
 | EXPR OP_DIV EXPR { $$ = mknode("/",$1,$3); }
 | INT_VAL  {$$=$1;}
 | REAL_VAL {$$=$1;}
 | ID  {$$ = mknode($1,NULL,NULL);var_finder($1, currentScope);}
 | STRING_VAL { $$ = mknode($1,NULL,NULL); }
 | CHAR_VAL { $$ = mknode($1,NULL,NULL); }
 | NOT_BOOL_VAL { $$ = mknode($1,NULL,NULL); }
 | POINET_EXPR  {  $$ = $1; }
 | '|' ID '|'	{$$ = mknode($2,mknode("STRING",NULL,NULL),NULL); var_finder($2, currentScope); if(strcmp(var_extractor($2, currentScope).type, "STRING") != 0){yyerror("THE EXPRESSION INSIDE | | IS NOT STRING!");}}
 | ID '[' EXPR ']' {$$ = mknode($1,mknode("CHAR_VALUE",NULL,NULL),NULL); var_finder($1, currentScope); if(strcmp(expression_type_valuator($3), "INT") != 0){yyerror("THE EXPRESSION WITHIN [] IS NOT INT!");}}
 | POINTER_ADD '[' EXPR ']' {$$ = mknode($1,NULL,NULL); var_finder($1, currentScope); if(strcmp(expression_type_valuator($3), "INT") != 0){yyerror("THE EXPRESSION WITHIN [] IS NOT INT!");}};


%%




#include "lex.yy.c"
main()
{
	scope_initiator();
	currentScope = globalScope;
	return yyparse();
}

node *mknode(char *token,node *left,node *right)
{
	node *newnode = (node*)malloc(sizeof(node));
	char *newstr = (char*)malloc(sizeof(token) + 1);
	strcpy(newstr,token);
	newnode->left = left;
	newnode->right = right;
	newnode->token = newstr;
	return newnode;
}

Scope* make_scope(char* name)
{	
	Scope* newScope = (Scope*)malloc(sizeof(Scope));
	newScope->name = name;
	newScope->var = NULL;
	newScope->Fcount = 0;
	newScope->nextScope = NULL;
	newScope->preScope = NULL;
	newScope->VarCount = 0;
	newScope->func = NULL;
	return newScope;
}

int tab_count = -1;
void printTabs(){
	int i;
	for (i=0; i < tab_count ; i++){
		printf("    ");
	}
}
void printtree(node *tree)
{	
	tab_count++;
	if(strcmp(tree->token,"") == 0){
		tab_count--;
	}
	else if(tree->left != NULL){
		printTabs();
		printf("(%s\n",tree->token);
	}
	else{
		printTabs();
		printf("(%s)\n",tree->token);
	}
	if(tree->left)printtree(tree->left);
	if(tree->right)printtree(tree->right);

	if(strcmp(tree->token, "") == 0){
		tab_count++;}
	else if((tree->left != NULL || tree->right != NULL) && (tree->token != "")){
		printTabs();
		printf(")\n");
	}
	tab_count--;
}

void scope_initiator(){
	Scope* s;
	s = make_scope("global");
	globalScope =  s;
}

void scope_creator(char* name){
	Scope* s;
	s = make_scope(name);
	s->preScope = currentScope;
	currentScope->nextScope = s;
	currentScope = s;
	//printf("created scope: %s\n", name);
}

void scope_deleter(){
	//printf("delete scope: %s\n", currentScope->name);
	currentScope = currentScope->preScope;
	currentScope->nextScope = NULL;
}

void addFunc_toScope(char* name,Scope* s)
{
	Function** temp;
	Scope* scopes = s;
	temp=scopes->func;
	scopes->func=(Function**) malloc(sizeof(Function*)*(scopes->Fcount+1));
	i = 0;
	for(i=0;i<scopes->Fcount;i++)
		{
				if(strcmp(temp[i]->name,name)==0 )//if name taken, ERROR exit
				{
					yyerror("FUNC/PROC re-declaration",temp[i]->name);
					exit(1);
				}
				scopes->func[i]=temp[i];
		}
	scopes->func[scopes->Fcount]=(Function*) malloc(sizeof(Function));
	scopes->func[scopes->Fcount]->name=name;
	scopes->func[scopes->Fcount]->argNum = scopes->nextScope->VarCount;
	Variable* args = (Variable*)malloc(sizeof(Variable)*(scopes->nextScope->VarCount));
	for(i = 0; i < scopes->nextScope->VarCount ; i++){args[i] = scopes->nextScope->var[i];}
	scopes->func[scopes->Fcount]->args = args;
	(scopes->Fcount)++;
}

void addVar_toScope(node* tree,Scope* s)
{
	int i;
	Scope* scopes = s;
	Variable* temp = (Variable*)malloc(sizeof(Variable)*(scopes->VarCount));
	for(i = 0; i < scopes->VarCount ; i++){temp[i] = scopes->var[i];}

	if(strcmp(tree->token,"") == 0){
		scopes->var = (Variable*)malloc(sizeof(Variable)*(scopes->VarCount+1));
		for(i=0 ; i < scopes->VarCount ; i++)
			{
					if(strcmp(temp[i].name,tree->left->token)==0 )//if name taken, ERROR exit
					{
						yyerror("VAR re-declaration",temp[i].name);
						exit(1);
					}
					scopes->var[i]=temp[i];
			}
		scopes->var[scopes->VarCount].name = tree->left->token;
		scopes->var[scopes->VarCount].type = var_type_extractor(tree);
		(scopes->VarCount)++;
	}
	if(tree->right != NULL) addVar_toScope(tree->right,currentScope);
}


void addVariabledstoscope(node* tree, Scope* currentScope){
	if(tree->left == NULL && tree->right == NULL){}
	else{
		if(tree->right != NULL){
			if(strcmp(tree->right->token,"VAR") == 0){
				addVariabledstoscope(tree->right, currentScope);
			}
		}
		if(tree->left != NULL && strcmp(tree->left->token,"") == 0){
			addVar_toScope(tree->left, currentScope);
		}
	}	
}


char* var_type_extractor(node* tree){
	if(strcmp(tree->token, "") == 0) return var_type_extractor(tree->right);
	else return tree->token; 
}

void var_finder(char* var , Scope* s){
	int i,k = 0;
	if(var[0] == '\'' || var[0] == '\"'){return;}
	if(var[0] == '^' || var[0] == '&'){
		char* temp;
		temp = &var[1];
		var = temp;
	}
	while(s != NULL){
		for(i = 0; i < s->VarCount; i++){
			if(strcmp(s->var[i].name, var) == 0){return;}			
		}
		s = s->preScope;
	}
	yyerror("CALLED TO UNDEFINED VARIABLE!");
	exit(1);
}

void func_checker(char* function, Scope* s, node* tree){
	int i;
	while(s != NULL){
		for(i = 0; i < s->Fcount; i++){
			if(strcmp(s->func[i]->name, function) == 0 && s->func[i]->argNum == func_call_var_counter){
				Variable* FC_variables = (Variable*)malloc(sizeof(Variable)*(func_call_var_counter));
				FC_variables = func_call_var_arr_creator(tree,FC_variables);
				func_checker_helper(func_call_var_counter, FC_variables, s->func[i]->args);
				func_call_var_counter = 0;
				return;
			}
			if(strcmp(s->func[i]->name, function) == 0){
				if(s->func[i]->argNum > func_call_var_counter){
					yyerror("FUNCTION/PROC ARGUMENTS ARE MISSING!");
					exit(1);
				}
				else{
					yyerror("TOO MUCH ARGUMENTS IN FUNCTION/PROC CALL!");
					exit(1);
				}
			}
		}
		s = s->preScope;
	}
	yyerror("CALLED TO UNDEFINED FUNCTION!");
	exit(1);

}

Variable var_extractor(char* varname, Scope* s){
	Variable V;
	int j = 0;
	int flag_ = 0;
	if(varname[0] == '\''){V.name = varname; V.type = "CHAR"; return V;}
	if(varname[0] == '\"'){V.name = varname; V.type = "STRING"; return V;}
	if(atoi(varname) != 0){
		for(j = 0 ; varname[j] != '\0' ; j++){
			if(varname[j] == '.'){V.name = varname; V.type = "REAL"; return V;}
		}
	V.name = varname; V.type = "INT"; return V;
	}
	if(varname[0] == '^'){
		char* temp;
		temp = &varname[1];
		varname = temp;
		flag_ = 1;
	}
	if(varname[0] == '&'){
		char* temp;
		temp = &varname[1];
		varname = temp;
		flag_ = 2;
	}
	if(varname[0] == '!'){
		char* temp;
		temp = &varname[1];
		varname = temp;
		flag_ = 3;
	}
	Scope* scopes = s;
	int i = 0;
	while(s != NULL){
		for(i = 0; i < s->VarCount ; i++){
			if(strcmp(s->var[i].name, varname) == 0){
				V = s->var[i];
				if(flag_ != 0){
					if(flag_ == 1){
						if(!(strcmp(V.type, "INT_ARR") == 0 || strcmp(V.type, "CHAR_ARR") == 0 || strcmp(V.type, "REAL_ARR") == 0)){
							yyerror("^ IS BEFORE SOMTING THAT IS NOT PTR");exit(1);
						}
					}
					if(flag_ == 2){
						if(!(strcmp(V.type,"INT")==0 || strcmp(V.type,"CHAR")==0 || strcmp(V.type,"REAL")==0 || strcmp(V.type,"STRING")==0)){
							yyerror("& IS BEFORE SOMTING THAT IS NOT CHAR/INT/REAL/STRING[]");exit(1);
						}
					}
					if(flag_ == 3){
						if((strcmp(V.type,"BOOL")!=0)){
							yyerror("! IS BEFORE SOMTING THAT IS NOT BOOL!");exit(1);
						}
					}
					flag_ = 0;
				}
				return V;
			}
		}
		s = s->preScope;
	}
}

Variable* func_call_var_arr_creator(node* tree, Variable* temp){
	int i = 0;
	if(tree != NULL){
		while(tree->left != NULL && tree->right != NULL){
			if(strcmp(tree->token, "+") == 0 || strcmp(tree->token, "-") == 0 || strcmp(tree->token, "*") == 0 || strcmp(tree->token, "/") == 0){
				temp[i].type = expression_type_valuator(tree);
				temp[i].name = "EXPR";

			}
			if(strcmp(tree->token, "") == 0){temp[i].type = expression_type_valuator(tree->left); temp[i].name = "EXPR";}
			i++;
			tree = tree->right;
		}
		temp[i].type = expression_type_valuator(tree);
		temp[i].name = "EXPR";
		return temp;
	}
}

void func_checker_helper(int numofvars, Variable* FC, Variable* FD){
	int t = 0;
	while(numofvars > 0){
		//printf("%s - %s\n", FC[numofvars - 1].type, FD[t].type);
		if(strcmp(FC[numofvars-1].type, FD[t].type) != 0){yyerror("INCOMPATIBLE TYPES WHILE CALLING FUNC/PROC!"); exit(1);}
		numofvars--;
		t++;
	}
}

char* expression_type_valuator(node* tree){
	int j = 0;
	if(tree != NULL){if(tree->left!=NULL){if(strcmp(tree->left->token,"CHAR_VALUE") == 0){return "CHAR";}}}
	if(tree != NULL){if(tree->left!=NULL){if(strcmp(tree->left->token,"STRING") == 0){return "INT";}}}
	if(tree->token[0] == '&'){return "INT";}
	if(tree->token[0] == '^'){return var_extractor(tree->token, currentScope).type;}
	if(atoi(tree->token) != 0 || tree->token[0] == '0'){
		for(j = 0 ; tree->token[j] != '\0' ; j++){
			if(tree->token[j] == '.'){return "REAL";}
		}
		return "INT";
	}
	if(strcmp(tree->token, "TRUE") == 0 || strcmp(tree->token, "FALSE") == 0 ){return "BOOL";}
	if(strcmp(tree->token, "NULL") == 0){return "NULL";}
	if(tree->left == NULL && tree->right == NULL){return var_extractor(tree->token, currentScope).type;}
	if((strcmp(tree->left->token,"+") != 0 && strcmp(tree->left->token,"-") != 0 && strcmp(tree->left->token,"*") != 0 && strcmp(tree->left->token,"/") != 0)){
		Variable left;
		left.type = expression_type_valuator(tree->left);
		Variable right;
		right.type = expression_type_valuator(tree->right);
		if(strcmp(left.type, right.type) == 0){return left.type;}
		if((strcmp(left.type,"INT") == 0 && strcmp(right.type,"REAL") == 0) || (strcmp(left.type,"REAL") == 0 && strcmp(right.type,"INT") == 0)){return "REAL";}
		yyerror("INCOMPATIBLE TYPES WITHIN EXPR!");
		exit(1);
	}
	else{
		Variable right = var_extractor(tree->right->token, currentScope);
		char* type_left = expression_type_valuator(tree->left);
		if(strcmp(right.type, type_left) == 0){return type_left;}
		if((strcmp(type_left,"INT") == 0 && strcmp(right.type,"REAL") == 0) || (strcmp(type_left,"REAL") == 0 && strcmp(right.type,"INT") == 0)){return "REAL";}
		yyerror("INCOMPATIBLE TYPES WITHIN EXPR!");
		exit(1);
	}
		
		
}

void func_return_checker(char* type, node* tree){
	if(strcmp(type, expression_type_valuator(tree)) != 0){yyerror("INCOMPATIBLE RETURN TYPE FOR FUNCTION!");}
}

void func_call_assaignment(char* id_type, char* func_name, Scope* s ){
	int i;
	Variable temp = var_extractor(id_type, currentScope);
	id_type = temp.type;
	char* func_return_type = NULL;
	while(s != NULL){
		for(i = 0; i < s->Fcount; i++){
			//printf("%s\n", func_name);
			if(strcmp(s->func[i]->name, func_name) == 0){
				if(s->func[i]->returnType != NULL){func_return_type = s->func[i]->returnType;}
				else{yyerror("CANNOT ASSAIGN A PROC CALL INTO VARIABLE!"); exit(1);}
			}
		}
		if(func_return_type != NULL){
			if(strcmp(func_return_type, id_type) == 0){ return;}
			else{yyerror("INCOMPATIBLE RETURN TYPE FROM FUNC TO VARIABLE!"); exit(1);}
		}
		s = s->preScope;
	}

}

void type_id_check(char* id_type,Scope* s, char* type){
	Variable temp = var_extractor(id_type, currentScope);
	if((id_type[0] == '^') && (strcmp(type, "CHAR")==0 || strcmp(type, "INT")==0 || strcmp(type, "REAL")==0 || strcmp(temp.type, "CHAR_ARR")==0 || strcmp(temp.type, "INT_ARR")==0 || strcmp(temp.type, "REAL_ARR")==0)){return;}
	if(strcmp(type, "NULL")==0 && (strcmp(temp.type, "CHAR_ARR")==0 || strcmp(temp.type, "INT_ARR")==0 || strcmp(temp.type, "REAL_ARR")==0)) {return;}
	if(strcmp(temp.type, "CHAR_ARR")==0 && strcmp(type, "CHAR")==0){return;}
	if(strcmp(temp.type, "INT_ARR")==0 && strcmp(type, "INT")==0){return;}
	if(strcmp(temp.type, "REAL_ARR")==0 && strcmp(type, "REAL")==0){return;}
	if(strcmp(temp.type, "CHAR")==0 && strcmp(type, "CHAR_ARR")==0){return;}
	if(strcmp(temp.type, "INT")==0 && strcmp(type, "INT_ARR")==0){return;}
	if(strcmp(temp.type, "REAL")==0 && strcmp(type, "REAL_ARR")==0){return;}
	if(strcmp(temp.type, type) == 0){return;}else{yyerror("WRONG TYPE!"); exit(1);}
}

void expression_checker_for_condition(node* exp1, node* exp2){
	if(strcmp(expression_type_valuator(exp1), expression_type_valuator(exp2)) != 0){
		if(strcmp(expression_type_valuator(exp1), "INT") == 0 || strcmp(expression_type_valuator(exp1), "REAL") == 0){
			if(strcmp(expression_type_valuator(exp2), "INT") == 0 || strcmp(expression_type_valuator(exp2), "REAL") == 0){
				return;
			}
		}
		yyerror("INCOMPATIBLE TYPES WHILE USING <= / >= / > / < / == / != !"); exit(1);
	}
}


int yyerror(char *err)
{
 printf("ERROR! '%s'\n", err);
 printf("YOUR ERROR IS IN LINE %d AND WITH CHAR '%s' \n", yylineno, yytext);
 return 0;
}
