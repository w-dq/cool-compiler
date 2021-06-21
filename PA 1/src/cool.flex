/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
  if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
    YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
int comment_stack = 0;

%}

%option noyywrap

/*
 * Define names for regular expressions here.
 */

digit       [0-9]

%x str invalid_str comment
%%

(c|C)(l|L)(a|A)(s|S)(s|S) return CLASS;

(e|E)(l|L)(s|S)(e|E)      return ELSE;

(F|f)(I|i)                return FI;

(I|i)(F|f)                return IF;

(I|i)(N|n)                return IN;

(i|I)(n|N)(h|H)(e|E)(r|R)(i|I)(t|T)(s|S)      return INHERITS;

(i|I)(s|S)(v|V)(o|O)(i|I)(d|D)                return ISVOID;

(l|L)(e|E)(t|T)           return LET;

(l|L)(o|O)(o|O)(p|P)      return LOOP;

(p|P)(o|O)(o|O)(l|L)      return POOL;

(t|T)(h|H)(e|E)(n|N)      return THEN;

(w|W)(h|H)(i|I)(l|L)(e|E) return WHILE;

(c|C)(a|A)(s|S)(e|E)      return CASE;

(e|E)(s|S)(a|A)(c|C)      return ESAC;

(n|N)(e|E)(w|W)           return NEW;

(o|O)(f|F)                return OF;

(n|N)(o|O)(t|T)           return NOT;

t(r|R)(u|U)(e|E)          {
                            cool_yylval.boolean = true; 
                            return BOOL_CONST;
                          }

f(a|A)(L|l)(S|s)(e|E)     {
                            cool_yylval.boolean = false; 
                            return BOOL_CONST;
                          }

\.        return '.';
\@        return '@';
\(        return '(';
\)        return ')';
\{        return '{';
\}        return '}';
\;        return ';';
\:        return ':';
\,        return ',';
\+        return '+';
\-        return '-';
\*        return '*';
\/        return '/';
\=        return '=';
\~        return '~';
\<        return '<';
\<\=      return  LE;
\=\>      return DARROW;
\<\-      return ASSIGN;


[A-Z][a-zA-Z0-9_]*  {
                        cool_yylval.symbol = idtable.add_string(yytext);
                        return TYPEID;
                    }

[a-z][a-zA-Z0-9_]*  {
                        cool_yylval.symbol = idtable.add_string(yytext);
                        return OBJECTID;
                    }
[0-9]+              {
                        cool_yylval.symbol = inttable.add_string(yytext);
                        return INT_CONST;
                    }

\"                  { 
                      string_buf_ptr = string_buf; 
                      BEGIN(str);
                    }

<str>\"             {
                      BEGIN(INITIAL);
                      *string_buf_ptr = '\0';
                      cool_yylval.symbol = stringtable.add_string(string_buf);
                      return STR_CONST;
                    }

<str>\n             {
                      ++curr_lineno;
                      BEGIN(INITIAL);
                      cool_yylval.error_msg = ("Unterminated string constant");
                      return ERROR;
                    }

<str>\\?\0          {
                      BEGIN(INITIAL);
                      cool_yylval.error_msg = ("String contains null character.");
                      return ERROR;
                    }

<str><<EOF>>        {
                      BEGIN(INITIAL);
                      cool_yylval.error_msg = ("EOF in string constant");
                      return ERROR;
                    }

<str>\\n            {
                      if (string_buf_ptr + 1 > &string_buf[MAX_STR_CONST - 1]) {
                        BEGIN(INITIAL);
                        cool_yylval.error_msg = ("String constant too long");
                        return ERROR;
                      }
                      *string_buf_ptr++ = '\n';
                    }

<str>\\t            {
                      if (string_buf_ptr + 1 > &string_buf[MAX_STR_CONST - 1]) {
                        BEGIN(INITIAL);
                        cool_yylval.error_msg = ("String constant too long");
                        return ERROR;
                      }
                      *string_buf_ptr++ = '\t';
                    }

<str>\\b            {
                      if (string_buf_ptr + 1 > &string_buf[MAX_STR_CONST - 1]) {
                        BEGIN(INITIAL);
                        cool_yylval.error_msg = ("String constant too long");
                        return ERROR;
                      }
                      *string_buf_ptr++ = '\b';
                    }

<str>\\f            {
                      if (string_buf_ptr + 1 > &string_buf[MAX_STR_CONST - 1]) {
                        BEGIN(INITIAL);
                        cool_yylval.error_msg = ("String constant too long");
                        return ERROR;
                      }
                      *string_buf_ptr++ = '\f';
                    }

<str>\\\n           {
                      ++curr_lineno;
                      if (string_buf_ptr + 1 > &string_buf[MAX_STR_CONST - 1]) {
                        BEGIN(INITIAL);
                        cool_yylval.error_msg = ("String constant too long");
                        return ERROR;
                      }
                      *string_buf_ptr++ = '\n';
                    }

<str>\\.            {
                      if (string_buf_ptr + 1 > &string_buf[MAX_STR_CONST - 1]) {
                        BEGIN(INITIAL);
                        cool_yylval.error_msg = ("String constant too long");
                        return ERROR;
                      }
                      *string_buf_ptr++ = yytext[1];
                    }

<str>[^\\\n\0\"]+   {
                      if (string_buf_ptr + yyleng > &string_buf[MAX_STR_CONST - 1]) {
                        BEGIN(INITIAL);
                        cool_yylval.error_msg = ("String constant too long");
                        return ERROR;
                      }
                      char *yptr = yytext;
                      while ( *yptr )
                      *string_buf_ptr++ = *yptr++;
                    }
  /*
   *<invalid_str>\"     BEGIN(INITIAL);
   *
   *<invalid_str>\n     {
   *                      ++curr_lineno;
   *                      BEGIN(INITIAL);
   *                      cool_yylval.error_msg = ("Unterminated string constant");
   *                      return ERROR;
   *                    }
   *
   *<invalid_str>\\\n   ++curr_lineno;
   *
   *<invalid_str>\\.         ;
   *
   *<invalid_str>[^\\\n\"]+  ;
   */


"--".*                  ;

\*\)                    {
                          cool_yylval.error_msg = ("Unmatched *)");
                          return ERROR;
                        }

<INITIAL,comment>\(\*   {
                          BEGIN(comment);
                          ++comment_stack;
                        }

<comment>\*+\)          {
                          if (--comment_stack < 1)
                              BEGIN(INITIAL);
                        }
<comment><<EOF>>        {
                          BEGIN(INITIAL); 
                          cool_yylval.error_msg = ("EOF in comment"); \
                          return ERROR;
                        }
<comment>\\\n               ++curr_lineno;
<comment>\\.                ;
<comment>[^(*\\\n]*         ;
<comment>\(+[^(*\\\n]*      ; 
<comment>\*+[^)*\\\n]*      ;
<comment>\n                 ++curr_lineno;



(\ |\r|\f|\t|\v)  ;

\n              curr_lineno++;

.               {
                  cool_yylval.error_msg = (yytext); \
                  return ERROR;
                }

%%