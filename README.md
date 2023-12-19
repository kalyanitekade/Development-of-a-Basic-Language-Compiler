#CSE 340 Project
# Development-of-a-Basic-Language-Compiler
The goal of this project is to implement a small compiler. We will write a compiler for a simple language. 
We will not be generating assembly code. Instead, we will generate an intermediate representation (a data structure
that represents the program). The execution of the program will be done after compilation by
interpreting the generated intermediate representation using an interpreter that was provided.

The grammar for this project is the following:
program -> var section body inputs
var section -> id list SEMICOLON
id list -> ID COMMA id list | ID
body -> LBRACE stmt list RBRACE
stmt list -> stmt stmt list | stmt
stmt -> assign stmt | while stmt | if stmt | switch stmt | for stmt
stmt -> output stmt | input stmt
assign stmt -> ID EQUAL primary SEMICOLON
assign stmt -> ID EQUAL expr SEMICOLON
expr -> primary op primary
primary -> ID | NUM
op -> PLUS | MINUS | MULT | DIV
output stmt -> output ID SEMICOLON
input stmt -> input ID SEMICOLON
while stmt -> WHILE condition body
if stmt -> IF condition body
condition -> primary relop primary
relop -> GREATER | LESS | NOTEQUAL
switch stmt -> SWITCH ID LBRACE case list RBRACE
switch stmt -> SWITCH ID LBRACE case list default case RBRACE
for stmt -> FOR LPAREN assign stmt condition SEMICOLON assign stmt RPAREN body
case list -> case case list | case
case -> CASE NUM COLON body
default case -> DEFAULT COLON body
inputs -> num list
num list -> NUM
num list -> NUM num list
