//Kalyani Pramod Tekade 
//ASU ID - 1230526221

#include <cstdlib>
#include <iostream>
#include "execute.h"
#include <unordered_map>
#include <string>
#include "lexer.h"
#include "inputbuf.h"

using namespace std;

LexicalAnalyzer lexer;

std::unordered_map<std::string, int> variableLocations;

int locationCounter = 0;
extern string reserved[];

struct CaseNode {
    int number;
    InstructionNode* body;
    bool isDefault;
    CaseNode* next;
};
//Forward Declaration 
int location(const std::string& varName);
Token expect(TokenType expectedType);
void parseVarSection();
void parseIdList();
InstructionNode* parseBody();
InstructionNode* parse_stmt_list();
InstructionNode* parse_stmt();
InstructionNode* parseAssignStmt();
void parseExpr(InstructionNode* node);
int parsePrimary(InstructionNode* node);
ArithmeticOperatorType parseOp();
InstructionNode* parseOutputStmt();
InstructionNode* parseInputStmt();
InstructionNode* parseWhileStmt();
InstructionNode* parseIfStmt();
void parseCondition(InstructionNode* whileNode);
ConditionalOperatorType parseRelOp();
InstructionNode* parseForStmt();
InstructionNode* parseSwitchStmt();
CaseNode* parseCaseList();
CaseNode* parseCase();
CaseNode* parseDefaultCase();
InstructionNode* translateSwitchToIfs(int switchVarIndex, CaseNode* caseList);
void parseInputs();
void parseNumList();
struct InstructionNode* parse_Generate_Intermediate_Representation();


int location(const std::string& token) {
    int loc;

    // Check if the token is a variable and already has a location
    auto var = variableLocations.find(token);
    if (var != variableLocations.end()) {
        loc = var->second;
    } else {
        // Assign a new location for a variable or a constant
        loc = next_available++;
        
        // If the token is a variable, update the variableLocations map
        if (isalpha(token[0])) {
            variableLocations[token] = loc;
        } else if (isdigit(token[0])) { // If the token is a number
            int value = stoi(token);
            mem[loc] = value; // Store the constant in memory
        }
    }
    return loc;
}

Token expect(TokenType expectedType) {
    Token token = lexer.GetToken();
    if (token.token_type != expectedType) {
        cout << "SYNTAX ERROR !!!" << endl;
        exit(1);
    }
    return token;
}

struct InstructionNode* parse_Generate_Intermediate_Representation() {
    variableLocations.clear();
    locationCounter = 0;
    next_available = 0;
    inputs.clear();

    parseVarSection(); 
    InstructionNode* bodyNode = parseBody(); 
    parseInputs(); 
    return bodyNode;
}

void parseVarSection() {
    parseIdList();
    expect(SEMICOLON);
}

void parseIdList() {
    string varName = expect(ID).lexeme;
    int loc = location(varName);

    Token token = lexer.peek(1);
    while (token.token_type == COMMA) {
        expect(COMMA);
        varName = expect(ID).lexeme;
        loc = location(varName);
        token = lexer.peek(1);
    }
}

InstructionNode* parseBody() {
    expect(LBRACE);
    InstructionNode* stmts = parse_stmt_list();
    expect(RBRACE);
    return stmts;
}

InstructionNode* parse_stmt_list() {
    InstructionNode* inst = parse_stmt();  

    Token nextToken = lexer.peek(1);
    if (nextToken.token_type == ID || nextToken.token_type == IF || nextToken.token_type == WHILE || nextToken.token_type == FOR || 
    nextToken.token_type == SWITCH || nextToken.token_type == INPUT || nextToken.token_type == OUTPUT ) {
        InstructionNode* instList = parse_stmt_list(); 
        InstructionNode* current = inst;
        while (current->next != nullptr) {
            current = current->next;
        }
        current->next = instList;
    }

    return inst;
}

InstructionNode* parse_stmt() {
    Token token = lexer.peek(1); 

    if (token.token_type == ID) {
        return parseAssignStmt();
    } 
    else if (token.token_type == IF) {
        return parseIfStmt();
    }
    else if (token.token_type == WHILE) {
        return parseWhileStmt();
    }
    else if (token.token_type == SWITCH) {
        return parseSwitchStmt();
    }
    else if (token.token_type == FOR) {
        return parseForStmt();
    }
    else if (token.token_type == OUTPUT) {
        return parseOutputStmt();
    }
    else if (token.token_type == INPUT) {
        return parseInputStmt();
    }
    else {
        exit(1);
        return nullptr;
    }
}


InstructionNode* parseAssignStmt() {
    Token idToken = expect(ID);
    int leftHandSideIndex = location(idToken.lexeme);
    expect(EQUAL);

    InstructionNode* assignNode = new InstructionNode();
    assignNode->type = ASSIGN;
    assignNode->assign_inst.left_hand_side_index = leftHandSideIndex;

    Token nextToken = lexer.peek(1);
    Token nextNextToken = lexer.peek(2);

    if ((nextToken.token_type == ID || nextToken.token_type == NUM) && 
        (nextNextToken.token_type == PLUS || nextNextToken.token_type == MINUS ||
         nextNextToken.token_type == MULT || nextNextToken.token_type == DIV)) {
        parseExpr(assignNode);
    } else {
        assignNode->assign_inst.op = OPERATOR_NONE;
        assignNode->assign_inst.opernd1_index = parsePrimary(assignNode); 
    }

    expect(SEMICOLON);
    assignNode->next = nullptr;
    return assignNode;
}

void parseExpr(InstructionNode* node) {
    node->assign_inst.opernd1_index = parsePrimary(node);
    node->assign_inst.op = parseOp();
    node->assign_inst.opernd2_index = parsePrimary(node);
}


int parsePrimary(InstructionNode* assignNode) {
    Token token = lexer.GetToken();
    if (token.token_type == ID) {
        int varIndex = location(token.lexeme);
        return varIndex;
    } else if (token.token_type == NUM) {
        int constIndex = next_available;
        mem[constIndex] = stoi(token.lexeme);
        next_available++;
        return constIndex;
    } else {
        exit(1);
    }
}



ArithmeticOperatorType parseOp() {
    Token token = lexer.GetToken(); 

    if (token.token_type == PLUS) {
        return OPERATOR_PLUS;
    } else if (token.token_type == MINUS) {
        return OPERATOR_MINUS;
    } else if (token.token_type == MULT) {
        return OPERATOR_MULT;
    } else if (token.token_type == DIV) {
        return OPERATOR_DIV;
    } else {
        return OPERATOR_NONE;
    }
}

InstructionNode* parseOutputStmt() {
    expect(OUTPUT);
    Token idToken = expect(ID);
    int varIndex = location(idToken.lexeme);
    expect(SEMICOLON);

    InstructionNode* outputNode = new InstructionNode();
    outputNode->type = OUT;
    outputNode->output_inst.var_index = varIndex;
    outputNode->next = nullptr;

    return outputNode;
}

InstructionNode* parseInputStmt() {
    expect(INPUT);
    Token idToken = expect(ID);
    int varIndex = location(idToken.lexeme);

    expect(SEMICOLON);

    InstructionNode* inputNode = new InstructionNode();
    inputNode->type = IN;
    inputNode->input_inst.var_index = varIndex;
    inputNode->next = nullptr;

    return inputNode;
}

InstructionNode* parseWhileStmt() {
    expect(WHILE);
    InstructionNode* whileNode = new InstructionNode();
    whileNode->type = CJMP;

    parseCondition(whileNode);

    whileNode->next = parseBody();

    InstructionNode* currentNode = whileNode->next;
    while (currentNode && currentNode->next) {
        currentNode = currentNode->next;
    }

    InstructionNode* jmpNode = new InstructionNode();
    jmpNode->type = JMP;
    jmpNode->jmp_inst.target = whileNode;
    currentNode->next = jmpNode;

    InstructionNode* noopNode = new InstructionNode();
    noopNode->type = NOOP;
    jmpNode->next = noopNode;
    whileNode->cjmp_inst.target = noopNode;

    return whileNode;
}


InstructionNode* parseIfStmt() {
    expect(IF);
    InstructionNode* ifNode = new InstructionNode();
    ifNode->type = CJMP;
    parseCondition(ifNode);
    ifNode->next = parseBody();
    InstructionNode* noopNode = new InstructionNode();
    noopNode->type = NOOP;
    noopNode->next = nullptr;

    InstructionNode* lastNodeInIfBody = ifNode->next;
    while (lastNodeInIfBody && lastNodeInIfBody->next) {
        lastNodeInIfBody = lastNodeInIfBody->next;
    }
    lastNodeInIfBody->next = noopNode;
    ifNode->cjmp_inst.target = noopNode;
    return ifNode;
}

void parseCondition(InstructionNode* whileNode) {
    whileNode->cjmp_inst.opernd1_index = parsePrimary(whileNode);
    whileNode->cjmp_inst.condition_op = parseRelOp();
    whileNode->cjmp_inst.opernd2_index = parsePrimary(whileNode);
}

ConditionalOperatorType parseRelOp() {
    Token token = lexer.GetToken(); 

    if (token.token_type == GREATER) {
        return CONDITION_GREATER;
    } else if (token.token_type == LESS) {
        return CONDITION_LESS;
    } else if (token.token_type == NOTEQUAL) {
        return CONDITION_NOTEQUAL;
    } else {
        exit(1); 
    }
}

InstructionNode* parseForStmt() {
    expect(FOR);
    expect(LPAREN);

    InstructionNode* initAssignStmt = parseAssignStmt();

    InstructionNode* conditionNode = new InstructionNode();
    conditionNode->type = CJMP;
    parseCondition(conditionNode);
    expect(SEMICOLON);

    InstructionNode* updateAssignStmt = parseAssignStmt();
    expect(RPAREN);

    InstructionNode* bodyNode = parseBody();

    initAssignStmt->next = conditionNode;
    conditionNode->next = bodyNode;

    InstructionNode* current = bodyNode;
    while (current->next != nullptr) {
        current = current->next;
    }

    current->next = updateAssignStmt;

    InstructionNode* jmpBackToCondition = new InstructionNode();
    jmpBackToCondition->type = JMP;
    jmpBackToCondition->jmp_inst.target = conditionNode;
    updateAssignStmt->next = jmpBackToCondition;
    
    InstructionNode* noopNode = new InstructionNode();
    noopNode->type = NOOP;
    jmpBackToCondition->next = noopNode;
    conditionNode->cjmp_inst.target = noopNode;

    return initAssignStmt;
}

InstructionNode* parseSwitchStmt() {
    expect(SWITCH);
    Token varToken = expect(ID); 
    int switchVarIndex = location(varToken.lexeme);
    expect(LBRACE);

    CaseNode* caseList = parseCaseList();
    
    InstructionNode* switchNode = translateSwitchToIfs(switchVarIndex, caseList);

    expect(RBRACE);
    return switchNode;
}

CaseNode* parseCaseList() {
    Token nextToken = lexer.peek(1);

    if (nextToken.token_type != CASE && nextToken.token_type != DEFAULT) {
        return nullptr;
    }

    CaseNode* newCase = new CaseNode();
    if (nextToken.token_type == CASE) {
        newCase = parseCase(); 
    } else if (nextToken.token_type == DEFAULT) {
        newCase = parseDefaultCase(); 
    }
    newCase->next = parseCaseList(); 
    return newCase; 
}


CaseNode* parseCase() {
    expect(CASE);
    Token caseValueToken = expect(NUM);
    expect(COLON);

    CaseNode* caseNode = new CaseNode();
    caseNode->number = stoi(caseValueToken.lexeme);
    caseNode->body = parseBody();
    caseNode->isDefault = false;
    caseNode->next = nullptr;
    
    return caseNode;
}

CaseNode* parseDefaultCase() {
    expect(DEFAULT);
    expect(COLON);

    CaseNode* defaultNode = new CaseNode();
    defaultNode->body = parseBody();
    defaultNode->isDefault = true;
    defaultNode->next = nullptr;

    return defaultNode;
}

InstructionNode* translateSwitchToIfs(int switchVarIndex, CaseNode* caseList) {
    InstructionNode* head = nullptr;
    InstructionNode* lastCjmpNode = nullptr;
    InstructionNode* noopNodeAtEnd = new InstructionNode();
    noopNodeAtEnd->type = NOOP;

    CaseNode* defaultCase = nullptr; 
    CaseNode* currentCase = caseList;
    while (currentCase != nullptr) {
        if (currentCase->isDefault) {
            defaultCase = currentCase; 
        } else {
            InstructionNode* ifNode = new InstructionNode();
            ifNode->type = CJMP;
            ifNode->cjmp_inst.condition_op = CONDITION_NOTEQUAL;
            ifNode->cjmp_inst.opernd1_index = switchVarIndex;
            ifNode->cjmp_inst.opernd2_index = location(std::to_string(currentCase->number));

            if (!head) {
                head = ifNode;
            } else {
                lastCjmpNode->next = ifNode; 
            }
            ifNode->cjmp_inst.target = currentCase->body;
            lastCjmpNode = ifNode; 
            InstructionNode* bodyLastNode = currentCase->body;
            while (bodyLastNode && bodyLastNode->next) {
                bodyLastNode = bodyLastNode->next;
            }
            InstructionNode* jmpToEnd = new InstructionNode();
            jmpToEnd->type = JMP;
            jmpToEnd->jmp_inst.target = noopNodeAtEnd;
            bodyLastNode->next = jmpToEnd;
        }
        currentCase = currentCase->next;
    }
    if (lastCjmpNode) {
        lastCjmpNode->next = defaultCase ? defaultCase->body : noopNodeAtEnd;
    }
    if (!head && defaultCase) {
        head = defaultCase->body;
    }
    if (defaultCase) {
        InstructionNode* current = defaultCase->body;
        while (current && current->next) {
            current = current->next;
        }
        current->next = noopNodeAtEnd;
    }
    return head;
}

void parseInputs() {
    parseNumList();
}

void parseNumList() {
    Token nextToken = lexer.peek(1);
    while (nextToken.token_type == NUM) {
        Token numToken = lexer.GetToken(); 
        int inputValue = std::stoi(numToken.lexeme);
        inputs.push_back(inputValue);
        nextToken = lexer.peek(1);
    }

    if (nextToken.token_type != END_OF_FILE) {
        exit(1);
    }
 
}
