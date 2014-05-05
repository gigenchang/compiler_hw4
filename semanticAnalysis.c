#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "header.h"
#include "symbolTable.h"
// This file is for reference only, you are not required to follow the implementation. //
// You only need to check for errors stated in the hw4 document. //
int g_anyErrorOccur = 0;

DATA_TYPE getBiggerType(DATA_TYPE dataType1, DATA_TYPE dataType2);
void processProgramNode(AST_NODE *programNode);
void processDeclarationNode(AST_NODE* declarationNode);
void declareIdList(AST_NODE* typeNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize);
void declareFunction(AST_NODE* returnTypeNode);
void processDeclDimList(AST_NODE* variableDeclDimList, TypeDescriptor* typeDescriptor, int ignoreFirstDimSize);
void processTypeNode(AST_NODE* typeNode);
void processBlockNode(AST_NODE* blockNode);
void processStmtNode(AST_NODE* stmtNode);
void processGeneralNode(AST_NODE *node);
void checkAssignOrExpr(AST_NODE* assignOrExprRelatedNode);
void checkWhileStmt(AST_NODE* whileNode);
void checkForStmt(AST_NODE* forNode);
void checkAssignmentStmt(AST_NODE* assignmentNode);
void checkIfStmt(AST_NODE* ifNode);
void checkWriteFunction(AST_NODE* functionCallNode);
void checkFunctionCall(AST_NODE* functionCallNode);
void processExprRelatedNode(AST_NODE* exprRelatedNode);
void checkParameterPassing(Parameter* formalParameter, AST_NODE* actualParameter);
void checkReturnStmt(AST_NODE* returnNode);
void processExprNode(AST_NODE* exprNode);
void processVariableLValue(AST_NODE* idNode);
void processVariableRValue(AST_NODE* idNode);
void processConstValueNode(AST_NODE* constValueNode);
void getExprOrConstValue(AST_NODE* exprOrConstNode, int* iValue, float* fValue);
void evaluateExprValue(AST_NODE* exprNode);


typedef enum ErrorMsgKind
{
    SYMBOL_IS_NOT_TYPE,
    SYMBOL_REDECLARE,
    SYMBOL_UNDECLARED,
    NOT_FUNCTION_NAME,
    TRY_TO_INIT_ARRAY,
    EXCESSIVE_ARRAY_DIM_DECLARATION,
    RETURN_ARRAY,
    VOID_VARIABLE,
    TYPEDEF_VOID_ARRAY,
    PARAMETER_TYPE_UNMATCH,
    TOO_FEW_ARGUMENTS,
    TOO_MANY_ARGUMENTS,
    RETURN_TYPE_UNMATCH,
    INCOMPATIBLE_ARRAY_DIMENSION,
    NOT_ASSIGNABLE,
    NOT_ARRAY,
    IS_TYPE_NOT_VARIABLE,
    IS_FUNCTION_NOT_VARIABLE,
    STRING_OPERATION,
    ARRAY_SIZE_NOT_INT,
    ARRAY_SIZE_NEGATIVE,
    ARRAY_SUBSCRIPT_NOT_INT,
    PASS_ARRAY_TO_SCALAR,
    PASS_SCALAR_TO_ARRAY
} ErrorMsgKind;

void printErrorMsgSpecial(AST_NODE* node1, char* name2, ErrorMsgKind errorMsgKind)
{
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node1->linenumber);
    /*
    switch(errorMsgKind)
    {
    default:
        printf("Unhandled case in void printErrorMsg(AST_NODE* node, ERROR_MSG_KIND* errorMsgKind)\n");
        break;
    }
    */
}


void printErrorMsg(AST_NODE* node, ErrorMsgKind errorMsgKind)
{
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    /*
    switch(errorMsgKind)
    {
        printf("Unhandled case in void printErrorMsg(AST_NODE* node, ERROR_MSG_KIND* errorMsgKind)\n");
        break;
    }
    */
}


void semanticAnalysis(AST_NODE *root)
{
    processProgramNode(root);
}


DATA_TYPE getBiggerType(DATA_TYPE dataType1, DATA_TYPE dataType2)
{
    if(dataType1 == FLOAT_TYPE || dataType2 == FLOAT_TYPE) {
        return FLOAT_TYPE;
    } else {
        return INT_TYPE;
    }
}


void processProgramNode(AST_NODE *programNode)
{
	AST_NODE* programNodeChild = programNode->child->leftmostSibling;

	while(programNodeChild != NULL) {
		switch(programNodeChild->nodeType) {
			case (DECLARATION_NODE):  //for function declaration node
				processDeclarationNode(programNodeChild);
				break;
			case (VARIABLE_DECL_LIST_NODE):
				variableDeclListNodeChlid = programNodeChild->child->leftmostSibling;
				while(variableDeclListNodeChlid != NULL) {
					processDeclarationNode(variableDeclListNodeChlid);
					variableDeclListNodeChlid = variableDeclListNodeChlid->rightSibling;	
				}
				break;
			default:
				printf("Error: 無法判斷的program_node的child node");
		}
		programNodeChild = programNodeChild->rightSibling;
	}
}

void processDeclarationNode(AST_NODE* declarationNode)
{
	//處理四種不同的type declration
	//VARIABLE_DECL  TYPE_DECL   FUNCTION_DECL  FUNCTION_PARAMETER_DECL
	switch (declarationNode->semantic_value.declSemanticValue.kind) {
		case(VARIABLE_DECL):
			//Insert type descriptor entry to symboltable 
			break;
		case(TYPE_DECL):
			//Insert  entry to symboltable
			break;
		case(FUNCTION_DECL):
			//Insert function signature entry to symboltable
			break:
		case(FUNCTION_PARAMETER_DECL):
			//parameter list (point to type descriptor in symboltable)
			break;
		default:
			printf("Error: 在processDeclarationNode發現傳入未知的DeclNode");
	}
}


void processTypeNode(AST_NODE* idNodeAsType)
{
	//**給ID Node
	//**可能是NORMAL_ID/ARRAY_ID/WITH_INIT_ID
}


void declareIdList(AST_NODE* declarationNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize)
{
	//底層，接下來要插入Entry了
}

void checkAssignOrExpr(AST_NODE* assignOrExprRelatedNode)
{
	//if assign ...call blabalbal(call checkAssignmentStmt)
	//if rela ... cla blablabla(call processExprRelatedNode)
	//call processExprNode
}

void checkWhileStmt(AST_NODE* whileNode)
{
	//deal with type WHILE_STMT as a stmt node
	//call the children of whileNode(processStmtNode and all processExprRelatedNode)
}


void checkForStmt(AST_NODE* forNode)
{
	//deal with type FOR_STMT as a stmt node
	//call the children of forNode
	//including some checkAssignmentStmt 
	//along with NORMAL_ID check if necessary
	//and all processExprRelatedNode
}


void checkAssignmentStmt(AST_NODE* assignmentNode)
{
	//insert table entry for ASSIGN_STMT node
}


void checkIfStmt(AST_NODE* ifNode)
{
	//deal with type IF_STMT as a stmt node
	//call the children same as while statement
}

void checkWriteFunction(AST_NODE* functionCallNode)
{
}

void checkFunctionCall(AST_NODE* functionCallNode)
{
}

void checkParameterPassing(Parameter* formalParameter, AST_NODE* actualParameter)
{
}


void processExprRelatedNode(AST_NODE* exprRelatedNode)
{
	AST_NODE* exprRelatedNodeChild = exprRelatedNode->child;

	while(exprRelatedNodeChild != NULL) {
		switch(exprRelatedNodeChild->semantic_value.op.binaryOp) {
			case BINARY_OP_AND:  
				//TODO
				break;
			case BINARY_OP_OR:
				//TODO  
				break;
			case BINARY_OP_EQ:
				//TODO  
				break;
			case BINARY_OP_GE:  
				//TODO
				break;
			case BINARY_OP_GT:
				//TODO  
				break;
			case BINARY_OP_LT:
				//TODO  
				break;
			case BINARY_OP_LE:  
				//TODO
				break;
			case BINARY_OP_NE:
				//TODO
				break;
			default:
				printf("Error: exprRelatedNodeChild type error\n");
		}
		exprRelatedNodeChild = exprRelatedNodeChild->rightSibling;
	}
}

void getExprOrConstValue(AST_NODE* exprOrConstNode, int* iValue, float* fValue)
{
	switch (exprOrConstNode.nodeType) {
		case(EXPR_NODE):
			SymbolAttribute attribute;
			attribute.attributeKind = VARIABLE_ATTRIBUTE;
			attribute.attr.typeDescriptor.kind = ;
			enterSymbol(function_name, attribute);			
			break;
		case(CONST_VALUE_NODE):
			//
			break;
		default:
			printf("Error: exprOrConstNode type error\n");
	}
}

void evaluateExprValue(AST_NODE* exprNode)
{
}


void processExprNode(AST_NODE* exprNode)
{
	switch (exprRelatedNode->semantic_value.exprSemanticValue.kind) {
		case(BINARY_OPERATION):
			//TODO 
			break;
		case(UNARY_OPERATION):
			//TODO
			break;
		default:
			printf("Error: exprRelatedNode type error\n");
	}
}


void processVariableLValue(AST_NODE* idNode)
{
}

void processVariableRValue(AST_NODE* idNode)
{
}


void processConstValueNode(AST_NODE* constValueNode)
{
	switch ((*constValueNode->semantic_value.const1).const_type) {
		case(INTEGERC):
			//TODO 
			break;
		case(FLOATC):
			//TODO
			break;
		case(STRINGC):
			//TODO
			break;
		default:
			printf("Error: constValudNode type error\n");
	}
}


void checkReturnStmt(AST_NODE* returnNode)
{
}


void processBlockNode(AST_NODE* blockNode)
{
	//處理
	//可能有decl_list node 和 stmt_list node
	//stmt_list_node
	//發現底下有stmt_list node，就依序call processStmtNode()
	//發現底下有decl_list node，就依序call 
	AST_NODE* blockNodeChild = blockNode->child->leftmostSibling;
	while(blockNodeChild != NULL) {
		//處理該child
		switch(blockNodeChild->nodeType) {
			case(VARIABLE_DECL_LIST_NODE):
				//TODO, call function to handle variable_decl_list_node				
				break;
			case(STMT_LIST_NODE):
				AST_NODE* stmtListNode = blockNodeChild;
				stmtListNodeChild = stmtListNode->child->leftmostSibling;
				while(stmtListNodeChild != NULL) {
					switch(stmtListNodeChild->nodeType){
						case STMT_NODE:
							processStmtNode(stmtListNodeChild);
							break;
						case BLOCK_NODE:
							processBlockNode(stmtListNodeChild);
							break;
						case NUL_NODE:   
							//空述句
							break;
						default:
							printf("Error: 無法判斷的STMT_LIST_NODE之子節點")	
					}
					stmtListNodeChild = stmtListNodeChild->rightSibling;
				}
				break;
			default:
				printf("Error: 無法判斷的blockNode之子節點")
		}
		//換到右邊的sibling繼續處理
		blockNodeChild = blockNodeChild->rightSibling;
	}
}


void processStmtNode(AST_NODE* stmtNode)
{
	//看是哪種stmt node
	//while/for/assign_stmt/if_stmt/function_call_stmt/return_stmt
	switch (stmtNode->semantic_value.stmtSemanticValue.kind) {
		case WHILE_STMT:
			checkWhileStmt(stmtNode);
			break;
		case FOR_STMT:
			checkForStmt(stmtNode);
			break;
		case ASSIGN_STMT:
			checkAssignmentStmt(stmtNode);
			break;
		case IF_STMT:
			checkIfStmt(stmtNode);
			break;
		case FUNCTION_CALL_STMT:
			checkFunctionCall(stmtNode);
			break;
		case RETURN_STMT:
			checkReturnStmt(stmtNode);
			break;
		default:
			//This should not happen;
			printf("Error: processStmtNode 出現無法判斷的Stmt");
	}
	
}


void processGeneralNode(AST_NODE *node)
{
	switch (node->dataType) {
		case INT_TYPE:
			//TODO
			break;
		case FLOAT_TYPE:
			//TODO
			break;
		case VOID_TYPE:
			//TODO
			break;
		case INT_PTR_TYPE:
			//TODO
			break;
		case FLOAT_PTR_TYPE:
			//TODO
			break;
		case CONST_STRING_TYPE:
			//TODO
			break;
		case NONE_TYPE:
			//TODO
			break;
		case ERROR_TYPE:
			//TODO
			break;
		default:
			printf("Error: processStmtNode 出現無法判斷的Stmt");
	}
}

void processDeclDimList(AST_NODE* idNode, TypeDescriptor* typeDescriptor, int ignoreFirstDimSize)
{
	// New an entry to the symbol table with typeDescriptor
}


void declareFunction(AST_NODE* declarationNode)
{
	SymbolAttribute attribute;
	attribute.attributeKind = FUNCTION_SIGNATURE;
	attribute.attr.functionSignature = ;
	enterSymbol(function_name, attribute);
}
