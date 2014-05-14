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
void printSymbolTable();

typedef enum ErrorMsgKind
{
    SYMBOL_IS_NOT_TYPE,
    SYMBOL_REDECLARE,  //**
    SYMBOL_UNDECLARED, //**
    NOT_FUNCTION_NAME,
    TRY_TO_INIT_ARRAY,
    EXCESSIVE_ARRAY_DIM_DECLARATION,
    RETURN_ARRAY,
    VOID_VARIABLE,
    TYPEDEF_VOID_ARRAY,
    PARAMETER_TYPE_UNMATCH,
    TOO_FEW_ARGUMENTS, //*
    TOO_MANY_ARGUMENTS, //*
    RETURN_TYPE_UNMATCH, //**
    INCOMPATIBLE_ARRAY_DIMENSION, //*
    NOT_ASSIGNABLE,
    NOT_ARRAY,
    IS_TYPE_NOT_VARIABLE,
    IS_FUNCTION_NOT_VARIABLE,
    STRING_OPERATION,
    ARRAY_SIZE_NOT_INT,
    ARRAY_SIZE_NEGATIVE,
    ARRAY_SUBSCRIPT_NOT_INT, //**
    PASS_ARRAY_TO_SCALAR, //*
    PASS_SCALAR_TO_ARRAY  //*
} ErrorMsgKind;

extern SymbolTable symbolTable;
extern int currentScopeDisplay;

void printSymbolTable()
{
	printf("SYMBOL TABLE:\n-----------\n");
	printf("Current Level: %d\n", symbolTable.currentLevel);
	printf("scope Display Element Count: %d\n", symbolTable.scopeDisplayElementCount);
	printf("scope Display: \n");
	int elementCount = 0;
	while(elementCount < symbolTable.scopeDisplayElementCount){
		SymbolTableEntry* STEpointer = symbolTable.scopeDisplay[elementCount];
		printf("Element Count: %d\n", elementCount);
		while(STEpointer != NULL){
			printf("(%s, %d)-> ", STEpointer->name, STEpointer->nestingLevel);
			STEpointer = STEpointer->nextInSameLevel;
		}
		printf("\n");
		elementCount += 1;
	}
}

void printASTNodeInfo(AST_NODE* node) {
	printf("AST_Node:\n");
	if (node == NULL) {
		printf("This node is NULL\n");
	} else {
		printf("nodeType:");
		switch(node->nodeType) {
			case PROGRAM_NODE:
				 printf("PROGRAM_NODE");
				 break;
			case DECLARATION_NODE:
				 printf("DECLARATION_NODE");
				 break;
			case IDENTIFIER_NODE:
				 printf("IDENTIFIER_NODE");
				 break;
			case PARAM_LIST_NODE:
				 printf("PARAM_LIST_NODE");
				 break;
			case NUL_NODE:
				 printf("NUL_NODE");
				 break;
			case BLOCK_NODE:
				 printf("BLOCK_NODE");
				 break;
			case VARIABLE_DECL_LIST_NODE:
				 printf("VARIABLE_DECL_LIST_NODE");
				 break;
			case STMT_LIST_NODE:
				 printf("STMT_LIST_NODE");
				 break;
			case STMT_NODE:
				 printf("STMT_NODE");
				 break;
			case EXPR_NODE:
				 printf("EXPR_NODE");
				 break;
			case CONST_VALUE_NODE:
				 printf("CONST_VALUE_NODE");
				 break;
			case NONEMPTY_ASSIGN_EXPR_LIST_NODE:
				 printf("PROGRAM_NODE");
				 break;
			case NONEMPTY_RELOP_EXPR_LIST_NODE:
				 printf("PROGRAM_NODE");
				 break;
		}
		printf("\n");
	}
}

void printErrorMsgSpecial(AST_NODE* node1, char* name2, ErrorMsgKind errorMsgKind)
{
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node1->linenumber);
    switch(errorMsgKind)
    {
		case(PASS_ARRAY_TO_SCALAR):
			printf("Array %s passed to scalar parameter %s.\n", node1->semantic_value.identifierSemanticValue.identifierName, name2);
			break;
		case(PASS_SCALAR_TO_ARRAY):
			printf("Scalar %s passed to array parameter %s.\n", node1->semantic_value.identifierSemanticValue.identifierName, name2);
			break;
		default:
			printf("Unhandled case in void printErrorMsg(AST_NODE* node, ERROR_MSG_KIND* errorMsgKind)\n");
			break;
    }
}




void printErrorMsg(AST_NODE* node, ErrorMsgKind errorMsgKind)
{
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    switch(errorMsgKind)
    {
		case(SYMBOL_UNDECLARED):
			printf("ID %s undeclared.\n", node->semantic_value.identifierSemanticValue.identifierName);
			break;
		case(SYMBOL_REDECLARE):
			printf("ID %s redeclared.\n", node->semantic_value.identifierSemanticValue.identifierName);
			break;
		case(TOO_FEW_ARGUMENTS):
			printf("too few arguments to function %s.\n", node->semantic_value.identifierSemanticValue.identifierName); //要傳functionStmt下的ID node進去
			break;
		case(TOO_MANY_ARGUMENTS):
			printf("too many arguments to function %s.\n", node->semantic_value.identifierSemanticValue.identifierName); //要傳functionStmt下的ID node進去
			break;
		case(RETURN_TYPE_UNMATCH):
			printf("Incompatible return type.\n");
			break;
		case(INCOMPATIBLE_ARRAY_DIMENSION):
			printf("Incompatible array dimensions.\n");
			break;
		case(ARRAY_SUBSCRIPT_NOT_INT):
			printf("Array subscript is not an integer.\n");
			break;
		default:
			printf("Unhandled case in void printErrorMsg(AST_NODE* node, ERROR_MSG_KIND* errorMsgKind)\n");
			break;
    }
}


void semanticAnalysis(AST_NODE *root)
{
	printf("[In semanticAnalysis]\n");
    processProgramNode(root);
}


DATA_TYPE getBiggerType(DATA_TYPE dataType1, DATA_TYPE dataType2)
{
	printf("[In getBiggerType]\n");
    if(dataType1 == FLOAT_TYPE || dataType2 == FLOAT_TYPE) {
        return FLOAT_TYPE;
    } else {
        return INT_TYPE;
    }
}


void processProgramNode(AST_NODE *programNode)
{
	printf("[In processProgramNode]\n");
	AST_NODE* programNodeChild = programNode->child;

	while(programNodeChild != NULL) {
		switch(programNodeChild->nodeType) {
			case (DECLARATION_NODE):  //for function declaration node
				processDeclarationNode(programNodeChild);
				//printf("after handling declaration node\n");
				break;
			case (VARIABLE_DECL_LIST_NODE):
				{
					AST_NODE* variableDeclListNodeChlid = programNodeChild->child;
					while(variableDeclListNodeChlid != NULL) {
						processDeclarationNode(variableDeclListNodeChlid);
						variableDeclListNodeChlid = variableDeclListNodeChlid->rightSibling;	
					}
				}
				break;
			default:
				printf("Error: 無法判斷的program_node的child node");
		}
		programNodeChild = programNodeChild->rightSibling;
	}
	printf("End processProgramNode\n");
	printSymbolTable();
}

void processDeclarationNode(AST_NODE* declarationNode)
{
	printf("[In processDeclarationNode]\n");
	//處理四種不同的type declration
	//VARIABLE_DECL  TYPE_DECL   FUNCTION_DECL  FUNCTION_PARAMETER_DECL
	switch (declarationNode->semantic_value.declSemanticValue.kind) {
		case(VARIABLE_DECL):
			declareIdList(declarationNode, VARIABLE_ATTRIBUTE, 0); //TODO 最後一個參數不知道要傳什麼
			break;
		case(TYPE_DECL):
			declareIdList(declarationNode, TYPE_ATTRIBUTE, 0); //TODO 最後一個參數不知道要傳什麼
			break;
		case(FUNCTION_DECL):
			declareFunction(declarationNode);
			break;
		case(FUNCTION_PARAMETER_DECL):
			//parameter list (point to type descriptor in symboltable)
			break;
		default:
			printf("Error: 在processDeclarationNode發現傳入未知的DeclNode");
	}
}


void processTypeNode(AST_NODE* idNodeAsType)
{
	printf("[In processTypeNode]\n");
	//負責檢查該Type(其實是一個Id node)是否有宣告過，如果沒有的話噴錯
	//如果是int, float, void或是有宣告過的話，那就幫他設定DataType
	char* typeName = idNodeAsType->semantic_value.identifierSemanticValue.identifierName;
	//printf("typeName:%s\n", typeName);
	
	if (!strcmp(typeName, "int")) {
		idNodeAsType->dataType = INT_TYPE;
	} else if (!strcmp(typeName, "float")) {
		idNodeAsType->dataType = FLOAT_TYPE;
	} else if (!strcmp(typeName, "void")) {
		idNodeAsType->dataType = VOID_TYPE;
	} else {
		SymbolTableEntry* typeEntryRetrieved = retrieveSymbol(typeName);
		if (typeEntryRetrieved == NULL) {
			printErrorMsg(idNodeAsType, SYMBOL_UNDECLARED); //發現自定義的type不存在，所以噴錯
		} else {
			idNodeAsType->dataType = typeEntryRetrieved->attribute->attr.typeDescriptor->properties.dataType;
		}
	}
}


void declareIdList(AST_NODE* declarationNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize)
{
	printf("[In declareIdlList]\n");
	//傳入的node大概會長這樣子
	//decl_node->int
	//         ->id (可能是NORMAL_ID)
	//         ->id (或是ARRAY_ID)
	//         ->id (或是有expression的WITH_INIT_ID)
	AST_NODE* declareTypeNode = declarationNode->child;
	AST_NODE* declareIdNode = declareTypeNode->rightSibling;

	//get type name, typeName may be "int", "float", "AAA"
	char* typeName = declareTypeNode->semantic_value.identifierSemanticValue.identifierName;
	SymbolAttribute* symbolAttr = (SymbolAttribute*)malloc(sizeof(SymbolAttribute));
	switch (isVariableOrTypeAttribute) {
		case(VARIABLE_ATTRIBUTE):
			//int a, b, c;這類的declartion.
			//A a, b, c;這類的declartion
			symbolAttr->attributeKind = VARIABLE_ATTRIBUTE;
			break;
		case(TYPE_ATTRIBUTE):
			//typedef int A B C[3];  這類的declaration.
			//typedef void A B C;
			symbolAttr->attributeKind = TYPE_ATTRIBUTE;
			break;
		default:
			printf("Error: 未知的SymboalAttributeKind\n");
	}

	//確認type已經為int, float. void或是已經宣告過
	processTypeNode(declareTypeNode);

	symbolAttr->attr.typeDescriptor = (TypeDescriptor*)malloc(sizeof(TypeDescriptor));
	while(declareIdNode != NULL){
		switch (declareIdNode->semantic_value.identifierSemanticValue.kind) {
			case(NORMAL_ID):
				symbolAttr->attr.typeDescriptor->kind = SCALAR_TYPE_DESCRIPTOR;
				symbolAttr->attr.typeDescriptor->properties.dataType = declarationNode->leftmostSibling->dataType;
				break;
			case(ARRAY_ID):
				processDeclDimList(declareIdNode, symbolAttr->attr.typeDescriptor, 0); //TODO: 我也不知道為什麼最後一個參數要填0
				break;
			case(WITH_INIT_ID):
				symbolAttr->attr.typeDescriptor->kind = SCALAR_TYPE_DESCRIPTOR;
				symbolAttr->attr.typeDescriptor->properties.dataType = declarationNode->leftmostSibling->dataType;
				processExprRelatedNode(declareIdNode->child);
				break;
			default:
				printf("Error: 無法判斷的declartionNode semantic type\n");
		} 
		//接下來要插入Entry了
		//先檢查有沒有同名的重複宣告
		SymbolTableEntry* entryRetrieved = retrieveSymbol(declareIdNode->semantic_value.identifierSemanticValue.identifierName);
		char* declaredName = declareIdNode->semantic_value.identifierSemanticValue.identifierName;
		if (entryRetrieved == NULL) {
			enterSymbol(declaredName, symbolAttr);
		} else {
			//SymbolTableEntry* sameScopeEntry = symbolTable.scopeDisplay[symbolTable.scopeDisplayElementCount];
			SymbolTableEntry* sameScopeEntry = symbolTable.scopeDisplay[currentScopeDisplay];
			while(sameScopeEntry != NULL) {
				if (!strcmp(sameScopeEntry->name, declaredName)) {
					printErrorMsg(declareIdNode, SYMBOL_REDECLARE);
					return;
				}
				sameScopeEntry = sameScopeEntry->nextInSameLevel;
			}
			enterSymbol(declaredName, symbolAttr);
		}

		//處理下一個ID node
		declareIdNode = declareIdNode->rightSibling;
	}
}

void checkAssignOrExpr(AST_NODE* assignOrExprRelatedNode)
{
	printf("[In checkAssignOrExpr]\n");
	//if assign ...call blabalbal(call checkAssignmentStmt)
	//if rela ... cla blablabla(call processExprRelatedNode)
	//call processExprNode
	if(assignOrExprRelatedNode->nodeType == STMT_NODE){
		//是assign_stmt
		//處理assign下面的ID node和relop Node
		checkAssignmentStmt(assignOrExprRelatedNode);
	} else {
		//是expression(可能是exprNode或const等)
		processExprRelatedNode(assignOrExprRelatedNode);
	}
}


void checkWhileStmt(AST_NODE* whileNode)
{
	printf("[In checkWhileStmt]\n");
	//printASTNodeInfo(whileNode);
	//deal with type WHILE_STMT as a stmt node
	//call the children of whileNode(processStmtNode and all processExprRelatedNode)
	AST_NODE* testNode = whileNode->child;
	//printf("II\n");
	AST_NODE* stmtNode = testNode->rightSibling;
	//printf("GG\n");
	checkAssignOrExpr(testNode);
	processStmtNode(stmtNode);
}


void checkForStmt(AST_NODE* forNode)
{
	printf("[In checkForStmt]\n");
	//deal with type FOR_STMT as a stmt node
	//call the children of forNode
	//including some checkAssignmentStmt 
	//along with NORMAL_ID check if necessary
	//and all processExprRelatedNode
	AST_NODE* firstPart = forNode->child;
	AST_NODE* secondPart = firstPart->rightSibling;
	AST_NODE* thirdPart = secondPart->rightSibling;
	AST_NODE* blockPart = thirdPart->rightSibling;
	switch (firstPart->nodeType) {
		case(NONEMPTY_ASSIGN_EXPR_LIST_NODE):
			{
				AST_NODE* firstPartChild = firstPart->child;
				while(firstPartChild != NULL){
					checkAssignmentStmt(firstPartChild);
					firstPartChild = firstPartChild->rightSibling;
				}
			}
			break;
		case(NUL_NODE):
			break;
		default:
			printf("Error: forNode part1部份出現無預期的節點");
	}
	switch (secondPart->nodeType) {
		case(NONEMPTY_RELOP_EXPR_LIST_NODE):
			{
				AST_NODE* secondPartChild = secondPart->child;
				while(secondPartChild != NULL) {
					processExprRelatedNode(secondPartChild); //TODO: 不確定對不對
					secondPartChild = secondPartChild->rightSibling;
				}
			}
			break;
		case(NUL_NODE):
			break;
		default:
			printf("Error: forNode part2部份出現無預期的節點");
	}
	switch (thirdPart->nodeType) {
		case(NONEMPTY_ASSIGN_EXPR_LIST_NODE):
			{
				AST_NODE* thirdPartChild = thirdPart->child;
				while(thirdPartChild != NULL){
					checkAssignmentStmt(thirdPartChild);
					thirdPartChild = thirdPartChild->rightSibling;
				}
			}
			break;
		case(NUL_NODE):
			break;
		default:
			printf("Error: forNode part3部份出現無預期的節點");
	}
	processBlockNode(blockPart);
}


void checkAssignmentStmt(AST_NODE* assignmentNode)
{
	printf("[In checkAssignmentStmt]\n");
	//insert table entry for ASSIGN_STMT node
	//該function只處理 a=3 之類的stmt, 不處理int a=3
	AST_NODE* leftIDNode = assignmentNode->child;
	AST_NODE* rightRelopExprNode = leftIDNode->rightSibling;
	//1. 要先檢查左值是否宣告過
	processVariableLValue(leftIDNode);
	//2. 要檢查出現在右邊的是否都宣告過
	processExprRelatedNode(rightRelopExprNode);
	
	//3. 要檢查左右型別是否相符(這次作業是否必要？應該是沒必要)
	//TODO

}


void checkIfStmt(AST_NODE* ifNode)
{
	printf("[In checkIfStmt]\n");
	//deal with type IF_STMT as a stmt node
	//call the children same as while statement
	AST_NODE* testNode = ifNode->child;
	AST_NODE* stmtNode = testNode->rightSibling;
	AST_NODE* elseIfNode = stmtNode->rightSibling;
	checkAssignOrExpr(testNode);
	processStmtNode(stmtNode);
	switch(elseIfNode->nodeType) {
		case NUL_NODE:
			//沒有else if
			break;
		case STMT_NODE:
			//else if stm
			checkIfStmt(elseIfNode);
			break;
		case BLOCK_NODE:
			processBlockNode(elseIfNode);
			break;
		default:
			printf("Error:無法identify的else if Node type\n");
	}
	
}

void checkWriteFunction(AST_NODE* functionCallNode)
{
	printf("[In checkWriteFunction]\n");
	AST_NODE* functionNameNode = functionCallNode->child;
	AST_NODE* callParaListNode = functionNameNode->rightSibling;
	AST_NODE* paraNode;
	if (callParaListNode->nodeType == NUL_NODE) {
		paraNode = NULL;
	} else {
		paraNode = callParaListNode->child;
	}

	if (paraNode == NULL) {
		printErrorMsg(functionNameNode, TOO_FEW_ARGUMENTS);
	} else if (paraNode->rightSibling != NULL) {
		printErrorMsg(functionNameNode, TOO_MANY_ARGUMENTS);
	} else if (paraNode->nodeType != CONST_VALUE_NODE) {
		printErrorMsg(functionNameNode, PARAMETER_TYPE_UNMATCH);
	} else if (paraNode->semantic_value.const1->const_type != STRINGC) {
		printErrorMsg(functionNameNode, PARAMETER_TYPE_UNMATCH);
	}

}

void checkFunctionCall(AST_NODE* functionCallNode)
{
	printf("[In checkFunctionCall]\n");
	//function_call_node -> function_name(id_node)
	//              -> NUL_NODE or NONEMPTY_RELOP_EXPR_LIST_NODE
	//檢查function name是否存在於symbol_table
	AST_NODE* functionNameNode = functionCallNode->child;
	char* functionName = functionNameNode->semantic_value.identifierSemanticValue.identifierName;
	if (!strcmp(functionName, "write")) {
		checkWriteFunction(functionCallNode);	
	} else {
		SymbolTableEntry* entry = retrieveSymbol(functionName);
		if ( entry == NULL) {
			printErrorMsg(functionNameNode, SYMBOL_UNDECLARED);
		} else {
			//檢查function參數, 檢查數量是否符合, type是否match, scalar和array的問題
			Parameter* para = entry->attribute->attr.functionSignature->parameterList;
			AST_NODE* callParaListNode = functionNameNode->rightSibling;
			AST_NODE* paraNode;
			if (callParaListNode->nodeType == NUL_NODE) {
				paraNode = NULL;
			} else {
				paraNode = callParaListNode->child;
			}
			
			while(para != NULL && paraNode != NULL) {
				checkParameterPassing(para, paraNode);

				para = para->next;
				paraNode = paraNode->rightSibling;
			}

			if (para == NULL && paraNode != NULL) {
				printErrorMsg(functionNameNode, TOO_MANY_ARGUMENTS);
			} else if (para != NULL && paraNode == NULL) {
				printErrorMsg(functionNameNode, TOO_FEW_ARGUMENTS);
			}

		}
	}
}

void checkParameterPassing(Parameter* formalParameter, AST_NODE* actualParameter)
{
	printf("[In checkParameterPassing]\n");
	//該函數只負責檢查宣告參數和傳進來的參數是否符合, 不負責檢查下一個參數是否符合
	switch (formalParameter->type->kind){
		case(SCALAR_TYPE_DESCRIPTOR):
			switch(actualParameter->nodeType){
				case(IDENTIFIER_NODE):
					//檢查是否為array, 若是，則再度確認是否為0維array(scalar)
					// TODO More test data required
					if(actualParameter->semantic_value.identifierSemanticValue.kind == ARRAY_ID) {
						int childNumberOfActualPara = 0;
						AST_NODE* actualParaChild = actualParameter->child;
						while(actualParaChild != NULL){
							childNumberOfActualPara += 1;
							actualParaChild = actualParaChild->rightSibling;
						}
						
						SymbolTableEntry* entry = retrieveSymbol(actualParameter->semantic_value.identifierSemanticValue.identifierName);
						if (entry == NULL) {
							//如果傳進去的id不存在，直接噴錯
							printErrorMsg(actualParameter, SYMBOL_UNDECLARED);
							return;
						}
						int actualParameterOriginalDimensions = entry->attribute->attr.typeDescriptor->properties.arrayProperties.dimension;
						int dimensionsOfActualPara = actualParameterOriginalDimensions - childNumberOfActualPara;
						if (dimensionsOfActualPara != 0) {
								//如果傳入的id的維度不是0, 代表不是scalar
							printErrorMsgSpecial(actualParameter, formalParameter->parameterName, PASS_ARRAY_TO_SCALAR);
						}
					}
					break;
				case (CONST_VALUE_NODE):
				case (EXPR_NODE):
				default:
					printf("預期參數為scalar, 收到參數包含了expression或const, 視為scalar\n");
			}
			//actualParameter可能會是一個expression,或是array之類的....所以還不會寫
			break;
		case(ARRAY_TYPE_DESCRIPTOR):
			switch(actualParameter->nodeType){
				case (IDENTIFIER_NODE):
					{	//check table first
						SymbolTableEntry* entry = retrieveSymbol(actualParameter->semantic_value.identifierSemanticValue.identifierName);
						if (entry == NULL) {
							//如果傳進去的id不存在，直接噴錯
							printErrorMsg(actualParameter, SYMBOL_UNDECLARED);
							return;
						}

						//check array dimension
						int expectedDimensions = formalParameter->type->properties.arrayProperties.dimension;				
						int childNumberOfActualPara = 0;
						AST_NODE* actualParaChild = actualParameter->child;
						while(actualParaChild != NULL){
							childNumberOfActualPara += 1;
							actualParaChild = actualParaChild->rightSibling;
						}
						
						int actualParameterOriginalDimensions = entry->attribute->attr.typeDescriptor->properties.arrayProperties.dimension;
						int dimensionsOfActualPara = actualParameterOriginalDimensions - childNumberOfActualPara;
						if (dimensionsOfActualPara != expectedDimensions) {
							if (dimensionsOfActualPara == 0) {
								//如果傳入的id的維度是0, 代表是scalar
								printErrorMsgSpecial(actualParameter, formalParameter->parameterName, PASS_SCALAR_TO_ARRAY);
							} else {
								printErrorMsgSpecial(actualParameter, formalParameter->parameterName, INCOMPATIBLE_ARRAY_DIMENSION);
							}
						}
					}
					break;
				case (CONST_VALUE_NODE):
				case (EXPR_NODE):
					printf("預期參數為Array, 收到參數包含了expression或const, 不處理\n");
					break;
			}
			break;
		default:
			printf("Error: checkParameterPassing Function出現無法判斷的formalParameter kind type\n");
	} 
}


void processExprRelatedNode(AST_NODE* exprRelatedNode)
{
	printf("[In processExprRelatedNode]\n");
	//收到的node其實不一定是expr, 有可能是expr, const, 或id node
	switch (exprRelatedNode->nodeType) {
		case EXPR_NODE:
			switch (exprRelatedNode->semantic_value.exprSemanticValue.kind) {
				case(BINARY_OPERATION):
					switch(exprRelatedNode->semantic_value.exprSemanticValue.op.binaryOp) {
						case BINARY_OP_AND:  
						case BINARY_OP_OR:
						case BINARY_OP_EQ:
						case BINARY_OP_GE:  
						case BINARY_OP_GT:
						case BINARY_OP_LT:
						case BINARY_OP_LE:  
						case BINARY_OP_NE:
							{
								AST_NODE* leftChild = exprRelatedNode->child;
								AST_NODE* rightChild = leftChild->rightSibling;
								processExprRelatedNode(leftChild);
								processExprRelatedNode(rightChild);
							}
							break;
						case BINARY_OP_ADD:
						case BINARY_OP_SUB:
						case BINARY_OP_MUL:
						case BINARY_OP_DIV:
							{
								AST_NODE* leftChild = exprRelatedNode->child;
								AST_NODE* rightChild = leftChild->rightSibling;
								processExprNode(leftChild);
								processExprNode(rightChild);
							}
							break;
						default:
							printf("Error: exprRelatedNodeChild type error\n");
					}
					break;
				case(UNARY_OPERATION):
					{
						AST_NODE* child = exprRelatedNode->child;
						processExprRelatedNode(child);
					}
					break;
				default:
					printf("Error: processRxprRelatedNode function出現無法判斷binary或unary的Expr Node\n");
			}
			break;
		case IDENTIFIER_NODE:
		case CONST_VALUE_NODE:
			processExprNode(exprRelatedNode);
			break;
		case STMT_NODE:
			if (exprRelatedNode->semantic_value.stmtSemanticValue.kind == FUNCTION_CALL_STMT) {
				checkFunctionCall(exprRelatedNode);
			} else {
				printf("Error: 無法判斷的ExprRelatedNode STMT Type\n");
			}
			break;
		default:
			printf("Error: 無法判斷的ExprRelatedNode Type\n");
	}
}

void getExprOrConstValue(AST_NODE* exprOrConstNode, int* iValue, float* fValue)
{
	printf("[In getExprOrConstValue]\n");
	switch (exprOrConstNode->nodeType) {
		case(EXPR_NODE):
			switch (exprOrConstNode->semantic_value.exprSemanticValue.kind) {
				case(BINARY_OPERATION):
					switch(exprOrConstNode->semantic_value.exprSemanticValue.op.binaryOp){
						case BINARY_OP_AND:  
						case BINARY_OP_OR:
						case BINARY_OP_EQ:
						case BINARY_OP_GE:  
						case BINARY_OP_GT:
						case BINARY_OP_LT:
						case BINARY_OP_LE:  
						case BINARY_OP_NE:
							*iValue = 1;
							*fValue = 0.0;
							break;
						case BINARY_OP_ADD:
						case BINARY_OP_SUB:
						case BINARY_OP_MUL:
						case BINARY_OP_DIV:
							{
								AST_NODE* leftChild = exprOrConstNode->child;
								AST_NODE* rightChild = leftChild->rightSibling;
								getExprOrConstValue(leftChild, iValue, fValue);
								if (*iValue == 1) {  //代表是iValue, 要檢查另一邊是不是float
									getExprOrConstValue(rightChild, iValue, fValue);
									if (*iValue == 1) {
										return;
									} else {
										*iValue = 0;
										*fValue = 1.0;
									}
								} else {            //代表是float, 可以直接return
									return;
								}
							}
							break;
						default:
							printf("Error: 出現無法預期的binary operator\n");
					}
					break;
				case(UNARY_OPERATION):
					getExprOrConstValue(exprOrConstNode->child, iValue, fValue);
					break;
				default:
					printf("Error: getExprOrConstValue的expr node出現無法預期的op\n");
			}
			break;
		//TODO: 涵蓋functinocall, idnode
		case(CONST_VALUE_NODE):
			switch(exprOrConstNode->semantic_value.const1->const_type){
				case(INTEGERC):
					*iValue = 1;
					*fValue = 0.0;
					break;
				case(FLOATC):
					*iValue = 0;
					*fValue = 1.0;
					break;
				case(STRINGC):
					printf("Error: 請勿在expression中使用String Value");
					break;
				default:
					printf("Error: unknown const_value_node const type");
			} 
			break;
		case(IDENTIFIER_NODE):
			//for id
			//應該要在processExpr中處理好判斷型別的問題，這裡直接看dataType
			//TODO

			break;
		case(STMT_NODE):
			//for function call
			//TODO!!!!!!!!!!!!!!!
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
	printf("[In processExprNode]\n");
	//TODO 處理可能出現rel_expr node的問題
	switch(exprNode->nodeType){
		case EXPR_NODE:
			switch (exprNode->semantic_value.exprSemanticValue.kind) {
				case(BINARY_OPERATION):
					switch(exprNode->semantic_value.exprSemanticValue.op.binaryOp) {
						case BINARY_OP_ADD:
						case BINARY_OP_SUB:
						case BINARY_OP_MUL:
						case BINARY_OP_DIV:
							{
								AST_NODE* leftChild = exprNode->child;
								AST_NODE* rightChild = leftChild->rightSibling;
								processExprNode(leftChild);
								processExprNode(rightChild);
							}
							break;
						default:
							printf("Error: 無法判斷的Binary Operation\n");
					}
					break;
				case(UNARY_OPERATION):
					{
						AST_NODE* child = exprNode->child;
						processExprNode(child);
					}
					break;
				default:
					printf("Error: exprRelatedNode type error\n");
			}
			break;	
		case CONST_VALUE_NODE:
			processConstValueNode(exprNode);
			break;
		case IDENTIFIER_NODE:
			processVariableRValue(exprNode);
			break;
		case STMT_NODE:
			if (exprNode->semantic_value.stmtSemanticValue.kind == FUNCTION_CALL_STMT) {
				checkFunctionCall(exprNode);
			} else {
				printf("Error: 無法判斷的ExprRelatedNode STMT Type\n");
			}
			break;
		default:
		   printf("Error: 出現無法判斷的ExprNode的type\n");
	}
}


void processVariableLValue(AST_NODE* idNode)
{
	printf("[In processVariableLValue]\n");
	SymbolTableEntry* entryRetrieved = retrieveSymbol(idNode->semantic_value.identifierSemanticValue.identifierName);
	if (entryRetrieved == NULL) {
		printErrorMsg(idNode, SYMBOL_UNDECLARED);
	} else {
		//沒有做check L value 和 R value 的 type	
	}
}

void processVariableRValue(AST_NODE* idNode)
{
	printf("[In processVariableRValue]\n");
	SymbolTableEntry* entryRetrieved = retrieveSymbol(idNode->semantic_value.identifierSemanticValue.identifierName);
	if (entryRetrieved == NULL) {
		printErrorMsg(idNode, SYMBOL_UNDECLARED);
	} else {
		switch(idNode->semantic_value.identifierSemanticValue.kind){
			case(ARRAY_ID):
				{
					int i;
					float f;
					AST_NODE* idNodeChild = idNode->child; 
					while(idNodeChild != NULL){
						i = -1;
						f = -1.0;
						getExprOrConstValue(idNodeChild, &i, &f);
						if (i != 1) {
							printErrorMsg(idNode, ARRAY_SUBSCRIPT_NOT_INT);
						}
	
						idNodeChild = idNodeChild->rightSibling;
					}
				}
				//TODO 幫array type的AST_NODE加上dataType, 可能是INT, FLOAT或是PTR(但是PTR不知道要怎麼處理)
			case(NORMAL_ID):
				//假設會搜尋到id node entry, 所以幫AST tree根據symboltable填上datatype
				idNode->dataType = entryRetrieved->attribute->attr.typeDescriptor->properties.dataType;
				break;
			default:
				printf("Error:遇到無法判斷的RValue的id 的id kind(array or scalar)\n");
		}
	}
}


void processConstValueNode(AST_NODE* constValueNode)
{
	printf("[In processConstValueNode]\n");
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
	printf("[In checkReturnStmt]\n");
	AST_NODE* returnNodeChild = returnNode->child;
	AST_NODE* returnNodeParent = returnNode->parent;
	while(returnNodeParent->semantic_value.declSemanticValue.kind != FUNCTION_DECL){
		returnNodeParent = returnNodeParent->parent;
	}
	char* returnTypeID = returnNodeParent->child->semantic_value.identifierSemanticValue.identifierName;
	if (returnNodeChild->nodeType == NUL_NODE) {
		if (strcmp(returnTypeID, "void")) {
			printErrorMsg(returnNode, RETURN_TYPE_UNMATCH);
		}
	} else {
		//確認expression的變數或function call宣告正確性
		processExprRelatedNode(returnNodeChild);

		//確認return type是否match
		int i = -1;
		float f = -1;
		getExprOrConstValue(returnNodeChild, &i, &f);
		//如果i, f都沒有變過，代表出現無法判斷的type(使用了沒有宣告過的變數)
		if (i == -1) {
			printf("Error: 無法判斷return expression的type");
		} else {
			if (i == 0 && f > 0.5) {
				if (strcmp(returnTypeID, "float")){
					printErrorMsg(returnNode, RETURN_TYPE_UNMATCH);
				}
			} else {
				if (strcmp(returnTypeID, "int")) {
					printErrorMsg(returnNode, RETURN_TYPE_UNMATCH);
				}
			}
		}

	}

}


void processBlockNode(AST_NODE* blockNode)
{
	printf("[In processBlockNode]\n");
	
	//處理
	//可能有decl_list node 和 stmt_list node
	//stmt_list_node
	//發現底下有stmt_list node，就依序call processStmtNode()
	//發現底下有decl_list node，就依序call
	openScope(); 
	AST_NODE* blockNodeChild = blockNode->child;
	//printASTNodeInfo(blockNodeChild);
	while(blockNodeChild != NULL) {
		//處理該child
		switch(blockNodeChild->nodeType) {
			case(VARIABLE_DECL_LIST_NODE):
				//TODO, call function to handle variable_decl_list_node				
				{
					AST_NODE* variableDeclListNodeChlid = blockNodeChild->child;
					while(variableDeclListNodeChlid != NULL) {
						processDeclarationNode(variableDeclListNodeChlid);
						variableDeclListNodeChlid = variableDeclListNodeChlid->rightSibling;	
					}
				}
				break;
			case(STMT_LIST_NODE):
				{
					AST_NODE* stmtListNodeChild = blockNodeChild->child;
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
								printf("Error: 無法判斷的STMT_LIST_NODE之子節點\n");	
						}
						stmtListNodeChild = stmtListNodeChild->rightSibling;
					}
				}
				break;
			default:
				printf("Error: 無法判斷的blockNode之子節點\n");
		}
		//換到右邊的sibling繼續處理
		blockNodeChild = blockNodeChild->rightSibling;
	}
	closeScope();
}


void processStmtNode(AST_NODE* stmtNode)
{
	printf("[In processStmtNode]\n");
	//printASTNodeInfo(stmtNode);
	//看是哪種stmt node
	//while/for/assign_stmt/if_stmt/function_call_stmt/return_stmt
	if (stmtNode->nodeType == BLOCK_NODE) {
		processBlockNode(stmtNode);
	} else {
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
}


void processGeneralNode(AST_NODE *node)
{
	printf("[In processGeneralNode]\n");
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
	printf("[In processDeclDimList]\n");
	//該function負責專心專注於
	typeDescriptor->kind = ARRAY_TYPE_DESCRIPTOR;
	AST_NODE* idNodeChild = idNode->child;
	int currentDimensionIndex = 0;
	int i;
	float f;
	typeDescriptor->properties.arrayProperties.dimension = 0;
	while(idNodeChild != NULL) {
		typeDescriptor->properties.arrayProperties.dimension += 1;
		typeDescriptor->properties.arrayProperties.sizeInEachDimension[currentDimensionIndex] = 0; //故意填0, 因為可能是expr無法算出
		i = -1;
		f = -1.0;
		//判斷a[expr] 中的expr是否會回傳int
		getExprOrConstValue(idNodeChild, &i, &f);
		if (i != 1) {
			printErrorMsg(idNode, ARRAY_SUBSCRIPT_NOT_INT);
		}

		idNodeChild = idNodeChild->rightSibling;
		currentDimensionIndex += 1;
	}
	typeDescriptor->properties.arrayProperties.elementType = idNode->leftmostSibling->dataType;
}


void declareFunction(AST_NODE* declarationNode)
{
	printf("[In declareFunction]\n");
	AST_NODE* returnTypeNode = declarationNode->child;
	AST_NODE* funcNameNode = returnTypeNode->rightSibling;
	AST_NODE* paraListNode = funcNameNode->rightSibling;
	AST_NODE* blockNode = paraListNode->rightSibling;

	//確認typeNode是否有宣告過
	processTypeNode(returnTypeNode);	
	
	//funcNameNode redefined check
	char* funcName = funcNameNode->semantic_value.identifierSemanticValue.identifierName;
 	SymbolTableEntry* entryRetrieved = retrieveSymbol(funcName);
	if (entryRetrieved != NULL) {
		printErrorMsg(funcNameNode, SYMBOL_REDECLARE);
	}


	// Declare function
	SymbolAttribute* attribute = (SymbolAttribute*)malloc(sizeof(SymbolAttribute));
	attribute->attributeKind = FUNCTION_SIGNATURE;
	attribute->attr.functionSignature = (FunctionSignature*)malloc(sizeof(FunctionSignature));
	
	//設定returnType
	attribute->attr.functionSignature->returnType = returnTypeNode->dataType;
	
	//設定參數
	AST_NODE* funcParaDeclNode = paraListNode->child;

	attribute->attr.functionSignature->parameterList = NULL;
	Parameter** param = &(attribute->attr.functionSignature->parameterList);

	while(funcParaDeclNode!=NULL){
		AST_NODE* paraTypeNode = funcParaDeclNode->child;
		AST_NODE* paraIdNode = paraTypeNode->rightSibling;
		
		//先檢查type是否有宣告過
		processTypeNode(paraTypeNode);

		//之後根據參數進行設定.....
		*param = (Parameter*)malloc(sizeof(Parameter));
		(*param)->parameterName = paraIdNode->semantic_value.identifierSemanticValue.identifierName;
		(*param)->type = (TypeDescriptor*)malloc(sizeof(TypeDescriptor));
		(*param)->next = NULL;
		switch(paraIdNode->semantic_value.identifierSemanticValue.kind){
			case NORMAL_ID:
				(*param)->type->kind = SCALAR_TYPE_DESCRIPTOR;
				(*param)->type->properties.dataType = paraTypeNode->dataType;
				break;
			case ARRAY_ID:
				(*param)->type->kind = ARRAY_TYPE_DESCRIPTOR;
				processDeclDimList(paraIdNode, (*param)->type, 0);
				break;
			default:
				printf("Error: 宣告function的參數出現無法判斷的id kind");
		}

		param = &((*param)->next);
		funcParaDeclNode = funcParaDeclNode->rightSibling;
	}
	
	//處理block區塊
	enterSymbol(funcName, attribute);
	processBlockNode(blockNode);

	//printSymbolTable();
}
