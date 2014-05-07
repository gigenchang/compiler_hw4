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

void printErrorMsgSpecial(AST_NODE* node1, char* name2, ErrorMsgKind errorMsgKind)
{
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node1->linenumber);
    switch(errorMsgKind)
    {
		case(PASS_ARRAY_TO_SCALAR):
			printf("Array %s passed to scalar parameter %s.", node1->semantic_value.identifierSemanticValue.identifierName, name2);
			break;
		case(PASS_SCALAR_TO_ARRAY):
			printf("Scalar %s passed to array parameter %s.", nore1->semantic_value.identifierSemanticValue.identifierName, name2);
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
			printf("ID %s undeclared.", node->semantic_value.identifierSemanticValue.identifierName);
			break;
		case(SYMBOL_REDECLARE):
			printf("ID %s redeclared.", node->semantic_value.identifierSemanticValue.identifierName);
			break;
		case(TOO_FEW_ARGUMENTS):
			printf("too few arguments to function %s.", node->semantic_value.identifierSemanticValue.identifierName); //要傳functionStmt下的ID node進去
			break;
		case(TOO_MANY_ARGUMENTS):
			printf("too many arguments to function %s.", node->semantic_value.identifierSemanticValue.identifierName); //要傳functionStmt下的ID node進去
			break;
		case(RETURN_TYPE_UNMATCH):
			printf("Incompatible return type.");
			break;
		case(INCOMPATIBLE_ARRAY_DIMENSION):
			printf("Incompatible array dimensions.");
			break;
		case(ARRAY_SUBSCRIPT_NOT_INT):
			printf("Array subscript is not an integer.");
			break;
		default:
			printf("Unhandled case in void printErrorMsg(AST_NODE* node, ERROR_MSG_KIND* errorMsgKind)\n");
			break;
    }
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
	SymbolAttribute* symbolAttr = (SymbolAttribute*)malloc(sizeof(SymbolAttribute));
	switch (isVariableOrTypeAttribute) {
		case(VARIABLE_ATTRIBUTE):
			symbolAttr->attributeKind = VARIABLE_ATTRIBUTE;
			symbolAttr->attr.typeDescriptor = (TypeDescriptor*)malloc(sizeof(TypeDescriptor));
			switch (declarationNode->semantic_value.identifierSemanticValue.kind) {
				case(NORMAL_ID):
					symbolAttr->attr.typeDescriptor->kind = SCALAR_TYPE_DESCRIPTOR;
					symbolAttr->attr.typeDescriptor->properties.dataType = declartionNode->leftmostSibling->dataType;
					break;
				case(ARRAY_ID):
					symbolAttr->attr.typeDescriptor->kind = ARRAY_TYPE_DESCRIPTOR;
					AST_NODE* declNodechild = declartionNode->child;
					int currentDimensionIndex = 0, i;
					float f;
					while(declNodechild != NULL) {
						symbolAttr->attr.typeDescriptor->properties.arrayProperties.dimension += 1;
						symbolAttr->attr.typeDescriptor->properties.arrayProperties.sizeInEachDimension[currentDimensionIndex] = 0; //故意填0, 因為可能是expr無法算出
						i = -1;
						f = -1.0;
						getExprOrConstValue(declNodeChild, &i, &f);
						if (i != 1) {
							printErrorMsg(idNode, ARRAY_SUBSCRIPT_NOT_INT);
						}

						declNodechild = declNodechild->rightSibling;
						currentDimensionIndex += 1;
					}
					symbolAttr->attr.typeDescriptor->properties.arrayProperties.elementType = declartionNode->leftmostSibling->dataType;
					break;
				default:
					printf("Error: 無法判斷的declartionNode semantic type\n");
			} 
			break;
		case(TYPE_ATTRIBUTE):
			symbolAttr->attributeKind = TYPE_ATTRIBUTE;
			//TODO
			printf("尚未實作typedef\n");
			break;
		default:
			printf("Error: 未知的declarationAttr\n");
	}
	SymbolTableEntry* entryRetrieved = retrieveSymbol(declartionNode->semantic_value.identifierSemanticValue.identifierName);
	char* declaredName = declartionNode->semantic_value.identifierSemanticValue.identifierName;
	if (entryRetrieved == NULL) {
		enterSymbol(declaredName, symbolAttr);
	} else {
		SymbolTableEntry* sameScopeEntry = SymbolTable.scopeDisplay[SymbolTable.scopeDisplayElementCount];
		while(sameScopeEntry != NULL) {
			if (sameScopeEntry->name == declaredName) {
				printErrorMsg(declartionNode, SYMBOL_REDECLARE);
				return;
			}
			sameScopeEntry = sameScopeEntry->nextInSameLevel;
		}
		enterSymbol(declaredName, symbolAttr);
	}
	
}

void checkAssignOrExpr(AST_NODE* assignOrExprRelatedNode)
{
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
	//deal with type WHILE_STMT as a stmt node
	//call the children of whileNode(processStmtNode and all processExprRelatedNode)
	AST_NODE* testNode = whileNode->child->leftmostSibling;
	AST_NODE* stmtNode = testNode->rightSibling;
	checkAssignOrExpr(testNode);
	processStmtNode(stmtNode);
}


void checkForStmt(AST_NODE* forNode)
{
	//deal with type FOR_STMT as a stmt node
	//call the children of forNode
	//including some checkAssignmentStmt 
	//along with NORMAL_ID check if necessary
	//and all processExprRelatedNode
	AST_NODE* firstPart = forNode->child->leftmostSibling;
	AST_NODE* secondPart = firstPart->rightSibling;
	AST_NODE* thirdPart = secondPart->rightSibling;
	switch (firstPart->nodeType) {
		case(NONEMPTY_ASSIGN_EXPR_LIST_NODE):
			AST_NODE* firstPartChild = firstPart->child;
			while(firstPartChild != NULL){
				checkAssignmentStmt(firstPartChild);
				firstPartChild = firstPartChild->rightSibling;
			}
			break;
		case(NUL_NODE):
			break;
		default:
			printf("Error: forNode part1部份出現無預期的節點");
	}
	switch (secondPart->nodeType) {
		case(NONEMPTY_RELOP_EXPR_LIST_NODE):
			AST_NODE* secondPartChild = secondPart->child;
			while(secondPartChild != NULL) {
				processExprRelatedNode(secondPartChild); //TODO: 不確定對不對
				secondPartChild = secondPartChild->rightSibling;
			}
			break;
		case(NUL_NODE):
			break;
		default:
			printf("Error: forNode part2部份出現無預期的節點");
	}
	switch (thirdPart->nodeType) {
		case(NONEMPTY_ASSIGN_EXPR_LIST_NODE):
			AST_NODE* thirdPartChild = thirdPart->child;
			while(thirdPartChild != NULL){
				checkAssignmentStmt(thirdPartChild);
				thirdPartChild = thirdPartChild->rightSibling;
			}
			break;
		case(NUL_NODE):
			break;
		default:
			printf("Error: forNode part3部份出現無預期的節點");
	}
}


void checkAssignmentStmt(AST_NODE* assignmentNode)
{
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
	//deal with type IF_STMT as a stmt node
	//call the children same as while statement
	AST_NODE* testNode = ifNode->child;
	AST_NODE* stmtNode = testNode->rightSibling;
	checkAssignOrExpr(testNode);
	processStmtNode(stmtNode);
}

void checkWriteFunction(AST_NODE* functionCallNode)
{
}

void checkFunctionCall(AST_NODE* functionCallNode)
{
	//function_call_node -> function_name(id_node)
	//              -> NUL_NODE or NONEMPTY_RELOP_EXPR_LIST_NODE
	//檢查function name是否存在於symbol_table
	functionNameNode = functionCallNode->child;
	char* functionName = functionNameNode->semantic_value.identifierSemanticValue.identifierName;
	SymbolTableEntry* entry = retrieveSymbol(functionName);
	if ( entry == NULL) {
		printErrorMsg(functionNameNode, SYMBOL_UNDECLARED);
	} else {
		//檢查function參數, 檢查數量是否符合, type是否match, scalar和array的問題
		Parameter* para = entry->attribute->attr.functionSignature->parameterList;
		AST_NODE* callParaListNode = functionNameNode->rightSibling;
		AST_NODE* paraNode;
		if (callParaListNode.nodeType == NUL_NODE) {
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

void checkParameterPassing(Parameter* formalParameter, AST_NODE* actualParameter)
{
	//該函數只負責檢查宣告參數和傳進來的參數是否符合, 不負責檢查下一個參數是否符合
	switch (formalParameter->type->kind){
		case(SCALAR_TYPE_DESCRIPTOR):
			switch(actualParameter->nodeType){
				case(IDENTIFIER_NODE):
					//檢查是否為array, 若是，則再度確認是否為0維array(scalar)
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
						int actualParameterOriginalDimensions = entry->attribute->attr.typeDescriptor->properties.arrayProperties.dimensions;
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
					if(actualParameter->semantic_value.identifierSemanticValue.kind == ARRAY_ID) {
						//check table first
						SymbolTableEntry* entry = retrieveSymbol(actualParameter->semantic_value.identifierSemanticValue.identifierName);
						if (entry == NULL) {
							//如果傳進去的id不存在，直接噴錯
							printErrorMsg(actualParameter, SYMBOL_UNDECLARED);
							return;
						}

						//check array dimension
						int expectedDimensions = formalParameter->type->properties.arrayProperties.dimensions;				
						int childNumberOfActualPara = 0;
						AST_NODE* actualParaChild = actualParameter->child;
						while(actualParaChild != NULL){
							childNumberOfActualPara += 1;
							actualParaChild = actualParaChild->rightSibling;
						}
						
						int actualParameterOriginalDimensions = entry->attribute->attr.typeDescriptor->properties.arrayProperties.dimensions;
						int dimensionsOfActualPara = actualParameterOriginalDimensions - childNumberOfActualPara;
						if (dimensionsOfActualPara != expectedDimensions) {
							if (dimensionsOfActualPara == 0) {
								//如果傳入的id的維度是0, 代表是scalar
								printErrorMsgSpecial(actualParameter, formalParameter->parameterName, PASS_SCALAR_TO_ARRAY);
							} else {
								printErrorMsgSpecial(actualParameter, INCOMPATIBLE_ARRAY_DIMENSION);
							}
						}
					} else {
						printErrorMsgSpecial(actualParameter, formalParameter->parameterName, PASS_SCALAR_TO_ARRAY);
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
	//收到的node其實不一定是expr, 有可能是expr, const, 或id node
	switch (exprRelatedNode->nodeType) {
		case EXPR_NODE:
			switch (exprRelatedNode->semantic_value.exprSemanticValue.kind) {
				case(BINARY_OPERATION):
					switch(exprRelatedNode->semantic_value.op.binaryOp) {
						case BINARY_OP_AND:  
						case BINARY_OP_OR:
						case BINARY_OP_EQ:
						case BINARY_OP_GE:  
						case BINARY_OP_GT:
						case BINARY_OP_LT:
						case BINARY_OP_LE:  
						case BINARY_OP_NE:
							AST_NODE* leftChild = exprRelatedNode->child;
							AST_NODE* rightChild = leftChild->rightSibling;
							processExprRelatedNode(leftChild);
							processExprRelatedNode(rightChild);
							break;
						case BINARY_OP_ADD:
						case BINARY_OP_SUB:
						case BINARY_OP_MUL:
						case BINARY_OP_DIV:
							AST_NODE* leftChild = exprRelatedNode->child;
							AST_NODE* rightChild = leftChild->rightSibling;
							processExprNode(leftChild);
							processExprNode(rightChild);
							break;
						default:
							printf("Error: exprRelatedNodeChild type error\n");
					}
					break;
				case(UNARY_OPERATION):
					AST_NODE* child = exprRelatedNode->child;
					processExprRelatedNode(child);
					break;
				default:
					printf("Error: processRxprRelatedNode function出現無法判斷binary或unary的Expr Node");
			break;
		case IDENTIFIER_NODE:
		case CONST_VALUE_NODE:
			processExprNode(exprRelatedNode);
			break;
		default:
			printf("Error: 無法判斷的ExprRelatedNode Type");
	}
}

void getExprOrConstValue(AST_NODE* exprOrConstNode, int* iValue, float* fValue)
{
	switch (exprOrConstNode.nodeType) {
		case(EXPR_NODE):
			switch (exprOrConstNode->semantic_value.exprSemanticValue.kind) {
				case(BINARY_OPERATION):
					switch(exprOrConstNode->semantic_value.op.binaryOp){
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
							AST_NODE* leftChild = exprOrConstNode->child;
							AST_NODE* rightChild = leftChild->rightSibling;
							getExprOrConstValue(leftChild, iValue, fValue);
							if (*iValue == 1) {
								getExprOrConstValue(rightSibling, iValue, fValue);
								if (*iValue == 1) {
									return;
								} else {
									*iValue = 0;
									*fValue = 1.0;
								}
							} else {
								return;
							}

							break;
						default:
							printf("Error: 出現無法預期的binary operator\n");
					}
					break;
				case(UNARY_OPERATION):
					getExprOrConstValue(exprOrConstNode->child, iValue, fvalue);
					break:
				default:
					printf("Error: getExprOrConstValue的expr node出現無法預期的op\n");
			}
			break;
		//TODO: 涵蓋functinocall, idnode
		case(CONST_VALUE_NODE):
			switch(exprOrConstNode->semantic_value.const1->const_type){
				case(INTEGERC):
					*ivalue = 1;
					*fvalue = 0.0;
					break;
				case(FLOATC):
					*ivalue = 0;
					*fvalue = 1.0;
					break;
				case(STRINGC):
					printf("Error: 請勿在expression中使用String Value");
					break;
				default:
					printf("Error: unknown const_value_node const type");
			} 
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
	switch(exprNode->nodeType){
		case EXPR_NODE:
			switch (exprNode->semantic_value.exprSemanticValue.kind) {
				case(BINARY_OPERATION):
					switch(exprRelatedNode->semantic_value.op.binaryOp) {
						case BINARY_OP_ADD:
						case BINARY_OP_SUB:
						case BINARY_OP_MUL:
						case BINARY_OP_DIV:
							AST_NODE* leftChild = exprRelatedNode->child;
							AST_NODE* rightChild = leftChild->rightSibling;
							processExprNode(leftChild);
							processExprNode(rightChild);
							break;
						default:
							printf("Error: 無法判斷的Binary Operation\n");
					}
					break;
				case(UNARY_OPERATION):
					AST_NODE* child = exprRelatedNode->child;
					processExprNode(child);
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
		default:
		   printf("Error: 出現無法判斷的ExprNode的type\n");
	}
}


void processVariableLValue(AST_NODE* idNode)
{
	SymbolTableEntry* entryRetrieved = retrieveSymbol(idNode->semantic_value.identifierSemanticValue.identifierName);
	if (entryRetrieved == NULL) {
		printErrorMsg(idNode, SYMBOL_UNDECLARED);
	} else {
		//沒有做check L value 和 R value 的 type	
	}
}

void processVariableRValue(AST_NODE* idNode)
{
	SymbolTableEntry* entryRetrieved = retrieveSymbol(idNode->semantic_value.identifierSemanticValue.identifierName);
	if (entryRetrieved == NULL) {
		printErrorMsg(idNode, SYMBOL_UNDECLARED);
	} else {
		switch(idNode->semantic_value.identifierSemanticValue.kind){
			case(ARRAY_ID):
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
				break;
			case(NORMAL_ID):
				break;
			default:
				printf("Error:遇到無法判斷的RValue的id 的id kind(array or scalar)\n");
		}
	}
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
	AST_NODE* returnNodeChild = returnNode->chlid;
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
				AST_NODE* stmtListNodeChild = blockNodeChild->child->leftmostSibling;
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
