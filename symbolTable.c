#include "symbolTable.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
// This file is for reference only, you are not required to follow the implementation. //
//
int HASH(char * str) {
	int idx=0;
	while (*str){
		idx = idx << 1;
		idx+=*str;
		str++;
	}
	return (idx & (HASH_TABLE_SIZE-1));
}

SymbolTable symbolTable;

SymbolTableEntry* newSymbolTableEntry(int nestingLevel)
{
    SymbolTableEntry* symbolTableEntry = (SymbolTableEntry*)malloc(sizeof(SymbolTableEntry));
    symbolTableEntry->nextInHashChain = NULL;
    symbolTableEntry->prevInHashChain = NULL;
    symbolTableEntry->nextInSameLevel = NULL;
    symbolTableEntry->sameNameInOuterLevel = NULL;
    symbolTableEntry->attribute = NULL;
    symbolTableEntry->name = NULL;
    symbolTableEntry->nestingLevel = nestingLevel;
    return symbolTableEntry;
}

void removeFromHashTrain(int hashIndex, SymbolTableEntry* entry)
{
	if(symbolTable.hashTable[hashIndex] == entry){
		symbolTable.hashTable[hashIndex] = entry->nextInHashChain; 
		symbolTable.hashTable[hashIndex]->prevInHashChain = NULL;
	}
	else{
		entry->prevInHashChain->nextInHashChain = entry->nextInHashChain;
		if(entry->nextInHashChain != NULL)
			entry->nextInHashChain->prevInHashChain = entry->prevInHashChain;
	}
	free(entry);
}

void enterIntoHashTrain(int hashIndex, SymbolTableEntry* entry)
{
	entry->nextInHashChain = symbolTable.hashTable[hashIndex];
	if (symbolTable.hashTable[hashIndex] != NULL) {
		symbolTable.hashTable[hashIndex]->prevInHashChain = entry;
	}
	symbolTable.hashTable[hashIndex] = entry;
}

void initializeSymbolTable()
{
	int counter = 0;
	for(counter; counter < HASH_TABLE_SIZE; counter++)
		symbolTable.hashTable[counter] = NULL;
	symbolTable.scopeDisplay = (SymbolTableEntry**)malloc(256 * sizeof(SymbolTableEntry*));
	*symbolTable.scopeDisplay = NULL;
	symbolTable.currentLevel = 0;
	symbolTable.scopeDisplayElementCount = 1;
}

void symbolTableEnd()
{
	//TODO: what does this function do?
}

SymbolTableEntry* retrieveSymbol(char* symbolName)
{
	SymbolTableEntry* temp = symbolTable.hashTable[HASH(symbolName)];

	while(temp != NULL){
		if(strcmp(temp->name, symbolName))
			temp = temp->nextInHashChain;
		else 
			return temp;
	}
	return NULL;
}

SymbolTableEntry* enterSymbol(char* symbolName, SymbolAttribute* attribute)
{
	SymbolTableEntry* temp = newSymbolTableEntry(symbolTable.currentLevel);
	SymbolTableEntry* retr = retrieveSymbol(symbolName);
	temp->name = symbolName;
	temp->attribute = attribute;
	if(retr == NULL){
		enterIntoHashTrain(HASH(symbolName), temp);
	}
	else{ 
		temp->nextInHashChain = retr->nextInHashChain;
		temp->prevInHashChain = retr->prevInHashChain;
		retr->nextInHashChain->prevInHashChain = temp;
		retr->prevInHashChain->nextInHashChain = temp;
		temp->sameNameInOuterLevel = retr;
	}
	temp->nextInSameLevel = symbolTable.scopeDisplay[symbolTable.scopeDisplayElementCount - 1];
	symbolTable.scopeDisplay[symbolTable.scopeDisplayElementCount - 1] = temp;
	return temp;
}

//remove the symbol from the current scope
void removeSymbol(char* symbolName)
{
	SymbolTableEntry* temp = retrieveSymbol(symbolName);
	if(temp == NULL)
		return;
	SymbolTableEntry* tempsame = temp->sameNameInOuterLevel;
	if(tempsame != NULL){
		tempsame->nextInHashChain = temp->nextInHashChain;
		tempsame->prevInHashChain = temp->prevInHashChain;
		temp->nextInHashChain->prevInHashChain = tempsame;
		temp->prevInHashChain->nextInHashChain = tempsame;
	}
	else
		temp->nextInHashChain->prevInHashChain = temp->prevInHashChain;
		temp->prevInHashChain->nextInHashChain = temp->nextInHashChain;
}

int declaredLocally(char* symbolName)
{
	return retrieveSymbol(symbolName)->nestingLevel;
}

void openScope()
{
	symbolTable.currentLevel++;
	symbolTable.scopeDisplayElementCount++;
	symbolTable.scopeDisplay[symbolTable.scopeDisplayElementCount - 1] = NULL;
}

void closeScope()
{
	symbolTable.currentLevel--;
}
