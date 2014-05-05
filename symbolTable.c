#include "symbolTable.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
// This file is for reference only, you are not required to follow the implementation. //

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
	if(hashTable[hashIndex] == entry){
		hashTable[hashIndex] = entry->nextInHashChain; 
		hashTable[hashIndex]->prevInHashChain = NULL;
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
	symbolTable.hashTable[hashIndex]->prevInHashChain = entry;
	symbolTable.hashTable[hashIndex] = entry;
}

void initializeSymbolTable()
{
	int counter = 0;
	for(counter; counter < HASH_TABLE_SIZE; counter++)
		symbolTable.hashTable[counter] = NULL;
	symbolTable.scopeDisplay = NULL;
	symbolTable.currentLevel = 0;
	symbolTable.scopeDisplayElementCount = 0;
}

void symbolTableEnd()
{
	scopeDisplay = NULL;
	//TODO: what does this function do?
}

SymbolTableEntry* retrieveSymbol(char* symbolName)
{
	SymbolTableEntry* temp = symbolTable.hashTable[HASH(symbolName)];

	while(temp != NULL){
		if(strcmp(temp.name, symbolName))
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
	if(retr == NULL){
		temp->name = symbolName;
		temp->attribute = attribute;
		enterIntoHashTrain(HASH(symbolName), temp);
	}
	else{ 
		temp->nextInHashChain = retr->nextInHashChain;
		temp->prevInHashChain = retr->prevInHashChain;
		temp->sameNameInOuterLevel = retr;
	}
	return temp;
}

//remove the symbol from the current scope
void removeSymbol(char* symbolName)
{
	SymbolTableEntry* temp = retrieveSymbol(symbolTable);
	SymbolTableEntry* tempsame = temp->sameNameInOuterLevel;
	if(tempsame != NULL){
		removeFromHashTrain(HASH(symbolName), temp);
		enterIntoHashTrain(HASH(symbolName), tempsame);
	}
	else
		removeFromHashTrain(HASH(symbolName), temp);
}

int declaredLocally(char* symbolName)
{
	return retrieveSymbol(symbolName)->nestingLevel;
}

void openScope()
{
	symbolTable.currentLevel++;
	symbolTable.scopeDisplayElementCount++;
}

void closeScope()
{
	symbolTable.currentLevel--;
}
