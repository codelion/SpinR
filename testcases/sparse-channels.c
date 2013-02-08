#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

/* Copyright 2007 by Moti Ben-Ari under the GNU GPL; see readme.txt */

struct ENTRY {
	int row;
	int col;
	int value
}e;

struct ENTRY [5] sa1;
struct ENTRY [5] sa2;
struct ENTRY [10] sa3;


void initEntry(ENTRY* S, int R, int C, int V) {
	e.row = R;
	e.col = C;
	e.value = V;
    S = e;
}

void printSA(ENTRY* S) {
	i = len(S);
	while(1)
	{
	if(i == 0)
		break;
	else
	{
         e = S;
		 S = e;
		 printf("Row = %d, column = %d, value = %d\n", 
            e.row, e.col, e.value);
         i--;
	}
	}
}

void addSA(ENTRY* S1,ENTRY* S2,ENTRY* S3) {
	while(1)
	{
	if(empty(S1) && empty(S2))
		break;
    if(nempty(S1) && empty(S2))
	{
         while(1)
		 {
         if (nempty(S1))
		 {
			e1 = S2; 
			S3 = e1;
		 }
         if (empty(S1))
			break;
         }
         break;
    }
    if (empty(S1) && nempty(S2))
	{
         while(1)
		 {
         if (nempty(S2))
		 {	
			e2 = S2; 
			S3 = e2;
		 }
         if (empty(S2))
			break;
         }
         break;
	}
    if (nempty(S1) && nempty(S2)) 
	{
         e1 = S1;
         e2 = S2;
         if ((e1.row == e2.row) && (e1.col == e2.col))
		 {
            e1 = S1;
            e2 = S2;
            initEntry(S3, e1.row, e1.col, e1.value + e2.value);
		}
		 if ((e1.row < e2.row) || ((e1.row == e2.row) && (e1.col < e2.col)))
		 {
            e1 = S1;
			S3 = e1;
		}
        else
		{
            e2 = S2; 
			S3 = e2;
		}
	}
	}
}

void P() {
	int i;
	ENTRY e, e1, e2;

	initEntry(sa1, 0, 1, -5);
	initEntry(sa1, 0, 3, 8);
	initEntry(sa1, 2, 0, 20);
	initEntry(sa1, 3, 3, -3);
	printf("Sparse array 1:\n");
    printSA(sa1);

	initEntry(sa2, 0, 2, -2);
	initEntry(sa2, 0, 3, 5);
	initEntry(sa2, 2, 0, -15);
	printf("Sparse array 2:\n");
    printSA(sa2);

	addSA(sa1, sa2, sa3);
	printf("Sparse array 3:\n");
    printSA(sa3);
}
