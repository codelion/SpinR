#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

/* Copyright 2007 by Moti Ben-Ari under the GNU GPL; see readme.txt */

#include "for.h" 
/* includes the marco definition shared between promela and C*/

int result[8];
int a[8];
int b[15];
int c[15];

int stub_func(int n)
{
return 1 + rand() % n;
}


void Choose() {
	int c = stub_func(8);
	switch(c)
	{
	case 1: row = 1;
	case 2: row = 2;
	case 3: row = 3;
	case 4: row = 4;
	case 5: row = 5;
	case 6: row = 6;
	case 7: row = 7;
	case 8: row = 8;
	}
}

void Write() {
	for (i, 1, 8)
	{
		printf("%d, ", result[i-1])
	rof (i);
	}
    printf("\n")
}

void Queens() {
	int col = 1;
	int row;
	while(1)
	{
		Choose();
		!a[row-1];
		!b[row+col-2];
		!c[row-col+7];
		a[row-1]     = true;
		b[row+col-2] = true;
		c[row-col+7] = true;
		result[col-1] = row;
		if( col == 8) 
			break;
		else col++;
	}
	_ = result[0];
	Write();
	assert(false);
}
