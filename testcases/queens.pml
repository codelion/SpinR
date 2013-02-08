/* Copyright 2007 by Moti Ben-Ari under the GNU GPL; see readme.txt */

#include "for.h"

byte result[8];
bool a[8];
bool b[15];
bool c[15];

inline Choose() {
	if
	:: row = 1
	:: row = 2
	:: row = 3
	:: row = 4
	:: row = 5
	:: row = 6
	:: row = 7
	:: row = 8
	fi
}

inline Write() {
	for (i, 1, 8)
		printf("%d, ", result[i-1])
	rof (i);
    printf("\n")
}

active proctype Queens() {
	byte col = 1;
	byte row;
	do
	::	Choose();
		!a[row-1];
		!b[row+col-2];
		!c[row-col+7];
		a[row-1]     = true;
		b[row+col-2] = true;
		c[row-col+7] = true;
		result[col-1] = row;
		if
		:: col == 8 -> break
		:: else     -> col++
		fi
	od;
	_ = result[0];
	Write();
	assert(false)
}
