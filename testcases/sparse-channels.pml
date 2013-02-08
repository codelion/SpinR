/* Copyright 2007 by Moti Ben-Ari under the GNU GPL; see readme.txt */

typedef ENTRY {
	byte row;
	byte col;
	int value
}

chan sa1 =  [5] of { ENTRY };
chan sa2 =  [5] of { ENTRY };
chan sa3 = [10] of { ENTRY };


inline initEntry(S, R, C, V) {
	e.row = R;
	e.col = C;
	e.value = V;
    S ! e
}

inline printSA(S) {
	i = len(S);
	do
	:: i == 0 -> break
	:: else -> 
         S ? e;
		 S ! e;
		 printf("Row = %d, column = %d, value = %d\n", 
            e.row, e.col, e.value);
         i--
	od
}

inline addSA(S1, S2, S3) {
	do
	:: empty(S1) && empty(S2) -> break
    :: nempty(S1) && empty(S2) ->
         do 
         :: nempty(S1) -> S1 ? e1; S3 ! e1
         :: empty(S1) -> break
         od;
         break
    :: empty(S1) && nempty(S2) ->
         do 
         :: nempty(S2) -> S2 ? e2; S3 ! e2
         :: empty(S2) -> break
         od;
         break
    :: nempty(S1) && nempty(S2) ->
         S1 ? <e1>;
         S2 ? <e2>;
         if
         :: (e1.row == e2.row) && (e1.col == e2.col) ->
            S1 ? e1;
            S2 ? e2;
            initEntry(S3, e1.row, e1.col, e1.value + e2.value)
		 :: (e1.row < e2.row) || 
            ((e1.row == e2.row) && (e1.col < e2.col)) ->
            S1 ? e1; S3 ! e1
         :: else ->
            S2 ? e2; S3 ! e2
         fi
	od
}

active proctype P() {
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
    printSA(sa3)
}
