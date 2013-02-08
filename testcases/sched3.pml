/* Copyright 2007 by Moti Ben-Ari under the GNU GPL; see readme.txt */

#define MAX 4
byte clock = 0;
byte maxPeriod;
chan queue = [MAX] of { byte };

proctype T(byte ID; byte period; byte exec) {
	byte next = 0;
	byte deadline = period;
	byte current = 0;
end:do
	::  atomic {
			(clock >= next) && (clock < deadline) &&
			(clock < maxPeriod) &&
			!(queue ?? [eval(ID)]) ->
				queue !! ID;
				printf("Process=%d put on queue\n", ID)
		}
	::  atomic {
			(clock >= next) && (clock < deadline) &&
			(queue ? [eval(ID)]) ->
				current++;
				clock++;
				printf("Process=%d, clock=%d, current=%d\n", 
					ID, clock, current);
				if
				:: current == exec -> 
					queue ? eval(ID);
					current = 0;
					next = next + period;
					printf("Process=%d taken off queue\n", ID)
				:: else
				fi
		}
	::  atomic {
			(clock >= deadline) ->
				assert (!(queue ?? [eval(ID)]));
				deadline = deadline + period
		}
	od
}

proctype Idle() {
end:
	do
	:: 	atomic {	
			(clock < maxPeriod) && timeout -> {
			clock++;
			printf("Idle, clock=%d\n", clock)
			}
		}
	od
}

init {
	atomic {
		run Idle();

		maxPeriod = 5;
		queue ! 0; queue ! 1;
		run T(0, 2, 1);
		run T(1, 5, 2);

/*
		maxPeriod = 15;
		queue ! 0; queue ! 1; queue ! 2;
		run T(0, 8, 3);
		run T(1, 9, 3);
		run T(2, 15, 3);
*/
/*
		maxPeriod = 20;
		queue ! 0; queue ! 1; queue ! 2;
		run T(0, 8, 4);
		run T(1, 12, 4);
		run T(2, 20, 4);
*/
/*
		maxPeriod = 12;
		queue ! 0; queue ! 1; queue ! 2;
		run T(0, 8, 4);
		run T(1, 10, 2);
		run T(2, 12, 3);
*/
	}
}
