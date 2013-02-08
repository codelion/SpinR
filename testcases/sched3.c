#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

/* Copyright 2007 by Moti Ben-Ari under the GNU GPL; see readme.txt */

#define MAX 4
int clock = 0;
int maxPeriod;
int queue[MAX];

void T(int ID, int period, int exec) {
	int next = 0;
	int deadline = period;
	int current = 0;
	
end:while(1)
	{
	if ((clock >= next) && (clock < deadline) &&
			(clock < maxPeriod) &&
			!([eval(ID)] = queue))
		{
			queue = ID;
			printf("Process=%d put on queue\n", ID);
		}
	if((clock >= next) && (clock < deadline) &&
			( [eval(ID)] = queue))
			{
				current++;
				clock++;
				printf("Process=%d, clock=%d, current=%d\n", 
					ID, clock, current);
				if (current == exec)
					{
					eval(ID) = queue;
					current = 0;
					next = next + period;
					printf("Process=%d taken off queue\n", ID);
				}
			
			}
	if((clock >= deadline))
	{	
				assert (!([eval(ID)] = queue));
				deadline = deadline + period;
	}
	}
}

void Idle() {
end:
	while(1)
	{
	if ((clock < maxPeriod) && timeout) 
		{
			clock++;
			printf("Idle, clock=%d\n", clock);
		}
	}
}

void main(){
		Idle();

		maxPeriod = 5;
		queue = 0; 
		queue = 1;
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
