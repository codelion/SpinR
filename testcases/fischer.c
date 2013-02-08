#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

/* Copyright 2007 by Moti Ben-Ari under the GNU GPL; see readme.txt */

#define N 6
#define DELAY1 2
#define DELAY2 2

int turn  = 0;
int timer[N] = 0;
int critical = 0;

void P() {
start:
  while(1)
  {
  if(turn == 0)
  {
	timer[_pid] = DELAY1;
        printf("Process %d checks turn==0, timer = %d\n", _pid+1, timer[_pid]);
      }
	  if( timer[_pid] > 0 )
	  {
             turn = _pid+1;
             timer[_pid] = DELAY2;
             printf("Process %d sets turn=%d, timer = %d\n", _pid+1, _pid+1, timer[_pid]);
        else goto start;
       
      }

	  if (timer[_pid] == 0)
	  {
          if (turn == _pid+1) 
		  {
			critical++;
              printf("Process %d in CS, timer = %d\n", _pid+1, timer[_pid]);
		  }
         else goto start;
          fi
      }
	  
      if (assert (critical <= 1))
	  {
        critical--;
        turn = 0;
      }
  }
}

void Clock() {
  int i;
  while(1)
  {
  
       i = 0;
       while(1)
	   {
       if( i >= N )
		break;
       else
       {   
		if (timer[i] > 0)
		{
			timer[i]--;
	        printf("Timer of process %d ticked, timer = %d\n", i+1, timer[i]);
          
         }
          i++
       }
     }
  }
}
