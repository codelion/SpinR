/* Copyright 2007 by Moti Ben-Ari under the GNU GPL; see readme.txt */

#define N 6
#define DELAY1 2
#define DELAY2 2

byte turn  = 0;
byte timer[N] = 0;
byte critical = 0;

active [N] proctype P() {
start:
  do
  ::  atomic {
        turn == 0 -> timer[_pid] = DELAY1;
        printf("Process %d checks turn==0, timer = %d\n", _pid+1, timer[_pid])
      }
      atomic {
        if
        :: timer[_pid] > 0 ->
             turn = _pid+1;
             timer[_pid] = DELAY2;
             printf("Process %d sets turn=%d, timer = %d\n", _pid+1, _pid+1, timer[_pid])
        :: else -> goto start
        fi
      }
      atomic {
        timer[_pid] == 0 ->
          if
          ::  (turn == _pid+1) -> critical++;
              printf("Process %d in CS, timer = %d\n", _pid+1, timer[_pid])
          ::  else -> goto start
          fi
      }
      atomic {
        assert (critical <= 1);
        critical--;
        turn = 0
      }
  od
}

active proctype Clock() {
  byte i;
  do
  :: d_step { 
       i = 0;
       do
       :: i >= N -> break
       :: else -> 
          if
          :: timer[i] > 0 -> timer[i]--;
	        printf("Timer of process %d ticked, timer = %d\n", i+1, timer[i])
          :: else
          fi;
          i++
       od
     }
  od
}
