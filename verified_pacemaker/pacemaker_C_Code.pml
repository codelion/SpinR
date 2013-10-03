int timer=0;
chan pulsea = [0] of {bit};
chan pulsev = [0] of {bit};
chan sensea = [0] of {bit};
chan sensev = [0] of {bit};
bit sena = 0;
bit senv = 0;
bit pula = 0;
bit pulv = 0;
int avd = 150;
int avdelay = -1;
int lastpulse = 0;
int lastpacedpulsea = 0;
int lastsenseda = 0;
int lastpacedpulsev =0;
int lastsensedv = 0;
int maxtime = 1000;
int mintime = 500;
int nr = 830;
int sensortime = 500;
int incrementtime = 50;
int responsefactor = 5;
int hysteresistime = 1000;
int hysteresis = 0;
int vrp =320;
int arp =250;
int pvarp = 250;
int schd = 1;
mtype  =  { nondet,dead,missv };
mtype = { VOO, AOO, VVI, AAI, VVT, AAT, DOO, DDI,DDD,VDD,AOOR,AAIR,VOOR,VVIR,VDDR,DOOR,DDIR,DDDR };
mtype = {low,med,high};
c_code{#include "pacemaker.h"};
int  imode = 0; 
int endstate = 0;

proctype updatetimers()
{
do
::(timer>=0  && schd ==1)-> timer = timer+50;
if
::(avdelay == -1  && lastpulse >0)->
timer = 0;
avdelay=-1;
lastpulse=0;
::else->skip;
fi;
if
::(lastpacedpulsev >0 && lastpulse >0)->
timer =0;
avdelay=-1;
lastpacedpulsev=0;
lastpulse=0;
::else->skip;
fi;
if
::(lastpacedpulsev >0)->
avdelay=-1;
lastpacedpulsev=0;
::else->skip;
fi;
if
::(lastpacedpulsea >0)->
avdelay=0;
lastpacedpulsea=0;
::else->skip;
fi;
if
::(avdelay>=0)->
avdelay = avdelay+50;
::else -> skip;
fi;
if
::(schd == 1)  -> schd  = 2;
fi;
od;
}
proctype environment(mtype b)
{
do
::(schd  == 2) ->
if
::(b == nondet) ->
/* Normal Non Deterministic Heart */
if
::((timer - lastpulse) >= nr)->
if
::pula=1;lastpulse = timer;avdelay=0;
::timer  = 0;lastpulse = timer;avdelay=0;
fi;
::else ->skip;
fi;
if
::(avdelay >= avd)->
if
::pulv=1;avdelay=-1;
::timer =0;avdelay=-1;
fi;
::else -> skip;
fi;
::else ->skip;
fi;
if
::(b == missv) ->
/* Missing Ventricle Pulse Heart */
if
::((timer - lastpulse) >= nr)->
if
::pula = 1;lastpulse = timer;avdelay=0;
fi;
::(avdelay >= avd)->
if
/*::pulsev!1;avdelay=-1;*/
::timer =0;avdelay=-1;
fi;
::else -> skip;
fi;
::else ->skip
fi;
if
::(b == dead) ->
/* Dead Heart */
if
::((timer - lastpulse) >= nr)->
if
/*::pulsea!1;lastpulse = timer;avdelay=0;*/
::timer=0;lastpulse = timer;avdelay=0;
fi;
::(avdelay >= avd)->
if
/*::pulsev!1;avdelay=-1;*/
::timer =0;avdelay=-1;
fi;
::else -> skip;
fi;
::else -> skip;
fi;
if
::(schd  == 2) -> schd  = 3;
fi
od;
}
proctype sensor()
{
do
::(schd  == 3)->
if
::(pula ==1 && sena ==1) -> sena=0; pula=0;
::else ->skip;
fi;
if
::(pula ==1) -> sena = 1;pula =0;lastsenseda = timer;
::else -> sena = 0;lastsenseda = 0;
fi;
if
::(pulv ==1 && senv ==1) -> senv=0; pulv=0;
::else -> skip;
fi;
if
::(pulv==1) -> senv =1;pulv=0;lastsensedv = timer;
::else -> senv =0;lastsensedv = 0;
fi;
if
::(schd  == 3) -> schd  = 4;
fi;
od;
}
proctype pacegen(mtype mode)
{
do
::(schd  == 5) ->
c_code
{
enum m mode = now.imode;
int hysteresis =  now.hysteresis;
if(mode == VOO || mode == DOO || mode == DDD|| mode == VDD || mode == VOOR || mode == DOOR || mode == DDDR || mode == VDDR)
{
if((timer - lastpacedpulsev) > mintime && (timer - lastpacedpulsev) < maxtime )
if(mode == VOOR || mode == DOOR || mode == DDDR || mode == VDDR) 
{
if((timer - lastpacedpulsev) >= (sensortime + responsefactor*incrementtime)) 
{pulv =1;lastpacedpulsev = timer;avdelay=-1;}
}
else { pulv = 1;lastpacedpulsev = timer;avdelay=-1; }
}

if(mode == AOO || mode == DOO || mode == DDD || mode == AOOR || mode == DOOR || mode == DDDR)
{
if((timer - lastpacedpulsea) > mintime && (timer - lastpacedpulsea) < maxtime )
if(mode ==AOOR ||mode == DOOR || mode == DDDR)  
{
if((timer - lastpacedpulsea) >= (sensortime + responsefactor*incrementtime))
{pula =1;lastpacedpulsea = timer;avdelay=0;}
}
else {pula =1;lastpacedpulsea = timer;avdelay=0;}
}

if(mode == VVI || mode == DDI || mode == VVIR || mode == DDIR )
{
if(senv == 1 && timer - lastpacedpulsev > vrp) {lastpacedpulsev = timer; avdelay = -1;}
if(hysteresis == 1) responsefactor = 10;
if((timer - lastpacedpulsev) > mintime && (timer - lastpacedpulsev) < maxtime )
if(mode == VVIR || mode == DDIR )
{
if((timer - lastpacedpulsev) >= (sensortime + responsefactor*incrementtime)) {pulv =1;lastpacedpulsev = timer;avdelay=-1;}
}
else {pulv = 1;lastpacedpulsev = timer;avdelay=-1;}
}

if(mode == AAI || mode == DDI || mode == AAIR ||mode == DDIR)
{
if(sena ==1 && timer - lastpacedpulsea > arp) {lastpacedpulsea = timer;avdelay=0;}
if(hysteresis == 1) responsefactor = 10;
if((timer - lastpacedpulsea) > mintime && (timer - lastpacedpulsea) < maxtime )
if(mode == AAIR ||mode == DDIR)
{
if ((timer - lastpacedpulsea) >= (sensortime + responsefactor*incrementtime)) {pula =1;lastpacedpulsea = timer;avdelay=0;}
}
else {pula=1;lastpacedpulsea = timer;avdelay=0;}
}

if(mode == VVT && timer - lastpacedpulsev > vrp)
{
if(senv==1) { pulv=1;lastpacedpulsev = timer;avdelay=-1;}
if((timer - lastpacedpulsev) > mintime && (timer - lastpacedpulsev) < maxtime )
{
pulv=1;lastpacedpulsev = timer;avdelay=-1;
}
}
if(mode == AAT && timer - lastpacedpulsea > arp)
{
if (sena == 1) {pula = 1;lastpacedpulsea = timer;avdelay=0;}
if((timer - lastpacedpulsea) > mintime && (timer - lastpacedpulsea) < maxtime )
{
pula=1;lastpacedpulsea = timer;avdelay=0;
}
}

if(mode == DDD || mode == VDD ||mode ==VDDR ||mode == DDDR)
{
if(sena == 1 && timer - lastpacedpulsev > pvarp) avdelay=0;
if(hysteresis == 1)  responsefactor = 10;
if(senv == 1 && timer - lastpacedpulsev > pvarp)  avdelay=-1;
}
if(mode == DOO || mode == DDI || mode ==VDD || mode == DDD ||mode ==DOOR || mode == DDIR || mode == VDDR ||mode ==DDDR)
{
if(avdelay >= avd) {pulv =1;lastpacedpulsev = timer;avdelay=-1;}
}
now.hysteresis = hysteresis;
now.lastpacedpulsea =lastpacedpulsea;
now.lastpacedpulsev = lastpacedpulsev;
now.avdelay = avdelay;
now.pulv=pulv;
now.pula=pula;
now.senv=senv;
now.sena = sena;
now.responsefactor = responsefactor;
};
endstate =1;
if
::(schd  == 5) -> schd  = 1;
fi;
od;
}
proctype accelerometer()
{
do
::(schd  == 4) ->
mtype act ;
if
::act = low;
::act=med;
::act=high;
fi;
if
::(act == low)->
if
::(responsefactor > 3) -> responsefactor = 3;
::(responsefactor == 3) -> responsefactor = 2;
::(responsefactor == 2)-> responsefactor = 1;
::else -> skip;
fi;
::else ->skip;
fi;
if
::(act == med)->
if
::(responsefactor > 6) -> responsefactor =  6;
::(responsefactor == 6) -> responsefactor = 5;
::(responsefactor <= 5) -> responsefactor = 4;
::else -> skip;
fi;
::else ->skip;
fi;
if
::(act == high)->
if
::(responsefactor < 7) -> responsefactor = 7;
::(responsefactor == 7) -> responsefactor = 8;
::(responsefactor == 8)-> responsefactor = 9;
::else -> skip;
fi;
::else ->skip;
fi;
if
::(schd  == 4) -> schd  =  5;
fi;
od;
}
init
{
mtype behave = nondet;
mtype mode = nondet; 
hysteresis = 1;
atomic{
run updatetimers();
run environment(behave);
run sensor();
run accelerometer();
run pacegen(mode);
}
}
