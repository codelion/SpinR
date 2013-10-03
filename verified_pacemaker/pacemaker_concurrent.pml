int timer=0;
int maxtimer = 1000;
chan pulsea = [0] of {bit};
chan pulsev = [0] of {bit};
bit sensea=0;
bit sensev=0;
int avd = 150;
int avdelay = -1;
int lastpulsea = 0;
int lastpulsev = 0;
int lastpacedpulsev = 0;
int lastpacedpulsea = 0;
int maxtime = 1000;
int mintime = 500;
int nr = 830;
mtype = {VOO,AOO,VVI,AAI,DDI,DOO,AAT,VVT,VDD,DDD};
mtype = {normal,missv,dead,nondet};

proctype sensor()
{
do
::pulsea?_->sensea=1;
::pulsev?_->sensev=1;
od;
}

proctype environment(mtype behave)
{
do
::(behave == nondet) ->
if
::behave = normal;
::behave = missv;
::behave = dead;
fi;

::(behave == normal)->

if
::(timer - lastpulsea >= nr && avdelay == -1)->
pulsea!1;lastpulsea =timer; avdelay=0;
::else ->skip;
fi;

if
::(avdelay >= avd)->
pulsev!1;lastpulsev=timer;avdelay=-1;
::else -> skip;
fi;

if
::(timer >=0 &&  timer < maxtimer)->
timer =timer +50;
::else ->skip;
fi;

if
::(avdelay >=0  && avdelay/10 == 0)->
avdelay = avdelay +50;
::else ->skip;
fi;

if
::(timer >= maxtimer && (lastpacedpulsev >0 && lastpacedpulsea>0))->
avdelay = -1;
lastpulsea = 0;
lastpulsev = 0;
lastpacedpulsev = 0;
lastpacedpulsea = 0;
timer = 0;
::else ->skip;
fi;

::(behave == missv)->

if
::((timer - lastpulsea >= nr && avdelay == -1) )->
pulsea!1;lastpulsea =timer; avdelay=0;
::else ->skip;
fi;

if
::(avdelay >= avd)->
avdelay=-1;
::else -> skip;
fi;

if
::(timer >=0 &&  timer < maxtimer)->
timer =timer +50;
::else ->skip;
fi;

if
::(avdelay >=0 && avdelay/10 == 0)->
avdelay = avdelay +50;
::else ->skip;
fi;

if
::(timer >= maxtimer && (lastpacedpulsev >0 && lastpacedpulsea>0))->
avdelay = -1;
lastpulsea = 0;
lastpulsev = 0;
lastpacedpulsev = 0;
lastpacedpulsea = 0;
timer = 0;
::else ->skip;
fi;

::(behave == dead)->

if
::((timer - lastpulsea >= nr && avdelay == -1))->
avdelay = 0;
::else ->skip;
fi;

if
::(avdelay >= avd)->
avdelay=-1;
::else -> skip;
fi;

if
::(timer >=0 &&  timer < maxtimer)->
timer =timer +50;
::else ->skip;
fi;

if
::(avdelay >= 0 && avdelay/10 == 0)->
avdelay = avdelay +50;
::else -> skip;
fi;

if
::(timer >= maxtimer  && (lastpacedpulsev >0 && lastpacedpulsea>0))->
avdelay = -1;
lastpulsea = 0;
lastpulsev = 0;
lastpacedpulsev = 0;
lastpacedpulsea = 0;
timer = 0;
::else ->skip;
fi;
od;
}

proctype pacegen(mtype mode)
{
do
::(mode == nondet) ->
if
::mode = AOO;
::mode = VOO;
::mode = AAI;
::mode = VVI;
::mode = AAT;
::mode = VVT;
::mode = DOO;
::mode = DDI;
::mode = DDD;
::mode = VDD;
fi;
::(mode == VOO || mode == DOO|| mode == VVI || mode == DDI || mode == VVT || mode == VDD || mode == DDD)->
if
::((timer -lastpacedpulsev) > mintime && (timer - lastpacedpulsev) < maxtime)  ->
pulsev!1;lastpacedpulsev=timer;avdelay =-1;
::else ->skip;
fi;
::(mode == AOO || mode == DOO || mode ==AAI ||mode == DDI || mode == AAT || mode == DDD)->
if
::((timer -lastpacedpulsea) > mintime && (timer - lastpacedpulsea) < maxtime) ->
pulsea!1;lastpacedpulsea=timer;avdelay =0;
::else ->skip;
fi;
::(mode == VDD|| mode == DDD)->
if
::(avdelay >=avd) ->
pulsev!1;lastpacedpulsev=timer;avdelay =-1;
::else ->skip;
fi;
::(mode == VVI || mode == DDI)->
if
::(sensev == 1)-> sensev=0;avdelay =-1;
::else -> skip;
fi;
::( mode == AAI || mode == DDI)->
if
::(sensea == 1)-> sensea=0;avdelay =0;
::else -> skip;
fi;
::(mode == VVT)->
if
::(sensev == 1) -> pulsev!1;lastpacedpulsev=timer;avdelay =-1;sensev=0;
::else ->skip;
fi;
::(mode == AAT)->
if
::(sensea == 1) -> pulsea!1;lastpacedpulsea=timer;avdelay =0;sensea=0;
::else ->skip;
fi;
::(mode == VDD || mode == DDD)->
if
::(sensea==1) -> sensea=0; avdelay =0;
::else ->skip;
fi;
if
::(sensev == 1)-> sensev=0;avdelay =-1;
::else -> skip;
fi;
od;
}
init
{
mtype mode = nondet;
mtype behave =nondet;
atomic{
run environment(behave);
run sensor();
run pacegen(mode);
}
}
