/* Header File with Globals */
/* Generated from Data Refinement */

int timer = 0;
int sena = 0;
int senv = 0;
int pula = 0;
int pulv = 0;
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
enum behave {nondet,dead,missv};
enum m {VOO, AOO, VVI, AAI, VVT, AAT, DOO, DDI,DDD,VDD,AOOR,AAIR,VOOR,VVIR,VDDR,DOOR,DDIR,DDDR,all};
enum activity {low,med,high};
/*End Data Refinement*/