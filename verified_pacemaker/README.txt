List of Files Contained - 

AAI.ltl
AAI_H.ltl
AAT.ltl
ARP.ltl
AVD.ltl
Distributed_AVD.ltl
LRLURLA.ltl
LRLURLA_R.ltl
LRLURLA_RC.ltl
LRLURLV.ltl
LRLURLV_R.ltl
LRLURLV_RC.ltl
pacemaker.c (C Implementation Code)
pacemaker.h
pacemaker_concurrent.pml
pacemaker_C_Code.pml
pacemaker_distributed.pml
pacemaker_sequential.pml
PVARP.ltl
README.txt (this file) 
VRP.ltl
VVI.ltl
VVI_H.ltl
VVT.ltl
XDD.ltl
XDD_H.ltl


===================
Models of Pacemaker
===================

-------------------------------------
Sequential - pacemaker_sequential.pml
-------------------------------------

Change the default max depth from 10000 to 100000

- Default Configuration is 
	
	mode =  nondet
	behave = nondet
	hysteresis = 1

	Check for invalid end state - Deadlock freeness

- TO verify all the generic properties, in the init process set
	
	mode = nondet
	behave = nondet
	hysteresis = 0

	use LTL property manager to load and check
	LRLURLA, LRLURLV, AVD, ARP, VRP and PVARP


- To verify Inhibiting properties, in the init process set
	
	mode = VVI or AAI
	behave = nondet
	hysteresis = 0

	use LTL property manager to load and check
	AAI and VVI

- To verify Triggering properties, in the init process set
	
	mode = VVT or AAT
	behave = nondet
	hysteresis = 0

	use LTL property manager to load and check
	AAT and VVT

- To verify Tracking properties, in the init process set
	
	mode = VDD or DDD
	behave = nondet
	hysteresis = 0

	use LTL property manager to load and check
	XDD

- To verify Rate Control properties, in the init process set
	
	mode = VOOR or AOOR or DOOR or VVIR or AAIR or DDIR or VDDR or DDDR
	behave = nondet
	hysteresis = 0

	use LTL property manager to load and check
	LRLURLA_R, LRLURLA_RC, LRLURLV_R and LRLURLV_RC

- To verify Hysteresis properties, in the init process set
	
	mode = nondet
	behave = nondet
	hysteresis = 1

	use LTL property manager to load and check
	AAI_H, VVI_H and XDD_H
	
-------------------------------------
Concurrent - pacemaker_concurrent.pml
-------------------------------------

Change the max memory used by SPIN to 2000 MB


- Default Configuration is 
	
	mode =  nondet
	behave = nondet

	Check for invalid end state - Deadlock freeness

- TO verify all the generic properties, in the init process set
	
	mode = nondet
	behave = nondet


	use LTL property manager to load and check
	LRLURLA, LRLURLV and AVD

---------------------------------------
Distributed - pacemaker_distributed.pml
---------------------------------------

Choose to use memory compression in SPIN 


- Default Configuration is 
	
	mode =  nondet
	behave = nondet

	Check for invalid end state - Deadlock freeness

- TO verify all the generic properties, in the init process set
	
	mode = nondet
	behave = nondet


	use LTL property manager to load and check
	LRLURLA, LRLURLV, AVD and Distributed_AVD

------------------------------------------------------
Embedded C Code - pacemaker_C_Code.pml and pacemaker.h
------------------------------------------------------

- Default Configuration is 
	
	PROMELA init()
	mode =  nondet
	behave = nondet

	c_code config
	mode = DDDR
	hysteresis = 1

	Check for invalid end state - Deadlock freeness

- TO verify all the LTL properties, in the c_code part of Pace Generator process set
	
	mode = XXXX (Any given mode)
	
	use LTL property manager to load and check properties as in Sequential Model

------------------------------
C Implementation - pacemaker.c
------------------------------

A sequential implementaion of the pacemaker in C that can be compiled and run with gcc.
