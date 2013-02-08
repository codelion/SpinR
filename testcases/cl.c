#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>


/* Copyright 2007 by Moti Ben-Ari under the GNU GPL; see readme.txt */

#define DEBUG

#include "for.h"
/*Contains Macro for loop shared between PROMELA and C */ 

#define NODES 4     /* Number of nodes in distributed system */
#define MESSAGES 10 /* Number of messages before markers are sent */

#define isOne(v,n) (v >> n & 1)
                    /* Check a bit in a byte */

#define message 2
#define marker 1 

int ch[NODES]; 
                    /* Channel type: message/marker(source, number) */
					
int startSnapshot; /* To block environment node from starting snapshot */


void Env(int outgoing) {
  startSnapshot;       /* Wait to start snapshot */
  for (I, 1, NODES-1)  /* Send marker on all outgoing channels */
  {
    if (isOne(outgoing,I))
	{
		ch[I] = marker, 0, 0; 
		
#ifdef DEBUG
         printf("Sent marker from environment to %d\n", I);
#endif
    
   }
  rof (I);
  }
}

/* Choose an arbitrary outgoing node to send a message on */
/* Precondition: numOutgoing > 0 */
void getOutgoing() {
             /* Atomic to save verification steps */
    int num;           /* "Local" variables to save original values */
    int out;
    num = numOutgoing;  /* Do not initial "local" variables in declaration! */
    out = outgoing;
    destination = 0;    /* Set the destination chosen */
    
    while(1)
	{
    if ((out & 1) == 0)
	{   /* Skip over zero bits with no outgoing edges */
          out = out >> 1; 
		  destination++
                          /* If still at least two bits, nondeterministically */
                          /*   choose to skip the first one */
						  }
    if (((out & 1) == 1) && (num >  1))
    {	
          num--; 
		  out = out >> 1; 
		  destination++
                         /* If at least one bit, choose it */
	}
    if (((out & 1) == 1))
          break;
  }
}

/* Print the state in a deterministic step */
void PrintState() {

         for (K, 1, NODES-1)  /* Messages sent out (none to environment) */
		 {
           if (isOne(outgoing,K))
		   {
                printf("Node %d, last sent to %d = %d\n", 
                  me, K, stateAtRecord[K]);
		   
           }
                           /* Messages received */
           if(isOne(incoming,K))
		   {
                printf("Node %d, last received from %d = %d\n", 
                  me, K, messageAtRecord[K]);
                
				          /* Contents of the channel */
				
		if (messageAtRecord[K] != messageAtMarker[K])
		{
                     printf("Messages in channel %d -> %d = %d .. %d\n",
                         K, me, messageAtRecord[K] + 1, messageAtMarker[K]);
               }

            }
         rof (K);
		 
       }
}

void Node(
    int me;                           /* My ID */ 
    int numIncoming; 
	int incoming;   /* Encoding of incoming nodes */ 
    int numOutgoing; 
	int outgoing) /* Encoding of outgoing nodes */
	{ 
    
  int lastSent[NODES];             /* Data structures used by algorithm */
  int lastReceived[NODES];
  int stateAtRecord[NODES];
  int messageAtRecord[NODES];
  int messageAtMarker[NODES];

  int markerCount;          /* Count of incoming markers */
  int recorded;             /* State has been recorded */
  int messageNumber;        /* Message number sent */
  
  /* Local variables for sending and receiving messages */
  int destination;  
  int source;
  int received;

   /* Infinite loop with nondeterministic choice of task to do */
     /* Send a message on an arbitrary outgoing channel - if any */
  while(1)
  {
  if( numOutgoing != 0)
  {
       getOutgoing();
       if (full(ch[destination]))  /* If channel is full, continue */
       if( nfull(ch[destination]))
	   {
            ch[destination] = message(me, messageNumber);
            lastSent[destination] = messageNumber;
	   }
#ifdef DEBUG
            printf("Sent message %d from %d to %d\n", 
              messageNumber, me, destination);
#endif
            /* Tell environment to initiate snapshot, if enough messages sent */
            messageNumber++;
            if (messageNumber > MESSAGES)
				startSnapshot = true;
    }
     /* Receive a message */
  if ( message(source, received) = ch[me] )
  {
       lastReceived[source] = received;
#ifdef DEBUG
            printf("Received message %d at %d from %d\n", 
              received, me, source);
#endif
}
     /* Receive a marker */
  if (marker(source, _) = ch[me] )
  {
#ifdef DEBUG
       printf("Received marker at %d from %d\n", me, source);
#endif
       messageAtMarker[source] = lastReceived[source];
       markerCount++;
       /* If state not recorded, record the data */
       /* Send markers on outgoing channels */ 
       if (recorded)
		;
       else
	   {
            recorded = true;
            for (I, 0, NODES-1)
			{
              stateAtRecord[I] = lastSent[I];
              messageAtRecord[I] = lastReceived[I]
            rof (I);
			}
            for (J, 0, NODES-1)
			{
              if (isOne(outgoing,J))
			  {
                   ch[J] = marker(me, 0);
#ifdef DEBUG
                   printf("Sent marker from %d to %d\n", me, J);
#endif
              
              }
            rof (J);
			}
       }
     /* If markers received from all incoming channels, */
     /*   print state and terminate */
  if(markerCount == numIncoming)
  {
       PrintState();
       break;
	}
  }
}
}

void main() {
  /* Activate nodes with bit encoding of channels */
    
      Env(4+2);
      Node(1, 2, 4+1, 2, 8+4);
      Node(2, 3, 8+2+1, 1, 2);
      Node(3, 1, 2, 1, 4);
    
}
