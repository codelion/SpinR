/* Copyright 2007 by Moti Ben-Ari under the GNU GPL; see readme.txt */

#define DEBUG

#include "for.h"
#define NODES 4     /* Number of nodes in distributed system */
#define MESSAGES 10 /* Number of messages before markers are sent */

#define isOne(v,n) (v >> n & 1)
                    /* Check a bit in a byte */

mtype = { message, marker }; 
chan ch[NODES] = [NODES] of { mtype, byte, byte };
                    /* Channel type: message/marker(source, number) */
bool startSnapshot; /* To block environment node from starting snapshot */

proctype Env(byte outgoing) {
  startSnapshot;       /* Wait to start snapshot */
  for (I, 1, NODES-1)  /* Send marker on all outgoing channels */
    if
    :: isOne(outgoing,I) -> ch[I] ! marker, 0, 0; 
#ifdef DEBUG
         printf("Sent marker from environment to %d\n", I);
#endif
    :: else
    fi
  rof (I)
}

/* Choose an arbitrary outgoing node to send a message on */
/* Precondition: numOutgoing > 0 */
inline getOutgoing() {
  atomic {              /* Atomic to save verification steps */
    byte num;           /* "Local" variables to save original values */
    byte out;
    num = numOutgoing;  /* Do not initial "local" variables in declaration! */
    out = outgoing;
    destination = 0;    /* Set the destination chosen */
    
    do
    :: (out & 1) == 0 ->  /* Skip over zero bits with no outgoing edges */
          out = out >> 1; destination++
                          /* If still at least two bits, nondeterministically */
                          /*   choose to skip the first one */
    :: ((out & 1) == 1) && (num >  1) -> 
          num--; out = out >> 1; destination++
                         /* If at least one bit, choose it */
    :: ((out & 1) == 1)  -> 
          break
    od
  }
}

/* Print the state in a deterministic step */
inline PrintState() {
       d_step{
         for (K, 1, NODES-1)  /* Messages sent out (none to environment) */
           if
           :: isOne(outgoing,K) ->
                printf("Node %d, last sent to %d = %d\n", 
                  me, K, stateAtRecord[K]);
           :: else
           fi;
           if                /* Messages received */
           :: isOne(incoming,K) ->
                printf("Node %d, last received from %d = %d\n", 
                  me, K, messageAtRecord[K]);
                if          /* Contents of the channel */
		:: messageAtRecord[K] != messageAtMarker[K] ->
                     printf("Messages in channel %d -> %d = %d .. %d\n",
                         K, me, messageAtRecord[K] + 1, messageAtMarker[K])
                :: else
                fi
            :: else
            fi
         rof (K)
       }
}

proctype Node(
    byte me;                           /* My ID */ 
    byte numIncoming; byte incoming;   /* Encoding of incoming nodes */ 
    byte numOutgoing; byte outgoing) { /* Encoding of outgoing nodes */
    
  byte lastSent[NODES];             /* Data structures used by algorithm */
  byte lastReceived[NODES];
  byte stateAtRecord[NODES];
  byte messageAtRecord[NODES];
  byte messageAtMarker[NODES];

  byte markerCount;          /* Count of incoming markers */
  bool recorded;             /* State has been recorded */
  byte messageNumber;        /* Message number sent */
  
  /* Local variables for sending and receiving messages */
  byte destination;  
  byte source;
  byte received;

  do /* Infinite loop with nondeterministic choice of task to do */
     /* Send a message on an arbitrary outgoing channel - if any */
  :: numOutgoing != 0 ->
       getOutgoing();
       if
       :: full(ch[destination])  /* If channel is full, continue */
       :: nfull(ch[destination]) ->
            ch[destination] ! message(me, messageNumber);
            lastSent[destination] = messageNumber;
#ifdef DEBUG
            printf("Sent message %d from %d to %d\n", 
              messageNumber, me, destination);
#endif
            /* Tell environment to initiate snapshot, if enough messages sent */
            messageNumber++;
            if :: messageNumber > MESSAGES -> startSnapshot = true :: else fi
       fi
     /* Receive a message */
  :: ch[me] ? message(source, received) ->
       lastReceived[source] = received;
#ifdef DEBUG
            printf("Received message %d at %d from %d\n", 
              received, me, source)
#endif
     /* Receive a marker */
  :: ch[me] ? marker(source, _) ->
#ifdef DEBUG
       printf("Received marker at %d from %d\n", me, source);
#endif
       messageAtMarker[source] = lastReceived[source];
       markerCount++;
       /* If state not recorded, record the data */
       /* Send markers on outgoing channels */ 
       if
       :: recorded -> skip
       :: else ->
            recorded = true;
            for (I, 0, NODES-1)
              stateAtRecord[I] = lastSent[I];
              messageAtRecord[I] = lastReceived[I]
            rof (I);
            for (J, 0, NODES-1)
              if
              :: isOne(outgoing,J) ->
                   ch[J] ! marker(me, 0);
#ifdef DEBUG
                   printf("Sent marker from %d to %d\n", me, J)
#endif
              :: else
              fi
            rof (J)
       fi
     /* If markers received from all incoming channels, */
     /*   print state and terminate */
  :: markerCount == numIncoming ->
       PrintState();
       break
  od
}

init {
  /* Activate nodes with bit encoding of channels */
    atomic {
      run Env(4+2);
      run Node(1, 2, 4+1, 2, 8+4);
      run Node(2, 3, 8+2+1, 1, 2);
      run Node(3, 1, 2, 1, 4)
    }
}
