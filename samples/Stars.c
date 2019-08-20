/****************************************/
/* STARS.C                              */
/* (C) Copyright 1993 David Bollinger   */
/* send comments to CIS ID# 72510,3623  */
/* compiled with Borland C++ 3.0        */
/* command line: bcc -ms -v- stars.c    */
/****************************************/

#include <bios.h>
#include <stdlib.h>
#include <time.h>
#include "stars.h"

#define TRUE     (0==0)
#define FALSE    (0==1)
#define AND      &&
#define OR       ||
#define NOT      !
#define MAXX     320       // screen x dimension
#define MAXY     200       // screen y dimension
#define CENTERX  160       // center x coord of screen
#define CENTERY  100       // center y coord of screen
#define MAXSTARS 128       // how many stars on screen
#define SPINFREQ 32        // how rapidly to spin
                           //   lower numbers cause more rapid spinning
#define DIRFREQ  25000     // how often to change spin direction
                           //   lower numbers cause more frequent changes
#define DELAY    50        // how fast are stars, higher # = slower
                           //   500 looks about right on a 486-25

typedef struct
   {
   int x;                  // current screen x coord
   int y;                  // current screen y coord
   int vel;                // current velocity
   int delay;              // inverse of velocity
   int angle;              // angle of path away from center
   int dist;               // distance from center
   } STARS;

/********************/
/* global variables */
/********************/
STARS stars[MAXSTARS];
int theta=0,               // offset into trig tables for starfield rotation
    spincycle=0,           // delay counter for rotation
    direction=0,           // current spin direction -1=CW 0=no spin 1=CCW
    delay;                 // slow-down loop counter
char notice[] = "STARS.C (C) 1993 David Bollinger";

/***********************/
/* function prototypes */
/***********************/
void InitStars(void);
void InitStar(STARS *thisstar);
void UpdateStars(void);
void DrawStar(int x, int y, int c, int dist, int vel);
void SetVideoMode(int mode);
void PutPixel(int x, int y, int c);
void SetupPalette(void);

/***************************************************************************/
main()
   {
   SetVideoMode(19);       // 320x200 256 color VGA graphics mode
   SetupPalette();         // make a greyscale palette
   randomize();

   InitStars();            // set up starting positions

   while(bioskey(1))       // chew up any keys waiting in buffer
      bioskey(0);

   while(NOT bioskey(1))   // do until user presses a key
      {
      UpdateStars();

      /***********************/
      /* is it time to spin? */
      /***********************/
      spincycle = (spincycle+1)%SPINFREQ;
      if (spincycle==0)
         theta = (theta + direction + 360) % 360;

      /****************************************/
      /* is it time to change spin direction? */
      /****************************************/
      if (random(DIRFREQ)==0)
         direction = random(3)-1;

      /***********************************/
      /* slow things down for fast CPU's */
      /***********************************/
      for (delay=DELAY; delay>0; delay--);
      }

   bioskey(0);             // chew up that key
   SetVideoMode(3);        // return to 80x25 text mode
   return 0;
   }

/***************************************************************************/
void InitStars(void)
   {
   register int star;
   STARS *thisstar = &stars[0];

   for (star=MAXSTARS-1; star>=0; star--, thisstar++)
      InitStar(thisstar);
   }

/***************************************************************************/
void InitStar(STARS *thisstar)
   {
   /*************************************************************/
   /* use a modified bell curve distribution of velocities 0-31 */
   /*************************************************************/
   thisstar->vel   = random(16) + random(8) + random(4) + random(4);

   /************************************/
   /* delay is the inverse of velocity */
   /************************************/
   thisstar->delay = 32 - thisstar->vel;

   /***************************/
   /* pick any angle for path */
   /***************************/
   thisstar->angle = random(360);

   /*************************************************************/
   /* pick an initial distance a little way out from the center */
   /*************************************************************/
   thisstar->dist  = 10 + random(20);
   }

/***************************************************************************/
void UpdateStars(void)
   {
   register int star;
   STARS *thisstar = &stars[0];

   for (star=MAXSTARS-1; star>=0; star--, thisstar++)
      {
      /*****************************************************/
      /* one less period to wait before updating this star */
      /*****************************************************/
      thisstar->delay--;

      /**********************************/
      /* is it time to do updating yet? */
      /**********************************/
      if (thisstar->delay == 0)
         {
         /**************************/
         /* reset the delay period */
         /**************************/
         thisstar->delay = 32 - thisstar->vel;

         /*************************************/
         /* star gets faster as it approaches */
         /*************************************/
         if (((thisstar->dist%10) == 0) AND (thisstar->vel < 31))
            thisstar->vel++;

         /******************************/
         /* erase star at old position */
         /******************************/
         DrawStar(thisstar->x, thisstar->y, 0, thisstar->dist, thisstar->vel);

         /******************************/
         /* move star away from center */
         /******************************/
         thisstar->dist++;

         /**************************/
         /* calculate new position */
         /**************************/
         thisstar->x = CENTERX + (thisstar->dist * sintable[(theta+thisstar->angle)%360])/128;
         thisstar->y = CENTERY + (thisstar->dist * costable[(theta+thisstar->angle)%360])/128;

         /************************************/
         /* check if star is still on screen */
         /************************************/
         if ( (thisstar->x < 0) OR (thisstar->x >= MAXX-1) OR
              (thisstar->y < 0) OR (thisstar->y >= MAXY-1) )
            {
            /* star is off screen, reinitialize it */
            InitStar(thisstar);
            }
         else
            {
            /* star is on screen, draw it at new position */
            DrawStar(thisstar->x, thisstar->y, thisstar->vel, thisstar->dist, thisstar->vel);
            }
         }
      }
   }

/***************************************************************************/
/* draw a single star, calculate size based on distance and velocity */
/*********************************************************************/
void DrawStar(int x, int y, int c, int dist, int vel)
   {
   PutPixel(x, y, c);
   if (dist > 80-vel/2)
      PutPixel(x+1, y, c);
   if (dist > 160-vel)
      {
      PutPixel(x, y+1, c);
      PutPixel(x+1, y+1, c);
      }
   }

/***************************************************************************/
/* BIOS call to set video mode */
/*******************************/
void SetVideoMode(int mode)
   {
   asm   mov   ah, 0
   asm   mov   al, byte ptr mode
   asm   int   10h
   }

/***************************************************************************/
/* direct video access to screen to plot a single pixel */
/* destroys ax, bx, es                                  */
/********************************************************/
void PutPixel(int x, int y, int c)
   {
   asm   mov   ax, 0xa000
   asm   mov   es, ax
   asm   mov   bx, word ptr y
   asm   shl   bx, 1
   asm   mov   ax, word ptr ytable[bx]
   asm   add   ax, word ptr x
   asm   mov   bx, ax
   asm   mov   ax, word ptr c
   asm   mov   byte ptr es:[bx], al
   }

/***************************************************************************/
/* make a nice greyscale palette */
/* destroys ax, bx, cx, dx, es   */
/*********************************/
void SetupPalette(void)
   {
   asm   mov   ax, SEG palette       // get segment of palette
   asm   mov   es, ax
   asm   mov   dx, OFFSET palette    // get offset of palette
   asm   mov   bx, 0                 // start with color # 0
   asm   mov   cx, 32                // load 32 colors
   asm   mov   ax, 0x1012            // bios function 10 subfunction 12
   asm   int   0x10                  // call bios to load palette
   }

/***************************************************************************/
/* end of stars.c */






