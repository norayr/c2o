/****************************************/
/* STARS.H                              */
/* (C) Copyright 1993 David Bollinger   */
/* send comments to CIS ID# 72510,3623  */
/* compiled with Borland C++ 3.0        */
/* pre-calculated tables for stars.c    */
/* (save time & code, waste data space) */
/****************************************/

/* sin(theta) for 0-359 degrees scaled * 128 */
int sintable[360]={0,2,4,6,8,11,13,15,17,20,22,24,26,28,30,33,35,37,39,41,43,
                   45,47,50,52,54,56,58,60,62,63,65,67,69,71,73,75,77,78,80,
                   82,83,85,87,88,90,92,93,95,96,98,99,100,102,103,104,106,
                   107,108,109,110,111,113,114,115,116,116,117,118,119,120,
                   121,121,122,123,123,124,124,125,125,126,126,126,127,127,
                   127,127,127,127,127,128,127,127,127,127,127,127,127,126,
                   126,126,125,125,124,124,123,123,122,121,121,120,119,118,
                   117,116,116,115,114,113,111,110,109,108,107,106,104,103,
                   102,100,99,98,96,95,93,92,90,88,87,85,83,82,80,78,77,75,73,
                   71,69,67,65,63,62,60,58,56,54,52,50,47,45,43,41,39,37,35,
                   33,30,28,26,24,22,20,17,15,13,11,8,6,4,2,0,-2,-4,-6,-8,-11,
                   -13,-15,-17,-20,-22,-24,-26,-28,-30,-33,-35,-37,-39,-41,
                   -43,-45,-47,-50,-52,-54,-56,-58,-60,-62,-64,-65,-67,-69,
                   -71,-73,-75,-77,-78,-80,-82,-83,-85,-87,-88,-90,-92,-93,
                   -95,-96,-98,-99,-100,-102,-103,-104,-106,-107,-108,-109,
                   -110,-111,-113,-114,-115,-116,-116,-117,-118,-119,-120,
                   -121,-121,-122,-123,-123,-124,-124,-125,-125,-126,-126,
                   -126,-127,-127,-127,-127,-127,-127,-127,-128,-127,-127,
                   -127,-127,-127,-127,-127,-126,-126,-126,-125,-125,-124,
                   -124,-123,-123,-122,-121,-121,-120,-119,-118,-117,-116,
                   -116,-115,-114,-113,-111,-110,-109,-108,-107,-106,-104,
                   -103,-102,-100,-99,-98,-96,-95,-93,-92,-90,-88,-87,-85,-83,
                   -82,-80,-78,-77,-75,-73,-71,-69,-67,-65,-64,-62,-60,-58,
                   -56,-54,-52,-50,-47,-45,-43,-41,-39,-37,-35,-33,-30,-28,
                   -26,-24,-22,-20,-17,-15,-13,-11,-8,-6,-4,-2};

/* cos(theta) for 0-359 degrees scaled * 128 */
int costable[360]={128,127,127,127,127,127,127,127,126,126,126,125,125,124,
                   124,123,123,122,121,121,120,119,118,117,116,116,115,114,
                   113,111,110,109,108,107,106,104,103,102,100,99,98,96,95,93,
                   92,90,88,87,85,83,82,80,78,77,75,73,71,69,67,65,64,62,60,
                   58,56,54,52,50,47,45,43,41,39,37,35,33,30,28,26,24,22,20,
                   17,15,13,11,8,6,4,2,0,-2,-4,-6,-8,-11,-13,-15,-17,-20,-22,
                   -24,-26,-28,-30,-33,-35,-37,-39,-41,-43,-45,-47,-50,-52,
                   -54,-56,-58,-60,-62,-63,-65,-67,-69,-71,-73,-75,-77,-78,
                   -80,-82,-83,-85,-87,-88,-90,-92,-93,-95,-96,-98,-99,-100,
                   -102,-103,-104,-106,-107,-108,-109,-110,-111,-113,-114,
                   -115,-116,-116,-117,-118,-119,-120,-121,-121,-122,-123,
                   -123,-124,-124,-125,-125,-126,-126,-126,-127,-127,-127,
                   -127,-127,-127,-127,-128,-127,-127,-127,-127,-127,-127,
                   -127,-126,-126,-126,-125,-125,-124,-124,-123,-123,-122,
                   -121,-121,-120,-119,-118,-117,-116,-116,-115,-114,-113,
                   -111,-110,-109,-108,-107,-106,-104,-103,-102,-100,-99,-98,
                   -96,-95,-93,-92,-90,-88,-87,-85,-83,-82,-80,-78,-77,-75,
                   -73,-71,-69,-67,-65,-64,-62,-60,-58,-56,-54,-52,-50,-47,
                   -45,-43,-41,-39,-37,-35,-33,-30,-28,-26,-24,-22,-20,-17,
                   -15,-13,-11,-8,-6,-4,-2,0,2,4,6,8,11,13,15,17,20,22,24,26,
                   28,30,33,35,37,39,41,43,45,47,50,52,54,56,58,60,62,64,65,
                   67,69,71,73,75,77,78,80,82,83,85,87,88,90,92,93,95,96,98,
                   99,100,102,103,104,106,107,108,109,110,111,113,114,115,116,
                   116,117,118,119,120,121,121,122,123,123,124,124,125,125,
                   126,126,126,127,127,127,127,127,127,127};

/* screen memory row offsets */
unsigned int ytable[200] =
{0u,320u,640u,960u,1280u,1600u,1920u,2240u,2560u,2880u,3200u,3520u,3840u,
 4160u,4480u,4800u,5120u,5440u,5760u,6080u,6400u,6720u,7040u,7360u,7680u,
 8000u,8320u,8640u,8960u,9280u,9600u,9920u,10240u,10560u,10880u,11200u,11520u,
 11840u,12160u,12480u,12800u,13120u,13440u,13760u,14080u,14400u,14720u,15040u,
 15360u,15680u,16000u,16320u,16640u,16960u,17280u,17600u,17920u,18240u,18560u,
 18880u,19200u,19520u,19840u,20160u,20480u,20800u,21120u,21440u,21760u,22080u,
 22400u,22720u,23040u,23360u,23680u,24000u,24320u,24640u,24960u,25280u,25600u,
 25920u,26240u,26560u,26880u,27200u,27520u,27840u,28160u,28480u,28800u,29120u,
 29440u,29760u,30080u,30400u,30720u,31040u,31360u,31680u,32000u,32320u,32640u,
 32960u,33280u,33600u,33920u,34240u,34560u,34880u,35200u,35520u,35840u,36160u,
 36480u,36800u,37120u,37440u,37760u,38080u,38400u,38720u,39040u,39360u,39680u,
 40000u,40320u,40640u,40960u,41280u,41600u,41920u,42240u,42560u,42880u,43200u,
 43520u,43840u,44160u,44480u,44800u,45120u,45440u,45760u,46080u,46400u,46720u,
 47040u,47360u,47680u,48000u,48320u,48640u,48960u,49280u,49600u,49920u,50240u,
 50560u,50880u,51200u,51520u,51840u,52160u,52480u,52800u,53120u,53440u,53760u,
 54080u,54400u,54720u,55040u,55360u,55680u,56000u,56320u,56640u,56960u,57280u,
 57600u,57920u,58240u,58560u,58880u,59200u,59520u,59840u,60160u,60480u,60800u,
 61120u,61440u,61760u,62080u,62400u,62720u,63040u,63360u,63680u};

/* 32 color greyscale palette */
unsigned char palette[32*3] =
{0,0,0,2,2,2,4,4,4,6,6,6,8,8,8,10,10,10,12,12,12,14,14,14,16,16,16,18,18,18,
20,20,20,22,22,22,24,24,24,26,26,26,28,28,28,30,30,30,32,32,32,34,34,34,
36,36,36,38,38,38,40,40,40,42,42,42,44,44,44,46,46,46,48,48,48,50,50,50,
52,52,52,54,54,54,56,56,56,58,58,58,60,60,60,62,62,62};
