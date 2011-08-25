//=======================================================================
//$Id$
//=======================================================================
//the file contains functions descpibing specific phhysical functions for sodium

#ifndef _Na_PAHYSICAL_PARAMETERS_
#define _Na_PAHYSICAL_PARAMETERS_

#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <string>
#include <list>
#include <math.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#include <iostream>
#include <iostream>
#include <fstream>
#include <time.h>

#include <sys/time.h>
#include <sys/resource.h>

#include "Na.h"

//the solar dariation pressure as a function of heliocentric distrance and velocty
//taken from Combi-1997-icarus.pdf

//input parameters: heliocentric velociy (m/s) and heliocentric distance (m)
//return the radiation pressure (m/s^2)
//the positive velocity is in the direction out from the sun
//the acceleration is in teh direction out of the sun

double SodiumRadiationPressureAcceleration__Combi_1997_icarus(double HeliocentricVelocity,double HeliocentricDistance) {

  static const int sodiumRadiationPressure_TableLength__Combi_1997_icarus=1201;
  static const double sodiumRadiationPressure_HeliocenticVelocity_Min__Combi_1997_icarus=-60.0E3;
  static const double sodiumRadiationPressure_HeliocenticVelocity_Max__Combi_1997_icarus=60.0E3;
  static const double sodiumRadiationPressure_dHeliocenticVelocity__Combi_1997_icarus=0.01E3;

  static const double sodiumRadiationPressure_Table__Combi_1997_icarus[sodiumRadiationPressure_TableLength__Combi_1997_icarus]={
    46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,
    46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,
    46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,
    46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,
    46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,
    46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,
    46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,
    46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,
    46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,
    46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,
    46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,
    46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,
    46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,     46.4855,
    46.4064,     46.4064,     46.3362,     46.3238,     46.3238,     46.2430,     46.2430,     46.1797,     46.1656,     46.1656,     46.0883,     46.0883,     46.0391,     46.0233,     46.0233,
    45.9617,     45.9617,     45.9231,     45.9090,     45.9090,     45.8633,     45.8633,     45.8317,     45.8211,     45.8211,     45.7754,     45.7754,     45.7754,     45.7297,     45.7297,
    45.6875,     45.6717,     45.6717,     45.6031,     45.6031,     45.5469,     45.5240,     45.5240,     45.4326,     45.4326,     45.3658,     45.3377,     45.3377,     45.2374,     45.2374,
    45.1707,     45.1425,     45.1425,     45.0476,     45.0476,     44.9843,     44.9579,     44.9579,     44.8718,     44.8718,     44.8718,     44.7891,     44.7891,     44.7030,     44.7030,
    44.7030,     44.6186,     44.6186,     44.5554,     44.5272,     44.5272,     44.4375,     44.4375,     44.3672,     44.3391,     44.3391,     44.2406,     44.2406,     44.1668,     44.1369,
    44.1369,     44.0279,     44.0279,     43.9471,     43.9119,     43.9119,     43.7870,     43.7870,     43.7870,     43.6499,     43.6499,     43.5058,     43.5058,     43.5058,     43.3475,
    43.3475,     43.2316,     43.1805,     43.1805,     42.9941,     42.9941,     42.8606,     42.7972,     42.7972,     42.5827,     42.5827,     42.4352,     42.3507,     42.3507,     42.1062,
    42.1062,     41.9516,     41.8477,     41.8477,     41.5822,     41.5822,     41.5822,     41.3220,     41.3220,     41.0688,     41.0688,     41.0688,     40.8385,     40.8385,     40.6310,
    40.6310,     40.6310,     40.4553,     40.4553,     40.3147,     40.3077,     40.3077,     40.1865,     40.1865,     40.0635,     40.0793,     40.0793,     39.9757,     39.9757,     39.8703,
    39.8738,     39.8738,     39.7666,     39.7666,     39.7666,     39.6453,     39.6453,     39.5204,     39.5204,     39.5204,     39.3903,     39.3903,     39.2602,     39.2602,     39.2602,
    39.1423,     39.1423,     39.0861,     39.0333,     39.0333,     38.9419,     38.9419,     38.8962,     38.8645,     38.8645,     38.7977,     38.7977,     38.7555,     38.7432,     38.7432,
    38.6869,     38.6869,     38.6869,     38.6289,     38.6289,     38.5657,     38.5657,     38.5657,     38.4884,     38.4884,     38.3987,     38.3987,     38.3987,     38.2950,     38.2950,
    38.2107,     38.1825,     38.1825,     38.0524,     38.0524,     37.9470,     37.9083,     37.9083,     37.7571,     37.7571,     37.6376,     37.5918,     37.5918,     37.4196,     37.4196,
    37.4196,     37.2332,     37.2332,     37.0416,     37.0416,     37.0416,     36.8377,     36.8377,     36.6250,     36.6250,     36.6250,     36.4035,     36.4035,     36.1714,     36.1714,
    36.1714,     35.9323,     35.9323,     35.7531,     35.6845,     35.6845,     35.4366,     35.4366,     35.2539,     35.1834,     35.1834,     34.9320,     34.9320,     34.9320,     34.6789,
    34.6789,     34.4293,     34.4293,     34.4293,     34.1814,     34.1814,     33.9317,     33.9317,     33.9317,     33.6750,     33.6750,     33.4148,     33.4148,     33.4148,     33.1494,
    33.1494,     32.9666,     32.8733,     32.8733,     32.5902,     32.5902,     32.4005,     32.3001,     32.3001,     32.0030,     32.0030,     32.0030,     31.6971,     31.6971,     31.3876,
    31.3876,     31.3876,     31.0677,     31.0677,     30.7424,     30.7424,     30.7424,     30.4066,     30.4066,     30.0637,     30.0637,     30.0637,     29.7156,     29.7156,     29.3675,
    29.3675,     29.3675,     29.0246,     29.0246,     28.8103,     28.6817,     28.6817,     28.3459,     28.3459,     28.3459,     28.0206,     28.0206,     27.6988,     27.6988,     27.6988,
    27.3770,     27.3770,     27.0552,     27.0552,     27.0552,     26.7263,     26.7263,     26.3834,     26.3834,     26.3834,     26.0265,     26.0265,     25.6537,     25.6537,     25.6537,
    25.2651,     25.2651,     25.0191,     24.8571,     24.8571,     24.4333,     24.4333,     24.4333,     23.9938,     23.9938,     23.5366,     23.5366,     23.5366,     23.0601,     23.0601,
    22.5554,     22.5554,     22.5554,     22.0279,     22.0279,     21.4652,     21.4652,     21.4652,     20.8568,     20.8568,     20.2045,     20.2045,     20.2045,     19.4941,     19.4941,
    19.0092,     18.7275,     18.7275,     17.9047,     17.9047,     17.9047,     17.0290,     17.0290,     16.1059,     16.1059,     16.1059,     15.1529,     15.1529,     14.1770,     14.1770,
    14.1770,     13.1906,     13.1906,     12.2077,     12.2077,     12.2077,     11.2458,     11.2458,    10.30332,    10.30332,    10.30332,     9.39423,     9.39423,     8.53087,     8.53087,
    8.53087,     7.71324,     7.71324,     7.71324,     6.95540,     6.95540,     6.25737,     6.25737,     6.25737,     5.63144,     5.63144,     5.07585,     5.07585,     5.07585,     4.59587,
    4.59587,     4.18975,     4.18975,     4.18975,     3.85395,     3.85395,     3.57794,     3.57794,     3.57794,     3.35994,     3.35994,     3.18413,     3.18413,     3.18413,     3.04349,
    3.04349,     3.04349,     2.93800,     2.93800,     2.85186,     2.85186,     2.85186,     2.79034,     2.79034,     2.74464,     2.74464,     2.74464,     2.71829,     2.71829,     2.71128,
    2.71128,     2.71128,     2.72713,     2.72713,     2.76233,     2.76233,     2.76233,     2.82390,     2.82390,     2.91360,     2.91360,     2.91360,     3.03496,     3.03496,     3.18621,
    3.18621,     3.18621,     3.37615,     3.37615,     3.37615,     3.61182,     3.61182,     3.89673,     3.89673,     3.89673,     4.24319,     4.24319,     4.65471,     4.65471,     4.65471,
    5.14185,     5.14185,     5.70812,     5.70812,     5.70812,     6.35527,     6.35527,     7.07802,     7.07802,     7.07802,     7.87286,     7.87286,     8.73274,     8.73274,     8.73274,
    9.64360,     9.64360,     9.64360,     10.5914,     10.5914,     11.5655,     11.5655,     11.5655,     12.5467,     12.5467,     13.5313,     13.5313,     13.5313,     14.4966,     14.4966,
    15.4425,     15.4425,     15.4425,     16.3498,     16.3498,     17.2166,     17.2166,     17.2166,     18.0306,     18.0306,     18.7919,     18.7919,     18.7919,     19.4986,     19.4986,
    19.7117,     20.1509,     20.1509,     20.7575,     20.7575,     20.7575,     21.3183,     21.3183,     21.8423,     21.8423,     21.8423,     22.3398,     22.3398,     22.8110,     22.8110,
    22.8110,     23.2629,     23.2629,     23.6954,     23.6954,     23.6954,     24.1069,     24.1069,     24.5007,     24.5007,     24.5007,     24.8752,     24.8752,     25.0126,     25.2339,
    25.2339,     25.5786,     25.5786,     25.5786,     25.9127,     25.9127,     26.2450,     26.2450,     26.2450,     26.5738,     26.5738,     26.9009,     26.9009,     26.9009,     27.2244,
    27.2244,     27.5445,     27.5445,     27.5445,     27.8557,     27.8557,     28.1652,     28.1652,     28.1652,     28.4606,     28.4606,     28.5838,     28.7525,     28.7525,     29.0444,
    29.0444,     29.0444,     29.3293,     29.3293,     29.6142,     29.6142,     29.6142,     29.8885,     29.8885,     30.1488,     30.1488,     30.1488,     30.3809,     30.3809,     30.5779,
    30.5779,     30.5779,     30.7345,     30.7345,     30.8613,     30.8613,     30.8613,     30.9599,     30.9599,     31.0637,     31.0567,     31.0567,     31.1694,     31.1694,     31.2680,
    31.3242,     31.3242,     31.5370,     31.5370,     31.5370,     31.8218,     31.8218,     32.1734,     32.1734,     32.1734,     32.5794,     32.5794,     33.0206,     33.0206,     33.0206,
    33.4758,     33.4758,     33.9240,     33.9240,     33.9240,     34.3459,     34.3459,     34.4216,     34.7343,     34.7343,     35.0824,     35.0824,     35.1545,     35.3970,     35.3970,
    35.6853,     35.6853,     35.6853,     35.9578,     35.9578,     36.2162,     36.2162,     36.2162,     36.4693,     36.4693,     36.7225,     36.7225,     36.7225,     36.9669,     36.9669,
    37.2112,     37.2112,     37.2112,     37.4485,     37.4485,     37.5101,     37.6823,     37.6823,     37.9091,     37.9091,     37.9690,     38.1306,     38.1306,     38.3433,     38.3433,
    38.4014,     38.5525,     38.5525,     38.7547,     38.7547,     38.7547,     38.9498,     38.9498,     39.1362,     39.1362,     39.1362,     39.3173,     39.3173,     39.4966,     39.4966,
    39.4966,     39.6724,     39.6724,     39.7182,     39.8411,     39.8411,     40.0099,     40.0099,     40.0522,     40.1787,     40.1787,     40.3404,     40.3404,     40.3809,     40.5004,
    40.5004,     40.6568,     40.6568,     40.6568,     40.8062,     40.8062,     40.9521,     40.9521,     40.9521,     41.0946,     41.0946,     41.2352,     41.2352,     41.2352,     41.3794,
    41.3794,     41.4198,     41.5218,     41.5218,     41.6624,     41.6624,     41.6958,     41.8013,     41.8013,     41.9331,     41.9331,     41.9613,     42.0632,     42.0632,     42.1880,
    42.1880,     42.1880,     42.3128,     42.3128,     42.4341,     42.4341,     42.4341,     42.5624,     42.5624,     42.6942,     42.6942,     42.6942,     42.8296,     42.8296,     42.8613,
    42.9667,     42.9667,     43.1056,     43.1056,     43.1337,     43.2462,     43.2462,     43.3762,     43.3762,     43.3991,     43.4975,     43.4975,     43.6118,     43.6118,     43.6347,
    43.7225,     43.7225,     43.8297,     43.8297,     43.8297,     43.9352,     43.9352,     44.0442,     44.0442,     44.0442,     44.1549,     44.1549,     44.1796,     44.2709,     44.2709,
    44.3887,     44.3887,     44.4116,     44.4994,     44.4994,     44.6067,     44.6067,     44.6243,     44.7016,     44.7016,     44.7895,     44.7895,     44.8035,     44.8633,     44.8633,
    44.9371,     44.9371,     44.9371,     45.0004,     45.0004,     45.0636,     45.0636,     45.0636,     45.1322,     45.1322,     45.1410,     45.2007,     45.2007,     45.2710,     45.2710,
    45.2781,     45.3413,     45.3413,     45.4169,     45.4169,     45.4257,     45.4960,     45.4960,     45.5733,     45.5733,     45.5839,     45.6471,     45.6471,     45.7192,     45.7192,
    45.7280,     45.7877,     45.7877,     45.8492,     45.8492,     45.8492,     45.9037,     45.9037,     45.8931,     45.9458,     45.9458,     45.9827,     45.9827,     45.9598,     46.0090,
    46.0090,     46.0335,     46.0335,     46.0089,     46.0546,     46.0546,     46.0809,     46.0809,     46.0703,     46.1160,     46.1160,     46.1582,     46.1582,     46.1722,     46.2179,
    46.2179,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,
    46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,
    46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,
    46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,
    46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,
    46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,
    46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,
    46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,
    46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,
    46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,
    46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,
    46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,
    46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,     46.2883,
    46.2883};
  
  

  double res=0.0;
    
  if (HeliocentricVelocity<=sodiumRadiationPressure_HeliocenticVelocity_Min__Combi_1997_icarus) res=sodiumRadiationPressure_Table__Combi_1997_icarus[0];
  else if (HeliocentricVelocity>=sodiumRadiationPressure_HeliocenticVelocity_Max__Combi_1997_icarus) res=sodiumRadiationPressure_Table__Combi_1997_icarus[sodiumRadiationPressure_TableLength__Combi_1997_icarus-1];
  else {
    int level;
    double x;
      
    x=(HeliocentricVelocity-sodiumRadiationPressure_HeliocenticVelocity_Min__Combi_1997_icarus)/sodiumRadiationPressure_dHeliocenticVelocity__Combi_1997_icarus;
    level=(int)x;
    x-=level;

    res=sodiumRadiationPressure_Table__Combi_1997_icarus[level]+x*(sodiumRadiationPressure_Table__Combi_1997_icarus[level+1]-sodiumRadiationPressure_Table__Combi_1997_icarus[level]);
  }
  
  //convert the acceleration from [cm s^{-2}] to [m s^{-2}] and scale it to the required heliocentric distance 
  res*=0.01*pow(149598000000.0/HeliocentricDistance,2); 

  return res;
}


#endif

