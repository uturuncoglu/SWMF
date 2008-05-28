!^CFG COPYRIGHT UM
!
!Data are provided in electron volts (eV)
!=================================



module ModIonizPotential
  implicit none
  PRIVATE !Except
 !/////////////////////////////////////////////////////////////////////////////

!The data for the first 3 tables are taken from the book "Allen' Astrophysical Quantities, Edn iV" p. 36
!by Allen, Clabon W. Editor: Cox, Arthur N.
!Publisher: Springer (2000)

!First 10 elements - full ionizations 

real, parameter, dimension(10,10) :: cPotential10_II = reshape(  (/   &
  !   1   !    2    !    3   !    4   !   5   !   6   !   7   !   8   !   9   !    10    !
  13.59844, 0.      , 0.     , 0.     , 0.    , 0.    , 0.    , 0.    , 0.    ,  0.      ,&  ! 1 - H
  24.58741, 54.41778, 0.     , 0.     , 0.    , 0.    , 0.    , 0.    , 0.    ,  0.      ,&  ! 2 - He
  5.39172 , 75.64018, 122.454, 0.     , 0.    , 0.    , 0.    , 0.    , 0.    ,  0.      ,&  ! 3 - Li
  9.32263 , 18.21116, 153.897, 217.713, 0.    , 0.    , 0.    , 0.    , 0.    ,  0.      ,&  ! 4 - Be
  8.29803 , 25.15484, 37.931 , 259.366, 340.22, 0.    , 0.    , 0.    , 0.    ,  0.      ,&  ! 5 - B
  11.26030, 24.38332, 47.888 , 64.492 , 392.08, 489.98, 0.    , 0.    , 0.    ,  0.      ,&  ! 6 - C
  14.53414, 29.6013 , 47.449 , 77.472 , 97.89 , 552.06, 667.03, 0.    , 0.    ,  0.      ,&  ! 7 - N
  13.61806, 35.11730, 54.936 , 77.413 , 113.90, 138.12, 739.29, 871.41, 0.    ,  0.      ,&  ! 8 - O
  17.42282, 34.97082, 62.708 , 87.140 , 114.24, 157.17, 185.19, 953.91, 1103.1,  0.      ,&  ! 9 - F
  21.56454, 40.96328, 63.45  , 97.12  , 126.21, 157.93, 207.28, 239.10, 1195.8, 1362.2 /),&  !10 - Ne
                                                                                          (/10,10/))

 !Elements 11 - 20 full ionizations

 real, parameter, dimension(20,11:20) :: cPotential20_II = reshape((/&
  !   1   !    2    !    3  !    4   !    5  !   6   !   7   !    8  !   9  !   10  !
  5.13908 , 47.2864 , 71.620, 98.91  , 138.40, 172.18, 208.50, 264.25, 299.9, 1465.1,& ! 11 - Na
  1648.7  , 0.      , 0.    , 0.     , 0.    , 0.    , 0.    , 0.    , 0.   , 0.     & ! 10-s Ionization levels
                                                                                    ,&  
  7.64624 , 15.03528, 80.144, 109.265, 141.27, 186.76, 225.02, 265.96, 328.1, 367.5 ,& ! 12 - Mg
  1761.8  , 1963.   , 0.    , 0.     , 0.    , 0.    , 0.    , 0.    , 0.   , 0.     & ! 10-s
                                                                                    ,&
  5.98577 , 18.82856, 28.448, 119.99 , 153.83, 190.49, 241.76, 284.66, 330.1, 398.8 ,& ! 13 - Al
  442.0   , 2086.   , 2304. , 0.     , 0.    , 0.    , 0.    , 0.    , 0.   , 0.     & ! 10-s
                                                                                    ,&
  8.15169 , 16.34585, 33.493, 45.142 , 166.77, 205.27, 246.49, 303.54, 351.1, 401.4 ,& ! 14 - Si
  476.4   , 523.    , 2438. , 2673.  , 0.    , 0.    , 0.    , 0.    , 0.   , 0.     & ! 10-s
                                                                                    ,&
  10.48669, 19.7694 , 30.203, 51.444 , 65.03 , 220.42, 263.57, 309.60, 372.1, 424.4 ,& ! 15 - P
  479.5   , 561.    , 612.  , 2817.  , 3070. , 0.    , 0.    , 0.    , 0.   , 0.     & ! 10-s
                                                                                    ,&
  10.36001, 23.3379 , 34.79 , 47.222 , 72.59 , 88.05 , 280.95, 328.75, 379.6, 447.5 ,& ! 16 - S
  504.8   , 564.    , 652.  , 707.   , 3224. , 3494. , 0.    , 0.    , 0.   , 0.     & ! 10-s
                                                                                    ,&
  12.96764, 23.814  , 39.61 , 53.465 , 67.8  , 97.03 , 114.20, 348.28, 400.1, 455.6 ,& ! 17 - Cl
  529.3   , 592.    , 657.  , 750.   , 809.  , 3658. , 3946. , 0.    , 0.   , 0.     & ! 10-s
                                                                                    ,&
  15.75962, 27.62967, 40.74 , 59.81  , 75.02 , 91.01 , 124.32, 143.46, 422.5, 478.7 ,& ! 18 - Ar
  539.0   , 618.    , 686.  , 756.   , 855.  , 918.  , 4121. , 4426. , 0.   , 0.     & ! 10-s
                                                                                    ,&
  4.34066 , 31.63   , 45.806, 60.91  , 82.66 , 99.4  , 117.56, 154.88, 175.8, 503.8 ,& ! 19 - K
  564.7   , 629.    , 715.  , 787.   , 862.  , 968.  , 1034. , 4611. , 4934., 0.     & ! 10-s
                                                                                    ,&
  6.11316 , 11.87172, 50.913, 67.27  , 84.50 , 108.78, 127.2 , 147.24, 188.5, 211.3 ,& ! 20 - Ca
  591.9   , 657.    , 727.  , 818.   , 895.  , 972.  , 1087. , 1157. , 5129., 5470./),& !10-s
                                                                                     (/20,10/))

  !Elements 21 - 30 - full ionizations

  !   1  !    2    !   3   !   4   !   5  !   6   !   7   !   8   !   9      !  10  !
  real, parameter, dimension(30,21:30) :: cPotential30_II = reshape((/&
  6.56144, 12.79967, 24.757, 73.489, 91.65, 111.68, 138.0 , 158.1 , 180.0    , 225.2,& !    21 - Sc
  249.8  , 688.    , 757.  , 831.  , 927. , 1009. , 1094. , 1213. , 1288.    , 5675.,& ! 10-s Ionization levels
  6034.  , 0.      , 0.    , 0.    , 0.   , 0.    , 0.    , 0.    , 0.       , 0.   ,& ! 20-s Ionization levels
  6.8282 , 13.5755 , 27.492, 43.267, 99.30, 119.53, 140.8 , 170.4 , 192.1    , 215.9,& !    22 - Ti
  265.1  , 292.    , 788.  , 863.  , 941. , 1044. , 1131. , 1221. , 1346.    , 1425.,& ! 10-s
  6249.  , 6626.   , 0.    , 0.    , 0.   , 0.    , 0.    , 0.    , 0.       , 0.   ,& ! 20-s
  6.7463 , 14.66   , 29.311, 46.71 , 65.28, 128.1 , 150.6 , 173.4 , 205.8    , 230.5,& !    23 - V
  255.1  , 308.    , 336.  , 896.  , 975. , 1060. , 1168. , 1260. , 1355.    , 1486.,& ! 10-s
  1569.  , 6851.   , 7246. , 0.    , 0.   , 0.    , 0.    , 0.    , 0.       , 0.   ,& ! 20-s
  6.76664, 16.4857 , 30.96 , 49.16 , 69.46, 90.64 , 161.18, 184.7 , 209.3    , 244.4,& !    24 - Cr
  270.7  , 298.    , 355.  , 384.  , 1011., 1097. , 1185. , 1299. , 1396.    , 1496.,& ! 10-s
  1634.  , 1721.   , 7482. , 7895. , 0.   , 0.    , 0.    , 0.    , 0.       , 0.   ,& ! 20-s
  7.43402, 15.63999, 33.668, 51.2  , 72.4 , 95.6  , 119.20, 194.5 , 221.8    , 248.3,& ! 25 - Mn
  286.0  , 314.    , 344.  , 404.  , 435. , 1136. , 1224. , 1317. , 1437.    , 1539.,& ! 10-s
  1644.  , 1788.   , 1879. , 8141. , 8572., 0.    , 0.    , 0.    , 0.       , 0.   ,& ! 20-s
  7.9024 , 16.1878 , 30.652, 54.8  , 75.0 , 99.1  , 124.98, 151.06, 233.6    , 262.1,& ! 26 - Fe
  290.2  , 331.    , 361.  , 392.  , 457. , 489.  , 1266. , 1358. , 1456.    , 1582.,& ! 10-s
  1689.  , 1799.   , 1950. , 2045. , 8828., 9278. , 0.    , 0.    , 0.       , 0.   ,& ! 20-s
  7.8810 , 17.083  , 33.50 , 51.3  , 79.5 , 103.  , 131.  , 160.  , 186.2    , 276.2,& ! 27 - Co
  305.   , 336.    , 379.  , 411.  , 444. , 512.  , 547.  , 1402. , 1500.    , 1602.,& ! 10-s
  1734.  , 1846.   , 1962. , 2119. , 2218., 9544. , 10030., 0.    , 0.       , 0.   ,& ! 20-s
  7.6398 , 18.16884, 35.19 , 54.9  , 75.5 , 108.  , 134.  , 164.  , 193.     , 224.6,& ! 28 - Ni
  321.   , 352.    , 384.  , 430.  , 464. , 499.  , 571.  , 607.  , 1546.    , 1648.,& ! 10-s
  1756.  , 1894.   , 2010. , 2131. , 2295., 2398. , 10280., 10790., 0.       , 0.   ,& ! 20-s
  7.72638, 20.29240, 36.841, 55.2  , 79.9 , 103.  , 139.  , 167.  , 199.     , 232. ,& ! 29 - Cu
  266.   , 369.    , 401.  , 435.  , 484. , 520.  , 557.  , 633.  , 671.     , 1698.,& ! 10-s
  1804.  , 1919.   , 2060. , 2182. , 2310., 2478. , 2560. , 11050., 11567.617, 0.   ,& ! 20-s  
  9.39405, 17.96440, 39.723, 59.4  , 82.6 , 108.  , 136.  , 175.  , 203.     , 238. ,& ! 30 - Zn
  274.   , 311.    , 412.  , 454.  , 490. , 542.  , 579.  , 619.  , 698.     , 738. ,& ! 10-s
  1856.  , 1970.   , 2088. , 2234. , 2363., 2495. , 2664.4, 2781.7, 11864.8  , 12389./), (/30,10/))! 20-s  
  !Cu: 29th potential is taken from: David R. Lide, CRC Handbook of Chemistry and Physics,(2004),p.10-187
  !Zn: 27 - 30 potentials are taken from the database sited below
 

  !  Xe (54) - full ionization
  !  The data are taken from an online data base SPECTR-W3 
  !  (online database on spectral properties of atoms and ions)
  !  Database supported by: ISTC (International Science And Technology Center);
  !  Leading Institution: Russian Federal Nuclear Center, Snezhinsk, Russia
  !  Website: http://spectr-w3.snz.ru/new/index.phtml
  !  Most of the data is based on theoretical calculations with accuracy estimated as shown
  real, parameter, dimension(54) :: cPotentialXe_I = (/&
   !    1   !    2    !    3    !   4  !   5  !    6   !    7   !   8  !   9  !   10   !
   12.129874, 21.20979, 32.12295, 46.68, 59.69, 82.    , 100.   , 120. , 204.7, 225.86,& ! error: \pm 10 for 7-10
   240.     , 263.5   , 294.4   , 325.3, 358.3, 389.6  , 420.9  , 452.2, 572.5, 607.7 ,& ! 10-s; error: \pm 10: \pm 30
   642.9    , 678.1   , 724.    , 762.4, 852.7, 857.487, 1489.67, 1491., 1587., 1684. ,& ! 20-s; error: \pm 32: \pm 84
   1781.    , 1877.   , 1937.   , 2085., 2183., 2281.  , 2548.  , 2637., 2726., 2814. ,& ! 30-s; error: \pm 89: \pm 141
   3001.    , 3093.   , 3296.   , 3386., 7224., 7491.  , 7758.  , 8024., 8617., 8899. ,& ! 40-s; error: \pm 150:\pm 445
   9330.    , 9809.24 , 40267.6 , 41298./) ! 50-s; error: \pm 467 for I51, \pm 22 for I54, no estimate for others. 


 public :: get_ioniz_potential

contains
  subroutine get_ioniz_potential( nZ, cPotential_I)
         integer,intent(in) :: nZ									  

         real,intent(out), dimension(nZ) :: cPotential_I
	   
         select case (nZ)
           case (1:10)
             cPotential_I = cPotential10_II( 1:nZ, nZ)
           case (11:20)
             cPotential_I = cPotential20_II( 1:nZ, nZ)
           case (21:30)
             cPotential_I = cPotential30_II( 1:nZ, nZ)
		   case (54)
		     cPotential_I = cPotentialXe_I
           case default
             write(*,*) "No such element found in the database"
         end select

  end subroutine get_ioniz_potential  
end module ModIonizPotential
  

