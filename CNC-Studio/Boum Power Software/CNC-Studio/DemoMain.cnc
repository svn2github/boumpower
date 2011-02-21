



    %
:0100
(_IWC0107M MONDSCHEIBE IWA12374)


($ MACHINE("TSUGAMIB018"))
(<SUB=DemoSUB>)
(PROGRAMME MAIN)

(LAITON D18.0)

(DEBUT OUTILS)
(T05 03 EBAUCHE AR D16.35)

(T12 12 DECREUSEUR D1.60)
(T12 13 DECREUSEUR D1.60)
(T13 01 FACE AV D6.50, D13.60)
(T15 18  COUPER)

(T21 21 FRAISE 1 DENT D1.60)
(T22 22 MECHE A CANON D0.50)
(T23 23 FRAISE 1 DENT D5.60/D1.52)
(FIN OUTILS)

     G300        X38.0 Y38.0 Z20.0 T051500(REFERENCE)
     M8                                (ARROSAGE)
     G50 S2500                         (MAXI TR/MIN)
     G96 S200                          (LAITON)
     G28         U0 V0                 (REFERENCE)
     G0          Z-0.20

     M36                               (CONTROLE COUPE)
     M90100                            (!APPEL SUB PROGRAMME)
     M500                              (_CODE ATTENTE)

     /M99 P10
     M0                                (ARRET SI SAUT BLOC)

N10  M501                              (_TOURNAGE AR)
     T0503
     G130        Y38.0
     G96 M13
     M12
     G0          X18.10
                 Z-0.20, T1300
     G1 G99      X16.35 F0.020
                 Z0.50 F0.025
     G0          X18.10
     G97
     G28         U0

N20  T1301                             (_FACE AV)
     G96 M13
     M12
     G0          Z0.19
                 Y16.45
     G1 G99      Y13.59 W-0.020 F0.012
     G4          U0.05
     G1 G99      Z0.08 F0.010
     G4          U0.05
     G1 G99      Y13.56 Z0.065 F0.005
     G4          U0.05
     G1 G99      Y6.50 W-0.009 F0.012
     G4          U0.05
     G1 G99      Y6.40 Z0.005 F0.005
     G4          U0.05
     G1 G99      Y5.50 F0.012
                 Z-0.20 F0.025
     G97 M14
     G28         V0

N30  T0023                             (_FRAISAGED5.60/D1.52)
     G0          Z-0.20
     G97 S2500 M14
     M12
     M502
(*AV BROCHE DE REPRISE)
     M503
     G1 G99      Z0.80 F0.010
     G0          Z-0.20
     M504
(*RECUL BROCHE DE REPRISE)
     M505
     T1500

N40  T0400
     G0          Z-2.00
     G97 S0
     M50
     G28         H0
     G0          C0
     M506
(AV BROCHE DE REPRISE)
     M507
     M231
     T2121
     G1 G98      X22.00 C3.00 F1000
                 Z0.50
     G112
     G1 G42 G98  X17.427 C1.694 F100
                 X14.883 C1.694
     G2          X13.224 C2.298R0.90 F60
     G3          X0 C7.00R7.00 F100
                 X-13.40 C-2.027R7.00
     G2          X-14.032 C-3.004R0.90 F25
     G3          X-12.827 C-4.137R0.65
     G2          X-10.855 C-4.421R0.90
     G3          X0 C-7.00R7.00 F100
                 X13.979 C-0.386R7.00
     G2          X15.241 C0.423R0.90 F25
     G3          X14.883 C1.694R0.65
     G2          X13.96 C3.353R0.90
     G1 G40      X18.00 C5.00 F500
     G113
     M508
     M230
     M509
     M51                               (RECUL BROCHE DE REPRISE)
     M510

N50  T1210                             (_DECREUSEURD1.60)
     G96 M14
     M12
     G0          Z-0.20
                 Y1.76
     G1 G99      Z0 F0.010
                 Y1.60 Z0.08 F0.003
     T1211
     G1 G99      Z0.32 F0.005
                 W0.05 V0.10 F0.003
                 W0.02
                 V-0.12
     G4          U0.05
     T1210
     G0          Z-1.00
     G4          U0.10
     G97 S0
     G28         V0
     M511

N60  T0022                             (_PERCAGED0.50)
     G0          Z-0.50
     G97 S0
     M50
     G28         H0
     G0          C0
                 H10.8
     M82
     M512
(AV BROCHE DE REPRISE)
     M513
     G0          Z-0.20
     G1 G98      Z0.70 F15
     G4          U0.05
     G0          Z-0.20
     M83
     G0          H200.0
     M82
     G1 G98      Z0.70 F15
     G4          U0.05
     G0          Z-0.20
     M51
     M83
     M514
(RECUL BROCHE DE REPRISE)
     M515

N65  T0503                             (_Ebauche AR)
     G96 M13
     M12
     G0          Z0.42
                 X18.10
     G1 G99      X8.50 F0.025
     G0          X18.10
     G97
     G28         U0

N70  T150518                           (_Ebauche AR+COUPE)
     G96 M13
     M12
     G0          Z0.35
                 Y18.15 X18.12
     G121                              (SYNCHRO X ET Y)
     T1519
     G1 G99      Y1.85 W0.001 F0.010
     G120 G28    U0                    ((SYNCHRO X ET Y OFF)M17)
     G1 G99      Y1.10 F0.005
     M18
     G1 G99      Y-2.00 F0.050

N999 M95                               (_Chargement)
(& LIGNE VIDE)
     /M91
     M96
     G97 S100 M14
     M12
     M11
     G4          U0.20
     G0 T0       Z-0.05                (SUR-EPAISSEUR)
     M10
     G4          U0.50
     G0          Z-0.20
     G28         V0
     M97                               (*COMPTEUR)
     M599
     M30

    %
