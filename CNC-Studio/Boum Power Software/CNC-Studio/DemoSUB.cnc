


    %
:0100
(_IWC0107S MONDSCHEIBE IWA12374)

($ MACHINE("TSUGAMIBS18"))
(PROGRAMME SUB)

(LAITOND18.0)

(DEBUT OUTILS)
(T21 21 FRAISE 1 DENT D1.60)
(T22 22 MECHE A CANON D0.50)
(T23 23 FRAISE 1 DENT D5.60/D1.52)
(FIN OUTILS)

     G310        Z140.0 T2100          (_REFERENCE)
     M500                              (CODE ATTENTE)
     M501
     T2323                             (_PREP FRAISAGE)
     G0          Z40.0
     M502
     G0          Z0
     M503

     M504                              (_Frais. finition 5.70)
     G0          Z40.0
     M505
     T2100                             (_PREP FRAISAGE)
     M103 S5000
     M506
     G0          Z0
     M507
     M231                              (_Frais. extérieur)
     M508
     M230
     M509
     G0          Z40.0
     M105
     M510
     T2222                             (_PREP PERCAGE)
     G0          X15.003
     M511
     M104 S3000
     M512
     G0          Z0
     M513
(_PERCAGED0.50)
     M514
     G0          Z60.0
     M105
     M515
     G0          Z140.0
     T3100
     M599
     M30

    %
