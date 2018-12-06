(require 'procura)
(require 'G06)
(in-package :user)

;(faz-afectacao '((L1 L2 1014 1085) (L1 L2 974 1045) (L1 L3 1024 1075) (L7 L7 656 697) (L1 L11 894 985) (L1 L1 626 655) (L1 L8 638 701) (L1 L4 922 977) (L1 L2 674 735) (L1 L2 1114 1185) (L1 L7 1196 1283) (L1 L1 26 51) (L1 L8 1238 1301) (L1 L11 1374 1465) (L1 L6 1306 1373) (L1 L11 1214 1305) (L1 L11 644 735) (L1 L1 924 950) (L1 L2 714 775) (L10 L10 664 693) (L2 L2 601 625) (L1 L6 666 733) (L1 L10 596 663) (L1 L11 1334 1425) (L1 L7 736 823) (L1 L7 742 837) (L1 L6 886 953) (L1 L10 876 943) (L1 L12 610 689) (L1 L1 902 945) (L1 L1 884 925) (L1 L5 872 967) (L1 L6 1426 1494) (L1 L8 978 1041) (L1 L10 566 633) (L1 L5 670 749) (L1 L10 446 513) (L1 L5 972 1067) (L1 L10 736 803) (L1 L8 678 741) (L1 L2 1274 1345) (L1 L5 730 809) (L1 L11 824 915) (L1 L4 1202 1257) (L1 L8 1058 1121) (L1 L4 1002 1057) (L1 L11 994 1085) (L1 L2 744 805) (L1 L5 492 587) (L1 L8 1118 1181) (L1 L11 374 465) (L5 L9 348 491) (L1 L10 1346 1413) (L1 L6 566 633) (L6 L11 329 445) (L1 L10 946 1013) (L1 L6 686 753) (L1 L10 1266 1333) (L1 L2 1054 1125) (L1 L4 522 577) (L1 L10 766 833) (L1 L12 890 944) (L8 L12 615 669) (L1 L9 908 1011) (L5 L9 328 471) (L1 L9 588 691) (L1 L7 1216 1303) (L1 L6 426 493) (L1 L2 1214 1285) (L1 L8 1198 1261) (L1 L5 1072 1167) (L1 L9 1028 1131) (L1 L5 752 847) (L1 L5 852 947) (L1 L11 634 725) (L1 L10 406 473) (L1 L2 374 445) (L1 L2 1314 1385) (L1 L2 994 1065) (L1 L2 534 595) (L1 L11 1174 1265) (L1 L7 696 783) (L1 L5 750 829) (L1 L5 732 827) (L1 L6 806 873) (L1 L9 308 411) (L1 L11 784 875) (L1 L4 1162 1217) (L1 L13 838 861) (L1 L5 1312 1407) (L1 L5 332 427) (L1 L3 1324 1375) (L1 L12 830 909) (L1 L5 1352 1447) (L1 L2 474 545) (L1 L11 494 585) (L1 L10 1386 1453) (L1 L4 622 677) (L1 L13 698 721) (L1 L8 1138 1201) (L1 L11 294 385) (L1 L2 454 525) (L1 L3 464 515) (L5 L9 288 431) (L1 L3 504 555) (L1 L11 734 825) (L1 L8 578 641) (L1 L4 942 997) (L1 L8 1098 1161) (L1 L9 968 1071) (L1 L8 398 461) (L1 L4 1122 1177) (L1 L4 1062 1117) (L1 L6 306 373) (L1 L9 568 671) (L1 L9 1168 1271) (L1 L8 998 1061) (L1 L10 986 1053) (L1 L5 772 867) (L1 L5 770 849) (L1 L10 676 743) (L1 L3 1264 1315) (L1 L10 526 593) (L1 L4 862 917) (L1 L11 714 805) (L1 L5 710 789) (L1 L2 394 465) (L1 L11 594 685) (L2 L2 581 605) (L1 L10 1066 1133) (L1 L7 796 883) (L1 L4 1262 1317) (L1 L7 1376 1464) (L1 L7 1176 1263) (L1 L10 1326 1393) (L1 L2 764 825) (L1 L11 844 935) (L1 L7 556 643) (L1 L2 1394 1465) (L1 L9 608 711) (L1 L10 466 533) (L1 L9 628 731) (L1 L5 612 707) (L1 L8 1178 1241) (L1 L4 462 517) (L1 L3 1164 1215) (L1 L6 386 453) (L1 L1 944 970) (L1 L12 850 904) (L1 L6 826 893) (L1 L10 606 673) (L1 L9 1308 1411) (L1 L2 834 896) (L1 L2 1154 1225) (L1 L11 474 565) (L1 L8 918 981) (L1 L5 810 889) (L1 L1 26 55) (L1 L2 514 575) (L1 L10 906 973) (L1 L1 14 40) (L1 L12 630 709)) "ILDS")
(faz-afectacao '((L2 L1 1 25) (L10 L1 4 33) (L1 L2 14 40) (L1 L11 14 55) (L4 L1 16 37) (L2 L1 21 45) (L1 L10 26 55) (L1 L9 28 72) (L1 L2 34 60) (L1 L10 46 75) (L10 L1 364 393) (L1 L1 394 465) (L5 L1 408 447) (L1 L1 448 551) (L1 L1 474 565) (L1 L1 512 607) (L1 L1 552 647) (L1 L1 566 633) (L1 L1 594 665) (L1 L1 608 711) (L1 L1 666 733) (L1 L1 684 735) (L1 L1 698 761) (L1 L1 722 777) (L1 L1 736 823) (L1 L1 752 847) (L1 L1 762 817) (L1 L1 776 863) (L1 L1 778 841) (L1 L1 802 857) (L1 L1 808 911) (L1 L1 814 905) (L1 L1 848 951) (L1 L1 856 943) (L1 L1 864 915) (L1 L1 866 933) (L1 L1 886 973) (L1 L1 906 953) (L1 L1 906 973) (L1 L1 934 1005) (L1 L1 944 995) (L1 L1 946 1013) (L1 L1 954 1045) (L1 L1 974 1045) (L1 L1 994 1085) (L1 L1 996 1083) (L1 L1 1018 1081) (L1 L1 1046 1113) (L1 L1 1058 1121) (L1 L1 1078 1141) (L1 L1 1082 1137) (L1 L1 1084 1135) (L1 L1 1086 1153) (L1 L1 1114 1185) (L1 L1 1122 1177) (L1 L1 1144 1195) (L1 L1 1148 1251) (L1 L1 1152 1247) (L1 L1 1168 1271) (L1 L1 1178 1241) (L1 L1 1194 1265) (L1 L1 1196 1283) (L1 L1 1224 1275) (L1 L1 1232 1327) (L1 L1 1234 1325) (L1 L1 1248 1351) (L1 L1 1272 1367) (L1 L1 1276 1363) (L1 L1 1284 1335) (L1 L1 1286 1353) (L1 L1 1318 1381) (L1 L1 1328 1431) (L1 L1 1336 1423) (L1 L1 1358 1421) (L1 L1 1382 1437) (L1 L1 1386 1453) (L1 L1 1386 1453) (L1 L2 1394 1420) (L1 L10 1406 1435) (L1 L2 1414 1440) (L1 L4 1422 1444) (L1 L5 1432 1472)) "ILDS")