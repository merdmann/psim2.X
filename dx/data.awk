BEGIN {
   print("T0;M;X1;X2;X3;V1;V2;V3");
}

{ printf("%s; %s; %s; %s; %s; %s; %s; %s\n", $2, $3, $4,$5,$6, $7,$8,$9); }

