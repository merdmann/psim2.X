( echo "P,T,M,X,Y,Z,VX,VY,VZ"
  fgrep "P 1" $1 | tr -d 'P' | tr ';' ',' ) > $1.txt


