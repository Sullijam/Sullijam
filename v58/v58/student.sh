#!/bin/bash
if [[ $1 =~ ^[a-z]{4}[0-9]{4}$ ]]
then
  FIRST=${1:0:2}
  LAST=${1:2:2}
  NUMBERS=${1:4:4}
  FIRSTNAME=$FIRST$FIRST$FIRST
  LASTNAME=$LAST$LAST$LAST
  echo $FIRSTNAME $LASTNAME "  " $FIRSTNAME.$LASTNAME.$NUMBERS@student.uu.se
else
  exit 1
fi

