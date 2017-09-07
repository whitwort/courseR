#!/bin/bash

for i in `cat user-list.txt`
do 
  echo adding user $i
  useradd -m -d /home/$i -s /bin/bash -g {{course-group}} $i;
done
