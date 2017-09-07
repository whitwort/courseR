#!/bin/bash

echo Enter a password to assign to all students accounts:
read pass
for i in `cat user-list.txt`
do 
  echo setting password for $i
  echo $i:$pass | chpasswd
done
