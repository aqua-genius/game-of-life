#!/usr/bin/env bash

mvn package
cd target
java -jar game-of-life-*.jar ../$1 $2
