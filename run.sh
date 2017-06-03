#!/usr/bin/env bash

mvn package
cd target
java -jar game-of-life-*.jar ../gosper-glider-gun.txt
#java -jar game-of-life-*.jar ../pulsar.txt
