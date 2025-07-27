#!/bin/bash

cd jlox && javac tool/GenerateAst.java && java tool.GenerateAst . && cd -

javac jlox/*.java
