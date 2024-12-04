#!/bin/sh
# Usage: ./new_day.sh 2024 01
BASEDIR=$(dirname "$0")

YEAR=$1
DAY=$2

MAIN_DIR="$BASEDIR/src/main/scala/adventofscala"

NEW_SRC_PATH="$MAIN_DIR/y$YEAR/Day$DAY.scala"
if [ -f $NEW_SRC_PATH ]; then
   echo "$NEW_SRC_PATH already exists."
   exit 1
fi
cp -n "$MAIN_DIR/Template.scala" $NEW_SRC_PATH
sed -i "" -e "s/y20xx/y$YEAR/g" $NEW_SRC_PATH
sed -i "" -e "s/DayXX/Day$DAY/g" $NEW_SRC_PATH
echo "Created new source file at $NEW_SRC_PATH"

TEST_DIR="$BASEDIR/src/test/scala/adventofscala"
NEW_TEST_PATH="$TEST_DIR/y$YEAR/Day${DAY}Spec.scala"
if [ -f $NEW_TEST_PATH ]; then
   echo "$NEW_TEST_PATH already exists."
   exit 1
fi
cp -n "$TEST_DIR/TemplateSpec.scala" $NEW_TEST_PATH
sed -i "" -e "s/y20xx/y$YEAR/g" $NEW_TEST_PATH
sed -i "" -e "s/DayXX/Day$DAY/g" $NEW_TEST_PATH
sed -i "" -e "s/"20xx"/"$YEAR"/g" $NEW_TEST_PATH
sed -i "" -e "s/"XX"/"$DAY"/g" $NEW_TEST_PATH
echo "Created new spec file at $NEW_TEST_PATH"