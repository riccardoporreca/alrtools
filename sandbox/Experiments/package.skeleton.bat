@echo off


REM Required subdirectories

mkdir R
mkdir data
mkdir demo
mkdir exec
mkdir inst
mkdir man
mkdir po
mkdir src
mkdir tests

REM REM The following should be used when installation requires compilation
REM REM Or when it is otherwise difficult
REM
REM   echo.>>configure
REM   echo.>>cleanup
REM   echo.>>INSTALL

REM Required files
REM Some of these are technically optional
REM But it is good practice to include all of them

echo.>>DESCRIPTION
echo.>>INDEX
echo.>>NAMESPACE
echo.>>LICENSE
echo.>>LICENCE
echo.>>NEWS
echo.>>README
echo.>>ChangeLog