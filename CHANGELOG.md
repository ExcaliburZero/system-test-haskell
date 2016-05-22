# Changelog

## 0.1.2
* Fix handling of when no test files are given.
* Add changelog to package.
* Add an animated screenshot of the program to the readme file.
* Restructure the program into a library and an executable to allow for easier testing.
* Change the build tool from Cabal to Stack.
* Add system and unit tests.
* Add coverage reports through coveralls.

## 0.1.1
* Add a notice to the readme that the tests are not run in a sandbox.
* Improve the Cabal build configuration file.
* Add a check for package validity.
* Set Cabal to compile with checks for warnings.
* Fix several GHC warnings.
* Add a changelog.
* Add information on how to make a release of the program.

## 0.1.0
* Enable running of system tests.
* Print out system test results in color, based on the results of the test.
* Gives a failure status when any of the system tests fail.
* Allow system test definitions in JSON files.
