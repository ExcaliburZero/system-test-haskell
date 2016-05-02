# System Test [![Travis CI Status](https://api.travis-ci.org/ExcaliburZero/system-test-haskell.svg)](https://travis-ci.org/ExcaliburZero/system-test-haskell)
System Test is a Haskell application which allows you to specify and run system tests of applications. System tests can be defined in text files with the following format, see `examples/` for more examples:

```
Hello World Test
echo "Hello, World!"
Hello, World!
----
GoodBye World Test
echo "GoodBye, World!"
GoodBye, World!
```

## Usage
The System Test executable file should be run with all of the files containing system tests being passed in as arguments.

```
system-test test1.txt test2.txt
system-test tests/*.txt
```

### Test File Structure
Each test file should contain one or more system tests. Each test has a name, command, and expected output. If you have more than one test in a file, then they should be separated by a line of four dashes (`----`). An example of the structure is as follows:

```
NAME
COMMAND
EXPECTED OUTPUT
----
NAME
COMMAND
EXPECTED OUTPUT
```

## License
System Test is available under the [MIT License](https://opensource.org/licenses/MIT), see `LICENSE` for more information.
