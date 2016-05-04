# System Test [![Travis CI Status](https://api.travis-ci.org/ExcaliburZero/system-test-haskell.svg)](https://travis-ci.org/ExcaliburZero/system-test-haskell)
System Test is a Haskell application which allows you to specify and run system tests of applications. System tests can be defined in text files with the following format, see `examples/` for more examples:

```
[
  {
    "name": "Hello World Test",
    "command": "echo 'Hello, World!'",
    "expectedOutput": "Hello, World!"
  },
  {
    "name": "GoodBye World Test",
    "command": "echo 'GoodBye, World!'",
    "expectedOutput": "Hello, World!"
  }
]
```

**Note:** System Test does not run the test commands in sandboxes, so be careful of running system tests that may cause harm to your system.

## Usage
The System Test executable file should be run with all of the files containing system tests being passed in as arguments.

```
system-test test1.txt test2.txt
system-test tests/*.txt
```

### Test File Structure
The system test files are formatted using JSON. The file should contain a list of Tests, each of which should have Strings for the test name, command, and expected output.

```
[
  {
    "name": "Hello World Test",
    "command": "echo 'Hello, World!'",
    "expectedOutput": "Hello, World!"
  },
  {
    "name": "GoodBye World Test",
    "command": "echo 'GoodBye, World!'",
    "expectedOutput": "Hello, World!"
  },
  {
    "name": "MultiLine Test",
    "command": "echo '1' && echo '2'",
    "expectedOutput": "1\n2"
  }
]
```

## License
System Test is available under the [MIT License](https://opensource.org/licenses/MIT), see `LICENSE` for more information.
