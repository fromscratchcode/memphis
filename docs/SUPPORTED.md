## Feature Support

This doc covers the high-level language features per engine.

For builtin functions, builtin types, and per-type method/operator inventory, see [API.md](./API.md).

**GOTCHA**: "Support" here does not mean bug free.

|Feature|treewalk|bytecode VM|
|-|-|-|
|Integer expressions|✅|✅|
|String literals|✅|✅|
|Boolean operators|✅|✅|
|Comparison operators|✅|✅|
|Logical operators|✅|✅|
|Variable assignment|✅|✅|
|Comments|✅|✅|
|REPL|✅|✅|
|Error handling|✅|✅|
|Control flow statements|✅|✅|
|Function defintion and function calls|✅|✅|
|Class definition, instatiation, and method calls|✅|✅|
|Lexical scoping|✅|✅|
|Module imports|✅|✅|
|Floating point|✅|✅|
|Negative numbers|✅|✅|
|Stack traces|✅|✅|
|Lists|✅|✅|
|List comprehension|✅||
|Sets|✅||
|Set comprehension|✅||
|Dictionaries|✅||
|Dict comprehension|✅||
|Iterables|✅|✅|
|Tuples|✅|✅|
|Ranges|✅|✅|
|Generator functions|✅|✅|
|`yield from`|✅|✅|
|Index access|✅|✅|
|Slices|✅||
|Inheritance|✅||
|Operator overloading|✅||
|Object creation and metaclasses|✅||
|async/await|✅|✅|
|Try-except blocks|✅||
|Args and kwargs|✅||
|Closures|✅|✅|
|Decorators|✅|✅|
|Descriptor protocol|✅||
|Context managers|✅||
|Compound assignment (`+=`, etc)|✅|✅|
|Comparison operator chaining|✅|✅|
|Unpacking assignment|✅|✅|
|Multiple assignment|✅||
|f-strings (without escape characters)|✅|✅|
|Class variables, class methods, and static methods|✅||
|Type hints (without enforcement)|✅||
|Exception groups|||
|Assignment expressions (`:=`)|||
|`async with` and `async for`|||
|Async generators|||
|Regular expressions|||
|Garbage collection|||
|Threading|||
|Match-case statements|||
|Monkey patching|||

### Builtin Availability

For builtin modules, builtin types, and method-level API details, see [API.md](./API.md).

|builtin|treewalk|bytecode VM|
|-|-|-|
|`abs`|||
|`aiter`|||
|`all`|||
|`anext`|||
|`any`|||
|`ascii`|||
|`bin`|||
|`bool`|✅|✅|
|`breakpoint`|||
|`bytearray`|✅||
|`bytes`|✅||
|`callable`|✅||
|`chr`|||
|`classmethod`|✅||
|`compile`|||
|`complex`|✅||
|`delattr`|||
|`dict`|✅||
|`dir`|✅||
|`divmod`|||
|`enumerate`|||
|`eval`|||
|`exec`|||
|`filter`|||
|`float`|✅|✅|
|`format`|||
|`frozenset`|✅||
|`getattr`|✅||
|`globals`|✅||
|`hasattr`|||
|`hash`|✅||
|`help`|||
|`hex`|||
|`id`|||
|`input` (but not yet via WASM)|✅|✅|
|`int`|✅|✅|
|`isinstance`|✅||
|`issubclass`|✅||
|`iter`|✅|✅|
|`len`|✅||
|`list`|✅|✅|
|`locals`|||
|`map`|||
|`max`|||
|`memoryview`|✅||
|`min`|||
|`next`|✅|✅|
|`object`|✅||
|`oct`|||
|`open`|||
|`ord`|||
|`pow`|||
|`print`|✅|✅|
|`property`|✅||
|`range`|✅|✅|
|`repr`|||
|`reversed`|✅||
|`round`|||
|`set`|✅||
|`setattr`|✅||
|`slice`|✅||
|`sorted`|✅||
|`staticmethod`|✅||
|`str`|✅||
|`sum`|||
|`super`|✅||
|`tuple`|✅|✅|
|`type`|✅|✅|
|`vars`|||
|`zip`|✅||
|`__import__`|||

[Python Reference](https://docs.python.org/3/library/functions.html)
