## Feature Support
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
|Compound assignment (`+=`) for integers|✅||
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

### Builtins
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
|`float`|✅||
|`format`|||
|`frozenset`|✅||
|`getattr`|✅||
|`globals`|✅||
|`hasattr`|||
|`hash`|✅||
|`help`|||
|`hex`|||
|`id`|||
|`input`|||
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
|`type`|✅||
|`vars`|||
|`zip`|✅||
|`__import__`|||

[Python Reference](https://docs.python.org/3/library/functions.html)

### Keywords

The Memphis lexer supports all Python keywords.

|keyword|supported?|
|-|-|
|`True`|✅|
|`None`|✅|
|`False`|✅|
|`and`|✅|
|`as`|✅|
|`assert`|✅|
|`async`|✅|
|`await`|✅|
|`break`|✅|
|`class`|✅|
|`continue`|✅|
|`def`|✅|
|`del`|✅|
|`elif`|✅|
|`else`|✅|
|`except`|✅|
|`finally`|✅|
|`for`|✅|
|`from`|✅|
|`global`|✅|
|`if`|✅|
|`import`|✅|
|`in`|✅|
|`is`|✅|
|`lambda`|✅|
|`nonlocal`|✅|
|`not`|✅|
|`or`|✅|
|`pass`|✅|
|`raise`|✅|
|`return`|✅|
|`try`|✅|
|`while`|✅|
|`with`|✅|
|`yield`|✅|

[Python Reference](https://docs.python.org/3/reference/lexical_analysis.html#keywords)

### Dunder Methods and Attributes
|method|treewalk|bytecode VM|
|-|-|-|
Object Creation and Destruction
\_\_new__(cls, [...])|✅|
\_\_init__(self, [...])|✅|
\_\_del__(self)||
Representation
\_\_repr__(self)||
\_\_str__(self)||
\_\_hash__(self)|✅|
\_\_format__(self, format_spec)
Comparison and Equality
\_\_eq__(self, other)|✅|
\_\_ne__(self, other)|✅|
\_\_lt__(self, other)|✅|
\_\_le__(self, other)|✅|
\_\_gt__(self, other)|✅|
\_\_ge__(self, other)|✅|
Numeric Operators
\_\_add__(self, other)|✅|
\_\_sub__(self, other)|✅|
\_\_mul__(self, other)|✅|
\_\_truediv__(self, other)|✅|
\_\_floordiv__(self, other)|✅|
\_\_mod__(self, other)|✅|
\_\_divmod__(self, other)||
\_\_pow__(self, other[, modulo])|✅|
\_\_lshift__(self, other)|✅|
\_\_rshift__(self, other)|✅|
\_\_and__(self, other)|✅|
\_\_or__(self, other)|✅|
\_\_xor__(self, other)|✅|
Unary Operators and Functions
\_\_neg__(self)||
\_\_pos__(self)||
\_\_abs__(self)||
\_\_invert__(self)||
Type Conversion
\_\_int__(self)||
\_\_float__(self)||
\_\_complex__(self)||
\_\_bool__(self)||
Container Types
\_\_len__(self)||
\_\_getitem__(self, key)|✅|
\_\_setitem__(self, key, value)|✅|
\_\_delitem__(self, key)|✅|
\_\_iter__(self)||
\_\_reversed__(self)||
\_\_contains__(self, item)|✅|
Attribute Access
\_\_getattr__(self, name)||
\_\_getattribute__(self, name)||
\_\_setattr__(self, name, value)||
\_\_delattr__(self, name)||
Descriptors
\_\_get__(self, instance, owner)|✅|
\_\_set__(self, instance, value)|✅|
\_\_delete__(self, instance)|✅|
Callable Objects
\_\_call__(self, [...])||
Context Managers
\_\_enter__(self)|✅|
\_\_exit__(self, exc_type, exc_value, traceback)|✅|
Instance Creation and Destruction (for classes)
\_\_init_subclass__(cls)||
\_\_instancecheck__(self, instance)||
\_\_subclasscheck__(self, subclass)||
Metaclass Methods
\_\_prepare__(cls, name, bases)||
\_\_class_getitem__(cls, item)||
Customizing Module Import
\_\_path__(self)||
\_\_loader__(self)||
\_\_package__(self)||
\_\_spec__(self)||

This list is a subset of the full spec found at the [Python Reference](https://docs.python.org/3/reference/datamodel.html).
