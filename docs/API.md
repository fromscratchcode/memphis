# Memphis Builtin API

This page lists the builtin functions and builtin type APIs currently exposed by Memphis.

For language-level feature support by engine, see [SUPPORTED.md](./SUPPORTED.md).

## Treewalk Interpreter

### Builtin Functions

| Name | Notes |
| - | - |
| `callable` | |
| `dir` | |
| `getattr` | Supports a default value |
| `setattr` | |
| `globals` | |
| `hash` | |
| `isinstance` | Supports tuples of types |
| `issubclass` | |
| `iter` | |
| `len` | |
| `next` | |
| `print` | |
| `sorted` | |

### Builtin Modules

| Module | Exposes | Notes |
| - | - | - |
| `asyncio` | `run`, `sleep`, `create_task` | |
| `time` | `sleep` | |
| `memphis.net` | `listen`, `Socket`, `Connection` | Network support |

### Builtin Types

| Type | Constructor | Operators | Methods | Notes |
| - | - | - | - | - |
| `bool` | `bool()` |  |  | |
| `int` | `int()` | `+`, `-`, `*`, `/`, `//`, `%`, `&`, `\|`, `^`, `<<`, `>>`, `**`, `<`, `<=`, `>`, `>=` |  | Coercion from `int`, `float`, `str` |
| `float` | `float()` | `+`, `-`, `*`, `/`, `<`, `<=`, `>`, `>=` |  | Coercion from `float`, `int`, `str` |
| `str` | `str()` | `+`, `*`, `<`, `in`, `[]`, slicing | `join`, `split`, `lower`, `upper`, `encode` | |
| `list` | `list()` | `+`, `[]`, slicing, item assignment, item deletion | `append`, `extend` | |
| `tuple` | `tuple()` | `[]`, slicing |  | |
| `dict` | `dict()` | `[]`, item assignment, item deletion | `get`, `items`, `keys`, `values` | |
| `set` | `set()` | `<=` | `add` | |
| `frozenset` | `frozenset()` | `in` |  | |
| `range` | `range()` | iteration |  | |
| `slice` | `slice()` |  |  | |
| `bytes` | `bytes()` |  | `decode` | Constructor support is partial |
| `bytearray` | `bytearray()` |  |  | |
| `memoryview` | `memoryview()` |  |  | |
| `complex` | `complex()` |  |  | |
| `type` | `type()` |  | `__new__` | Also exposes `__dict__` and `__mro__` |
| `object` | `object()` | `==`, `!=`, `<`, `<=`, `>`, `>=`, `+`, `-`, `*`, `/`, `in` | `__new__`, `__init__`, `__eq__`, `__ne__`, `__lt__`, `__le__`, `__gt__`, `__ge__`, `__add__`, `__sub__`, `__mul__`, `__truediv__`, `__contains__`, `__hash__`, `__str__` | Also exposes `__dict__` |
| `super` | `super()` |  |  | |
| `zip` | `zip()` | iteration |  | |
| `reversed` | `reversed()` | iteration |  | |
| `classmethod` | `classmethod()` |  |  | |
| `staticmethod` | `staticmethod()` |  |  | |
| `property` | `property()` |  |  | |
| `mappingproxy` |  | `[]` |  | Not exposed as a direct builtin constructor |
| `coroutine` |  |  | `close` | Produced by async functions |

### Exceptions

Builtin exception types include:

`BaseException`, `Exception`, `StopIteration`, `TypeError`, `ZeroDivisionError`, `RuntimeError`, `ImportError`, `LookupError`, `KeyError`, `ValueError`, `NameError`, `AttributeError`, `AssertionError`, `SyntaxError`, `IOError`

### Common Dunder Support

Treewalk currently exposes common dunder behavior in a few broad areas:

| Area | Examples |
| - | - |
| Object construction | `__new__`, `__init__` |
| Comparison | `__eq__`, `__ne__`, `__lt__`, `__le__`, `__gt__`, `__ge__` |
| Numeric operators | `__add__`, `__sub__`, `__mul__`, `__truediv__`, `__floordiv__`, `__mod__`, `__pow__`, `__and__`, `__or__`, `__xor__`, `__lshift__`, `__rshift__` |
| Container access | `__getitem__`, `__setitem__`, `__delitem__`, `__contains__` |
| Descriptors | `__get__`, `__set__`, `__delete__` |
| Context managers | `__enter__`, `__exit__` |

Some dunder behavior is exposed through builtin types and operators rather than as standalone documented methods. See the builtin type table above for the most user-facing view of current support.

## Bytecode VM

### Builtin Functions

| Name |
| - |
| `type` |
| `print` |
| `iter` |
| `next` |

### Builtin Modules

| Module | Exposes | Notes |
| - | - | - |
| `asyncio` | `run`, `create_task`, `sleep` | |

### Builtin Types

| Type | Constructor | Operators / Operations | Methods | Notes |
| - | - | - | - | - |
| `bool` | `bool()` |  |  | |
| `int` | `int()` |  |  | |
| `float` | `float()` |  |  | |
| `str` |  | `[]` |  | |
| `list` | `list()` | `[]`, item assignment |  | |
| `tuple` | `tuple()` | `[]` |  | |
| `dict` |  | `[]` |  | |
| `range` | `range()` | iteration |  | |
