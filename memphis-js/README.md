# memphis-js

`memphis-js` is the thin JavaScript wrapper for the Memphis WebAssembly build.

It unifies the three existing consumer patterns:

- [Ozark](https://github.com/fromscratchcode/ozark): inspect (`lex`, `parse`, `compile`)
- [Tupelo](https://github.com/fromscratchcode/tupelo): run the Memphis REPL (`createRepl`)
- [Shreve](https://github.com/fromscratchcode/shreve): run a script (`run`)

## Initial API

```ts
import { getMemphis } from "@fromscratchcode/memphis-js";

const memphis = await getMemphis();

memphis.lex("x = 1");
memphis.parse("x = 1");
memphis.compile("x = 1");
memphis.run("print('hi')");

const repl = memphis.createRepl({ engine: "treewalk" });
repl.insertText("1 + 1");
const step = repl.submit();
repl.free();
```

## Notes

- `run()` reflects the current Rust wasm export and uses the treewalk engine.
- `createRepl()` supports both `"treewalk"` and `"bytecode_vm"`.
- `getMemphis()` initializes the wasm module once and returns a cached runtime facade.
