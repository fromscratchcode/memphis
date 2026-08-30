import init, {
  WasmRepl,
  compile as compileRaw,
  lex as lexRaw,
  parse as parseRaw,
  run as runRaw,
} from "../pkg/memphis.js";

let singletonPromise = null;
let singleton = null;

function createReplInterface(repl) {
  return {
    version() {
      return repl.version();
    },
    engine() {
      return repl.engine();
    },
    backspace() {
      repl.backspace();
    },
    currentLine() {
      return repl.current_line();
    },
    cursorIndex() {
      return repl.cursor_index();
    },
    free() {
      repl.free();
    },
    historyDown() {
      repl.history_down();
    },
    historyUp() {
      repl.history_up();
    },
    insertText(text) {
      repl.insert_text(text);
    },
    interrupt() {
      repl.interrupt();
    },
    moveLeft() {
      repl.move_left();
    },
    moveRight() {
      repl.move_right();
    },
    prompt() {
      return repl.prompt();
    },
    submit() {
      return repl.submit();
    },
    [Symbol.dispose]() {
      repl.free();
    },
  };
}

function createInterface() {
  return {
    lex(code) {
      return lexRaw(code);
    },
    parse(code) {
      return parseRaw(code);
    },
    compile(code) {
      return compileRaw(code);
    },
    run(code, { onStdout, onStderr }) {
      return runRaw(code, onStdout, onStderr);
    },
    createRepl(options = {}) {
      return createReplInterface(new WasmRepl(options.engine ?? "treewalk"));
    },
  };
}

export async function getMemphis() {
  if (singleton) {
    return singleton;
  }

  if (!singletonPromise) {
    singletonPromise = Promise.resolve(init())
      .then(() => createInterface())
      .then((instance) => {
        singleton = instance;
        return instance;
      })
      .catch((error) => {
        singletonPromise = null;
        throw new Error("Failed to initialize Memphis.", { cause: error });
      });
  }

  return singletonPromise;
}
