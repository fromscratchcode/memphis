export type MemphisEngine = "treewalk" | "bytecode_vm";

export type ReplResult =
  { type: "none" } | { type: "ok" | "err"; value: string };

export type ReplStep =
  { type: "complete"; data: ReplResult } | { type: "incomplete"; data: number };

export interface StdoutOption {
  onStdout: (chunk: string) => void;
}

export interface CreateReplOptions extends StdoutOption {
  engine?: MemphisEngine;
}

export interface RunOptions extends StdoutOption {
  onStderr: (chunk: string) => void;
}

export interface MemphisRepl {
  version(): string;
  engine(): MemphisEngine;
  backspace(): void;
  currentLine(): string;
  cursorIndex(): number;
  free(): void;
  historyDown(): void;
  historyUp(): void;
  insertText(text: string): void;
  interrupt(): void;
  moveLeft(): void;
  moveRight(): void;
  prompt(): string;
  submit(): ReplStep;
  [Symbol.dispose](): void;
}

export interface Memphis {
  lex(code: string): unknown;
  parse(code: string): unknown;
  compile(code: string): unknown;
  run(code: string, options: RunOptions): void;
  createRepl(options: CreateReplOptions): MemphisRepl;
}

export declare function getMemphis(): Promise<Memphis>;
