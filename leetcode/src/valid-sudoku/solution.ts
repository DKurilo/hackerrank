type PredicateG<T> = (x: T) => boolean;

const mkArray = (from: number, to: number) =>
  [...new Array(to - from + 1)].map((_, i) => i);

const all = <T>(ps: PredicateG<T>[]): PredicateG<T> => (x: T) =>
  ps.reduce((r, p) => r && p(x), true);

const allUniq = <T>(xs: T[]) => xs.length === new Set(xs).size;
const isBetween = <T>(l: T, h: T): PredicateG<T> => (x) => x >= l && x <= h;
const allBetween = <T>(l: T, h: T): PredicateG<T[]> => (xs) =>
  all(xs.map((x) => () => isBetween(l, h)(x)))(undefined);
const allBetween19 = allBetween<string>('1', '9');
const goodChunk: PredicateG<string[]> = all([allBetween19, allUniq]);
const notDot: PredicateG<string> = (c) => c !== '.';

const getRow = <T>(mx: T[][]) => (i: number): T[] => mx[i];
const getColumn = <T>(mx: T[][]) => (k: number): T[] => mx.map((xs) => xs[k]);
const getBlock = (rowSize: number, columnSize: number) => <T>(mx: T[][]) => (
  i: number,
  k: number,
): T[] =>
  mx
    .slice(k * columnSize, k * columnSize + columnSize)
    .map((xs) => xs.slice(i * rowSize, i * rowSize + rowSize))
    .flat();
const getBlock33 = getBlock(3, 3);

const isValidSudoku: PredicateG<string[][]> = (board) =>
  all(
    mkArray(0, 8)
      .map(getRow(board))
      .concat(mkArray(0, 8).map(getColumn(board)))
      .concat(
        mkArray(0, 2)
          .map((i) => mkArray(0, 2).map((k) => getBlock33(board)(i, k)))
          .flat(),
      )
      .map((xs) => xs.filter(notDot))
      .map((xs) => () => goodChunk(xs)),
  )(undefined);

console.log(
  isValidSudoku([
    ['5', '3', '.', '.', '7', '.', '.', '.', '.'],
    ['6', '.', '.', '1', '9', '5', '.', '.', '.'],
    ['.', '9', '8', '.', '.', '.', '.', '6', '.'],
    ['8', '.', '.', '.', '6', '.', '.', '.', '3'],
    ['4', '.', '.', '8', '.', '3', '.', '.', '1'],
    ['7', '.', '.', '.', '2', '.', '.', '.', '6'],
    ['.', '6', '.', '.', '.', '.', '2', '8', '.'],
    ['.', '.', '.', '4', '1', '9', '.', '.', '5'],
    ['.', '.', '.', '.', '8', '.', '.', '7', '9'],
  ]),
  true,
);
console.log(
  isValidSudoku([
    ['8', '3', '.', '.', '7', '.', '.', '.', '.'],
    ['6', '.', '.', '1', '9', '5', '.', '.', '.'],
    ['.', '9', '8', '.', '.', '.', '.', '6', '.'],
    ['8', '.', '.', '.', '6', '.', '.', '.', '3'],
    ['4', '.', '.', '8', '.', '3', '.', '.', '1'],
    ['7', '.', '.', '.', '2', '.', '.', '.', '6'],
    ['.', '6', '.', '.', '.', '.', '2', '8', '.'],
    ['.', '.', '.', '4', '1', '9', '.', '.', '5'],
    ['.', '.', '.', '.', '8', '.', '.', '7', '9'],
  ]),
  false,
);
