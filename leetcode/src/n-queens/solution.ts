type Maybe<T> = { tag: 'Just'; value: T } | { tag: 'Nothing' };
const nothing = (): Maybe<never> => ({ tag: 'Nothing' });
const just = <T>(x: T): Maybe<T> => ({ tag: 'Just', value: x });

const Queen = Symbol('Queen');
const Empty = Symbol('Empty');
const Forbidden = Symbol('Forbidden');
type Cell = typeof Queen | typeof Empty | typeof Forbidden;
type Board = Cell[][];

const showCell = (c: Cell) => {
  switch (c) {
    case Queen:
      return 'Q';
    default:
      return '.';
  }
};
const show = (b: Board): string[] => b.map((row) => row.map(showCell).join(''));

const mkArray = (from: number, to: number): number[] =>
  to - from + 1 <= 0
    ? []
    : [...new Array(to - from + 1)].map((_, i) => i + from);

const emptyBoard = (n: number): Board =>
  new Array(n).fill(new Array(n).fill(Empty));

const addQueen = (i: number, j: number, b: Board): Maybe<Board> =>
  b.reduce((mbNewBoard, row, jc) => {
    if (mbNewBoard.tag === 'Nothing') {
      return mbNewBoard;
    }
    const mbNewRow = row.reduce((mbNR, c, ic) => {
      if (mbNR.tag === 'Nothing') {
        return nothing();
      }
      if (i === ic && j === jc) {
        if (c === Empty) {
          return just(mbNR.value.concat([Queen]));
        }
        return nothing();
      }
      if (i === ic || j === jc || i - j === ic - jc || i + j === ic + jc) {
        if (c === Empty || c === Forbidden) {
          return just(mbNR.value.concat([Forbidden]));
        }
        if (c === Queen) {
          return nothing();
        }
      }
      return just(mbNR.value.concat([c]));
    }, just([]));
    if (mbNewRow.tag === 'Nothing') {
      return nothing();
    }
    return just(mbNewBoard.value.concat([mbNewRow.value]));
  }, just([]));

const solve = (i: number, b: Board): Board[] => {
  if (i === b.length) {
    return [b];
  }
  return mkArray(0, b.length - 1)
    .map((j) => {
      const mbNewBoard = addQueen(i, j, b);
      if (mbNewBoard.tag === 'Nothing') {
        return [];
      }
      return solve(i + 1, mbNewBoard.value);
    })
    .flat();
};

const solveNQueens = (n: number): string[][] =>
  solve(0, emptyBoard(n)).map(show);

console.log(solveNQueens(4), [
  ['.Q..', '...Q', 'Q...', '..Q.'],
  ['..Q.', 'Q...', '...Q', '.Q..'],
]);
console.log(solveNQueens(1), [['Q']]);
console.log(solveNQueens(9));
