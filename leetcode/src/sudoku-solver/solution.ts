const chips = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '.'] as const;
type Chip = typeof chips[number];
const size = 9;
const blockSize = 3;

const getPossible = (board: Chip[][], m: number, n: number): Chip[] => {
  const map: { [key in Chip]: number } = {
    '1': 0,
    '2': 1,
    '3': 2,
    '4': 3,
    '5': 4,
    '6': 5,
    '7': 6,
    '8': 7,
    '9': 8,
    '.': 9,
  };
  const possible = chips.slice();
  const blockM = Math.trunc(m / blockSize) * blockSize;
  const blockN = Math.trunc(n / blockSize) * blockSize;
  for (let k = 0; k < size; k += 1) {
    possible[map[board[k][n]]] = '.';
    possible[map[board[m][k]]] = '.';
    possible[
      map[board[blockM + Math.trunc(k / blockSize)][blockN + (k % blockSize)]]
    ] = '.';
  }
  return possible.filter((x) => x !== '.');
};

/**
 Do not return anything, modify board in-place instead.
 */
const solveSudoku = (board: Chip[][]): void => {
  const solver = (n: number): boolean => {
    if (n === size * size) {
      return true;
    }
    const i = Math.trunc(n / size);
    const j = n % size;
    if (board[i][j] !== '.') {
      return solver(n + 1);
    }
    const possible = getPossible(board, i, j);
    for (let k = 0; k < possible.length; k += 1) {
      // eslint-disable-next-line no-param-reassign
      board[i][j] = possible[k];
      if (solver(n + 1)) {
        return true;
      }
      // eslint-disable-next-line no-param-reassign
      board[i][j] = '.';
    }
    return false;
  };
  solver(0);
};

const sudoku: Chip[][] = [
  ['5', '3', '.', '.', '7', '.', '.', '.', '.'],
  ['6', '.', '.', '1', '9', '5', '.', '.', '.'],
  ['.', '9', '8', '.', '.', '.', '.', '6', '.'],
  ['8', '.', '.', '.', '6', '.', '.', '.', '3'],
  ['4', '.', '.', '8', '.', '3', '.', '.', '1'],
  ['7', '.', '.', '.', '2', '.', '.', '.', '6'],
  ['.', '6', '.', '.', '.', '.', '2', '8', '.'],
  ['.', '.', '.', '4', '1', '9', '.', '.', '5'],
  ['.', '.', '.', '.', '8', '.', '.', '7', '9'],
];
const solution: Chip[][] = [
  ['5', '3', '4', '6', '7', '8', '9', '1', '2'],
  ['6', '7', '2', '1', '9', '5', '3', '4', '8'],
  ['1', '9', '8', '3', '4', '2', '5', '6', '7'],
  ['8', '5', '9', '7', '6', '1', '4', '2', '3'],
  ['4', '2', '6', '8', '5', '3', '7', '9', '1'],
  ['7', '1', '3', '9', '2', '4', '8', '5', '6'],
  ['9', '6', '1', '5', '3', '7', '2', '8', '4'],
  ['2', '8', '7', '4', '1', '9', '6', '3', '5'],
  ['3', '4', '5', '2', '8', '6', '1', '7', '9'],
];
const flatSolution = solution.flat();
solveSudoku(sudoku);
sudoku.forEach((xs) => console.log(xs.join('')));
console.log(
  sudoku.flat().reduce((r, v, i) => r && v === flatSolution[i], true),
);
