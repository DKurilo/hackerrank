/* eslint-disable no-param-reassign */
/**
 Do not return anything, modify matrix in-place instead.
 */
function rotate(matrix: number[][]): void {
  const l = matrix.length - 1;
  if (l < 0) {
    return;
  }
  const iMax = Math.ceil(matrix.length / 2);
  const jMax = Math.trunc(matrix.length / 2);
  for (let i = 0; i < iMax; i += 1) {
    for (let j = 0; j < jMax; j += 1) {
      const c = matrix[j][i];
      matrix[j][i] = matrix[l - i][j];
      matrix[l - i][j] = matrix[l - j][l - i];
      matrix[l - j][l - i] = matrix[i][l - j];
      matrix[i][l - j] = c;
    }
  }
}

const m1 = [
  [1, 2, 3],
  [4, 5, 6],
  [7, 8, 9],
];
rotate(m1);
console.log(m1, [
  [7, 4, 1],
  [8, 5, 2],
  [9, 6, 3],
]);
const m2 = [
  [5, 1, 9, 11],
  [2, 4, 8, 10],
  [13, 3, 6, 7],
  [15, 14, 12, 16],
];
rotate(m2);
console.log(m2, [
  [15, 13, 2, 5],
  [14, 3, 4, 1],
  [12, 6, 8, 9],
  [16, 7, 10, 11],
]);
const m3 = [
  [1, 2],
  [3, 4],
];
rotate(m3);
console.log(m3, [
  [3, 1],
  [4, 2],
]);
const m4 = [[1]];
rotate(m4);
console.log(m4, [[1]]);
