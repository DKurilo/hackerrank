const minPathSum = (grid: number[][]): number => {
  const m = grid.length;
  if (m === 0) {
    return 0;
  }
  const n = grid[0].length;
  const matrix: number[][] = [];
  for (let i = 0; i < m; i += 1) {
    matrix[i] = [];
    for (let j = 0; j < n; j += 1) {
      if (i === 0 && j === 0) {
        matrix[i][j] = grid[i][j];
      } else if (i === 0) {
        matrix[i][j] = matrix[i][j - 1] + grid[i][j];
      } else if (j === 0) {
        matrix[i][j] = matrix[i - 1][j] + grid[i][j];
      } else {
        matrix[i][j] =
          Math.min(matrix[i][j - 1], matrix[i - 1][j]) + grid[i][j];
      }
    }
  }
  return matrix[m - 1][n - 1];
};

console.log(
  minPathSum([
    [1, 3, 1],
    [1, 5, 1],
    [4, 2, 1],
  ]),
  7,
);
console.log(
  minPathSum([
    [1, 2, 3],
    [4, 5, 6],
  ]),
  12,
);
