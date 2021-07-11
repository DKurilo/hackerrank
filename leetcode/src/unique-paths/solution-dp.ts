const uniquePaths = (m: number, n: number): number => {
  if (m <= 1 || n <= 1) {
    return 1;
  }
  const matrix: number[][] = [];
  for (let i = 0; i < m; i += 1) {
    matrix[i] = [];
    for (let j = 0; j < n; j += 1) {
      if (i === 0 || j === 0) {
        matrix[i][j] = 1;
      } else {
        matrix[i][j] = matrix[i - 1][j] + matrix[i][j - 1];
      }
    }
  }
  return matrix[m - 1][n - 1];
};

console.log(uniquePaths(3, 7), 28);
console.log(uniquePaths(3, 2), 3);
console.log(uniquePaths(7, 3), 28);
console.log(uniquePaths(3, 3), 6);
console.log(uniquePaths(1, 1), 1);
console.log(uniquePaths(100, 10));
console.log(uniquePaths(100, 100));
