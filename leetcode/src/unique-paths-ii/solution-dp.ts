const uniquePathsWithObstacles = (obstacleGrid: number[][]): number => {
  const m = obstacleGrid.length;
  if (m < 1) {
    return 0;
  }
  const n = obstacleGrid[0].length;
  if (n < 1) {
    return 0;
  }
  const matrix: number[][] = [];
  for (let i = 0; i < m; i += 1) {
    matrix[i] = [];
    for (let j = 0; j < n; j += 1) {
      if (i === 0 && j === 0) {
        matrix[i][j] = obstacleGrid[i][j] === 0 ? 1 : 0;
      } else if (i === 0) {
        matrix[i][j] = obstacleGrid[i][j] === 0 ? matrix[i][j - 1] : 0;
      } else if (j === 0) {
        matrix[i][j] = obstacleGrid[i][j] === 0 ? matrix[i - 1][j] : 0;
      } else {
        matrix[i][j] =
          obstacleGrid[i][j] === 0 ? matrix[i - 1][j] + matrix[i][j - 1] : 0;
      }
    }
  }
  return matrix[m - 1][n - 1];
};

console.log(uniquePathsWithObstacles([[0]]), 1);
console.log(uniquePathsWithObstacles([[1]]), 0);
console.log(uniquePathsWithObstacles([[0, 0]]), 1);
console.log(uniquePathsWithObstacles([[0, 1]]), 0);
console.log(uniquePathsWithObstacles([[1, 0]]), 0);
console.log(uniquePathsWithObstacles([[0], [0]]), 1);
console.log(uniquePathsWithObstacles([[0], [1]]), 0);
console.log(
  uniquePathsWithObstacles([
    [0, 0, 0],
    [0, 1, 0],
    [0, 0, 0],
  ]),
  2,
);
console.log(
  uniquePathsWithObstacles([
    [0, 1],
    [0, 0],
  ]),
  1,
);
console.log(
  uniquePathsWithObstacles([
    [0, 0],
    [1, 0],
  ]),
  1,
);
console.log(
  uniquePathsWithObstacles([
    [0, 1],
    [1, 0],
  ]),
  0,
);
console.log(
  uniquePathsWithObstacles([
    [0, 0],
    [0, 1],
  ]),
  0,
);
console.log(
  uniquePathsWithObstacles(
    [...new Array(100)].map(() => [...new Array(10)].map(() => 0)),
  ),
  3911395881900,
);
console.log(
  uniquePathsWithObstacles(
    [...new Array(100)].map(() =>
      [...new Array(10)].map(() => Math.trunc(Math.random() * 1.07)),
    ),
  ),
  '<3911395881900',
);
