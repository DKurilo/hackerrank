const uniquePathsWithObstacles = (obstacleGrid: number[][]): number => {
  const memo: Record<string, number> = {};

  const doer = (i: number, j: number, m: number, n: number): number => {
    if (obstacleGrid[i][j] !== 0) {
      return 0;
    }
    if (m === 1 && n === 1) {
      return 1;
    }
    const key = `${m}#${n}`;
    if (memo[key] === undefined) {
      memo[key] =
        (m > 1 ? doer(i + 1, j, m - 1, n) : 0) +
        (n > 1 ? doer(i, j + 1, m, n - 1) : 0);
    }
    return memo[key];
  };

  return doer(0, 0, obstacleGrid.length, obstacleGrid[0].length);
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
