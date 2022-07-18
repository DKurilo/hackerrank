const numberOfSubmatricesThatSumToTarget = () => {
  const numSubmatrixSumTarget = (
    matrix: number[][],
    target: number,
  ): number => {
    let res = 0;
    const prefixSums: number[][] = [];
    for (let i = 0; i < matrix.length; i += 1) {
      prefixSums.push([]);
      for (let j = 0; j < matrix[i].length; j += 1) {
        if (i === 0 && j === 0) {
          prefixSums[i].push(matrix[i][j]);
        } else if (i === 0) {
          prefixSums[i].push(prefixSums[i][j - 1] + matrix[i][j]);
        } else if (j === 0) {
          prefixSums[i].push(prefixSums[i - 1][j] + matrix[i][j]);
        } else {
          prefixSums[i].push(
            prefixSums[i - 1][j] +
              prefixSums[i][j - 1] -
              prefixSums[i - 1][j - 1] +
              matrix[i][j],
          );
        }
      }
    }
    for (let i = 0; i < matrix.length; i += 1) {
      for (let j = 0; j < matrix[i].length; j += 1) {
        for (let k = i; k < matrix.length; k += 1) {
          for (let l = j; l < matrix[k].length; l += 1) {
            const sum =
              prefixSums[k][l] -
              (j > 0 ? prefixSums[k][j - 1] : 0) -
              (i > 0 ? prefixSums[i - 1][l] : 0) +
              (i > 0 && j > 0 ? prefixSums[i - 1][j - 1] : 0);
            if (sum === target) {
              res += 1;
            }
          }
        }
      }
    }
    return res;
  };

  console.log(
    numSubmatrixSumTarget(
      [
        [0, 1, 0],
        [1, 1, 1],
        [0, 1, 0],
      ],
      0,
    ),
    4,
  );

  console.log(
    numSubmatrixSumTarget(
      [
        [1, -1],
        [-1, 1],
      ],
      0,
    ),
    5,
  );

  console.log(numSubmatrixSumTarget([[904]], 0), 0);

  console.log(
    numSubmatrixSumTarget(
      [
        [2, 3, 8, -6],
        [4, 6, -7, 3],
        [3, 15, -10, 1],
      ],
      33,
    ),
    1,
  );

  const mx = new Array(100)
    .fill(0)
    .map(() =>
      new Array(100).fill(0).map(() => Math.floor(Math.random() * 2000) - 1000),
    );
  console.log(
    numSubmatrixSumTarget(mx, Math.floor(Math.random() * 20000) - 10000),
  );
};

numberOfSubmatricesThatSumToTarget();
