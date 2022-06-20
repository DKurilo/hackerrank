const setMatrixZeroes = () => {
  /**
 Do not return anything, modify matrix in-place instead.
 */
  const setZeroes = (matrix: number[][]): void => {
    // This task is good for c/c++ where you don't have undefined and null for numbers.
    // For JS it's always possible to mark some cell as null, for example.
    // But I want to try to solve it when no null/undefined available.
    // 1. find first absent number.
    // There are maximum 200x200 distinct numbers in the matrix and 2^32 available numbers.
    // so just getting random number and check if it's not in the matrix is good idea.
    const m = matrix.length;
    const n = m > 0 ? matrix[0].length : 0;
    let found = false;
    let mark;
    while (found) {
      found = true;
      mark = Math.ceil(Math.random() * 2 ** 31) - 2 ** 31;
      for (let i = 0; i < m; i += 1) {
        for (let j = 0; j < n; j += 1) {
          if (matrix[i][j] === mark) {
            found = false;
            break;
          }
        }
        if (!found) {
          break;
        }
      }
    }

    // 2. mark all we need to change
    for (let i = 0; i < m; i += 1) {
      for (let j = 0; j < n; j += 1) {
        if (matrix[i][j] === 0) {
          for (let k = 0; k < m; k += 1) {
            if (matrix[k][j] !== 0) {
              // eslint-disable-next-line no-param-reassign
              matrix[k][j] = mark;
            }
          }
          for (let k = 0; k < n; k += 1) {
            if (matrix[i][k] !== 0) {
              // eslint-disable-next-line no-param-reassign
              matrix[i][k] = mark;
            }
          }
        }
      }
    }

    // 3. actualy change it to 0
    for (let i = 0; i < m; i += 1) {
      for (let j = 0; j < n; j += 1) {
        if (matrix[i][j] === mark) {
          // eslint-disable-next-line no-param-reassign
          matrix[i][j] = 0;
        }
      }
    }
  };

  const mx1 = [
    [1, 1, 1],
    [1, 0, 1],
    [1, 1, 1],
  ];
  setZeroes(mx1);
  console.log(mx1, [
    [1, 0, 1],
    [0, 0, 0],
    [1, 0, 1],
  ]);

  const mx2 = [
    [0, 1, 2, 0],
    [3, 4, 5, 2],
    [1, 3, 1, 5],
  ];
  setZeroes(mx2);
  console.log(mx2, [
    [0, 0, 0, 0],
    [0, 4, 5, 0],
    [0, 3, 1, 0],
  ]);
};

setMatrixZeroes();
