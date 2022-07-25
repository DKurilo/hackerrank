const searchA2dMatrixII = () => {
  const searchMatrixFirstIdea = (
    matrix: number[][],
    target: number,
  ): boolean => {
    const m = matrix.length;
    const n = matrix[0].length;
    const horiz = [];
    for (let i = 0; i < m; i += 1) {
      if (matrix[i][0] === target || matrix[i][n - 1] === target) {
        return true;
      }
      if (matrix[i][0] < target && matrix[i][n - 1] > target) {
        horiz.push(i);
      }
    }
    const vert = [];
    for (let i = 0; i < n; i += 1) {
      if (matrix[0][i] === target || matrix[m - 1][i] === target) {
        return true;
      }
      if (matrix[0][i] < target && matrix[m - 1][i] > target) {
        vert.push(i);
      }
    }
    const [getter, toSearchIn, last]: [
      (i: number, k: number) => number,
      number[],
      number,
    ] =
      horiz.length < vert.length
        ? [(i, k) => matrix[i][k], horiz, n - 1]
        : [(i, k) => matrix[k][i], vert, m - 1];

    const find = (i: number, from: number, to: number): boolean => {
      if (to - from <= 1) {
        return getter(i, to) === target;
      }
      const middle = Math.floor((from + to) / 2);
      const x = getter(i, middle);
      if (x === target) {
        return true;
      }
      if (x < target) {
        return find(i, middle, to);
      }
      return find(i, from, middle);
    };

    for (let i = 0; i < toSearchIn.length; i += 1) {
      if (find(toSearchIn[i], 0, last)) {
        return true;
      }
    }

    return false;
  };

  const searchMatrix = (matrix: number[][], target: number): boolean => {
    let i = 0;
    let j = matrix.length - 1;
    while (j >= 0 && i < matrix[j].length) {
      const x = matrix[j][i];
      if (x === target) {
        return true;
      }
      if (x > target) {
        j -= 1;
      } else {
        i += 1;
      }
    }
    return false;
  };

  console.log(
    searchMatrix(
      [
        [1, 4, 7, 11, 15],
        [2, 5, 8, 12, 19],
        [3, 6, 9, 16, 22],
        [10, 13, 14, 17, 24],
        [18, 21, 23, 26, 30],
      ],
      5,
    ),
    true,
  );

  console.log(
    searchMatrix(
      [
        [1, 4, 7, 11, 15],
        [2, 5, 8, 12, 19],
        [3, 6, 9, 16, 22],
        [10, 13, 14, 17, 24],
        [18, 21, 23, 26, 30],
      ],
      20,
    ),
    false,
  );

  console.log(
    searchMatrix(
      [
        [1, 2, 3, 4, 5],
        [6, 7, 8, 9, 10],
        [11, 12, 13, 14, 15],
        [16, 17, 18, 19, 20],
        [21, 22, 23, 24, 25],
      ],
      19,
    ),
    true,
  );
};

searchA2dMatrixII();
