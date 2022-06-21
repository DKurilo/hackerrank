const searchA2DMatrix = () => {
  const searchMatrix = (matrix: number[][], target: number): boolean => {
    const m = matrix.length;
    const n = matrix[0].length;

    const xy2i = ([x, y]: [number, number]): number => x * n + y;

    const i2xy = (i: number): [number, number] => [Math.floor(i / n), i % n];

    const binarySearch = (
      from: [number, number],
      to: [number, number],
    ): [number, number] | [] => {
      const iFrom = xy2i(from);
      const iTo = xy2i(to);
      if (iTo - iFrom <= 1) {
        if (matrix[from[0]][from[1]] === target) {
          return from;
        }
        if (matrix[to[0]][to[1]] === target) {
          return to;
        }
        return [];
      }
      const middle = i2xy(Math.floor((xy2i(from) + xy2i(to)) / 2));

      if (matrix[middle[0]][middle[1]] > target) {
        return binarySearch(from, middle);
      }
      if (matrix[middle[0]][middle[1]] < target) {
        return binarySearch(middle, to);
      }
      return middle;
    };

    if (target < matrix[0][0] || target > matrix[m - 1][n - 1]) {
      return false;
    }

    return binarySearch([0, 0], [m - 1, n - 1]).length !== 0;
  };

  console.log(
    searchMatrix(
      [
        [1, 3, 5, 7],
        [10, 11, 16, 20],
        [23, 30, 34, 60],
      ],
      3,
    ),
    true,
  );

  console.log(
    searchMatrix(
      [
        [1, 3, 5, 7],
        [10, 11, 16, 20],
        [23, 30, 34, 60],
      ],
      13,
    ),
    false,
  );

  console.log(
    searchMatrix(
      [
        [1, 3, 5, 7],
        [10, 11, 16, 20],
        [23, 30, 34, 60],
      ],
      -13,
    ),
    false,
  );

  console.log(
    searchMatrix(
      [
        [1, 3, 5, 7],
        [10, 11, 16, 20],
        [23, 30, 34, 60],
      ],
      80,
    ),
    false,
  );
};

searchA2DMatrix();
