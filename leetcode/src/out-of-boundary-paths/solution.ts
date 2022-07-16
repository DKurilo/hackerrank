const outOfBoundaryPaths = () => {
  const findPaths = (
    m: number,
    n: number,
    maxMove: number,
    startRow: number,
    startColumn: number,
  ): number => {
    const modulo = 10 ** 9 + 7;
    const sum = (x: number, y: number): number => (x + y) % modulo;
    const grid: number[][] = [];
    for (let i = 0; i < m + 2; i += 1) {
      grid.push([]);
      for (let j = 0; j < n + 2; j += 1) {
        grid[i].push(0);
      }
    }
    grid[startRow + 1][startColumn + 1] = 1;
    let pathsOut = 0;
    const cond = (startRow + startColumn) % 2;
    for (let move = 0; move < maxMove; move += 1) {
      for (let i = 1; i < m + 1; i += 1) {
        const test1 = (move + i + 1) % 2;
        if (cond === test1) {
          pathsOut = sum(pathsOut, grid[i][1]);
        }
        const test2 = (move + i + n) % 2;
        if (cond === test2) {
          pathsOut = sum(pathsOut, grid[i][n]);
        }
      }
      for (let j = 1; j < n + 1; j += 1) {
        const test1 = (move + j + 1) % 2;
        if (cond === test1) {
          pathsOut = sum(pathsOut, grid[1][j]);
        }
        const test2 = (move + j + m) % 2;
        if (cond === test2) {
          pathsOut = sum(pathsOut, grid[m][j]);
        }
      }
      for (let i = 1; i < m + 1; i += 1) {
        for (let j = 1; j < n + 1; j += 1) {
          const test = (move + i + j - 1) % 2;
          if (cond === test) {
            grid[i][j] = grid[i - 1][j];
            grid[i][j] = sum(grid[i][j], grid[i + 1][j]);
            grid[i][j] = sum(grid[i][j], grid[i][j - 1]);
            grid[i][j] = sum(grid[i][j], grid[i][j + 1]);
          }
        }
      }
    }
    return pathsOut;
  };

  console.log(findPaths(2, 2, 2, 0, 0), 6);

  console.log(findPaths(1, 3, 3, 0, 1), 12);

  console.log(findPaths(1, 2, 50, 0, 0), 150);

  console.log(
    findPaths(
      50,
      50,
      50,
      Math.floor(Math.random() * 50),
      Math.floor(Math.random() * 50),
    ),
  );
};

outOfBoundaryPaths();
