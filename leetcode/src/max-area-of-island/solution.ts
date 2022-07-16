const maxAreaOfIslandEx = () => {
  const maxAreaOfIsland = (grid: number[][]): number => {
    const height = grid.length;
    const width = grid[0].length;
    const hash = (i: number, j: number) => i * width + j;
    const used = new Set();
    const getIslandSize = (i: number, j: number, h: number): number => {
      if (
        !used.has(h) &&
        i >= 0 &&
        i < height &&
        j >= 0 &&
        j < width &&
        grid[i][j] === 1
      ) {
        used.add(h);
        return (
          1 +
          getIslandSize(i, j + 1, hash(i, j + 1)) +
          getIslandSize(i, j - 1, hash(i, j - 1)) +
          getIslandSize(i + 1, j, hash(i + 1, j)) +
          getIslandSize(i - 1, j, hash(i - 1, j))
        );
      }
      return 0;
    };
    let biggestIslandSize = 0;
    for (let i = 0; i < height; i += 1) {
      for (let j = 0; j < width; j += 1) {
        const ijHash = hash(i, j);
        if (grid[i][j] === 1 && !used.has(ijHash)) {
          const islandSize = getIslandSize(i, j, ijHash);
          if (islandSize > biggestIslandSize) {
            biggestIslandSize = islandSize;
          }
        }
      }
    }
    return biggestIslandSize;
  };

  console.log(
    maxAreaOfIsland([
      [0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0],
      [0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0],
      [0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0],
    ]),
    6,
  );

  console.log(maxAreaOfIsland([[0, 0, 0, 0, 0, 0, 0, 0]]), 0);

  console.log(maxAreaOfIsland([[1]]), 1);

  const map = new Array(50)
    .fill(0)
    .map(() => new Array(50).fill(0).map(() => Math.round(Math.random())));
  console.log(maxAreaOfIsland(map));
};

maxAreaOfIslandEx();
