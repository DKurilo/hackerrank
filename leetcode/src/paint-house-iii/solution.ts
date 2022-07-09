const paintHouseIII = () => {
  const minCost = (
    houses: number[],
    cost: number[][],
    m: number,
    n: number,
    target: number,
  ): number => {
    // Matrix init
    const minCosts: number[][][] = [];
    // house number
    for (let i = 0; i < m; i += 1) {
      minCosts.push([]);
      // neighborhood number
      for (let j = 0; j < target; j += 1) {
        // color number
        minCosts[i].push([]);
        for (let k = 0; k < n; k += 1) {
          minCosts[i][j].push(Infinity);
        }
      }
    }
    for (let i = 0; i < m; i += 1) {
      const maxTarget = Math.min(i + 1, target);
      for (let j = 0; j < maxTarget; j += 1) {
        const minColor = houses[i] === 0 ? 0 : houses[i] - 1;
        const maxColor = houses[i] === 0 ? n : houses[i];
        for (let k = minColor; k < maxColor; k += 1) {
          let minCostNewNeigh = Infinity;
          for (let l = 0; l < n && i > 0 && j > 0; l += 1) {
            if (l !== k && minCostNewNeigh > minCosts[i - 1][j - 1][l]) {
              minCostNewNeigh = minCosts[i - 1][j - 1][l];
            }
          }
          const minCostSameNeigh = i === 0 ? 0 : minCosts[i - 1][j][k];
          minCosts[i][j][k] =
            Math.min(minCostSameNeigh, minCostNewNeigh) +
            (houses[i] !== 0 ? 0 : cost[i][k]);
        }
      }
    }
    const res = Math.min(...minCosts[m - 1][target - 1]);
    return res === Infinity ? -1 : res;
  };

  console.log(
    minCost(
      [0, 0, 0, 0, 0],
      [
        [1, 10],
        [10, 1],
        [10, 1],
        [1, 10],
        [5, 1],
      ],
      5,
      2,
      3,
    ),
    9,
  );

  console.log(
    minCost(
      [0, 2, 1, 2, 0],
      [
        [1, 10],
        [10, 1],
        [10, 1],
        [1, 10],
        [5, 1],
      ],
      5,
      2,
      3,
    ),
    11,
  );

  console.log(
    minCost(
      [3, 1, 2, 3],
      [
        [1, 1, 1],
        [1, 1, 1],
        [1, 1, 1],
        [1, 1, 1],
      ],
      4,
      3,
      3,
    ),
    -1,
  );
};

paintHouseIII();
