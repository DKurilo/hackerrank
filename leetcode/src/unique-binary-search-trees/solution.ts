const uniqueBinarySearchTrees = () => {
  const numTrees = (n: number): number => {
    const memo: Record<number, number> = {};
    const doer = (m: number): number => {
      if (m <= 1) {
        return 1;
      }
      if (memo[m] !== undefined) {
        return memo[m];
      }
      let count = 0;
      for (let i = 0; i < Math.floor(m / 2); i += 1) {
        count += 2 * doer(i) * doer(m - i - 1);
      }
      if (m % 2 === 1) {
        count += doer(Math.floor(m / 2)) ** 2;
      }
      memo[m] = count;
      return memo[m];
    };

    return doer(n);
  };

  console.log(numTrees(3), 5);

  console.log(numTrees(1), 1);

  console.log(numTrees(19));
};

uniqueBinarySearchTrees();
