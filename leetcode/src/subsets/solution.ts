const subsetsEx = () => {
  const subsets = (nums: number[]): number[][] => {
    const l = nums.length;
    const max = 2 ** l;
    const res = [];
    for (let i = 0; i < max; i += 1) {
      const element = [];
      for (let j = 0; j < l; j += 1) {
        // eslint-disable-next-line no-bitwise
        if (i & (2 ** j)) {
          element.push(nums[j]);
        }
      }
      res.push(element);
    }
    return res;
  };

  console.log(subsets([1, 2, 3]), [
    [],
    [1],
    [2],
    [1, 2],
    [3],
    [1, 3],
    [2, 3],
    [1, 2, 3],
  ]);

  console.log(subsets([0]), [[], [0]]);
};

subsetsEx();
