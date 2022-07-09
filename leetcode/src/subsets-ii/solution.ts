const subsetsII = () => {
  const subsetsWithDup = (nums: number[]): number[][] => {
    nums.sort((a, b) => a - b);
    const l = nums.length;
    const max = 2 ** l;
    const res = [];
    for (let i = 0; i < max; i += 1) {
      const element = [];
      let currentN: number | undefined;
      let hasHoles = false;
      for (let j = 0; j < l; j += 1) {
        // eslint-disable-next-line no-bitwise
        if (i & (2 ** j)) {
          // eslint-disable-next-line no-bitwise
          if (currentN === nums[j] && !(i & (2 ** (j - 1)))) {
            hasHoles = true;
            break;
          }
          element.push(nums[j]);
        }
        if (currentN === undefined || nums[j] !== currentN) {
          currentN = nums[j];
        }
      }
      if (!hasHoles) {
        res.push(element);
      }
    }
    return res;
  };

  console.log(subsetsWithDup([1, 2, 2]), [
    [],
    [1],
    [1, 2],
    [1, 2, 2],
    [2],
    [2, 2],
  ]);

  console.log(subsetsWithDup([0]), [[], [0]]);
};

subsetsII();
