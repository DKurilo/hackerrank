const minimumMovesToEqualArrayElementsII = () => {
  const minMoves2 = (nums: number[]): number => {
    nums.sort((a, b) => a - b);
    const center = Math.floor((nums.length - 1) / 2);
    const median =
      nums.length % 2 === 0
        ? Math.floor((nums[center] + nums[center + 1]) / 2)
        : nums[center];
    return nums.reduce((s, n) => s + Math.abs(n - median), 0);
  };

  console.log(minMoves2([1, 2, 3]), 2);

  console.log(minMoves2([1, 10, 2, 9]), 16);
};

minimumMovesToEqualArrayElementsII();
