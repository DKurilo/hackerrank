const searchInRotatedSortedArrayII = () => {
  const search = (nums: number[], target: number): boolean => {
    const searchPivot = (from: number, to: number): number => {
      if (to - from <= 1) {
        if (nums[from] > nums[to]) {
          return to;
        }
        return from;
      }
      const middle = Math.ceil((from + to) / 2);
      let fixedMiddle = middle;
      if (nums[from] === nums[fixedMiddle] && nums[to] === nums[fixedMiddle]) {
        while (fixedMiddle < to && nums[fixedMiddle] === nums[to]) {
          fixedMiddle += 1;
        }
      }
      if (fixedMiddle === to) {
        return searchPivot(from, middle);
      }
      if (nums[from] <= nums[fixedMiddle] && nums[fixedMiddle] <= nums[to]) {
        return from;
      }
      if (nums[fixedMiddle] >= nums[from]) {
        return searchPivot(fixedMiddle, to);
      }
      return searchPivot(from, fixedMiddle);
    };
    const pivot = searchPivot(0, nums.length - 1);

    const index = (i: number): number => (i + pivot) % nums.length;

    const searchWithPivot = (from: number, to: number): number => {
      if (to - from <= 1) {
        if (nums[index(to)] === target) {
          return to;
        }
        if (nums[index(from)] === target) {
          return from;
        }
        return -1;
      }
      const middle = Math.floor((from + to) / 2);
      if (nums[index(middle)] === target) {
        return middle;
      }
      if (nums[index(middle)] < target) {
        return searchWithPivot(middle, to);
      }
      return searchWithPivot(from, middle);
    };

    return searchWithPivot(0, nums.length - 1) >= 0;
  };

  console.log(search([2, 2, 0, 2, 2, 2, 2], 0), true);

  console.log(search([2, 2, 2, 2, 0, 2, 2], 0), true);

  console.log(search([2, 5, 6, 0, 0, 1, 2], 0), true);

  console.log(search([2, 5, 6, 0, 0, 1, 2], 3), false);

  console.log(search([1, 0, 1, 1, 1], 0), true);

  console.log(search([1, 3, 5], 1), true);
  console.log(search([5, 1, 3], 1), true);
  console.log(search([3, 5, 1], 1), true);
};

searchInRotatedSortedArrayII();
