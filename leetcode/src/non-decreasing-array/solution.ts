const nonDecreasingArray = () => {
  const checkPossibility = (nums: number[]): boolean =>
    nums.slice(0, nums.length - 1).reduce((cnt, x, i) => {
      if (nums[i + 1] < x) {
        if (
          (i === 0 && i + 2 < nums.length && nums[i + 2] > nums[i + 1]) ||
          ((i === 0 || nums[i - 1] <= x) &&
            (i + 2 >= nums.length || x <= nums[i + 2])) ||
          ((i < 1 || nums[i - 1] <= nums[i + 1]) &&
            (i + 2 >= nums.length || nums[i + 1] <= nums[i + 2]))
        ) {
          return cnt + 1;
        }
        return cnt + 2;
      }
      return cnt;
    }, 0) <= 1;

  const checkPossibility1 = (nums: number[]): boolean => {
    let cnt = 0;
    for (let i = 0; i < nums.length - 1; i += 1) {
      if (nums[i + 1] < nums[i]) {
        if (
          (i === 0 && i + 2 < nums.length && nums[i + 2] > nums[i + 1]) ||
          ((i === 0 || nums[i - 1] <= nums[i]) &&
            (i + 2 >= nums.length || nums[i] <= nums[i + 2])) ||
          ((i < 1 || nums[i - 1] <= nums[i + 1]) &&
            (i + 2 >= nums.length || nums[i + 1] <= nums[i + 2]))
        ) {
          cnt += 1;
          if (cnt > 1) {
            return false;
          }
        } else {
          return false;
        }
      }
    }
    return cnt <= 1;
  };

  console.log(checkPossibility([4, 2, 3]), true);

  console.log(checkPossibility([4, 2, 5]), true);

  console.log(checkPossibility([3, 4, 5, 1]), true);

  console.log(checkPossibility([4, 2, 1]), false);

  console.log(checkPossibility([3, 4, 2, 3]), false);

  console.log(checkPossibility([3]), true);

  console.log(checkPossibility([3, 5]), true);

  console.log(checkPossibility([5, 3]), true);

  console.log(checkPossibility([5, 7, 1, 8]), true);

  console.log(checkPossibility([-1, 4, 2, 3]), true);

  console.log(checkPossibility([5, 1, 2, 3]), true);
};

nonDecreasingArray();
