const countOfSmallerNumbersAfterSelf = () => {
  const countSmaller = (nums: number[]): number[] => {
    const index: number[] = [];
    const find = (n: number): number => {
      if (index.length === 0 || n <= index[0]) {
        return -1;
      }
      if (n > index[index.length - 1]) {
        return index.length - 1;
      }
      const doer = (from: number, to: number): number => {
        if (to - from <= 1) {
          if (index[from] < n) {
            return from;
          }
          if (index[to] <= n) {
            return to;
          }
          return index.length;
        }
        const middle = Math.floor((from + to) / 2);
        if (index[middle] < n) {
          return doer(middle, to);
        }
        return doer(from, middle);
      };
      return doer(0, index.length - 1);
    };
    const addToIndex = (n: number): void => {
      const found = find(n);
      if (found < 0) {
        index.unshift(n);
      } else if (found >= index.length) {
        index.push(n);
      } else {
        index.splice(found + 1, 0, n);
      }
    };

    const res: number[] = new Array(nums.length).fill(0);
    for (let i = nums.length - 1; i >= 0; i -= 1) {
      const k = find(nums[i]);
      res[i] = k + 1;
      addToIndex(nums[i]);
    }
    return res;
  };

  console.log(countSmaller([5, 2, 6, 1]), [2, 1, 1, 0]);

  console.log(countSmaller([-1]), [0]);

  console.log(countSmaller([-1, -1]), [0, 0]);

  console.log(countSmaller([6, 2, 5, 1, 2, 2, 1, 8]), [6, 2, 4, 0, 1, 1, 0, 0]);

  const arr1 = [1].concat(new Array(100000).fill(0));
  console.log(countSmaller(arr1));

  const arr2 = new Array(100000)
    .fill(0)
    .map(() => Math.floor(Math.random() * 20000) - 10000);
  console.log(countSmaller(arr2));
};

countOfSmallerNumbersAfterSelf();

/**
  5 2 6 1
5 0 1 1 2
2 0 0 0 1
6 0 0 0 1
1 0 0 0 0

5 2 6 1

*/
