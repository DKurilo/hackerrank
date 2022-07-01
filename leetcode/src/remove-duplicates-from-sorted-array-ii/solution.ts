const removeDuplicatesFromSortedArrayII = () => {
  const removeDuplicates = (nums: number[]): number => {
    let i = 0;
    let k = 0;
    let cnt = 0;
    let curr;
    while (k < nums.length) {
      if (nums[k] !== curr || cnt !== 2) {
        if (nums[k] !== curr) {
          cnt = 1;
          curr = nums[k];
        } else {
          cnt = 2;
        }
        if (i !== k) {
          // eslint-disable-next-line no-param-reassign
          nums[i] = nums[k];
        }
        i += 1;
      }
      k += 1;
    }
    return i;
  };

  const ns1 = [1, 1, 1, 2, 2, 3];
  console.log(removeDuplicates(ns1), 5, ns1);

  const ns2 = [0, 0, 1, 1, 1, 1, 2, 3, 3];
  console.log(removeDuplicates(ns2), 7, ns2);
};

removeDuplicatesFromSortedArrayII();
