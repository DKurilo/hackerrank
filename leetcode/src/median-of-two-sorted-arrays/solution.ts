// Follow up: The overall run time complexity should be O(log (m+n)).

const findMedianElements = <T>(
  nums1: T[],
  nums2: T[],
  i1: number,
  i2: number,
): [T, T] => {
  const [v, nextNums1, nextNums2] =
    nums2.length === 0 || (nums1.length > 0 && nums1[0] < nums2[0])
      ? [nums1[0], nums1.slice(1), nums2]
      : [nums2[0], nums1, nums2.slice(1)];
  if (i1 === 0 && i2 === 0) {
    return [v, v];
  }
  if (i1 === 0) {
    const vs = findMedianElements(nextNums1, nextNums2, 0, i2 - 1);
    return [v, vs[1]];
  }
  return findMedianElements(nextNums1, nextNums2, i1 - 1, i2 - 1);
};

function findMedianSortedArrays(nums1: number[], nums2: number[]): number {
  const l2 = (nums1.length + nums2.length - 1) / 2;
  const [n1, n2] = findMedianElements(
    nums1,
    nums2,
    Math.floor(l2),
    Math.ceil(l2),
  );
  return (n1 + n2) / 2;
}

console.log(findMedianSortedArrays([1, 3], [2]), 2);
console.log(findMedianSortedArrays([1, 2], [3, 4]), 2.5);
console.log(findMedianSortedArrays([0, 0], [0, 0]), 0);
console.log(findMedianSortedArrays([], [1]), 1);
console.log(findMedianSortedArrays([2], []), 2);
const generateArray = () =>
  [...new Array(1000)].map(() => Math.random() * 2 * 1000000 - 1000000).sort();
console.log(findMedianSortedArrays(generateArray(), generateArray()));
