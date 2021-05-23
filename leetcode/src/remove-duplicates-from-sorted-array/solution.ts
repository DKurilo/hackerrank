function removeDuplicates(nums: number[]): number {
  if (nums.length <= 1) {
    return nums.length;
  }
  let i = 1;
  while (i < nums.length) {
    if (nums[i - 1] === nums[i]) {
      nums.splice(i, 1);
    } else {
      i += 1;
    }
  }
  return nums.length;
}

const nums1 = [1, 1, 2];
console.log(removeDuplicates(nums1), nums1);
const nums2 = [0, 0, 1, 1, 1, 2, 2, 3, 3, 4];
console.log(removeDuplicates(nums2), nums2);
