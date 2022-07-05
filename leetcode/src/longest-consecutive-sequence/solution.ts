const longestConsecutiveSequence = () => {
  const longestConsecutive = (nums: number[]): number => {
    const seqs: Record<number, number> = {};
    let longest = 0;
    for (let i = 0; i < nums.length; i += 1) {
      if (seqs[nums[i]] === undefined) {
        if (
          seqs[nums[i] - 1] !== undefined &&
          seqs[nums[i] + 1] !== undefined
        ) {
          const to1 = nums[i] - 1;
          const from1 = seqs[to1];
          const from2 = nums[i] + 1;
          const to2 = seqs[from2];
          if (from1 <= to1 && to1 < from2 && from2 <= to2) {
            seqs[from1] = to2;
            seqs[to2] = from1;
            if (to1 !== from1) {
              delete seqs[to1];
            }
            if (to2 !== from2) {
              delete seqs[from2];
            }
            longest = Math.max(to2 - from1 + 1, longest);
          }
        } else if (seqs[nums[i] - 1] !== undefined) {
          const to = nums[i] - 1;
          const from = seqs[to];
          if (to >= from) {
            seqs[from] = nums[i];
            seqs[nums[i]] = from;
            if (from !== to) {
              delete seqs[to];
            }
            longest = Math.max(nums[i] - from + 1, longest);
          }
        } else if (seqs[nums[i] + 1] !== undefined) {
          const from = nums[i] + 1;
          const to = seqs[from];
          if (to >= from) {
            seqs[to] = nums[i];
            seqs[nums[i]] = to;
            if (from !== to) {
              delete seqs[from];
            }
            longest = Math.max(to - nums[i] + 1, longest);
          }
        } else {
          seqs[nums[i]] = nums[i];
          if (longest === 0) {
            longest = 1;
          }
        }
      }
    }
    return longest;
  };

  console.log(longestConsecutive([100, 4, 200, 1, 3, 2]), 4);

  console.log(longestConsecutive([0, 3, 7, 2, 5, 8, 4, 6, 0, 1]), 9);

  console.log(longestConsecutive([1, 0, -1]), 3);

  console.log(
    longestConsecutive([
      -7, -1, 3, -9, -4, 7, -3, 2, 4, 9, 4, -9, 8, -7, 5, -1, -7,
    ]),
    4,
  );

  console.log(longestConsecutive([3, 2, 4, 4, 5]), 4);

  console.log(
    longestConsecutive([-2, -3, -3, 7, -3, 0, 5, 0, -8, -4, -1, 2]),
    5,
  );

  console.log(
    longestConsecutive([
      -6, 8, -5, 7, -9, -1, -7, -6, -9, -7, 5, 7, -1, -8, -8, -2, 0,
    ]),
    5,
  );

  console.log(longestConsecutive([1, 2, 3, 4, 2, 5]), 5);

  console.log(longestConsecutive([1, 2, 3, 4, 3, 5]), 5);

  console.log(
    longestConsecutive([
      -6, 6, -9, -7, 0, 3, 4, -2, 2, -1, 9, -9, 5, -3, 6, 1, 5, -1, -2, 9, -9,
      -4, -6, -5, 6, -1, 3,
    ]),
    14,
  );
};

longestConsecutiveSequence();
