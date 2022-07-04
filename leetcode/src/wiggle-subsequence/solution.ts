const wiggleSubsequence = () => {
  const wiggleMaxLength = (nums: number[]): number =>
    nums.reduce((ns: number[], n: number): number[] => {
      if (ns.length === 0) {
        ns.push(n);
        return ns;
      }
      if (ns.length === 1 && n !== ns[0]) {
        ns.push(n);
        return ns;
      }
      const prevDiff = Math.sign(ns[ns.length - 1] - ns[ns.length - 2]);
      const diff = Math.sign(n - ns[ns.length - 1]);
      if (prevDiff === diff || diff === 0) {
        ns.pop();
      }
      ns.push(n);
      return ns;
    }, []).length;

  console.log(wiggleMaxLength([1, 7, 4, 9, 2, 5]), 6);

  console.log(wiggleMaxLength([1, 17, 5, 10, 13, 15, 10, 5, 16, 8]), 7);

  console.log(wiggleMaxLength([1, 2, 3, 4, 5, 6, 7, 8, 9]), 2);

  console.log(wiggleMaxLength([3, 1, 2, 5, 4, 2, 6]), 5);

  console.log(wiggleMaxLength([0, 0]), 1);
};

wiggleSubsequence();
