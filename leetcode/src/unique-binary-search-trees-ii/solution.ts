const uniqueBinarySearchTreesII = () => {
  class TreeNode {
    val: number;

    left: TreeNode | null;

    right: TreeNode | null;

    constructor(val?: number, left?: TreeNode | null, right?: TreeNode | null) {
      this.val = val === undefined ? 0 : val;
      this.left = left === undefined ? null : left;
      this.right = right === undefined ? null : right;
    }
  }

  const generateTrees = (n: number): (TreeNode | null)[] => {
    const maxN = 10000;
    const hash = (from: number, to: number): number => from * maxN + to;
    const memo: Map<number, (TreeNode | null)[]> = new Map();
    const doer = (from: number, to: number): (TreeNode | null)[] => {
      const h = hash(from, to);
      if (memo.has(h)) {
        return memo.get(h);
      }
      if (from === to) {
        memo.set(h, [null]);
      } else if (to - from === 1) {
        memo.set(h, [new TreeNode(from)]);
      } else {
        const tree = [];
        for (let i = 0; i < to - from; i += 1) {
          const ltrees = doer(from, from + i);
          const rtrees = doer(i + from + 1, to);
          for (let j = 0; j < ltrees.length; j += 1) {
            for (let k = 0; k < rtrees.length; k += 1) {
              tree.push(new TreeNode(from + i, ltrees[j], rtrees[k]));
            }
          }
        }
        memo.set(h, tree);
      }
      return memo.get(h);
    };

    return doer(1, n + 1);
  };

  console.log(generateTrees(3));

  console.log(generateTrees(1));

  console.log(generateTrees(8).length);
};

uniqueBinarySearchTreesII();
