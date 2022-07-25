const binaryTreeZigzagLevelOrderTraversal = () => {
  class TreeNode {
    val: number;

    // eslint-disable-next-line no-use-before-define
    left: TreeNode | null;

    // eslint-disable-next-line no-use-before-define
    right: TreeNode | null;

    constructor(val?: number, left?: TreeNode | null, right?: TreeNode | null) {
      this.val = val === undefined ? 0 : val;
      this.left = left === undefined ? null : left;
      this.right = right === undefined ? null : right;
    }
  }

  const zigzagLevelOrder = (root: TreeNode | null): number[][] => {
    const res: number[][] = [];
    const doer = (node: TreeNode | null, depth: number): void => {
      if (node === null) {
        return;
      }
      if (res.length === depth) {
        res.push([]);
      }
      doer(node.left, depth + 1);
      if (depth % 2 === 0) {
        res[depth].push(node.val);
      } else {
        res[depth].unshift(node.val);
      }
      doer(node.right, depth + 1);
    };
    doer(root, 0);
    return res;
  };
};

binaryTreeZigzagLevelOrderTraversal();
