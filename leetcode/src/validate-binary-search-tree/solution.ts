const validateBinarySearchTree = () => {
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

  const isValidBST = (root: TreeNode | null): boolean => {
    const doer = (node: TreeNode | null, min: number, max: number): boolean => {
      if (node === null) {
        return true;
      }
      if (node.val < min || node.val > max) {
        return false;
      }
      return doer(node.left, min, node.val) && doer(node.right, node.val, max);
    };

    return doer(root, -Infinity, Infinity);
  };
};

validateBinarySearchTree();
