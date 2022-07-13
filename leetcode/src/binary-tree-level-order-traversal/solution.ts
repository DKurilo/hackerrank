const binaryTreeLevelOrderTraversal = () => {
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

  const levelOrder = (root: TreeNode | null): number[][] => {
    const result: number[][] = [];

    const traverse = (node: TreeNode | null, level: number): void => {
      if (node === null) {
        return;
      }
      while (result.length <= level) {
        result.push([]);
      }
      result[level].push(node.val);
      traverse(node.left, level + 1);
      traverse(node.right, level + 1);
    };

    traverse(root, 0);
    return result;
  };
};

binaryTreeLevelOrderTraversal();
