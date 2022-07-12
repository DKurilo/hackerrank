const binaryTreeRightSideView = () => {
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
  const rightSideView = (root: TreeNode | null): number[] => {
    const visible: number[] = [];
    const traverseFromRight = (
      node: TreeNode | null,
      level: number,
      visibleLevel: number,
    ): number => {
      if (node === null) {
        return visibleLevel;
      }
      if (level >= visibleLevel) {
        visible.push(node.val);
        visibleLevel += 1;
      }
      visibleLevel = traverseFromRight(node.right, level + 1, visibleLevel);
      visibleLevel = traverseFromRight(node.left, level + 1, visibleLevel);
      return visibleLevel;
    };
    traverseFromRight(root, 0, 0);
    return visible;
  };
};

binaryTreeRightSideView();
