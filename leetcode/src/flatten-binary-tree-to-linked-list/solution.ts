const flattenBinaryTreeToLinkedList = () => {
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

  /**
 Do not return anything, modify root in-place instead.
 */
  const flatten = (root: TreeNode | null): void => {
    if (root === null) {
      return;
    }

    const tempNode = new TreeNode();
    let currentAddPoint = tempNode;

    const traverse = (node: TreeNode | null): void => {
      if (node === null) {
        return;
      }
      const left = node.left;
      const right = node.right;
      currentAddPoint.right = node;
      node.left = null;
      currentAddPoint = node;
      traverse(left);
      traverse(right);
    };

    traverse(root);
  };
};
