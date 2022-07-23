const recoverBinarySearchTree = () => {
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

  /**
 Do not return anything, modify root in-place instead.
 */
  const recoverTree = (root: TreeNode | null): void => {
    const traverse = (node: TreeNode | null, res: TreeNode[]): void => {
      if (node === null) {
        return;
      }
      traverse(node.left, res);
      res.push(node);
      traverse(node.right, res);
    };
    const bst: TreeNode[] = [];
    traverse(root, bst);
    let n1 = 0;
    let n2 = bst.length - 1;
    while (n1 < bst.length - 1 && bst[n1].val < bst[n1 + 1].val) {
      n1 += 1;
    }
    while (n2 > 0 && bst[n2].val > bst[n2 - 1].val) {
      n2 -= 1;
    }
    const swap = bst[n1].val;
    bst[n1].val = bst[n2].val;
    bst[n2].val = swap;
  };
};

recoverBinarySearchTree();
