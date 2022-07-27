const lowestCommonAncestorOfABinaryTree = () => {
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

  const lowestCommonAncestor = (
    root: TreeNode | null,
    p: TreeNode | null,
    q: TreeNode | null,
  ): TreeNode | null => {
    const search = (
      node: TreeNode | null,
    ): [TreeNode, TreeNode] | TreeNode | undefined => {
      if (node === null) {
        return undefined;
      }
      const foundLeft = search(node.left);
      if (Array.isArray(foundLeft)) {
        return foundLeft;
      }
      const foundRight = search(node.right);
      if (Array.isArray(foundRight)) {
        return foundRight;
      }
      const found =
        (node === p || node === q ? 1 : 0) +
        (foundLeft !== undefined ? 1 : 0) +
        (foundRight !== undefined ? 1 : 0);
      if (found > 1) {
        return [node, node];
      }
      if (node === p || node === q) {
        return node;
      }
      if (foundLeft) {
        return foundLeft;
      }
      return foundRight;
    };

    const res = search(root);
    return Array.isArray(res) ? res[0] : null;
  };
};
