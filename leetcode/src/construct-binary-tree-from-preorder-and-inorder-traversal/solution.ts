const constructBinaryTreeFromPreorderAndInorderTraversal = () => {
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

  // preorder is Root, Left, Right
  // inorder is Left, Root, Right
  // postorder is Left, Right, Root
  const buildTree = (
    preorder: number[],
    inorder: number[],
  ): TreeNode | null => {
    const searchInorder = (
      target: number,
      from: number,
      to: number,
    ): number => {
      for (let i = from; i <= to; i += 1) {
        if (inorder[i] === target) {
          return i;
        }
      }
      return -1;
    };
    const doer = (
      pFrom: number,
      pTo: number,
      iFrom: number,
      iTo: number,
    ): TreeNode | null => {
      if (pFrom > pTo) {
        return null;
      }
      const root = preorder[pFrom];
      const rootIndexInInorder = searchInorder(root, iFrom, iTo);
      const leftLength = rootIndexInInorder - iFrom;
      return new TreeNode(
        root,
        doer(pFrom + 1, pFrom + leftLength, iFrom, rootIndexInInorder - 1),
        doer(pFrom + leftLength + 1, pTo, rootIndexInInorder + 1, iTo),
      );
    };

    return doer(0, preorder.length - 1, 0, inorder.length - 1);
  };

  console.log(buildTree([3, 9, 20, 15, 7], [9, 3, 15, 20, 7]), [
    3,
    9,
    20,
    null,
    null,
    15,
    7,
  ]);

  console.log(buildTree([-1], [-1]), [-1]);
};

constructBinaryTreeFromPreorderAndInorderTraversal();
