class ListNode {
  val: number;

  next: ListNode | null;

  constructor(val?: number, next?: ListNode | null) {
    this.val = val === undefined ? 0 : val;
    this.next = next === undefined ? null : next;
  }
}

type ListNodeF = ListNode | null;

const arrToListNode = (xs: number[]): ListNodeF =>
  xs.length === 0 ? null : new ListNode(xs[0], arrToListNode(xs.slice(1)));

const listNodeToArr = (ln: ListNodeF): number[] =>
  ln === null ? [] : [ln.val].concat(listNodeToArr(ln.next));

/**
 * Definition for singly-linked list.
 * class ListNode {
 *     val: number
 *     next: ListNode | null
 *     constructor(val?: number, next?: ListNode | null) {
 *         this.val = (val===undefined ? 0 : val)
 *         this.next = (next===undefined ? null : next)
 *     }
 * }
 */

const mergeTwoLists = (l1: ListNodeF, l2: ListNodeF): ListNodeF => {
  if (l1 === null) {
    return l2;
  }
  if (l2 === null) {
    return l1;
  }
  if (l1.val < l2.val) {
    return new ListNode(l1.val, mergeTwoLists(l1.next, l2));
  }
  return new ListNode(l2.val, mergeTwoLists(l1, l2.next));
};

console.log(
  listNodeToArr(
    mergeTwoLists(arrToListNode([1, 2, 4]), arrToListNode([1, 3, 4])),
  ),
);
console.log(listNodeToArr(mergeTwoLists(arrToListNode([]), arrToListNode([]))));
console.log(
  listNodeToArr(mergeTwoLists(arrToListNode([]), arrToListNode([0]))),
);
console.log(
  listNodeToArr(mergeTwoLists(arrToListNode([0]), arrToListNode([]))),
);
