/* eslint-disable max-classes-per-file */
const rangeSumQueryMutable = () => {
  class NumArraySlow {
    private sums: number[] = [0];

    private nums: number[];

    constructor(nums: number[]) {
      this.nums = nums;
      for (let i = 0; i < nums.length; i += 1) {
        this.sums.push(nums[i] + this.sums[i]);
      }
    }

    update(index: number, val: number): void {
      const diff = val - this.nums[index];
      for (let i = index + 1; i < this.sums.length; i += 1) {
        this.sums[i] += diff;
      }
      this.nums[index] = val;
    }

    sumRange(left: number, right: number): number {
      return this.sums[right + 1] - this.sums[left];
    }
  }

  class NumArray {
    // SegmentTreeNode
    private sumsTree: { start: number; end: number; sum: number }[] = [];

    private nums: number[];

    constructor(nums: number[]) {
      this.nums = nums;
      this.buildTree();
    }

    private static treeChildren(i: number): [number, number] {
      return [i * 2 + 1, i * 2 + 2];
    }

    buildTree() {
      const doer = (start: number, end: number, i: number): void => {
        while (this.sumsTree.length <= i) {
          this.sumsTree.push({ start: 0, end: 0, sum: 0 });
        }
        this.sumsTree[i].start = start;
        this.sumsTree[i].end = end;
        if (start === end) {
          this.sumsTree[i].sum = this.nums[start];
          return;
        }
        const [left, right] = NumArray.treeChildren(i);
        const middle = Math.floor((start + end) / 2);
        doer(start, middle, left);
        doer(middle + 1, end, right);
        this.sumsTree[i].sum =
          this.sumsTree[left].sum + this.sumsTree[right].sum;
      };
      doer(0, this.nums.length - 1, 0);
    }

    update(index: number, val: number): void {
      const doer = (i: number): void => {
        if (this.sumsTree[i].start === this.sumsTree[i].end) {
          this.sumsTree[i].sum = val;
          return;
        }
        const [left, right] = NumArray.treeChildren(i);
        if (this.sumsTree[left].end >= index) {
          doer(left);
        } else {
          doer(right);
        }
        this.sumsTree[i].sum =
          this.sumsTree[left].sum + this.sumsTree[right].sum;
      };
      doer(0);
    }

    sumRange(from: number, to: number): number {
      const doer = (i: number): number => {
        if (this.sumsTree[i].start >= from && this.sumsTree[i].end <= to) {
          return this.sumsTree[i].sum;
        }
        const [left, right] = NumArray.treeChildren(i);
        let sum = 0;
        if (from <= this.sumsTree[left].end) {
          sum += doer(left);
        }
        if (to >= this.sumsTree[right].start) {
          sum += doer(right);
        }
        return sum;
      };
      return doer(0);
    }
  }
  /**
   * Your NumArray object will be instantiated and called as such:
   * var obj = new NumArray(nums)
   * obj.update(index,val)
   * var param_2 = obj.sumRange(left,right)
   */
  const numArray: NumArray = new NumArray([1, 3, 5]);
  console.log(numArray.sumRange(2, 2), 5);
  console.log(numArray.sumRange(0, 2), 9); // return 1 + 3 + 5 = 9
  console.log(numArray.update(1, 2), null); // nums = [1, 2, 5]
  console.log(numArray.sumRange(0, 2), 8); // return 1 + 2 + 5 = 8
};

rangeSumQueryMutable();
