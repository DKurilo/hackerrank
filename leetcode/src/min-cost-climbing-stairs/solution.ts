const minCostClimbingStairsEx = () => {
  const minCostClimbingStairs = (cost: number[]): number => {
    if (cost.length <= 2) {
      return Math.min(...cost);
    }
    let cost1Step = cost[cost.length - 2];
    let cost2Step = cost[cost.length - 1];
    for (let i = cost.length - 3; i >= 0; i -= 1) {
      const newCost = cost[i] + Math.min(cost1Step, cost2Step);
      cost2Step = cost1Step;
      cost1Step = newCost;
    }
    return Math.min(cost2Step, cost1Step);
  };

  console.log(minCostClimbingStairs([10, 15, 20]), 15);

  console.log(minCostClimbingStairs([1, 100, 1, 1, 1, 100, 1, 1, 100, 1]), 6);
};

minCostClimbingStairsEx();
