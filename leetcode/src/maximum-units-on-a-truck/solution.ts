const maximumUnitsOnATruck = () => {
  const maximumUnits = (
    boxTypes: [number, number][],
    truckSize: number,
  ): number => {
    boxTypes.sort((a, b) => (a[1] === b[1] ? b[0] - a[0] : b[1] - a[1]));
    let units = 0;
    for (let i = 0; i < boxTypes.length; i += 1) {
      const toLoad = Math.min(truckSize, boxTypes[i][0]);
      // eslint-disable-next-line no-param-reassign
      truckSize -= toLoad;
      units += toLoad * boxTypes[i][1];
      if (truckSize === 0) {
        break;
      }
    }
    return units;
  };

  console.log(
    maximumUnits(
      [
        [1, 3],
        [2, 2],
        [3, 1],
      ],
      4,
    ),
    8,
  );

  console.log(
    maximumUnits(
      [
        [5, 10],
        [2, 5],
        [4, 7],
        [3, 9],
      ],
      10,
    ),
    91,
  );
};

maximumUnitsOnATruck();
