def day1_1(input: List[Int]): Int = 
  input.groupWithSum(2020, groupSize = 2).product

def day1_2(input: List[Int]): Int = 
  input.groupWithSum(2020, groupSize = 3).product

extension (input: List[Int]) def groupWithSum(sum: Int, groupSize: Int = 2): List[Int] =
  input.combinations(groupSize).filter(_.sum == sum).next


