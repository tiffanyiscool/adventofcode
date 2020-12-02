import scala.io.Source

object Day1 extends App {
  def twoSum(
    nums: Array[String], // our list of data
    numsIndex: Int, // where we're at in the list of data
    sum: Int, // the value we're looking to find as the sum of two elements in the list
    visitedNumbers: collection.mutable.Map[Int, Int] // elements in the list that we've already seen. used to do O(1) lookup on previous numbers. key=(num), val=(sum-num)
  ): Option[(Int, Int)] = {
    val currentNum = nums(numsIndex).toInt // the number we're currently looking at in the data
    val diff = sum - currentNum // the number we're looking for in our map
    if (visitedNumbers.contains(diff)) {
      // we found the numbers to get our desired sum. return the numbers and stop execution.
      Some((currentNum, diff))
    } else {
      // we haven't found our pair of numbers yet
      if (numsIndex+1 < nums.length) {
        // continue searching if we have more numbers in our data
        twoSum(nums, numsIndex+1, sum, visitedNumbers + (currentNum -> diff))
      } else {
        // return None if we searched our full dataset and did not find any numbers with desired sum
        None
      }
    }
  }

  def threeSum(
    nums: Array[String], // our list of data
    numsIndex: Int, // where we're at in the list of data
    sum: Int, // the value we're looking to find as the sum of two elements in the list
    visitedNumbers: collection.mutable.Map[Int, Int] // elements in the list that we've already seen. used to do O(1) lookup on previous numbers. key=(num), val=(sum-num)
  ): Option[(Int, Int, Int)] = {
    val currentNum = nums(numsIndex).toInt // the number we're currently looking at in the data
    val diff = sum - currentNum // the number we're looking for in our map
    twoSum(
      nums=nums, 
      numsIndex=numsIndex, 
      sum=diff, 
      visitedNumbers=collection.mutable.Map.empty[Int, Int]
    ) match {
      // twoSum returned two numbers, return all three numbers
      case Some(twoSumNumsTuple) => {
        Some(twoSumNumsTuple._1, twoSumNumsTuple._2, currentNum)
      }
      case None => {
        if (numsIndex+2 < nums.length) {
          // there are at least 3 numbers left in the data, keep searching.
          threeSum(nums, numsIndex+1, sum, visitedNumbers + (currentNum -> diff))
        } else {
          // there are < 3 numbers left in the data, there are no 3 numbers with desired sum.
          None
        }
      }
    }
  }

  val filename: String = "day1_data.txt"
  val data = Source.fromResource(filename).getLines.toArray
  
  // for part 1
  val twoSumResultOpt: Option[(Int, Int)] = twoSum(
    nums=data, 
    numsIndex=0, 
    sum=2020, 
    visitedNumbers=collection.mutable.Map.empty[Int,Int]
  )
  
  // for part 2
  val threeSumResultOpt: Option[(Int, Int, Int)] = threeSum(
    nums=data, 
    numsIndex=0, 
    sum=2020, 
    visitedNumbers=collection.mutable.Map.empty[Int,Int]
  )
  println(twoSumResultOpt)
  println(threeSumResultOpt)

  twoSumResultOpt.map(resultTuple => {
    println(s"(part 1) 2sum product=${resultTuple._1 * resultTuple._2}")
  })

  threeSumResultOpt.map(resultTuple => {
    println(s"(part 2) 3sum product=${resultTuple._1 * resultTuple._2 * resultTuple._3}")
  })
}