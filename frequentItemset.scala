import scala.io.Source
import util.control.Breaks._

object FrequentItemSets {

	def main(args:Array[String]){

		val fileName: String = "dataset/testDat.dat"
		//The size of the itemsets
		val k = 2
		//The support needed for frequent itemsets
		val s = 1
		//Define a list of all itemsets
		var counts: Array[Int] = Array.fill(1000)(0)


		for(basket <- Source.fromFile(fileName).getLines) {
			for(item <- basket.split(" ")) {
				counts(item.toInt) += 1
			}
		}
		counts.foreach(println)

	}

	def aPriori(frequentItems: Set[Int], k: Int, s: Int) {
		//fetch each basket from the data file
		val fileName: String = "dataset/testDat.dat"
		for(line <- Source.fromFile(fileName).getLines) {
			println(line)
		}
	}
}