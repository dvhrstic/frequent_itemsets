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
		var singletons: Set[Set[Int]]

		for(i <- 1 to n) yield {
    			r.nextInt(n)
		}
	}

	def aPriori(frequentItems: Set[Int], k: Int, s: Int) {
		//fetch each basket from the data file
		for(line <- Source.fromFile(fileName).getLines) {
			println(line)
		}
	}
}