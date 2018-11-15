import scala.io.Source
import util.control.Breaks._

object FrequentItemSets {

	def main(args:Array[String]){
		//The size of the itemsets
		val k = 2
		//The support needed for frequent itemsets
		val s = 1
		//Define a list of all itemsets
		var singletons: Set[Set[Int]] = Set()
		for(i <- 0 to 999) yield {
    			singletons = singletons + Set(i)
		}
		println(singletons)
	}

	def aPriori(frequentItems: Set[Int], k: Int, s: Int) {
		//fetch each basket from the data file
		val fileName: String = "dataset/testDat.dat"
		for(line <- Source.fromFile(fileName).getLines) {
			println(line)
		}
	}
}