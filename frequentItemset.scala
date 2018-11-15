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

		var item = 0;
		var indexMap = 0;
		var frequentItems: Map[Int, Int] = Map()
		for(item <- counts){
			if(item >= s ){
				frequentItems = frequentItems + (indexMap -> item)
				indexMap += 1;
			}
			item += 1;
		}

		
		frequentItems =	aPriori(frequentItems, k, s)
	
	}

	def aPriori(frequentItems: Map[Int, Int], k: Int, s: Int) {
		//fetch each basket from the data file
		val fileName: String = "dataset/testDat.dat"
		while(k > 0) {
			for(basket <- Source.fromFile(fileName).getLines) {
				for(item <- basket.split(" ")) {
				counts(item.toInt) += 1
				}
			}
		}
	}
}