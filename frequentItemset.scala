import scala.io.Source
import util.control.Breaks._

object FrequentItemSets {

	def main(args:Array[String]){

		val fileName: String = "dataset/miniTest.dat"
		//The size of the itemsets
		val k = 2
		//The support needed for frequent itemsets
		val s = 3
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
		for(i <- counts){
			if(i >= s ){
				frequentItems = frequentItems + (indexMap -> item)
				indexMap += 1;
			}
			item += 1;
		}

		//frequentItems =
		aPriori(frequentItems, k, s)
	}

	def aPriori(frequentItems: Map[Int, Int], k: Int, s: Int) {
		val fileName: String = "dataset/miniTest.dat"
		//number of frequent singletons
		val m = frequentItems.size
		//number of pairs (size of triangular array => round up)
		val numPairs: Int = Math.ceil(Math.pow(m,2)/2).toInt
		//triangular array k = (i - 1)(n - (i/2)) + j - i, counts[k] = count for pair (i, j)
		// i < j
		var counts: Array[Int] = Array.fill(numPairs)(0)

		//fetch each basket from the data file
		for(basket <- Source.fromFile(fileName).getLines) {
			var basketArray: Array[Int] = Array()
			for(item <- basket.split(" ")) {
				basketArray = basketArray :+ item.toInt
			}
			//create pairs given the frequent singletons
			for(i <- 1 to (m - 1)) {
				for(j <- (i + 1) to m) {
					val item1 = frequentItems(i - 1)
					val item2 = frequentItems(j - 1)

					if(basketArray.contains(item1) && basketArray.contains(item2)) {
						//add count, -1 at end since we are indexing from 0
						val k: Int = ((i - 1)*(m - (i/2D))).toInt + j - i - 1
						counts(k) += 1
					}
					// else if (!basketArray.contains(item1)) {
					// 	//skip inner loop if basket does not contain first item?
					// }
				}
			}
		}
		counts.foreach(println)
		//translate count to frequent pairs
		var frequentSets: Array[Seq[Int]] = Array()
		//TODO: Find more efficient way of computing pairs from k value
		for(i <- 1 to (m - 1)) {
			for(j <- (i + 1) to m) {
				//get k from possible pair from new index range, -1 since indexing from 0
				val k: Int = ((i - 1)*(m - (i/2D))).toInt + j - i - 1
				if(counts(k) >= s) {
					//fetch the corresponding items for the new index range
					// and add as frequent pair as support >= s
					val item1 = frequentItems(i - 1)
					val item2 = frequentItems(j - 1)
					frequentSets = frequentSets :+ Seq(item1, item2)
				}
			}
		}
		frequentSets.foreach(println)
		// var setSize = 2
		// while(setSize <= 3 || frequentSets.size == 0) {

		// 	for(freqSet <- frequentSets){

		// 		for(freqSing <- frequentItems.values) {
		// 			if(freqSet.last < freqSing){
		// 				if(isCandidateSet(freqSet, freqSing, frequentSets)) {
		// 				//add to candidate list

		// 				}
		// 			}
		// 		}
		// 	}
		// }
	}

	// def isCandidateSet(freqSet: Seq[Int], freqSing: Int, frequentSets: Array[Seq[Int]]): Boolean = {
	// 	for(i <- 0 to (freqSet.size - 1)){

	// 	}
	// }
}