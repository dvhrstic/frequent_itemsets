import scala.io.Source
import util.control.Breaks._

object FrequentItemSets {

	def main(args:Array[String]){

		val fileName: String = "dataset/miniTest.dat"
		//The size of the itemsets
		val k = 3
		//The support needed for frequent itemsets
		val s = 1
		//Define a list of all itemsets
		val f = scala.io.Source.fromFile(fileName)
		var counts: Array[Int] = Array.fill(1000)(0)

		try{
			for(basket <- f.getLines) {
				for(item <- basket.split(" ")) {
					counts(item.toInt) += 1
				}
			}
		}finally{f.close()}
		

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


		aPriori(frequentItems, k, s)
	}

	def aPriori(frequentItems: Map[Int, Int], k: Int, s: Int) {
		val fileName: String = "dataset/miniTest.dat"
		val f = scala.io.Source.fromFile(fileName).getLines.toSeq

		//number of frequent singletons
		val m = frequentItems.size
		//number of pairs (size of triangular array => round up)
		val numPairs: Int = Math.ceil(Math.pow(m,2)/2).toInt
		//triangular array k = (i - 1)(n - (i/2)) + j - i, counts[k] = count for pair (i, j)
		// i < j
		var counts: Array[Int] = Array.fill(numPairs)(0)

		//fetch each basket from the data file
		for(basket <- f) {
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
				}
			}
		}

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
	
		var setSize = 2
		while(setSize <= k || frequentSets.size != 0) {
			var candidateSetsToCount: Map[Seq[Int], Int] = Map()
			var candidateSets: Array[Seq[Int]] = Array()
			for(freqSet <- frequentSets){
				
				for(freqSing <- frequentItems.values) {
					if(freqSet.last < freqSing){
						if(isCandidateSet(freqSet, freqSing, frequentSets)) {
							candidateSets = candidateSets :+ (freqSet :+ freqSing)
						}
					}
				}
			}
			for(basket <- f) {
				var basketArray: Array[Int] = Array()
				for(item <- basket.split(" ")) {
					basketArray = basketArray :+ item.toInt
				}
				for(candidateSet <- candidateSets){
					var setLength = 0
					for(item <- candidateSet){
						if(basketArray.contains(item)){
							setLength += 1
						}
					}
					if(setLength == candidateSet.length){
						val candidateCount = candidateSetsToCount.get(candidateSet).getOrElse(0) 
						if(candidateCount == 0){
							candidateSetsToCount += (candidateSet ->  1) 
						}else{
							candidateSetsToCount += (candidateSet ->  (candidateCount.toInt + 1).toInt)
						}
						
					}
				}
			}

			candidateSetsToCount.foreach(println)
			frequentSets = candidateSetsToCount.filter(_._2 >= s).keys.toArray
			setSize += 1;
		}
	}	

	def isCandidateSet(freqSet: Seq[Int], freqSing: Int, frequentSets: Array[Seq[Int]]): Boolean = {
		val possibleCandidates = (freqSet :+ freqSing).combinations(freqSet.size).toList
		for (candidate <- possibleCandidates){
			if(!frequentSets.contains(candidate)){
				return false
			}
		}
		return true	
	}

}
