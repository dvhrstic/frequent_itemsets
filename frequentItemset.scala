import scala.io.Source
import util.control.Breaks._

object FrequentItemSets {

	def main(args:Array[String]){

		val fileName: String = "dataset/miniTest.dat"
		//The size of the itemsets
		val k: Int = 5
		//The support needed for frequent itemsets
		val s: Int = 1
		//The confidence threshold for association rules
		val c: Double = 0.5
		//Map containg all frequent items (so far) and their support
		//var allFrequentSets: Map[Seq[Int], Int] = Map()

		//firstPassResults = (frequentItems, allFrequentSets)
		val firstPassResults: (Map[Int, Int], Map[Seq[Int], Int]) = aPrioriFirstPass(fileName, k, s)
		//secondPassResults = (frequentSets, allFrequentSets)
		val secondPassResults: (Array[Seq[Int]], Map[Seq[Int], Int]) = aPrioriSecondPass(firstPassResults._1, k, s, firstPassResults._2, fileName)
		val allFrequentSets: Map[Seq[Int], Int] = aPrioriKPass(secondPassResults._1, k, s, secondPassResults._2, fileName, firstPassResults._1)
		val rules: List[(String, Double)] = associationRules(allFrequentSets, s, c)
		println("All frequent sets (s = " + s + "):")
		allFrequentSets.foreach(println)
		println("All association rules with confidence >= " + c + ": ")
		rules.foreach(println)

	}

	/** Perform the first pass of the A-Priori algorithm, i.e
	*		find all singletons with support atleast s
	*
	*  @param fileName: the name of the file containing the dataset
	*  @param k: the maximum size of the item sets
	*  @param s: the support threshold for frequent item sets
	*  @param allFreqSets: A map containing all frequent sets and their support (so far),
	*			needed for generating the association rules in a (somewhat) effecient manner
	*  @return A tuple with the map for all frequent items and their support and a "global" map
	*			for storing all frequent sets and their support (for passing along to the next step)
	*/
	def aPrioriFirstPass(fileName: String, k: Int, s: Int): (Map[Int, Int], Map[Seq[Int], Int]) = {
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

		var item = 0
		var indexMap = 0
		var allFrequentSets: Map[Seq[Int], Int] = Map()
		var frequentItems: Map[Int, Int] = Map()
		for(i <- counts){
			if(i >= s ){
				frequentItems = frequentItems + (indexMap -> item)
				allFrequentSets = allFrequentSets + (Seq(item) -> i)
				indexMap += 1;
			}
			item += 1;
		}
		(frequentItems, allFrequentSets)
	}

	/** Perform the second pass of the A-Priori algorithm, i.e
	*		find all pairs with support atleast s
	*
	*  @param frequentItems: the frequent singletons and their support
	*  @param k: the maximum size of the item sets
	*  @param s: the support threshold for frequent item sets
	*  @param allFreqSets: A map containing all frequent sets and their support (so far),
	*			needed for generating the association rules in a (somewhat) effecient manner
	*  @param fileName: the name of the file containing the dataset
	*  @return A tuple with an array of all frequent pairs and a "global" map for storing all
	*		 frequent sets and their support (for passing along to the next step)
	*/
	def aPrioriSecondPass(frequentItems: Map[Int, Int], k: Int, s: Int,
		allFreqSets: Map[Seq[Int], Int], fileName: String): (Array[Seq[Int]], Map[Seq[Int], Int]) = {

		val f = scala.io.Source.fromFile(fileName).getLines.toSeq
		var allFrequentSets: Map[Seq[Int], Int] = allFreqSets

		//number of frequent singletons
		val m = frequentItems.size
		//number of pairs (size of triangular array => round up)
		val numPairs: Int = Math.ceil(Math.pow(m,2)/2).toInt
		//triangular array k = (i - 1)(n - (i/2)) + j - i, counts[k] = count for pair (i, j)
		// i < j
		var counts: Array[Int] = Array.fill(numPairs)(0)

		//fetch each basket from the data file
		var basketCount = 0
		for(basket <- f) {
			if(basketCount % 1000 == 0){
				println("basket nr: " + basketCount)
			}
			basketCount += 1
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
					val items = Seq(item1, item2)
					frequentSets = frequentSets :+ items
					allFrequentSets = allFrequentSets + (items -> counts(k))
				}
			}
		}
		(frequentSets, allFrequentSets)
	}

	/** Find all frequent itemsets of size k with support atleast s
	*
	*  @param freqPairs: An array with all frequent pairs
	*			- used for constructing candidate sets of size k + 1
	*  @param k: the maximum size of the item sets
	*  @param s: the support threshold for frequent item sets
	*  @param allFreqSets: A map containing all frequent sets and their support (so far),
	*			needed for generating the association rules in a (somewhat) effecient manner
	*  @param fileName: the name of the file containing the dataset
	*  @param frequentItems: the frequent singletons and their support
	*			- used for constructing candidate sets of size k + 1
	*  @return the map containing all frequent sets up to size k and their support
	*/
	def aPrioriKPass(freqPairs: Array[Seq[Int]], k: Int, s: Int,
		allFreqSets: Map[Seq[Int], Int], fileName: String,
		frequentItems: Map[Int, Int]): Map[Seq[Int], Int] = {

		val f = scala.io.Source.fromFile(fileName).getLines.toSeq
		var allFrequentSets: Map[Seq[Int], Int] = allFreqSets
		var frequentSets: Array[Seq[Int]] = freqPairs

		var setSize = 3
		while(setSize <= k && frequentSets.size != 0) {
			println("k = " + setSize)
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
			var basketCount = 0
			for(basket <- f) {
				if (basketCount % 1000 == 0){
					println("k: " + setSize + " basket nr: " + basketCount)
				}
				basketCount += 1
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

			allFrequentSets = allFrequentSets ++ candidateSetsToCount
			frequentSets = candidateSetsToCount.filter(_._2 >= s).keys.toArray
			setSize += 1;
		}

		allFrequentSets
	}

	/** Function used to see if a possible set of size k + 1 could be frequent.
	*		that is: are all subsets of size k containg these elements frequent?
	*
	*  @param freqSet: A frequent set of size k, used for (trying to) bulding 
	*			a set of size k + 1
	*  @param freqSing: A frequent singelton that will be combined with the elements
	*					from freqSet
	*  @param frequentSets: The frequent sets of size k - 1, used to check if all subsets
	*				of size k - 1 from the set (freqSet U freqSing) are frequent
	*  @return True if all subsets are frequent, false otherwise
	*/
	def isCandidateSet(freqSet: Seq[Int], freqSing: Int, frequentSets: Array[Seq[Int]]): Boolean = {
		val possibleCandidates = (freqSet :+ freqSing).combinations(freqSet.size).toList
		for (candidate <- possibleCandidates){
			if(!frequentSets.contains(candidate)){
				return false
			}
		}
		return true
	}

	/** Function used to construct all association rules with confidence
	*		atleast c
	*
	*  @param frequentItemSets: A map containing all frequent item sets with support
	*			atleast s as well as their support.
	*  @param s: the support (not used right now)
	*  @param c: the threshold for the confidence
	*  @return A list of all rules and their confidence as ("set1 => set2", confidence)
	*/
	def associationRules(frequentItemSets: Map[Seq[Int], Int], s: Int, c: Double): List[(String, Double)] = {
		var associationRules: List[(String, Double)] = List()
		for(setTuple <- frequentItemSets){
			val set: Seq[Int] = setTuple._1
			val k: Int = set.size
			if (k > 1) {
				k match {
					case 2 => {
						for(i <- 0 to 1){
							var leftElem: Seq[Int] = Seq(set(i))
							var rightElem: Seq[Int] = Seq(set(1 - i))
							var conf: Double = calculateConfidence(leftElem, rightElem, frequentItemSets)
							if(conf >= c) {
								associationRules = associationRules :+ (leftElem + " => " + rightElem, conf)
							}
						}
					}
					case _ => {

						for (i <- 0 to (set.size - 1)){
							var rightSide: Seq[Int] = Seq(set(i))
							var leftSide: Seq[Int] = set.diff(rightSide)
							var confidence: Double = calculateConfidence(leftSide, rightSide, frequentItemSets)
							var index: Int = i + 1
							while(confidence >= c && leftSide.size > 1 && index < set.size){
								associationRules = associationRules :+ (leftSide + " => " + rightSide, confidence)
								rightSide = rightSide :+ set(index)
								leftSide = set.diff(rightSide)
								confidence = calculateConfidence(leftSide, rightSide, frequentItemSets)
								index += 1
							}
						}

					}
				}

			}
		}
		associationRules
	}

	/** Function to calculate the confidence of a association rule:
	*			left => right ("left implies right")
	*
	*  @param left: The left hand set
	*  @param right: The right hand set
	*  @param frequentItemSets: A map with the support for each frequent set
	*  @return The confidence of the rule left => right
	*/
	def calculateConfidence(left: Seq[Int], right: Seq[Int], frequentItemSets: Map[Seq[Int], Int]): Double = {
		val supportLeft: Double = frequentItemSets(left)
		val wholeSet: Seq[Int] = left ++ right
		val supportWhole: Double = frequentItemSets(wholeSet.sorted)
		supportWhole/supportLeft
	}

}
