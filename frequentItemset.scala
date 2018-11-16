import scala.io.Source
import util.control.Breaks._

object FrequentItemSets {

	def main(args:Array[String]){
		//The path to the file containing the data set
		val fileName: String = "dataset/miniTest.dat"

		//The size of the itemsets, should be at least 3
		val k: Int = 5
		//The support needed for frequent itemsets
		val s: Int = 1000
		//The confidence threshold for association rules
		val c: Double = 0.5

		//firstPassResults = (frequentItems, allFrequentSets)
		val firstPassResults: (Map[Int, Int], Map[Seq[Int], Int]) = aPrioriFirstPass(fileName, k, s)

		//secondPassResults = (frequentSets, allFrequentSets)
		val secondPassResults: (Array[Seq[Int]], Map[Seq[Int], Int]) = aPrioriSecondPass(firstPassResults._1, k, s, firstPassResults._2, fileName)

		//Get all frequent itemsets of size 1 to K
		val allFrequentSets: Map[Seq[Int], Int] = aPrioriKPass(secondPassResults._1, k, s, secondPassResults._2, fileName, firstPassResults._1)

		//Generate all the association rules with confidence c from all frequent sets
		val rules: List[(String, Double)] = associationRules(allFrequentSets, s, c)

		//Display the results
		println("All frequent sets (s = " + s + "):")
		allFrequentSets.filter(x => x._1.size > 1).foreach(println)
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
	def aPrioriFirstPass(fileName: String,
						 k: Int,
						 s: Int): (Map[Int, Int], Map[Seq[Int], Int]) = {

		//Define a list of all itemsets
		val f = scala.io.Source.fromFile(fileName)
		//There are 1000 unique items, init the count array to 0s.
		var counts: Array[Int] = Array.fill(1000)(0)

		//Read all baskets from the data set and count the
		//occurance of each item
		try{
			for(basket <- f.getLines) {
				for(item <- basket.split(" ")) {
					counts(item.toInt) += 1
				}
			}
		}finally{f.close()}

		//Remap all the frequent items to a new (smaller) range of
		//indices, so that a triangular array can be used for counting
		//the pairs and add frequent items to the "global" map.
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
	*  @param frequentItems: the frequent singletons and their new mapping
	*  @param k: the maximum size of the item sets
	*  @param s: the support threshold for frequent item sets
	*  @param allFreqSets: A map containing all frequent sets and their support (so far),
	*			needed for generating the association rules in a (somewhat) effecient manner
	*  @param fileName: the name of the file containing the dataset
	*  @return A tuple with an array of all frequent pairs and a "global" map for storing all
	*		 frequent sets and their support (for passing along to the next step)
	*/
	def aPrioriSecondPass(frequentItems: Map[Int, Int],
						  k: Int,
						  s: Int,
						  allFreqSets: Map[Seq[Int], Int],
						  fileName: String): (Array[Seq[Int]], Map[Seq[Int], Int]) = {

		val f = scala.io.Source.fromFile(fileName).getLines.toSeq
		//make a copy of the "global" map for adding new frequent sets
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
			//Add items to an array so that it can be examined if a pair
			//is in the basket or not.
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
	*  @param frequentItems: the frequent singletons
	*			- used for constructing candidate sets of size k + 1
	*  @return the map containing all frequent sets up to size k and their support
	*/
	def aPrioriKPass(freqPairs: Array[Seq[Int]],
				     k: Int,
					 s: Int,
					 allFreqSets: Map[Seq[Int], Int],
					 fileName: String,
					 frequentItems: Map[Int, Int]): Map[Seq[Int], Int] = {

		val f = scala.io.Source.fromFile(fileName).getLines.toSeq
		//make a copy of the "global" map for adding new frequent sets
		var allFrequentSets: Map[Seq[Int], Int] = allFreqSets
		//make a copy of the frequent pairs, this is used to keep track of
		//frequent items from the previous level (k - 1) in this function.
		var frequentSets: Array[Seq[Int]] = freqPairs

		var setSize = 3
		while(setSize <= k && frequentSets.size != 0) {
			println("k = " + setSize)
			//An array for storing the potential sets
			var candidateSets: Array[Seq[Int]] = Array()
			//Iterate over all frequent sets of size k - 1
			for(freqSet <- frequentSets){
				//iterate over all frequent singletons to generate candidate sets
				//of size k (by combining frequent sets of size k-1 and frequent singletons)
				for(freqSing <- frequentItems.values) {
					if(freqSet.last < freqSing){
						//check if all subsets of (freqSet U freqSing) are frequent
						//if so, add it as a candidate set
						if(isCandidateSet(freqSet, freqSing, frequentSets)) {
							candidateSets = candidateSets :+ (freqSet :+ freqSing)
						}
					}
				}
			}
			//A map for storing all candidate sets of size k and their support
			var candidateSetsToCount: Map[Seq[Int], Int] = Map()

			var basketCount = 0

			for(basket <- f) {
				if (basketCount % 1000 == 0){
					println("k: " + setSize + " basket nr: " + basketCount)
				}
				basketCount += 1

				//Add items to an array so that it can be examined if a k-set
				//is in the basket or not.
				var basketArray: Array[Int] = Array()
				for(item <- basket.split(" ")) {
					basketArray = basketArray :+ item.toInt
				}
				//Check if a candidate appear in a basket
				for(candidateSet <- candidateSets){
					var setLength = 0
					for(item <- candidateSet){
						if(basketArray.contains(item)){
							setLength += 1
						}
					}
					//if basket contains all items in the k-set, add it to the map
					//with its support
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
			//Add the new sets to the "global" map
			allFrequentSets = allFrequentSets ++ candidateSetsToCount
			//Extract all frequent k-sets for using when generating sets of size k + 1
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
	def isCandidateSet(freqSet: Seq[Int],
					   freqSing: Int,
					   frequentSets: Array[Seq[Int]]): Boolean = {

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
	def associationRules(frequentItemSets: Map[Seq[Int], Int],
	                     s: Int,
						 c: Double): List[(String, Double)] = {

		//A list to hold all rules with confidence atleast c
		var associationRules: List[(String, Double)] = List()
		//Iterate over all frequent sets
		for(setTuple <- frequentItemSets){
			val set: Seq[Int] = setTuple._1
			val k: Int = set.size
			//No association rules if set size < 2
			if (k > 1) {
				k match {
					case 2 => {
						//if set size = 2 there are only two possible rules
						// item1 => item2 or item2 => item1
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
						//for set size > 2 each subset must be tried
						for (i <- 0 to (set.size - 1)){
							//pick a single item from the set for the right hand side
							var rightSide: Seq[Int] = Seq(set(i))
							//keep the rest on the left hand side
							var leftSide: Seq[Int] = set.diff(rightSide)
							//get the confidence for this rule, if this is < c
							//we don't have to check further
							var confidence: Double = calculateConfidence(leftSide, rightSide, frequentItemSets)
							var index: Int = i + 1
							//if confidence >= c, and there are still possible combinations
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
	def calculateConfidence(left: Seq[Int],
							right: Seq[Int],
							frequentItemSets: Map[Seq[Int], Int]): Double = {

		val supportLeft: Double = frequentItemSets(left)
		val wholeSet: Seq[Int] = left ++ right
		val supportWhole: Double = frequentItemSets(wholeSet.sorted)
		supportWhole/supportLeft
	}

}
