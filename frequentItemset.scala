import scala.io.Source
import util.control.Breaks._

object FrequentItemSets {

	def main(args:Array[String]){

		val fileName: String = "dataset/testDat.dat"
		//fetch each basket from the data file
		var count = 0;

		for(line <- Source.fromFile(fileName).getLines) {
			println(line)
			count += 1

		}
	}
}