import scala.io.Source
import scala.util.{Try, Success, Failure}

case class Booking(
                    bookingId: String,
                    originCountry: String,
                    destinationCountry: String,
                    destinationCity: String,
                    hotelName: String,
                    bookingPrice: Double,
                    discount: Double,
                    profitMargin: Double,
                    rooms: Int
                  ) {

  lazy val netPrice: Double = bookingPrice * (1.0 - discount)
  lazy val actualProfit: Double = bookingPrice * profitMargin


  lazy val compositeName: String = s"$destinationCountry - $hotelName - $destinationCity"
}


case class HotelMetrics(
                         name: String,
                         numTransactions: Int,
                         avgPrice: Double,
                         avgProfitMargin: Double,
                         avgDiscount: Double
                       )


case class HotelScore(
                       name: String,
                       priceScore: Double,
                       profitScore: Double,
                       discountScore: Double
                     ) {

  lazy val finalScore: Double = (priceScore + profitScore + discountScore) / 3.0
}


object HotelAnalysis extends App {

  val filePath = "src/main/resources/Hotel_Dataset.csv"

  val source = Source.fromFile(filePath, "ISO-8859-1")

  val bookings: List[Booking] = try {
    source.getLines().drop(1)
      .flatMap { line =>
        if (line.trim.isEmpty) None
        else {
          val cols = line.split(",", -1).map(_.trim)
          if (cols.length < 24) None
          else {
            Try {
              Booking(
                bookingId = cols(0),
                originCountry = cols(6),
                destinationCountry = cols(9),
                destinationCity = cols(10),
                hotelName = cols(16),
                bookingPrice = cols(20).toDouble,
                discount = cols(21).replace("%", "").toDouble / 100.0,
                profitMargin = cols(23).toDouble,
                rooms = cols(15).toInt
              )
            }.toOption
          }
        }
      }.toList
  } finally {
    source.close()
  }

  println(s"✅ Successfully loaded ${bookings.size} bookings.\n")

  if (bookings.isEmpty) System.exit(0)


  val countryCounts = bookings.foldLeft(Map.empty[String, Int]) { (acc, booking) =>
    val currentCount = acc.getOrElse(booking.destinationCountry, 0)
    acc + (booking.destinationCountry -> (currentCount + 1))
  }

  val topCountry = countryCounts.maxBy(_._2)




  val hotelGroups = bookings.groupBy(_.compositeName)

  val allHotelMetrics = hotelGroups.flatMap { case (name, list) =>

    if (list.nonEmpty) {
      val count = list.size
      val totalPrice = list.map(_.bookingPrice).sum
      val totalDiscount = list.map(_.discount).sum
      val totalProfitMargin = list.map(_.profitMargin).sum

      Some(HotelMetrics(
        name = name,
        numTransactions = count,
        avgPrice = totalPrice / count.toDouble,
        avgProfitMargin = totalProfitMargin / count.toDouble,
        avgDiscount = totalDiscount / count.toDouble
      ))
    } else {
      None
    }
  }.toList


  if (allHotelMetrics.isEmpty) {
    println("❌ No valid hotel metrics could be calculated.")
    System.exit(0)
  }

  val minPrice = allHotelMetrics.minBy(_.avgPrice).avgPrice
  val maxPrice = allHotelMetrics.maxBy(_.avgPrice).avgPrice

  val minProfit = allHotelMetrics.minBy(_.avgProfitMargin).avgProfitMargin
  val maxProfit = allHotelMetrics.maxBy(_.avgProfitMargin).avgProfitMargin

  val minDiscount = allHotelMetrics.minBy(_.avgDiscount).avgDiscount
  val maxDiscount = allHotelMetrics.maxBy(_.avgDiscount).avgDiscount


  def calculateScore(value: Double, minVal: Double, maxVal: Double): Double = {

    if (maxVal == minVal) 100.0 else (1.0 - ((value - minVal) / (maxVal - minVal))) * 100.0
  }


  val scoredHotels = allHotelMetrics.map { metrics =>

    val priceScore = calculateScore(metrics.avgPrice, minPrice, maxPrice)


    val profitScore = calculateScore(metrics.avgProfitMargin, minProfit, maxProfit)


    val discountScore = if (maxDiscount == minDiscount) 100.0 else ((metrics.avgDiscount - minDiscount) / (maxDiscount - minDiscount)) * 100.0

    HotelScore(
      name = metrics.name,
      priceScore = priceScore,
      profitScore = profitScore,
      discountScore = discountScore
    )
  }


  val bestValueHotel = scoredHotels.maxBy(_.finalScore)



  // --- QUESTION 3: MOST PROFITABLE HOTEL (REVERTED TO SIMPLIFIED LOGIC)


  val profitability = hotelGroups.map { case (hotel, list) =>
    val totalVisitors = list.map(_.rooms).sum
    // IMPORTANT: This uses the profit margin of the first booking found for that location,
    // NOT the average, following the structure of the original flawed logic.
    val profitMargin = list.head.profitMargin
    val hotelProfit = totalVisitors.toDouble * profitMargin
    (hotel, hotelProfit)
  }

  val mostProfitable = profitability.maxBy(_._2)






  //Q2 PRINTING (Detailed Tables)

  println("--- NORMALIZATION RANGES (Basis for Scoring) ---")
  printf("| %-18s | %-10s | %-10s |\n", "Metric", "Min Value", "Max Value")
  println(f"| Avg Booking Price | $$${minPrice}%.2f | $$${maxPrice}%.2f |")
  println(f"| Avg Profit Margin | ${minProfit}%.4f | ${maxProfit}%.4f |")
  println(f"| Avg Discount      | ${minDiscount}%.4f | ${maxDiscount}%.4f |")
  println("----------------------------------------------\n")

  // Combine scores and metrics for detailed output
  val combinedData = scoredHotels.map { score =>
    val metrics = allHotelMetrics.find(_.name == score.name).get
    (score, metrics)
  }.sortBy(_._1.finalScore).reverse // Sort descending by FINAL SCORE

  // FUNCTION TO PRINT ALIGNED TABLE
  def printHotelTable(title: String, data: List[(HotelScore, HotelMetrics)]): Unit = {
    println(title)
    printf("| %-50s | %-5s | %-9s | %-11s | %-11s | %-11s | %-11s |\n",
      "Hotel Location/Name", "Trans", "Avg Price", "FINAL SCORE", "Price Score", "Profit Score", "Discount Score")
    println("|" + ("-" * 120) + "|")

    data.foreach { case (score, metrics) =>
      println(f"| ${metrics.name}%-50s | ${metrics.numTransactions}%-5d | $$${metrics.avgPrice}%.2f | ${score.finalScore}%.2f | ${score.priceScore}%.2f | ${score.profitScore}%.2f | ${score.discountScore}%.2f |")
    }
    println("|" + ("-" * 120) + "|")
  }

  // PRINT TOP 10 RESULTS
  printHotelTable("--- TOP 10 MOST ECONOMICAL HOTEL LOCATIONS ---", combinedData.take(10))


  printHotelTable("--- BOTTOM 5 LEAST ECONOMICAL HOTEL LOCATIONS (For Range Check) ---", combinedData.takeRight(5))

  //  Q1 PRINTING
  println(s"1. Country with the highest number of bookings:")
  println(s"   👉 ${topCountry._1} (Total Bookings: ${topCountry._2})\n")



  println("2. Most Economical Hotel (Best Overall Value Score):")
  println(s"   👉 Hotel: ${bestValueHotel.name}")
  println(f"      Price Score:   ${bestValueHotel.priceScore}%.2f")
  println(f"      Profit Score:  ${bestValueHotel.profitScore}%.2f")
  println(f"      Discount Score:${bestValueHotel.discountScore}%.2f")
  println(f"      FINAL SCORE:   ${bestValueHotel.finalScore}%.2f\n")


  // --- Q3 PRINTING ---
  println(s"3. Most Profitable Hotel (Rooms * Margin - SIMPLIFIED CALCULATION):")
  println(s"   👉 Hotel: ${mostProfitable._1}")
  println(f"      Calculated Profit Value: ${mostProfitable._2}%.2f")
}