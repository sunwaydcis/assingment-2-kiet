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
}

object HotelAnalysis extends App {

  val filePath = "src/main/resources/Hotel_Dataset.csv"


  val source = Source.fromFile(filePath, "ISO-8859-1")

  val bookings: List[Booking] = try {
    source.getLines().drop(1) // Skip header
      .flatMap { line =>
        if (line.trim.isEmpty) None
        else {
          val cols = line.split(",", -1).map(_.trim)
          if (cols.length < 24) None // Skip malformed lines
          else {

            Try {
              Booking(
                bookingId = cols(0),
                originCountry = cols(6),
                destinationCountry = cols(9),
                destinationCity = cols(10),
                hotelName = cols(16),
                bookingPrice = cols(20).toDouble,
                // Clean data: Remove '%' and convert to decimal (e.g. "10%" -> 0.10)
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

  println(s"1. Country with the highest number of bookings:")
  println(s"   👉 ${topCountry._1} (Total Bookings: ${topCountry._2})\n")



  val economical = bookings.minBy(_.netPrice)

  println(s"2. Most Economical Hotel (Lowest Net Price):")
  println(s"   👉 Hotel: ${economical.hotelName}")
  println(s"      Booking Price: $$${economical.bookingPrice}")
  println(s"      Discount:      ${economical.discount * 100}%")
  println(f"      Net Price:     $$${economical.netPrice}%.2f")
  println(s"      Profit Margin: ${economical.profitMargin} (Context)\n")



  val hotelProfits = bookings.foldLeft(Map.empty[String, Double]) { (acc, booking) =>
    val currentProfit = acc.getOrElse(booking.hotelName, 0.0)
    acc + (booking.hotelName -> (currentProfit + booking.actualProfit))
  }

  val mostProfitable = hotelProfits.maxBy(_._2)

  println(s"3. Most Profitable Hotel:")
  println(s"   👉 Hotel: ${mostProfitable._1}")
  println(f"      Total Profit: $$${mostProfitable._2}%.2f")
}